library(dplyr)
library(readr)
library(lubridate)
library(jsonlite)
library(here)

# Observations and fixed season folds -------------------------------------
out <- here("validation", "research", "mist")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
config_path <- here("research", "mist", "config.json")
config <- fromJSON(config_path)
data_path <- Sys.getenv("NGULIA_EXPERIMENT_DATA", here("data", "daily_coverage.csv"))
fold_path <- here("validation", "research", "season_folds.csv")
daily <- read_csv(data_path, show_col_types = FALSE)
folds <- read_csv(fold_path, show_col_types = FALSE)
labels <- daily |>
  filter(season <= config$last_label_season,
    mist_observation %in% c("none", "light_patchy", "good", "present_unspecified")) |>
  select(ringing_date, season, mist_observation) |>
  mutate(mist_present = as.integer(mist_observation != "none")) |>
  left_join(folds, by = "season") |>
  arrange(ringing_date)

# Forecast-compatible hourly features at Ngulia ---------------------------
archive <- Sys.getenv("NGULIA_ERA5_ARCHIVE", file.path(dirname(here()), "ngulia-dataset", "data",
  "01_raw", "weather", "era5_hourly_single_levels_timeseries",
  "era5_hourly_single_levels_timeseries_19691020_20240112.zip"))
hourly <- read_csv(unz(archive, unzip(archive, list = TRUE)$Name[[1]]), show_col_types = FALSE,
  col_types = cols(valid_time = col_datetime(), tcc = col_double(), u10 = col_double(),
    v10 = col_double(), t2m = col_double(), d2m = col_double(), .default = col_skip())) |>
  mutate(datetime_local = with_tz(valid_time, config$timezone),
    local_hour = hour(datetime_local),
    ringing_date = as.Date(datetime_local) + as.integer(local_hour >= 21),
    hour = if_else(local_hour >= 21, local_hour - 24L, local_hour),
    temperature = t2m - 273.15, dewpoint = d2m - 273.15,
    humidity = pmin(100, pmax(0, 100 * exp(17.67 * dewpoint / (dewpoint + 243.5) -
      17.67 * temperature / (temperature + 243.5)))),
    humidity_gamma = log(humidity / 100) + 17.67 * temperature / (temperature + 243.5),
    depression = pmax(0, temperature - 243.5 * humidity_gamma / (17.67 - humidity_gamma))) |>
  filter(ringing_date %in% labels$ringing_date, hour %in% config$relative_hours) |>
  select(ringing_date, hour, cloud = tcc, humidity, u = u10, v = v10, temperature, depression) |>
  arrange(ringing_date, hour)

# Common complete-case cohort and source audit ----------------------------
coverage <- hourly |> group_by(ringing_date) |>
  summarise(hours = n_distinct(hour), complete = all(complete.cases(pick(all_of(config$features)))),
    .groups = "drop")
cohort <- labels |> left_join(coverage, by = "ringing_date") |>
  mutate(included = !is.na(hours) & hours == length(config$relative_hours) & coalesce(complete, FALSE))
labels <- cohort |> filter(included) |> select(ringing_date, season, fold, mist_present, mist_observation)
hourly <- hourly |> semi_join(labels, by = "ringing_date")
means <- hourly |> group_by(ringing_date) |>
  summarise(across(all_of(config$features), mean), .groups = "drop")
parity <- hourly |> filter(hour >= 0) |> group_by(ringing_date) |>
  summarise(across(all_of(config$features), mean), .groups = "drop") |>
  left_join(daily, by = "ringing_date")
# These are scientific invariants of the cohort/window, rather than fallback behavior.
stopifnot(!anyDuplicated(labels$ringing_date), !anyNA(labels$fold),
  !anyDuplicated(hourly[c("ringing_date", "hour")]),
  nrow(hourly) == nrow(labels) * length(config$relative_hours),
  max(abs(parity$cloud - parity$total_cloud_cover_mean)) < 1e-6,
  max(abs(parity$humidity - parity$relative_humidity_mean_pct)) < 1e-6,
  max(abs(parity$u - parity$wind_u_10m_mean_ms)) < 1e-6)
write_csv(labels, file.path(out, "labels.csv"))
write_csv(hourly, file.path(out, "hourly.csv"))
write_csv(means, file.path(out, "night_means.csv"))
write_csv(cohort, file.path(out, "cohort_audit.csv"))
write_csv(tibble(source = c(data_path, archive, fold_path, config_path),
  md5 = unname(tools::md5sum(c(data_path, archive, fold_path, config_path)))),
  file.path(out, "input_manifest.csv"))
capture.output(sessionInfo(), file = file.path(out, "r_preparation_session.txt"))
cat("Prepared", nrow(labels), "labelled dates and", nrow(hourly), "hourly rows.\n")
