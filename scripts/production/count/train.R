library(dplyr)
library(readr)
library(mgcv)
library(cli)
library(here)

# Read operated dates ------------------------------------------------------
cli_h1("Train the selected daily count model (M2)")
daily <- read_csv(here("data", "daily_coverage.csv"), show_col_types = FALSE,
  col_types = cols(ringing_date = col_date(), .default = col_guess()))
weather <- c("total_precipitation_00_08_mm", "wind_speed_10m_mean_ms",
  "temperature_2m_mean_c", "surface_pressure_mean_hpa", "total_cloud_cover_mean",
  "relative_humidity_mean_pct", "wind_u_10m_mean_ms", "wind_v_10m_mean_ms")
training <- daily |>
  filter(season >= 1977, season <= 2023, ringing_happened |
    (daily_count_status == "zero_in_daily_summary" & effort_status == "documented_operation")) |>
  mutate(bush_period = case_when(
    !season %in% 1994:1995 ~ bush_net_configuration,
    grepl("back_bush", net_sites_observed) & grepl("front_bush", net_sites_observed) ~ "mixed_bush",
    grepl("front_bush", net_sites_observed) ~ "front_bush",
    grepl("back_bush", net_sites_observed) ~ "back_bush")) |>
  filter(bush_period %in% c("back_bush", "front_bush"),
    if_all(all_of(c("total_birds_ringed", "season_day",
      "moon_distance_from_new_moon", weather)), ~ !is.na(.x))) |>
  mutate(rain_log = log1p(total_precipitation_00_08_mm),
    bush_period = factor(bush_period, levels = c("back_bush", "front_bush")))

# Fit the evaluated M2 formula -------------------------------------------
m2 <- total_birds_ringed ~ s(season_day, k = 12) +
  s(moon_distance_from_new_moon, k = 6) + s(rain_log, k = 6) +
  s(wind_speed_10m_mean_ms, k = 6) + s(temperature_2m_mean_c, k = 6) +
  s(surface_pressure_mean_hpa, k = 6) + total_cloud_cover_mean +
  relative_humidity_mean_pct + wind_u_10m_mean_ms + wind_v_10m_mean_ms +
  bush_period + s(season, bs = "cs", k = 6)
fit <- bam(m2, data = training, family = nb(), method = "fREML", discrete = TRUE)
reference_weather <- training |>
  summarise(across(all_of(c("rain_log", weather[-1])), median))

count_model <- list(fit = fit, reference_weather = reference_weather,
  historical_reference = training |> select(season_day, total_birds_ringed),
  training = list(n_dates = nrow(training), n_seasons = n_distinct(training$season),
    first_date = min(training$ringing_date), last_date = max(training$ringing_date)),
  version = "count-m2-2023-v1")
dir.create(here("model"), showWarnings = FALSE)
saveRDS(count_model, here("model", "count_m2.rds"))
cli_alert_success("Count M2 trained on {nrow(training)} dates in {n_distinct(training$season)} seasons")
