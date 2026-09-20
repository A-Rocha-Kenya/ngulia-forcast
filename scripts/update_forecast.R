library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(jsonlite)
library(nnet)
library(cli)

# Set paths ---------------------------------------------------------------

project_dir <- normalizePath(".", mustWork = TRUE)
model_path <- file.path(project_dir, "model", "forecast_model.rds")
output_dir <- file.path(project_dir, "site", "data")
demo_mode <- identical(Sys.getenv("NGULIA_DEMO"), "1")
output_filename <- if (demo_mode) "forecast.demo.json" else "forecast.json"
output_path <- file.path(output_dir, output_filename)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

latitude <- -3.0142286
longitude <- 38.2108675
local_timezone <- "Africa/Nairobi"
synodic_month_days <- 29.530588853
reference_new_moon <- ymd_hms("2000-01-06 18:14:00", tz = "UTC")
today_local <- as.Date(with_tz(Sys.time(), local_timezone))

# Helpers -----------------------------------------------------------------

season_from_date <- function(date) if_else(month(date) >= 6, year(date), year(date) - 1L)

add_calendar_covariates <- function(data) {
  data |>
    mutate(
      season = season_from_date(date),
      season_start = make_date(season, 10L, 20L),
      season_end = make_date(season + 1L, 1L, 12L),
      season_day = as.integer(date - season_start) + 1L,
      moon_age_days = as.numeric(difftime(as_datetime(date, tz = "UTC") + hours(12), reference_new_moon, units = "days")) %%
        synodic_month_days,
      moon_days_from_new_moon = if_else(
        moon_age_days <= synodic_month_days / 2,
        moon_age_days,
        moon_age_days - synodic_month_days
      ),
      moon_distance_from_new_moon = abs(as.integer(round(moon_days_from_new_moon))),
      in_season = date >= season_start & date <= season_end
    )
}

next_season_start <- function(today) {
  this_year <- make_date(year(today), 10L, 20L)
  this_season_end <- make_date(year(today), 1L, 12L)
  if (today <= this_season_end) make_date(year(today) - 1L, 10L, 20L) else if (today <= this_year) this_year else make_date(year(today) + 1L, 10L, 20L)
}

clamp_weather <- function(data, bounds) {
  for (column in names(bounds)) {
    data[[column]] <- pmin(pmax(data[[column]], bounds[[column]][1]), bounds[[column]][2])
  }
  data
}

round_numeric <- function(data, digits = 1) {
  mutate(data, across(where(is.numeric), ~ round(.x, digits)))
}

# Fetch forecast ----------------------------------------------------------

cli_h1("Update Ngulia live forecast")
forecast_model <- readRDS(model_path)

query <- c(
  latitude = latitude,
  longitude = longitude,
  hourly = paste(
    c(
      "temperature_2m",
      "relative_humidity_2m",
      "precipitation",
      "surface_pressure",
      "cloud_cover",
      "cloud_cover_low",
      "wind_speed_10m",
      "wind_direction_10m"
    ),
    collapse = ","
  ),
  models = "ecmwf_ifs025",
  timezone = local_timezone,
  forecast_days = 15,
  wind_speed_unit = "ms"
)

api_url <- paste0(
  "https://api.open-meteo.com/v1/ecmwf?",
  paste(names(query), vapply(query, URLencode, character(1), reserved = TRUE), sep = "=", collapse = "&")
)

weather_response <- fromJSON(api_url, simplifyVector = TRUE)
hourly <- as_tibble(weather_response$hourly) |>
  mutate(
    datetime = ymd_hm(time, tz = local_timezone),
    date = as.Date(datetime),
    hour = hour(datetime)
  )

daily_weather <- hourly |>
  filter(hour >= 0, hour <= 8) |>
  group_by(date) |>
  summarise(
    n_forecast_hours = n(),
    total_precipitation_00_08_mm = sum(precipitation, na.rm = TRUE),
    wind_speed_10m_mean_ms = mean(wind_speed_10m, na.rm = TRUE),
    wind_direction_10m_mean_deg = mean(wind_direction_10m, na.rm = TRUE),
    temperature_2m_mean_c = mean(temperature_2m, na.rm = TRUE),
    relative_humidity_mean_pct = mean(relative_humidity_2m, na.rm = TRUE),
    surface_pressure_mean_hpa = mean(surface_pressure, na.rm = TRUE),
    cloud_cover_mean_pct = mean(cloud_cover, na.rm = TRUE),
    low_cloud_cover_mean_pct = mean(cloud_cover_low, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(date >= today_local) |>
  slice_head(n = 15) |>
  mutate(
    weather_valid_date = date,
    rain_log = log1p(total_precipitation_00_08_mm)
  )

if (demo_mode) {
  demo_start <- ymd(Sys.getenv("NGULIA_DEMO_START", "2026-11-12"))
  daily_weather <- daily_weather |>
    mutate(date = demo_start + row_number() - 1L)
}

daily_weather <- add_calendar_covariates(daily_weather)

daily_weather <- daily_weather |>
  mutate(
    cloud_cover_fraction = cloud_cover_mean_pct / 100,
    wind_u_10m_mean_ms = -wind_speed_10m_mean_ms * sin(wind_direction_10m_mean_deg * pi / 180)
  )

# Predict opportunity ----------------------------------------------------

prediction_data <- clamp_weather(daily_weather, forecast_model$training_bounds)
baseline_prediction <- predict(forecast_model$models$baseline, newdata = prediction_data, type = "response")
weather_prediction <- predict(forecast_model$models$weather, newdata = prediction_data, type = "response")

if (demo_mode) {
  weather_prediction <- weather_prediction * mean(baseline_prediction) / mean(weather_prediction)
}

mist_probability <- as.matrix(predict(forecast_model$mist_model, newdata = prediction_data, type = "probs"))
mist_probability_combined <- 100 * (mist_probability[, "light_patchy"] + mist_probability[, "good"])
catch_low <- qnbinom(0.1, mu = weather_prediction, size = forecast_model$negative_binomial_size)
catch_high <- qnbinom(0.9, mu = weather_prediction, size = forecast_model$negative_binomial_size)

historical_percentile <- vapply(seq_len(nrow(prediction_data)), function(i) {
  reference <- forecast_model$historical_reference |>
    filter(abs(season_day - prediction_data$season_day[[i]]) <= 7)
  100 * mean(reference$total_birds_ringed <= weather_prediction[[i]])
}, numeric(1))

daily_predictions <- prediction_data |>
  mutate(
    baseline_index = 100 * baseline_prediction / forecast_model$reference_mean,
    opportunity_index = 100 * weather_prediction / forecast_model$reference_mean,
    expected_catch = weather_prediction,
    catch_low = catch_low,
    catch_high = catch_high,
    historical_percentile = historical_percentile,
    mist_probability_pct = mist_probability_combined,
    weather_adjustment_pct = 100 * (weather_prediction / baseline_prediction - 1),
    forecast_lead_days = as.integer(date - min(date)),
    reliability = case_when(
      forecast_lead_days <= 2 ~ "higher",
      forecast_lead_days <= 6 ~ "moderate",
      TRUE ~ "lower"
    )
  ) |>
  filter(in_season) |>
  select(
    date,
    weather_valid_date,
    opportunity_index,
    baseline_index,
    expected_catch,
    catch_low,
    catch_high,
    historical_percentile,
    mist_probability_pct,
    weather_adjustment_pct,
    forecast_lead_days,
    reliability,
    moon_distance_from_new_moon,
    moon_days_from_new_moon,
    total_precipitation_00_08_mm,
    wind_speed_10m_mean_ms,
    wind_direction_10m_mean_deg,
    temperature_2m_mean_c,
    relative_humidity_mean_pct,
    surface_pressure_mean_hpa,
    cloud_cover_mean_pct,
    low_cloud_cover_mean_pct
  ) |>
  round_numeric()

# Build whole-season outlook ---------------------------------------------

upcoming_start <- next_season_start(today_local)
season_dates <- tibble(date = seq(upcoming_start, make_date(year(upcoming_start) + 1L, 1L, 12L), by = "day")) |>
  add_calendar_covariates()

season_baseline_prediction <- predict(forecast_model$models$baseline, newdata = season_dates, type = "response")
season_outlook <- season_dates |>
  mutate(baseline_expected_catch = season_baseline_prediction) |>
  select(date, baseline_expected_catch, moon_distance_from_new_moon, moon_days_from_new_moon) |>
  left_join(
    daily_predictions |>
      select(date, expected_catch, catch_low, catch_high),
    by = "date"
  ) |>
  round_numeric()

# Write site data ---------------------------------------------------------

payload <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  demo_mode = demo_mode,
  source = list(
    provider = "Open-Meteo",
    model = "ECMWF IFS 0.25°",
    endpoint = "https://api.open-meteo.com/v1/ecmwf",
    latitude = latitude,
    longitude = longitude,
    timezone = local_timezone
  ),
  model = c(
    forecast_model$training,
    list(
      version = forecast_model$model_version,
      response = "Relative positive-catch opportunity conditional on operation",
      reference_index = 100
    )
  ),
  status = list(
    in_season = any(daily_weather$in_season),
    next_season_start = format(next_season_start(today_local), "%Y-%m-%d"),
    forecast_dates_in_season = nrow(daily_predictions)
  ),
  forecast = daily_predictions,
  season_outlook = season_outlook
)

write_json(payload, output_path, pretty = TRUE, auto_unbox = TRUE, na = "null", dataframe = "rows", digits = 4)
if (demo_mode) {
  cli_alert_info("Demo dates use live weather mapped from {min(daily_weather$weather_valid_date)} to {max(daily_weather$weather_valid_date)}")
}
cli_alert_success("Wrote {nrow(daily_predictions)} in-season forecast dates to {output_path}")
