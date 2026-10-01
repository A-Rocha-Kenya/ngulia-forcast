library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(jsonlite)
library(readr)
library(cli)
library(here)

# Set paths and calendar ---------------------------------------------------
cli_h1("Update Ngulia count and mist forecast")
count_model <- readRDS(here("count", "model", "model.rds"))
output_path <- here("site", "data", "forecast.json")
local_timezone <- "Africa/Nairobi"
today_local <- as.Date(Sys.time(), tz = local_timezone)

# Use the same prediction implementation as the browser sandbox ------------
predict_shared <- function(mode, input) {
  input_path <- tempfile(fileext = ".json")
  result_path <- tempfile(fileext = ".json")
  write_json(input, input_path, dataframe = "rows", auto_unbox = TRUE, digits = 16)
  status <- system2(Sys.getenv("NGULIA_NODE", "node"),
    c(shQuote(here("scripts", "predict.mjs")), mode, shQuote(input_path), shQuote(result_path)))
  stopifnot(status == 0)
  result <- fromJSON(result_path)
  unlink(c(input_path, result_path))
  result
}

add_calendar <- function(data) {
  bind_cols(data, as_tibble(predict_shared("calendar", data |> select(date)))) |>
    mutate(season_start = make_date(season, 10L, 20L),
      season_end = make_date(season + 1L, 1L, 12L),
      in_season = date >= season_start & date <= season_end,
      model_season = pmin(season, 2023),
      bush_period = factor("front_bush", levels = c("back_bush", "front_bush")))
}
next_season_start <- function(today) {
  this_year <- make_date(year(today), 10L, 20L)
  this_season_end <- make_date(year(today), 1L, 12L)
  if (today <= this_season_end) make_date(year(today) - 1L, 10L, 20L) else
    if (today <= this_year) this_year else make_date(year(today) + 1L, 10L, 20L)
}

# Fetch one issued ECMWF hourly forecast for both models ------------------
query <- c(latitude = -3.0142286, longitude = 38.2108675,
  hourly = paste(c("temperature_2m", "relative_humidity_2m", "dew_point_2m",
    "precipitation", "surface_pressure", "cloud_cover", "cloud_cover_low",
    "wind_speed_10m", "wind_direction_10m"), collapse = ","),
  models = "ecmwf_ifs025", timezone = local_timezone, forecast_days = 15,
  past_days = 1, wind_speed_unit = "ms")
api_url <- paste0("https://api.open-meteo.com/v1/ecmwf?",
  paste(names(query), vapply(query, URLencode, character(1), reserved = TRUE),
    sep = "=", collapse = "&"))
response <- fromJSON(api_url, simplifyVector = TRUE)
hourly <- as_tibble(response$hourly) |>
  mutate(datetime = ymd_hm(time, tz = local_timezone),
    weather_valid_date = as.Date(datetime, tz = local_timezone), hour = hour(datetime),
    cloud = cloud_cover / 100, humidity = relative_humidity_2m,
    u = -wind_speed_10m * sin(wind_direction_10m * pi / 180),
    v = -wind_speed_10m * cos(wind_direction_10m * pi / 180),
    temperature = temperature_2m,
    depression = pmax(0, temperature_2m - dew_point_2m))

# Aggregate count inputs over 00:00–08:00 --------------------------------
daily_weather <- hourly |> filter(hour %in% 0:8) |>
  group_by(weather_valid_date) |>
  summarise(n_hours = n(),
    total_precipitation_00_08_mm = sum(precipitation),
    wind_speed_10m_mean_ms = mean(wind_speed_10m),
    wind_direction_10m_mean_deg = mean(wind_direction_10m),
    temperature_2m_mean_c = mean(temperature),
    relative_humidity_mean_pct = mean(humidity),
    surface_pressure_mean_hpa = mean(surface_pressure),
    total_cloud_cover_mean = mean(cloud),
    cloud_cover_mean_pct = mean(cloud_cover),
    low_cloud_cover_mean_pct = mean(cloud_cover_low),
    wind_u_10m_mean_ms = mean(u), wind_v_10m_mean_ms = mean(v), .groups = "drop") |>
  filter(weather_valid_date >= today_local, n_hours == 9) |>
  slice_head(n = 15) |>
  mutate(date = weather_valid_date,
    rain_log = log1p(total_precipitation_00_08_mm))

# Apply the retained 21:00–08:00 mist CNN --------------------------------
mist_hours <- hourly |>
  filter(hour %in% c(21:23, 0:8)) |>
  mutate(mist_date = weather_valid_date + as.integer(hour >= 21),
    relative_hour = if_else(hour >= 21, hour - 24L, hour)) |>
  group_by(mist_date) |> filter(n() == 12) |> ungroup() |>
  semi_join(daily_weather, by = c("mist_date" = "weather_valid_date")) |>
  arrange(mist_date, relative_hour) |>
  select(weather_valid_date = mist_date, hour = relative_hour,
    cloud, humidity, u, v, temperature, depression)
daily_weather <- semi_join(daily_weather, mist_hours, by = "weather_valid_date")
stopifnot(nrow(mist_hours) == 12 * nrow(daily_weather),
  all(complete.cases(mist_hours)))
mist_input <- lapply(split(mist_hours, mist_hours$weather_valid_date), function(night)
  list(weather_valid_date = as.character(night$weather_valid_date[[1]]),
    hours = as.matrix(night |> select(cloud, humidity, u, v, temperature, depression))))
mist <- as_tibble(predict_shared("mist", unname(mist_input))) |>
  mutate(weather_valid_date = as.Date(weather_valid_date))
daily_weather <- left_join(daily_weather, mist, by = "weather_valid_date")

# Predict conditional catch with fixed front-bush layout ------------------
daily_weather <- add_calendar(daily_weather)
prediction_data <- daily_weather |> mutate(season = model_season)
count_predictions <- predict_shared("count", prediction_data)
expected <- count_predictions$expected_catch
reference_data <- prediction_data
reference_data[names(count_model$reference_weather)] <- count_model$reference_weather[
  rep(1, nrow(reference_data)), ]
baseline <- predict_shared("count", reference_data)$expected_catch
reference_mean <- mean(fitted(count_model$fit))
historical_percentile <- vapply(seq_len(nrow(prediction_data)), function(i) {
  reference <- count_model$historical_reference |>
    filter(abs(season_day - prediction_data$season_day[[i]]) <= 7)
  100 * mean(reference$total_birds_ringed <= expected[[i]])
}, numeric(1))

daily_predictions <- daily_weather |>
  mutate(baseline_index = 100 * baseline / reference_mean,
    opportunity_index = 100 * expected / reference_mean,
    expected_catch = expected,
    catch_low = count_predictions$catch_low,
    catch_high = count_predictions$catch_high,
    historical_percentile = historical_percentile,
    weather_adjustment_pct = 100 * (expected / baseline - 1),
    forecast_lead_days = as.integer(date - min(date)),
    reliability = case_when(forecast_lead_days <= 2 ~ "higher",
      forecast_lead_days <= 6 ~ "moderate", TRUE ~ "lower")) |>
  filter(in_season) |>
  select(date, weather_valid_date, opportunity_index, baseline_index,
    expected_catch, catch_low, catch_high, historical_percentile,
    mist_probability_pct, weather_adjustment_pct, forecast_lead_days,
    reliability, moon_distance_from_new_moon, moon_days_from_new_moon,
    total_precipitation_00_08_mm, wind_speed_10m_mean_ms,
    wind_direction_10m_mean_deg, temperature_2m_mean_c,
    relative_humidity_mean_pct, surface_pressure_mean_hpa,
    cloud_cover_mean_pct, low_cloud_cover_mean_pct) |>
  mutate(across(where(is.numeric), ~ round(.x, 1)))

# Build the calendar and reference-weather outlook ------------------------
upcoming_start <- next_season_start(today_local)
season_dates <- tibble(date = seq(upcoming_start,
  make_date(year(upcoming_start) + 1L, 1L, 12L), by = "day")) |>
  add_calendar() |> mutate(season = model_season)
season_dates[names(count_model$reference_weather)] <- count_model$reference_weather[
  rep(1, nrow(season_dates)), ]
season_outlook <- season_dates |>
  mutate(baseline_expected_catch = predict_shared("count", season_dates)$expected_catch) |>
  select(date, baseline_expected_catch, moon_distance_from_new_moon,
    moon_days_from_new_moon) |>
  left_join(daily_predictions |> select(date, expected_catch, catch_low, catch_high),
    by = "date") |>
  mutate(across(where(is.numeric), ~ round(.x, 1)))

# Publish one count and one mist model ------------------------------------
payload <- list(generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  demo_mode = FALSE,
  source = list(provider = "Open-Meteo", model = "ECMWF IFS 0.25°",
    endpoint = "https://api.open-meteo.com/v1/ecmwf",
    latitude = -3.0142286, longitude = 38.2108675, timezone = local_timezone),
  model = c(count_model$training, list(version = "count-m2+mist-cnn-2023-v1",
    count = count_model$version, mist = "mist-cnn-2013-v1",
    response = "Catch conditional on operation and front-bush layout",
    reference_index = 100, year_trend_after_2023 = "held at 2023 level")),
  status = list(in_season = any(daily_weather$in_season),
    next_season_start = format(next_season_start(today_local), "%Y-%m-%d"),
    forecast_dates_in_season = nrow(daily_predictions)),
  forecast = daily_predictions, season_outlook = season_outlook)
write_json(payload, output_path, pretty = TRUE, auto_unbox = TRUE,
  na = "null", dataframe = "rows", digits = 4)
cli_alert_success("Wrote {nrow(daily_predictions)} count and mist forecast dates to {output_path}")
