library(dplyr)
library(tibble)
library(readr)
library(jsonlite)
library(cli)
library(here)

# Compare both elevation settings through the production pipeline ----------
out <- here("count", "intermediate-data")
reference <- fromJSON(here("raw-data", "weather_reference.json"))
query <- c(latitude = -3.0142286, longitude = 38.2108675,
  hourly = paste(c("temperature_2m", "relative_humidity_2m", "dew_point_2m",
    "precipitation", "surface_pressure", "cloud_cover", "cloud_cover_low",
    "wind_speed_10m", "wind_direction_10m"), collapse = ","),
  models = "ecmwf_ifs025", timezone = "Africa/Nairobi", forecast_days = 15,
  past_days = 1, wind_speed_unit = "ms")
predictions <- list()
for (scenario in c("default", "aligned")) {
  parameters <- query
  if (scenario == "aligned") parameters <- c(parameters, elevation = reference$elevation_m)
  url <- paste0("https://api.open-meteo.com/v1/ecmwf?",
    paste(names(parameters), vapply(parameters, URLencode, character(1), reserved = TRUE),
      sep = "=", collapse = "&"))
  weather_path <- file.path(out, paste0("elevation_", scenario, "_weather.json"))
  forecast_path <- file.path(out, paste0("elevation_", scenario, "_forecast.json"))
  download.file(url, weather_path, quiet = TRUE)
  status <- system2("Rscript", shQuote(here("scripts", "update_forecast.R")),
    env = c(paste0("NGULIA_WEATHER_FILE=", shQuote(weather_path)),
      paste0("NGULIA_FORECAST_OUTPUT=", shQuote(forecast_path))))
  stopifnot(status == 0)
  predictions[[scenario]] <- as_tibble(fromJSON(forecast_path)$forecast) |>
    select(date, expected_catch, mist_probability_pct, surface_pressure_mean_hpa,
      temperature_2m_mean_c, relative_humidity_mean_pct)
}
comparison <- inner_join(predictions$default, predictions$aligned, by = "date",
  suffix = c("_default", "_aligned")) |>
  mutate(catch_ratio = expected_catch_aligned / expected_catch_default,
    mist_change_percentage_points = mist_probability_pct_aligned - mist_probability_pct_default)
write_csv(comparison, file.path(out, "elevation_forecast_comparison.csv"))
print(comparison, n = Inf, width = Inf)

# Check historical API weather against the original ERA5 archive -----------
archive <- here("raw-data", "era5_hourly_single_levels_timeseries_19691020_20240112.zip")
original <- read_csv(unz(archive, unzip(archive, list = TRUE)$Name[[1]]),
  show_col_types = FALSE, col_types = cols(valid_time = col_character(),
    sp = col_double(), t2m = col_double(), .default = col_skip())) |>
  filter(valid_time >= "2023-10-20", valid_time < "2024-01-13") |>
  transmute(time = substr(gsub(" ", "T", valid_time), 1, 16),
    original_pressure = sp / 100, original_temperature = t2m - 273.15)
historical <- list()
for (scenario in c("default", "aligned")) {
  # Use the same ERA5 grid point; vary only the requested downscaling height.
  parameters <- c(latitude = reference$latitude, longitude = reference$longitude,
    start_date = "2023-10-20", end_date = "2024-01-12", models = "era5", timezone = "UTC",
    hourly = "surface_pressure,temperature_2m",
    elevation = if (scenario == "default") 951 else reference$elevation_m)
  url <- paste0("https://archive-api.open-meteo.com/v1/archive?",
    paste(names(parameters), vapply(parameters, URLencode, character(1), reserved = TRUE),
      sep = "=", collapse = "&"))
  path <- file.path(out, paste0("elevation_", scenario, "_era5.json"))
  download.file(url, path, quiet = TRUE)
  historical[[scenario]] <- as_tibble(fromJSON(path)$hourly) |>
    inner_join(original, by = "time") |>
    summarise(scenario, n_hours = n(),
      pressure_bias_hpa = mean(surface_pressure - original_pressure),
      pressure_rmse_hpa = sqrt(mean((surface_pressure - original_pressure)^2)),
      temperature_bias_c = mean(temperature_2m - original_temperature),
      temperature_rmse_c = sqrt(mean((temperature_2m - original_temperature)^2)))
}
historical <- bind_rows(historical)
write_csv(historical, file.path(out, "elevation_historical_comparison.csv"))
print(historical, width = Inf)
cli_alert_info("Historical checks assess input compatibility, not issued-forecast skill against new bird or mist observations.")
