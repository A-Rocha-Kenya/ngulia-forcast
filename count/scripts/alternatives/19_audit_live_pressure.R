library(dplyr)
library(tibble)
library(jsonlite)
library(readr)
library(mgcv)
library(cli)
library(here)

# Snapshot the published forecast -----------------------------------------
forecast_url <- "https://a-rocha-kenya.github.io/ngulia-forcast/data/forecast.json"
forecast_path <- here("count", "intermediate-data", "live_pressure_forecast.json")
download.file(forecast_url, forecast_path, quiet = TRUE)
published <- fromJSON(forecast_path)
model <- readRDS(here("count", "model", "model.rds"))

# Compare issued weather with the count training distribution --------------
weather_ranges <- bind_rows(lapply(names(model$reference_weather), function(variable) {
  values <- model$fit$model[[variable]]
  tibble(variable, minimum = min(values), median = median(values), maximum = max(values))
}))
print(weather_ranges, n = Inf)

# Isolate pressure while keeping every other fitted effect fixed -----------
reference <- model$fit$model[rep(1, nrow(published$forecast)), ]
reference$surface_pressure_mean_hpa <- model$reference_weather$surface_pressure_mean_hpa
reference_eta <- predict(model$fit, reference, type = "link", discrete = FALSE)
reference$surface_pressure_mean_hpa <- published$forecast$surface_pressure_mean_hpa
issued_eta <- predict(model$fit, reference, type = "link", discrete = FALSE)
audit <- as_tibble(published$forecast) |>
  transmute(generated_at = published$generated_at, date,
    pressure_hpa = surface_pressure_mean_hpa,
    training_min_hpa = min(model$fit$model$surface_pressure_mean_hpa),
    training_median_hpa = model$reference_weather$surface_pressure_mean_hpa,
    training_max_hpa = max(model$fit$model$surface_pressure_mean_hpa),
    published_catch = expected_catch,
    pressure_multiplier = exp(as.numeric(issued_eta - reference_eta)),
    catch_at_training_median_pressure = published_catch / pressure_multiplier)
write_csv(audit, here("count", "intermediate-data", "live_pressure_audit.csv"))
print(audit, n = Inf, width = Inf)
cli_alert_info("Median-pressure counts isolate one fitted effect; they are not calibrated forecasts.")
