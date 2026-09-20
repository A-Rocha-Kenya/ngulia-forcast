library(dplyr)
library(readr)
library(mgcv)
library(cli)

# Set paths ---------------------------------------------------------------

project_dir <- normalizePath(".", mustWork = TRUE)
data_path <- file.path(project_dir, "data", "daily_coverage.csv")
model_dir <- file.path(project_dir, "model")
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

# Prepare training data ---------------------------------------------------

cli_h1("Train Ngulia forecast models")

training_data <- read_csv(
  data_path,
  show_col_types = FALSE,
  guess_max = Inf,
  col_types = cols(ringing_date = col_date())
) |>
  filter(
    ringing_happened,
    if_all(
      c(
        total_birds_ringed,
        season_day,
        moon_distance_from_new_moon,
        total_precipitation_00_08_mm,
        wind_speed_10m_mean_ms,
        temperature_2m_mean_c,
        surface_pressure_mean_hpa
      ),
      ~ !is.na(.x)
    )
  ) |>
  mutate(rain_log = log1p(total_precipitation_00_08_mm))

baseline_formula <- total_birds_ringed ~
  s(season_day, k = 12) +
  s(moon_distance_from_new_moon, k = 6)

weather_formula <- update(
  baseline_formula,
  . ~ . +
    s(rain_log, k = 6) +
    s(wind_speed_10m_mean_ms, k = 6) +
    s(temperature_2m_mean_c, k = 6) +
    s(surface_pressure_mean_hpa, k = 6)
)

# Fit and save ------------------------------------------------------------

models <- list(
  baseline = bam(baseline_formula, data = training_data, family = nb(), method = "fREML", discrete = TRUE),
  weather = bam(weather_formula, data = training_data, family = nb(), method = "fREML", discrete = TRUE)
)

weather_columns <- c(
  "rain_log",
  "wind_speed_10m_mean_ms",
  "temperature_2m_mean_c",
  "surface_pressure_mean_hpa"
)

training_bounds <- lapply(training_data[weather_columns], range)

forecast_model <- list(
  models = models,
  reference_mean = mean(fitted(models$weather)),
  training_bounds = training_bounds,
  training = list(
    n_dates = nrow(training_data),
    n_seasons = n_distinct(training_data$season),
    first_date = min(training_data$ringing_date),
    last_date = max(training_data$ringing_date)
  ),
  model_version = "0.1.0"
)

saveRDS(forecast_model, file.path(model_dir, "forecast_model.rds"))
cli_alert_success("Model trained on {nrow(training_data)} dates across {n_distinct(training_data$season)} seasons")

