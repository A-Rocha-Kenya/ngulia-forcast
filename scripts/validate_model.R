library(dplyr)
library(readr)
library(mgcv)
library(tibble)
library(cli)

# Set paths ---------------------------------------------------------------

project_dir <- normalizePath(".", mustWork = TRUE)
data_path <- file.path(project_dir, "data", "daily_coverage.csv")
validation_dir <- file.path(project_dir, "validation")
dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

# Prepare data ------------------------------------------------------------

model_data <- read_csv(
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

formulas <- list(
  `Date + moon` = total_birds_ringed ~
    s(season_day, k = 12) +
    s(moon_distance_from_new_moon, k = 6),
  `Date + moon + weather` = total_birds_ringed ~
    s(season_day, k = 12) +
    s(moon_distance_from_new_moon, k = 6) +
    s(rain_log, k = 6) +
    s(wind_speed_10m_mean_ms, k = 6) +
    s(temperature_2m_mean_c, k = 6) +
    s(surface_pressure_mean_hpa, k = 6)
)

# Validate with seasons held out -----------------------------------------

set.seed(73)
season_folds <- tibble(season = sort(unique(model_data$season))) |>
  mutate(fold = sample(rep(1:5, length.out = n())))

model_data <- left_join(model_data, season_folds, by = "season")

prediction <- setNames(lapply(formulas, function(x) rep(NA_real_, nrow(model_data))), names(formulas))
mean_prediction <- rep(NA_real_, nrow(model_data))

for (fold in 1:5) {
  training <- filter(model_data, .data$fold != .env$fold)
  testing <- filter(model_data, .data$fold == .env$fold)
  rows <- model_data$fold == fold
  mean_prediction[rows] <- mean(training$total_birds_ringed)

  for (model_name in names(formulas)) {
    fit <- bam(formulas[[model_name]], data = training, family = nb(), method = "fREML", discrete = TRUE)
    prediction[[model_name]][rows] <- predict(fit, newdata = testing, type = "response")
  }
}

poisson_deviance <- function(observed, expected) {
  2 * sum(if_else(observed == 0, expected, observed * log(observed / expected) - observed + expected))
}

validation <- bind_rows(lapply(names(prediction), function(model_name) {
  predicted <- prediction[[model_name]]
  selected <- predicted >= quantile(predicted, 0.8)
  observed_high <- model_data$total_birds_ringed >= quantile(model_data$total_birds_ringed, 0.8)

  tibble(
    model = model_name,
    n_dates = nrow(model_data),
    n_seasons = n_distinct(model_data$season),
    cv_deviance_reduction = 1 - poisson_deviance(model_data$total_birds_ringed, predicted) /
      poisson_deviance(model_data$total_birds_ringed, mean_prediction),
    log_rmse = sqrt(mean((log1p(model_data$total_birds_ringed) - log1p(predicted))^2)),
    log_mae = mean(abs(log1p(model_data$total_birds_ringed) - log1p(predicted))),
    spearman_correlation = cor(model_data$total_birds_ringed, predicted, method = "spearman"),
    top_quintile_share_of_catch = sum(model_data$total_birds_ringed[selected]) / sum(model_data$total_birds_ringed),
    top_quintile_recall = sum(selected & observed_high) / sum(observed_high)
  )
}))

write_csv(validation, file.path(validation_dir, "forecast_model_cv.csv"))
cli_alert_success("Wrote season-blocked validation results")

