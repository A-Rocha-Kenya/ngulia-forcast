library(dplyr)
library(readr)
library(mgcv)
library(here)

# Verify cohorts, forecast-feasible inputs, and marginal expected counts ----
result_dir <- here("count", "intermediate-data")
data <- read_csv(file.path(result_dir, "count_annual_model_data.csv"), show_col_types = FALSE) |>
  mutate(year_id = factor(season), bush_period = factor(bush_period, levels = c("back_bush", "front_bush")))
predictions <- read_csv(file.path(result_dir, "count_annual_predictions.csv"), show_col_types = FALSE)
fits <- readRDS(here("count", "model", "count_annual_models.rds"))
stopifnot(!grepl("cloud_base", paste(deparse(formula(fits$M2)), collapse = "")),
  all(predictions |> count(ringing_date) |> pull(n) == 4),
  max(abs(filter(predictions, model == "M3a")$predicted /
    filter(predictions, model == "M3a")$typical_year_prediction -
    exp(filter(predictions, model == "M3a")$annual_sd^2 / 2))) < 1e-10)
audit <- read_csv(file.path(result_dir, "count_annual_training_audit.csv"), show_col_types = FALSE)
stopifnot(all(audit$train_dates + audit$test_dates == nrow(data)),
  all(filter(audit, evaluation == "whole-season")$shared_seasons == 0),
  all(filter(audit, evaluation == "within-year")$shared_seasons == n_distinct(data$season)))

# Independently integrate a refitted 2023 forecast, without Gaussian quadrature.
fit <- bam(formula(fits$M3a), data = filter(data, season < 2023),
  family = nb(), method = "fREML", discrete = TRUE)
test <- filter(data, season == 2023) |> arrange(ringing_date)
newdata <- test
newdata$year_id <- factor(levels(fit$model$year_id)[1], levels = levels(fit$model$year_id))
mu <- as.numeric(predict(fit, newdata, type = "response", exclude = "s(year_id)"))
theta <- fit$family$getTheta(TRUE)
sd <- sqrt(fit$sig2 / fit$sp["s(year_id)"])
forward <- read_csv(file.path(result_dir, "count_annual_forward_predictions.csv"), show_col_types = FALSE) |>
  filter(model == "M3a", season == 2023) |> arrange(ringing_date)
stopifnot(max(abs(forward$predicted - mu * exp(sd^2 / 2))) < 1e-7)

# Check posterior annual means, likelihoods and integer interval quantiles.
updated <- read_csv(file.path(result_dir, "count_annual_updated_predictions.csv"), show_col_types = FALSE)
verification <- list()
for (calibration_dates in c(0L, 5L, 10L)) {
  calibration_index <- seq_len(calibration_dates)
  reference_log_likelihood <- sum(dnbinom(test$total_birds_ringed[calibration_index],
    mu = mu[calibration_index], size = theta, log = TRUE))
  # This density is reused for mean, likelihood and CDF integration.
  annual_density <- function(b) {
    log_likelihood <- vapply(b, function(value) sum(dnbinom(test$total_birds_ringed[calibration_index],
      mu = mu[calibration_index] * exp(value), size = theta, log = TRUE)), numeric(1))
    exp(log_likelihood - reference_log_likelihood) * dnorm(b, sd = sd)
  }
  normalizer <- integrate(annual_density, -8 * sd, 8 * sd, rel.tol = 1e-9)$value
  multiplier <- integrate(function(b) exp(b) * annual_density(b),
    -8 * sd, 8 * sd, rel.tol = 1e-9)$value / normalizer
  saved <- filter(updated, model == "M3a", season == 2023, .data$calibration_dates == .env$calibration_dates) |>
    arrange(ringing_date)
  for (i in c(calibration_dates + 1L, nrow(test))) {
    row <- filter(saved, ringing_date == test$ringing_date[i])
    density <- integrate(function(b) dnbinom(test$total_birds_ringed[i],
      mu = mu[i] * exp(b), size = theta) * annual_density(b),
      -8 * sd, 8 * sd, rel.tol = 1e-9)$value / normalizer
    cdf <- vapply(c(row$catch_low - 1, row$catch_low, row$catch_high - 1, row$catch_high),
      function(count) integrate(function(b) pnbinom(count, mu = mu[i] * exp(b), size = theta) * annual_density(b),
        -8 * sd, 8 * sd, rel.tol = 1e-9)$value / normalizer, numeric(1))
    verification[[length(verification) + 1]] <- tibble(calibration_dates = calibration_dates,
      ringing_date = test$ringing_date[i], relative_mean_error = abs(row$predicted / (mu[i] * multiplier) - 1),
      log_density_error = abs(row$log_density - log(density)),
      low_cdf_previous = cdf[1], low_cdf = cdf[2], high_cdf_previous = cdf[3], high_cdf = cdf[4])
  }
}
verification <- bind_rows(verification)
stopifnot(max(verification$relative_mean_error) < 1e-5, max(verification$log_density_error) < 1e-5,
  all(verification$low_cdf_previous < .1), all(verification$low_cdf >= .1),
  all(verification$high_cdf_previous < .9), all(verification$high_cdf >= .9))
write_csv(verification, file.path(result_dir, "count_annual_verification.csv"))
print(verification, width = Inf)
cat("Verified shared cohorts, feasible inputs, and integrated M3a forecasts/updates.\n")
