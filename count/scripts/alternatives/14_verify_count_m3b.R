library(dplyr)
library(readr)
library(mgcv)
library(here)
source(here("count", "scripts", "alternatives", "count_m3b.R"))

# Check the rolling comparison and the dates used for each live update -----
result_dir <- here("count", "intermediate-data")
data <- read_csv(file.path(result_dir, "count_annual_model_data.csv"), show_col_types = FALSE) |>
  mutate(bush_period = factor(bush_period, levels = c("back_bush", "front_bush")),
    year_id = factor(season))
forward <- read_csv(file.path(result_dir, "count_m3b_forward_predictions.csv"), show_col_types = FALSE)
updates <- read_csv(file.path(result_dir, "count_m3b_updated_predictions.csv"), show_col_types = FALSE)
old_m2 <- read_csv(file.path(result_dir, "count_annual_forward_predictions.csv"), show_col_types = FALSE) |>
  filter(model == "M2")
matched <- inner_join(filter(forward, model == "M2", horizon == 1), old_m2,
  by = c("ringing_date", "season", "model", "observed"))
stopifnot(nrow(matched) == nrow(filter(old_m2, season >= 1997)),
  max(abs(matched$predicted.x - matched$predicted.y)) < 1e-3,
  all(forward$origin < forward$season),
  all(updates$ringing_date[updates$calibration_dates > 0] >
    updates$calibration_through[updates$calibration_dates > 0]))

# Independently integrate the 2023 annual posterior and predictive mixture.
full_fit <- readRDS(here("count", "model", "count_annual_models.rds"))$M2
fit <- bam(formula(full_fit), data = filter(data, season < 2023),
  family = nb(), method = "fREML", discrete = TRUE)
prior <- m3b_prior(fit, filter(data, season < 2023))
test <- filter(data, season == 2023) |> arrange(ringing_date)
base_mean <- exp(as.numeric(predict(fit, test, type = "link")) -
  as.numeric(predict(fit, test, type = "terms", terms = "s(season)")))
theta <- fit$family$getTheta(TRUE)
verification <- list()
for (calibration_dates in c(0L, 5L, 10L)) {
  known <- seq_len(calibration_dates)
  log_likelihood_at_mean <- sum(dnbinom(test$total_birds_ringed[known],
    mu = base_mean[known] * exp(prior$mean_log_level), size = theta, log = TRUE))
  annual_density <- function(level) {
    likelihood <- vapply(level, function(value) sum(dnbinom(test$total_birds_ringed[known],
      mu = base_mean[known] * exp(value), size = theta, log = TRUE)), numeric(1))
    exp(likelihood - log_likelihood_at_mean) *
      dnorm(level, prior$mean_log_level, prior$annual_sd)
  }
  bounds <- prior$mean_log_level + c(-8, 8) * prior$annual_sd
  normalizer <- integrate(annual_density, bounds[1], bounds[2], rel.tol = 1e-9)$value
  multiplier <- integrate(function(level) exp(level) * annual_density(level),
    bounds[1], bounds[2], rel.tol = 1e-9)$value / normalizer
  saved <- filter(updates, model == "M3b updated", season == 2023,
    .data$calibration_dates == .env$calibration_dates) |> arrange(ringing_date)
  for (i in c(calibration_dates + 1L, nrow(test))) {
    row <- filter(saved, ringing_date == test$ringing_date[i])
    density <- integrate(function(level) dnbinom(test$total_birds_ringed[i],
      mu = base_mean[i] * exp(level), size = theta) * annual_density(level),
      bounds[1], bounds[2], rel.tol = 1e-9)$value / normalizer
    cdf <- vapply(c(row$catch_low - 1, row$catch_low,
      row$catch_high - 1, row$catch_high), function(count)
        integrate(function(level) pnbinom(count,
          mu = base_mean[i] * exp(level), size = theta) * annual_density(level),
          bounds[1], bounds[2], rel.tol = 1e-9)$value / normalizer, numeric(1))
    verification[[length(verification) + 1L]] <- tibble(calibration_dates,
      ringing_date = test$ringing_date[i],
      relative_mean_error = abs(row$predicted / (base_mean[i] * multiplier) - 1),
      log_density_error = abs(row$log_density - log(density)),
      low_cdf_previous = cdf[1], low_cdf = cdf[2],
      high_cdf_previous = cdf[3], high_cdf = cdf[4])
  }
}
verification <- bind_rows(verification)
stopifnot(max(verification$relative_mean_error) < 1e-5,
  max(verification$log_density_error) < 1e-5,
  all(verification$low_cdf_previous < .1), all(verification$low_cdf >= .1),
  all(verification$high_cdf_previous < .9), all(verification$high_cdf >= .9))

# The future year smooth is explicitly removed, including beyond the data.
future <- slice(test, 1)
future$season <- 2025L
future$year_id <- factor(2025L, levels = levels(data$year_id))
future$total_birds_ringed <- NULL
future_2026 <- future
future_2026$season <- 2026L
pred_2025 <- m3b_predict(full_fit, future, m3b_prior(full_fit, data))
pred_2026 <- m3b_predict(full_fit, future_2026, m3b_prior(full_fit, data))
stopifnot(abs(pred_2025$predicted - pred_2026$predicted) < 1e-8,
  is.na(pred_2025$log_density),
  abs(pred_2025$annual_multiplier - m3b_prior(full_fit, data)$expected_level) < 1e-8)
write_csv(verification, file.path(result_dir, "count_m3b_verification.csv"))
cat("Verified M2 baseline, live-date ordering, M3b integration and flat future annual prior.\n")
