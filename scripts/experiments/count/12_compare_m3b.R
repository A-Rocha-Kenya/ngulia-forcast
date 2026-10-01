library(dplyr)
library(readr)
library(tidyr)
library(mgcv)
library(here)
source(here("scripts", "experiments", "count", "count_m3b.R"))

# Shared cohort and the existing M2 weather/calendar response --------------
result_dir <- here("validation", "research")
data <- read_csv(file.path(result_dir, "count_annual_model_data.csv"), show_col_types = FALSE) |>
  mutate(bush_period = factor(bush_period, levels = c("back_bush", "front_bush")),
    year_id = factor(season))
m2_formula <- formula(readRDS(file.path(result_dir, "count_annual_models.rds"))$M2)

m2_predict <- function(fit, test) {
  predicted <- as.numeric(predict(fit, test, type = "response"))
  theta <- fit$family$getTheta(TRUE)
  tibble(predicted = predicted,
    catch_low = qnbinom(0.1, mu = predicted, size = theta),
    catch_high = qnbinom(0.9, mu = predicted, size = theta),
    log_density = if ("total_birds_ringed" %in% names(test))
      dnbinom(test$total_birds_ringed, mu = predicted, size = theta, log = TRUE) else NA_real_,
    annual_multiplier = NA_real_, annual_log_level = NA_real_, annual_sd = NA_real_)
}

# Rolling origins compare one-, two- and three-season forecast horizons -----
forward <- list()
updates <- list()
recent_levels <- list()
for (origin in 1996:2022) {
  train <- filter(data, season <= origin)
  fit <- bam(m2_formula, data = train, family = nb(), method = "fREML", discrete = TRUE)
  prior <- m3b_prior(fit, train)
  recent_levels[[length(recent_levels) + 1]] <- mutate(prior$recent_levels, origin = origin)
  for (horizon in 1:3) {
    target_year <- origin + horizon
    if (target_year > 2023) next
    test <- filter(data, season == target_year) |> arrange(ringing_date)
    for (model_name in c("M2", "M3b")) {
      prediction <- if (model_name == "M2") m2_predict(fit, test) else m3b_predict(fit, test, prior)
      forward[[length(forward) + 1]] <- bind_cols(
        test |> select(ringing_date, season) |> mutate(model = model_name,
          origin = origin, horizon = horizon, observed = test$total_birds_ringed), prediction)
    }
    if (horizon != 1) next
    m2_prior <- m2_predict(fit, test)
    m3b_forecast <- m3b_predict(fit, test, prior)
    for (calibration_dates in 0:(nrow(test) - 1L)) {
      calibration <- if (calibration_dates == 0) NULL else slice_head(test, n = calibration_dates)
      remaining <- slice(test, (calibration_dates + 1):nrow(test))
      for (model_name in c("M2", "M3b prior", "M3b updated")) {
        prediction <- switch(model_name,
          "M2" = slice(m2_prior, (calibration_dates + 1):nrow(test)),
          "M3b prior" = slice(m3b_forecast, (calibration_dates + 1):nrow(test)),
          "M3b updated" = m3b_predict(fit, remaining, prior, calibration))
        updates[[length(updates) + 1]] <- bind_cols(
          remaining |> select(ringing_date, season) |> mutate(model = model_name,
            origin = origin, calibration_dates = calibration_dates,
            calibration_through = if (is.null(calibration)) as.Date(NA) else max(calibration$ringing_date),
            observed = remaining$total_birds_ringed), prediction)
      }
    }
  }
  cat("M3b rolling origin:", origin, "\n")
}
forward <- bind_rows(forward)
updates <- bind_rows(updates)
recent_levels <- bind_rows(recent_levels)

# Same date-level scores as the annual comparison --------------------------
score_count <- function(x) {
  annual <- x |> group_by(season) |>
    summarise(annual_log_error = log(mean(predicted) / mean(observed)), .groups = "drop")
  x |> summarise(dates = n(), seasons = n_distinct(season),
    mean_poisson_deviance = mean(2 * if_else(observed == 0, predicted,
      observed * log(observed / predicted) - observed + predicted)),
    log_rmse = sqrt(mean((log1p(observed) - log1p(predicted))^2)),
    annual_log_rmse = sqrt(mean(annual$annual_log_error^2)),
    observed_mean = mean(observed), predicted_mean = mean(predicted),
    mean_predictive_log_loss = -mean(log_density),
    interval_80_coverage = mean(observed >= catch_low & observed <= catch_high),
    interval_80_width = mean(catch_high - catch_low), .groups = "drop")
}
forward_scores <- forward |>
  mutate(period = if_else(season >= 2014, "2014–2023", "1997–2013")) |>
  group_by(model, horizon, period) |> group_modify(~ score_count(.x)) |> ungroup()
update_scores <- updates |> group_by(model, calibration_dates) |>
  group_modify(~ score_count(.x)) |> ungroup()

# Paired season bootstrap; comparisons are exploratory after model development.
paired <- list()
for (horizon in 1:3) {
  for (period in c("1997–2013", "2014–2023")) {
    evaluated <- forward |>
      filter(.data$horizon == .env$horizon,
        if (period == "2014–2023") season >= 2014 else season < 2014) |>
      mutate(loss = 2 * if_else(observed == 0, predicted,
        observed * log(observed / predicted) - observed + predicted))
    difference <- evaluated |> filter(model == "M2") |>
      select(origin, ringing_date, season, baseline = loss) |>
      inner_join(evaluated |> filter(model == "M3b") |>
        select(origin, ringing_date, candidate = loss), by = c("origin", "ringing_date")) |>
      group_by(season) |> summarise(delta = sum(candidate - baseline), dates = n(), .groups = "drop")
    set.seed(73)
    draws <- replicate(2000, {i <- sample(seq_len(nrow(difference)), replace = TRUE)
      sum(difference$delta[i]) / sum(difference$dates[i])})
    paired[[length(paired) + 1]] <- tibble(horizon = horizon, period = period,
      difference = sum(difference$delta) / sum(difference$dates),
      low = quantile(draws, .025), high = quantile(draws, .975))
  }
}
paired <- bind_rows(paired)

# Full-history artifact for future weather rows and supplied current counts.
full_fit <- readRDS(file.path(result_dir, "count_annual_models.rds"))$M2
full_prior <- m3b_prior(full_fit, data)
saveRDS(list(model = full_fit, prior = full_prior, response = "total_birds_ringed",
  first_training_season = min(data$season), last_training_season = max(data$season),
  recent_years = 10, recency_half_life_years = 5),
  file.path(result_dir, "count_m3b_research_model.rds"))

stopifnot(all(forward$origin < forward$season),
  all(forward$horizon == forward$season - forward$origin),
  all(forward$predicted > 0),
  all(updates$ringing_date[updates$calibration_dates > 0] >
    updates$calibration_through[updates$calibration_dates > 0]),
  all(forward |> count(origin, ringing_date) |> pull(n) == 2),
  all(updates |> count(ringing_date, calibration_dates) |> pull(n) == 3))
write_csv(forward, file.path(result_dir, "count_m3b_forward_predictions.csv"))
write_csv(forward_scores, file.path(result_dir, "count_m3b_forward_scores.csv"))
write_csv(updates, file.path(result_dir, "count_m3b_updated_predictions.csv"))
write_csv(update_scores, file.path(result_dir, "count_m3b_updated_scores.csv"))
write_csv(paired, file.path(result_dir, "count_m3b_paired_differences.csv"))
write_csv(recent_levels, file.path(result_dir, "count_m3b_recent_levels.csv"))
write_csv(full_prior$recent_levels, file.path(result_dir, "count_m3b_full_fit_recent_levels.csv"))
write_csv(tibble(source = c("validation/research/count_annual_model_data.csv",
    "scripts/experiments/count/12_compare_m3b.R", "scripts/experiments/count/count_m3b.R"),
  md5 = unname(tools::md5sum(c(file.path(result_dir, "count_annual_model_data.csv"),
    here("scripts", "experiments", "12_compare_m3b.R"),
    here("scripts", "experiments", "count", "count_m3b.R"))))),
  file.path(result_dir, "count_m3b_input_manifest.csv"))
capture.output(sessionInfo(), file = file.path(result_dir, "count_m3b_session.txt"))
print(full_prior$recent_levels, width = Inf)
print(paired, width = Inf)
print(forward_scores, width = Inf)
print(filter(update_scores, calibration_dates %in% c(0, 5, 10)), width = Inf)
