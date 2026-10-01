library(dplyr)
library(readr)
library(tibble)
library(mgcv)
library(here)

# Recover transition dates without inventing front/back effort proportions --
data_path <- here("raw-data", "daily_coverage.csv")
result_dir <- here("count", "intermediate-data")
daily <- read_csv(data_path, show_col_types = FALSE,
  col_types = cols(ringing_date = col_date(), .default = col_guess()))
weather <- c("total_precipitation_00_08_mm", "wind_speed_10m_mean_ms",
  "temperature_2m_mean_c", "surface_pressure_mean_hpa", "total_cloud_cover_mean",
  "relative_humidity_mean_pct", "wind_u_10m_mean_ms", "wind_v_10m_mean_ms")
operation_dates <- daily |>
  filter(season >= 1977, ringing_happened |
    (daily_count_status == "zero_in_daily_summary" & effort_status == "documented_operation")) |>
  mutate(bush_period = case_when(
    !season %in% 1994:1995 ~ bush_net_configuration,
    grepl("back_bush", net_sites_observed) & grepl("front_bush", net_sites_observed) ~ "mixed_bush",
    grepl("front_bush", net_sites_observed) ~ "front_bush",
    grepl("back_bush", net_sites_observed) ~ "back_bush"
  ))
transition_audit <- operation_dates |>
  filter(season %in% 1994:1995) |>
  select(ringing_date, season, total_birds_ringed, djp_site, net_sites_observed, bush_period) |>
  mutate(included = bush_period %in% c("back_bush", "front_bush"))
model_data <- operation_dates |>
  filter(bush_period %in% c("back_bush", "front_bush"), if_all(all_of(c("total_birds_ringed", "season_day",
    "moon_distance_from_new_moon", weather)), ~ !is.na(.x))) |>
  mutate(rain_log = log1p(total_precipitation_00_08_mm),
    bush_period = factor(bush_period, levels = c("back_bush", "front_bush")),
    year_id = factor(season))

# M2 has a continuous, shrinkage-penalized year smooth and layout category --
m0 <- total_birds_ringed ~ s(season_day, k = 12) + s(moon_distance_from_new_moon, k = 6)
m1 <- update(m0, . ~ . + s(rain_log, k = 6) + s(wind_speed_10m_mean_ms, k = 6) +
  s(temperature_2m_mean_c, k = 6) + s(surface_pressure_mean_hpa, k = 6) +
  total_cloud_cover_mean + relative_humidity_mean_pct +
  wind_u_10m_mean_ms + wind_v_10m_mean_ms)
m2 <- update(m1, . ~ . + bush_period + s(season, bs = "cs", k = 6))
m3a <- update(m2, . ~ . + s(year_id, bs = "re"))
formulas <- list(M0 = m0, M1 = m1, M2 = m2, M3a = m3a)
annual_models <- c("M2", "M3a")

# Deterministic Gaussian quadrature for an unseen year's random intercept ---
jacobi <- matrix(0, 61, 61)
jacobi[cbind(1:60, 2:61)] <- jacobi[cbind(2:61, 1:60)] <- sqrt(1:60)
quadrature <- eigen(jacobi, symmetric = TRUE)
year_nodes <- quadrature$values
year_weights <- quadrature$vectors[1, ]^2

year_sd <- function(fit) {
  if (!"s(year_id)" %in% names(fit$sp)) return(0)
  # The random intercept has an identity penalty; NB scale is one.
  sqrt(fit$sig2 / unname(fit$sp["s(year_id)"]))
}

# Conditional distributions for known years; NB-lognormal mixtures for new years.
# Early-season counts update only the annual intercept, with all other terms fixed.
predict_count <- function(fit, test, new_year = FALSE, calibration = NULL) {
  calibration_n <- if (is.null(calibration)) 0L else nrow(calibration)
  prediction_data <- bind_rows(calibration, test)
  random_year <- new_year && "s(year_id)" %in% names(fit$sp)
  if (random_year) prediction_data$year_id <- factor(levels(fit$model$year_id)[1],
    levels = levels(fit$model$year_id))
  mu <- as.numeric(predict(fit, newdata = prediction_data, type = "response",
    exclude = if (random_year) "s(year_id)" else NULL))
  theta <- fit$family$getTheta(TRUE)
  sd <- year_sd(fit)
  shifts <- if (random_year) sd * year_nodes else 0
  weights <- if (random_year) year_weights else 1
  means <- outer(mu, exp(shifts))
  if (random_year && !is.null(calibration)) {
    log_weights <- log(weights) + colSums(matrix(dnbinom(calibration$total_birds_ringed,
      mu = means[seq_len(nrow(calibration)), , drop = FALSE], size = theta, log = TRUE),
      nrow = nrow(calibration)))
    weights <- exp(log_weights - max(log_weights))
    weights <- weights / sum(weights)
  }
  means <- means[calibration_n + seq_len(nrow(test)), , drop = FALSE]
  # Find integer quantiles of the mixture without simulated count draws.
  intervals <- list()
  for (probability in c(0.1, 0.9)) {
    lower <- rep(-1, nrow(test))
    upper <- qnbinom(probability, mu = apply(means, 1, max), size = theta)
    while (any(upper - lower > 1)) {
      middle <- floor((lower + upper) / 2)
      cdf <- as.numeric(matrix(pnbinom(middle, mu = means, size = theta), nrow = nrow(test)) %*% weights)
      upper <- ifelse(cdf >= probability, middle, upper)
      lower <- ifelse(cdf < probability, middle, lower)
    }
    intervals[[length(intervals) + 1]] <- upper
  }
  log_density <- matrix(dnbinom(test$total_birds_ringed, mu = means,
    size = theta, log = TRUE), nrow = nrow(test))
  peak <- apply(log_density, 1, max)
  tibble(predicted = as.numeric(means %*% weights),
    typical_year_prediction = mu[calibration_n + seq_len(nrow(test))],
    catch_low = intervals[[1]], catch_high = intervals[[2]],
    log_density = peak + log(as.numeric(exp(log_density - peak) %*% weights)),
    annual_sd = sd)
}

# Fixed whole-season folds and chronological blocks within every season ----
set.seed(73)
folds <- tibble(season = sort(unique(model_data$season))) |>
  mutate(fold = sample(rep(1:5, length.out = n())),
    fold = case_when(season == 1994 ~ 1L, season == 1995 ~ 2L, TRUE ~ fold))
model_data <- left_join(model_data, folds, by = "season") |>
  arrange(season, ringing_date) |> group_by(season) |>
  mutate(within_fold = ceiling(5 * row_number() / n())) |> ungroup()
predictions <- list()
training_audit <- list()
for (model_name in names(formulas)) {
  for (fold_number in 1:5) {
    train <- filter(model_data, fold != fold_number)
    test <- filter(model_data, fold == fold_number)
    fit <- bam(formulas[[model_name]], data = train, family = nb(), method = "fREML", discrete = TRUE)
    predictions[[length(predictions) + 1]] <- bind_cols(
      test |> select(ringing_date, season) |> mutate(model = model_name,
        fold = fold_number, observed = test$total_birds_ringed),
      predict_count(fit, test, new_year = TRUE))
    training_audit[[length(training_audit) + 1]] <- tibble(evaluation = "whole-season",
      model = model_name, split = fold_number, train_dates = nrow(train), test_dates = nrow(test),
      shared_seasons = length(intersect(train$season, test$season)))
  }
  cat("Whole-season validation:", model_name, "\n")
}
predictions <- bind_rows(predictions)
within_predictions <- list()
for (model_name in annual_models) {
  for (fold_number in 1:5) {
    train <- filter(model_data, within_fold != fold_number)
    test <- filter(model_data, within_fold == fold_number)
    fit <- bam(formulas[[model_name]], data = train, family = nb(), method = "fREML", discrete = TRUE)
    within_predictions[[length(within_predictions) + 1]] <- bind_cols(
      test |> select(ringing_date, season) |> mutate(model = model_name,
        fold = fold_number, observed = test$total_birds_ringed), predict_count(fit, test))
    training_audit[[length(training_audit) + 1]] <- tibble(evaluation = "within-year",
      model = model_name, split = fold_number, train_dates = nrow(train), test_dates = nrow(test),
      shared_seasons = length(intersect(train$season, test$season)))
  }
  cat("Within-year validation:", model_name, "\n")
}
within_predictions <- bind_rows(within_predictions)

# Forecast years 1997-2023 before counts, then after 5 or 10 operated dates --
forward_predictions <- list()
updated_predictions <- list()
updated_prior_predictions <- list()
for (test_year in 1997:2023) {
  train <- filter(model_data, season < test_year)
  test <- filter(model_data, season == test_year)
  for (model_name in annual_models) {
    fit <- bam(formulas[[model_name]], data = train, family = nb(), method = "fREML", discrete = TRUE)
    prior <- predict_count(fit, test, new_year = TRUE)
    forward_predictions[[length(forward_predictions) + 1]] <- bind_cols(
      test |> select(ringing_date, season) |> mutate(model = model_name, train_through = test_year - 1L,
        train_dates = nrow(train), observed = test$total_birds_ringed),
      prior)
    for (calibration_dates in c(0L, 5L, 10L)) {
      if (nrow(test) <= calibration_dates) next
      calibration <- if (calibration_dates > 0) slice_head(test, n = calibration_dates) else NULL
      remaining <- slice(test, (calibration_dates + 1):nrow(test))
      updated_predictions[[length(updated_predictions) + 1]] <- bind_cols(
        remaining |> select(ringing_date, season) |> mutate(model = model_name,
          calibration_dates = calibration_dates, train_through = test_year - 1L,
          calibration_through = if (is.null(calibration)) as.Date(NA) else max(calibration$ringing_date),
          observed = remaining$total_birds_ringed),
        predict_count(fit, remaining, new_year = TRUE, calibration = calibration))
      updated_prior_predictions[[length(updated_prior_predictions) + 1]] <- bind_cols(
        remaining |> select(ringing_date, season) |> mutate(model = model_name,
          calibration_dates = calibration_dates, observed = remaining$total_birds_ringed),
        slice(prior, (calibration_dates + 1):nrow(test)))
    }
  }
  cat("Forward validation:", test_year, "\n")
}
forward_predictions <- bind_rows(forward_predictions)
updated_predictions <- bind_rows(updated_predictions)
updated_prior_predictions <- bind_rows(updated_prior_predictions)

# Scores distinguish annual-level error from centered daily log errors -----
season_score <- function(x) {
  x |> summarise(dates = n(), observed_mean = mean(observed), predicted_mean = mean(predicted),
    annual_log_error = log(mean(predicted) / mean(observed)),
    within_log_mse = mean(((log1p(predicted) - log1p(observed)) -
      mean(log1p(predicted) - log1p(observed)))^2),
    spearman = if (n_distinct(observed) > 1 && n_distinct(predicted) > 1)
      cor(observed, predicted, method = "spearman") else NA_real_, .groups = "drop")
}
score_count <- function(x) {
  annual <- x |> group_by(season) |> season_score()
  x |> summarise(dates = n(), seasons = n_distinct(season),
    mean_poisson_deviance = mean(2 * if_else(observed == 0, predicted,
      observed * log(observed / predicted) - observed + predicted)),
    log_rmse = sqrt(mean((log1p(observed) - log1p(predicted))^2)),
    observed_mean = mean(observed), predicted_mean = mean(predicted),
    mean_negative_binomial_log_loss = -mean(log_density),
    interval_80_coverage = mean(observed >= catch_low & observed <= catch_high),
    interval_80_width = mean(catch_high - catch_low),
    annual_log_rmse = sqrt(mean(annual$annual_log_error^2)),
    within_log_rmse = sqrt(weighted.mean(annual$within_log_mse, annual$dates)),
    mean_season_spearman = mean(annual$spearman, na.rm = TRUE),
    ranked_seasons = sum(is.finite(annual$spearman)), .groups = "drop")
}
# Split data explicitly so scoring functions always receive ungrouped rows.
summarize_count <- function(x, groups) {
  x |> group_by(across(all_of(groups))) |>
    group_modify(~ score_count(.x)) |> ungroup()
}
scores <- summarize_count(predictions, "model")
fold_scores <- summarize_count(predictions, c("model", "fold"))
season_scores <- predictions |> group_by(model, season) |> season_score()
within_scores <- summarize_count(within_predictions, "model")
within_season_scores <- within_predictions |> group_by(model, season) |> season_score()
forward_scores <- forward_predictions |> mutate(period = if_else(season >= 2014, "2014–2023", "1997–2013")) |>
  summarize_count(c("model", "period"))
updated_scores <- updated_predictions |> summarize_count(c("model", "calibration_dates"))
updated_prior_scores <- summarize_count(updated_prior_predictions, c("model", "calibration_dates"))

# Paired season bootstrap, conditional on these already explored data -------
paired <- list()
for (evaluation in c("whole-season", "within-year")) {
  evaluated <- if (evaluation == "whole-season") predictions else within_predictions
  losses <- evaluated |> mutate(loss = 2 * if_else(observed == 0, predicted,
    observed * log(observed / predicted) - observed + predicted))
  for (candidate in "M3a") {
    difference <- losses |> filter(model == "M2") |> select(ringing_date, season, baseline = loss) |>
      inner_join(losses |> filter(model == candidate) |> select(ringing_date, candidate_loss = loss),
        by = "ringing_date") |> group_by(season) |>
      summarise(delta = sum(candidate_loss - baseline), dates = n(), .groups = "drop")
    set.seed(73)
    draws <- replicate(2000, {i <- sample(seq_len(nrow(difference)), replace = TRUE)
      sum(difference$delta[i]) / sum(difference$dates[i])})
    paired[[length(paired) + 1]] <- tibble(evaluation = evaluation, baseline = "M2", candidate = candidate,
      difference = sum(difference$delta) / sum(difference$dates),
      low = quantile(draws, 0.025), high = quantile(draws, 0.975))
  }
}
paired <- bind_rows(paired)
fits <- lapply(formulas, function(formula) bam(formula, data = model_data,
  family = nb(), method = "fREML", discrete = TRUE))
model_parameters <- bind_rows(lapply(names(fits), function(name) tibble(model = name,
  annual_sd = year_sd(fits[[name]]), negative_binomial_size = fits[[name]]$family$getTheta(TRUE),
  year_smooth_edf = if (name %in% annual_models) summary(fits[[name]])$s.table["s(season)", "edf"] else NA_real_)))

# Scientific invariants: paired cohorts, independent splits, no future counts.
training_audit <- bind_rows(training_audit)
stopifnot(all(is.finite(predictions$predicted)), all(predictions$predicted > 0),
  all(predictions |> count(ringing_date) |> pull(n) == length(formulas)),
  all(within_predictions |> count(ringing_date) |> pull(n) == length(annual_models)),
  all(training_audit$shared_seasons[training_audit$evaluation == "whole-season"] == 0),
  all(training_audit$shared_seasons[training_audit$evaluation == "within-year"] == n_distinct(model_data$season)),
  all(forward_predictions$train_through < forward_predictions$season),
  all(updated_predictions$ringing_date[updated_predictions$calibration_dates > 0] >
    updated_predictions$calibration_through[updated_predictions$calibration_dates > 0]),
  all(predictions$catch_low <= predictions$catch_high),
  all(updated_predictions |> count(ringing_date, calibration_dates) |> pull(n) == length(annual_models)))
write_csv(model_data, file.path(result_dir, "count_annual_model_data.csv"))
write_csv(transition_audit, file.path(result_dir, "count_transition_audit.csv"))
write_csv(folds, file.path(result_dir, "count_annual_folds.csv"))
write_csv(predictions, file.path(result_dir, "count_annual_predictions.csv"))
write_csv(scores, file.path(result_dir, "count_annual_scores.csv"))
write_csv(fold_scores, file.path(result_dir, "count_annual_fold_scores.csv"))
write_csv(season_scores, file.path(result_dir, "count_annual_season_scores.csv"))
write_csv(within_predictions, file.path(result_dir, "count_within_predictions.csv"))
write_csv(within_scores, file.path(result_dir, "count_within_scores.csv"))
write_csv(within_season_scores, file.path(result_dir, "count_within_season_scores.csv"))
write_csv(forward_predictions, file.path(result_dir, "count_annual_forward_predictions.csv"))
write_csv(forward_scores, file.path(result_dir, "count_annual_forward_scores.csv"))
write_csv(updated_predictions, file.path(result_dir, "count_annual_updated_predictions.csv"))
write_csv(updated_prior_scores, file.path(result_dir, "count_annual_updated_prior_scores.csv"))
write_csv(updated_scores, file.path(result_dir, "count_annual_updated_scores.csv"))
write_csv(paired, file.path(result_dir, "count_annual_paired_differences.csv"))
write_csv(training_audit, file.path(result_dir, "count_annual_training_audit.csv"))
write_csv(model_parameters, file.path(result_dir, "count_annual_parameters.csv"))
saveRDS(fits, here("count", "model", "count_annual_models.rds"))
write_csv(tibble(source = c("raw-data/daily_coverage.csv", "count/scripts/alternatives/08_compare_m0_m2.R"),
  md5 = unname(tools::md5sum(c(data_path, here("count", "scripts", "alternatives", "08_compare_m0_m2.R"))))),
  file.path(result_dir, "count_annual_input_manifest.csv"))
capture.output(sessionInfo(), file = file.path(result_dir, "count_annual_session.txt"))
print(scores, width = Inf)
print(within_scores, width = Inf)
print(forward_scores, width = Inf)
print(updated_scores, width = Inf)
