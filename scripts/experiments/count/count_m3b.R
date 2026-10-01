# M3b annual level: historical weather response, recent annual baseline,
# and sequential updates from observed dates in the forecast season.

m3b_prior <- function(fit, history) {
  daily_eta <- as.numeric(predict(fit, history, type = "link")) -
    as.numeric(predict(fit, history, type = "terms", terms = "s(season)"))
  annual <- history |>
    mutate(reference_mean = exp(daily_eta)) |>
    group_by(season) |>
    summarise(dates = n(), observed_total = sum(total_birds_ringed),
      reference_total = sum(reference_mean),
      log_level = log(observed_total / reference_total), .groups = "drop") |>
    arrange(desc(season)) |> slice_head(n = 10) |>
    mutate(weight = 2^(-(max(season) - season) / 5), weight = weight / sum(weight))
  average_log_level <- weighted.mean(annual$log_level, annual$weight)
  annual_sd <- sqrt(sum(annual$weight * (annual$log_level - average_log_level)^2) /
    (1 - sum(annual$weight^2)))
  expected_level <- weighted.mean(exp(annual$log_level), annual$weight)
  list(mean_log_level = log(expected_level) - annual_sd^2 / 2,
    annual_sd = annual_sd, expected_level = expected_level,
    recent_levels = annual, train_through = max(history$season))
}

m3b_predict <- function(fit, test, prior, calibration = NULL) {
  calibration_n <- if (is.null(calibration)) 0L else nrow(calibration)
  prediction_data <- bind_rows(calibration, test)
  daily_eta <- as.numeric(predict(fit, prediction_data, type = "link")) -
    as.numeric(predict(fit, prediction_data, type = "terms", terms = "s(season)"))
  daily_mean <- exp(daily_eta)
  theta <- fit$family$getTheta(TRUE)
  jacobi <- matrix(0, 61, 61)
  jacobi[cbind(1:60, 2:61)] <- jacobi[cbind(2:61, 1:60)] <- sqrt(1:60)
  quadrature <- eigen(jacobi, symmetric = TRUE)
  shifts <- prior$mean_log_level + prior$annual_sd * quadrature$values
  weights <- quadrature$vectors[1, ]^2
  means <- outer(daily_mean, exp(shifts))
  if (!is.null(calibration)) {
    log_weights <- log(weights) + colSums(matrix(dnbinom(calibration$total_birds_ringed,
      mu = means[seq_len(calibration_n), , drop = FALSE], size = theta, log = TRUE),
      nrow = calibration_n))
    weights <- exp(log_weights - max(log_weights))
    weights <- weights / sum(weights)
  }
  means <- means[calibration_n + seq_len(nrow(test)), , drop = FALSE]
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
  if ("total_birds_ringed" %in% names(test)) {
    log_density <- matrix(dnbinom(test$total_birds_ringed, mu = means, size = theta, log = TRUE),
      nrow = nrow(test))
    peak <- apply(log_density, 1, max)
    log_density <- peak + log(as.numeric(exp(log_density - peak) %*% weights))
  } else log_density <- rep(NA_real_, nrow(test))
  posterior_mean_log_level <- sum(shifts * weights)
  posterior_sd_log_level <- sqrt(sum(weights * (shifts - posterior_mean_log_level)^2))
  tibble(predicted = as.numeric(means %*% weights),
    catch_low = intervals[[1]], catch_high = intervals[[2]], log_density = log_density,
    annual_multiplier = sum(exp(shifts) * weights),
    annual_log_level = posterior_mean_log_level, annual_sd = posterior_sd_log_level)
}
