library(dplyr)
library(readr)
library(mgcv)
library(here)
library(cli)

# Forecast each recent season using only earlier seasons -------------------
data <- read_csv(here("count", "intermediate-data", "training.csv"), show_col_types = FALSE) |>
  mutate(bush_period = factor(bush_period, levels = c("back_bush", "front_bush")))
model <- readRDS(here("count", "model", "model.rds"))
predictions <- list()
for (year in 2014:2023) {
  train <- filter(data, season < year)
  test <- filter(data, season == year)
  fit <- bam(formula(model$fit), data = train, family = nb(), method = "fREML", discrete = TRUE)
  # Apply the deployed rule: hold the year term at the last training season.
  newdata <- mutate(test, season = max(train$season), bush_period = factor("front_bush",
    levels = levels(data$bush_period)))
  predicted <- as.numeric(predict(fit, newdata = newdata, type = "response"))
  theta <- fit$family$getTheta(TRUE)
  predictions[[as.character(year)]] <- test |>
    transmute(ringing_date, season, training_last_year = max(train$season),
      observed = total_birds_ringed, predicted,
      low = qnbinom(0.1, mu = predicted, size = theta),
      high = qnbinom(0.9, mu = predicted, size = theta),
      deviance = 2 * (if_else(observed == 0, 0, observed * log(pmax(observed, 1) / predicted)) - observed + predicted),
      log_loss = -dnbinom(observed, mu = predicted, size = theta, log = TRUE))
  cli_alert_info("Validated {year} using training through {max(train$season)}")
}
predictions <- bind_rows(predictions)
stopifnot(all(predictions$training_last_year < predictions$season),
  nrow(predictions) == sum(data$season >= 2014), all(is.finite(predictions$predicted)))
scores <- predictions |>
  summarise(dates = n(), seasons = n_distinct(season),
    mean_absolute_error = mean(abs(observed - predicted)),
    mean_poisson_deviance = mean(deviance), observed_mean = mean(observed),
    predicted_mean = mean(predicted), interval_80_coverage = mean(observed >= low & observed <= high),
    mean_negative_binomial_log_loss = mean(log_loss))
season_scores <- predictions |> group_by(season) |>
  summarise(dates = n(), observed_total = sum(observed), predicted_total = sum(predicted),
    mean_absolute_error = mean(abs(observed - predicted)), .groups = "drop")
write_csv(predictions, here("count", "intermediate-data", "forecast_validation_predictions.csv"))
write_csv(scores, here("count", "intermediate-data", "forecast_validation_scores.csv"))
write_csv(season_scores, here("count", "intermediate-data", "forecast_validation_seasons.csv"))
print(scores)
