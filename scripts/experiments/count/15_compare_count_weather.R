library(dplyr)
library(readr)
library(mgcv)
library(here)

# Prepare operated dates and prior-morning rain ---------------------------
daily <- read_csv(here("data", "daily_coverage.csv"), show_col_types = FALSE,
  col_types = cols(ringing_date = col_date(), .default = col_guess()))
weather <- c("total_precipitation_00_08_mm", "wind_speed_10m_mean_ms",
  "temperature_2m_mean_c", "surface_pressure_mean_hpa", "total_cloud_cover_mean",
  "relative_humidity_mean_pct", "wind_u_10m_mean_ms", "wind_v_10m_mean_ms")
previous_rain <- daily |> transmute(ringing_date = ringing_date + 1,
  previous_rain_log = log1p(total_precipitation_00_08_mm))
model_data <- daily |>
  filter(season >= 1977, ringing_happened |
    (daily_count_status == "zero_in_daily_summary" & effort_status == "documented_operation")) |>
  mutate(bush_period = case_when(
    !season %in% 1994:1995 ~ bush_net_configuration,
    grepl("back_bush", net_sites_observed) & grepl("front_bush", net_sites_observed) ~ "mixed_bush",
    grepl("front_bush", net_sites_observed) ~ "front_bush",
    grepl("back_bush", net_sites_observed) ~ "back_bush")) |>
  filter(bush_period %in% c("back_bush", "front_bush"),
    if_all(all_of(c("total_birds_ringed", "season_day",
      "moon_distance_from_new_moon", weather)), ~ !is.na(.x))) |>
  left_join(previous_rain, by = "ringing_date") |>
  filter(is.finite(previous_rain_log)) |>
  mutate(rain_log = log1p(total_precipitation_00_08_mm),
    bush_period = factor(bush_period, levels = c("back_bush", "front_bush")))

# Compare the two deployable candidates in rolling recent seasons ---------
m2 <- total_birds_ringed ~ s(season_day, k = 12) +
  s(moon_distance_from_new_moon, k = 6) + s(rain_log, k = 6) +
  s(wind_speed_10m_mean_ms, k = 6) + s(temperature_2m_mean_c, k = 6) +
  s(surface_pressure_mean_hpa, k = 6) + total_cloud_cover_mean +
  relative_humidity_mean_pct + wind_u_10m_mean_ms + wind_v_10m_mean_ms +
  bush_period + s(season, bs = "cs", k = 6)
formulas <- list(M2 = m2, M2_previous_rain = update(m2, . ~ . +
  s(previous_rain_log, k = 5)))
predictions <- list()
for (test_year in 2014:2023) {
  train <- filter(model_data, season < test_year)
  test <- filter(model_data, season == test_year)
  for (name in names(formulas)) {
    fit <- bam(formulas[[name]], data = train, family = nb(), method = "fREML",
      discrete = TRUE)
    predictions[[length(predictions) + 1]] <- test |>
      transmute(ringing_date, season, model = name, observed = total_birds_ringed,
        predicted = as.numeric(predict(fit, newdata = test, type = "response")),
        theta = fit$family$getTheta(TRUE))
  }
  cat("Forecast season", test_year, "complete\n")
}
predictions <- bind_rows(predictions) |>
  mutate(poisson_deviance = 2 * if_else(observed == 0, predicted,
    observed * log(observed / predicted) - observed + predicted),
    absolute_error = abs(observed - predicted),
    log_error = log1p(predicted) - log1p(observed),
    nb_log_loss = -dnbinom(observed, mu = predicted, size = theta, log = TRUE))
scores <- predictions |> group_by(model) |>
  summarise(period = "2014–2023", dates = n(), seasons = n_distinct(season),
    mean_poisson_deviance = mean(poisson_deviance),
    mean_absolute_error = mean(absolute_error),
    log_rmse = sqrt(mean(log_error^2)), mean_nb_log_loss = mean(nb_log_loss),
    observed_mean = mean(observed), predicted_mean = mean(predicted), .groups = "drop")
season_scores <- predictions |> group_by(model, season) |>
  summarise(dates = n(), deviance = mean(poisson_deviance),
    mean_absolute_error = mean(absolute_error), observed_mean = mean(observed),
    predicted_mean = mean(predicted), .groups = "drop")

# Bootstrap paired season differences ------------------------------------
paired_seasons <- predictions |> filter(model == "M2") |>
  select(ringing_date, season, reference_deviance = poisson_deviance,
    reference_error = absolute_error) |>
  inner_join(predictions |> filter(model == "M2_previous_rain"),
    by = c("ringing_date", "season")) |>
  group_by(season) |>
  summarise(dates = n(), deviance_delta = sum(poisson_deviance - reference_deviance),
    mae_delta = sum(absolute_error - reference_error), .groups = "drop")
set.seed(73)
draws <- replicate(2000, {
  sampled <- sample(seq_len(nrow(paired_seasons)), replace = TRUE)
  sum(paired_seasons$deviance_delta[sampled]) / sum(paired_seasons$dates[sampled])
})
paired <- tibble(model = "M2_previous_rain", period = "2014–2023",
  improved_seasons = sum(paired_seasons$deviance_delta < 0),
  deviance_difference = sum(paired_seasons$deviance_delta) / sum(paired_seasons$dates),
  mae_difference = sum(paired_seasons$mae_delta) / sum(paired_seasons$dates),
  low = unname(quantile(draws, 0.025)), high = unname(quantile(draws, 0.975)))

out <- here("validation", "research")
write_csv(model_data |> select(ringing_date, season, total_birds_ringed,
  season_day, moon_distance_from_new_moon, bush_period, previous_rain_log),
  file.path(out, "count_weather_cohort.csv"))
write_csv(predictions, file.path(out, "count_weather_predictions.csv"))
write_csv(scores, file.path(out, "count_weather_scores.csv"))
write_csv(season_scores, file.path(out, "count_weather_season_scores.csv"))
write_csv(paired, file.path(out, "count_weather_paired.csv"))
write_csv(tibble(input = "daily_coverage.csv",
  md5 = unname(tools::md5sum(here("data", "daily_coverage.csv")))),
  file.path(out, "count_weather_inputs.csv"))
print(scores, width = Inf)
