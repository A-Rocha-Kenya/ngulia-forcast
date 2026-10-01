library(dplyr)
library(readr)
library(jsonlite)
library(tibble)
library(here)

# Verify identical prediction coverage and original season assignments -----
out <- here("validation", "research", "mist")
labels <- read_csv(file.path(out, "labels.csv"), show_col_types = FALSE)
means <- read_csv(file.path(out, "night_means.csv"), show_col_types = FALSE)
predictions <- read_csv(file.path(out, "predictions.csv"), show_col_types = FALSE)
scores <- read_csv(file.path(out, "scores.csv"), show_col_types = FALSE)
training <- read_csv(file.path(out, "training.csv"), show_col_types = FALSE)
config <- fromJSON(here("research", "mist", "config.json"))
data <- left_join(labels, means, by = "ringing_date")
coverage <- predictions |> count(model)
audit <- predictions |> left_join(labels, by = "ringing_date", suffix = c("", "_source"))
stopifnot(nrow(coverage) == 3, all(coverage$n == nrow(labels)),
  !anyDuplicated(predictions[c("model", "ringing_date")]),
  all(audit$fold == audit$fold_source), all(audit$season == audit$season_source),
  all(audit$mist_present == audit$mist_present_source),
  all(audit$mist_observation == audit$mist_observation_source),
  all(labels$season <= config$last_label_season),
  all(is.finite(predictions$probability)), all(between(predictions$probability, 0, 1)),
  all(training$fold != training$validation_fold), nrow(training) == 5 * length(config$seeds))

# Independent R GLM predictions on each outer fold -------------------------
errors <- list()
for (fold_number in sort(unique(data$fold))) {
  train <- filter(data, fold != fold_number)
  test <- filter(data, fold == fold_number)
  fit <- glm(reformulate(config$features, "mist_present"), data = train, family = binomial())
  check <- filter(predictions, fold == fold_number, model == "M1 logistic means") |>
    arrange(ringing_date)
  error <- max(abs(as.numeric(predict(fit, test, type = "response")) - check$probability))
  null <- filter(predictions, fold == fold_number, model == "M0 climatology")
  stopifnot(error < 1e-6, max(abs(null$probability - mean(train$mist_present))) < 1e-6)
  training_fold <- filter(training, fold == fold_number)
  stopifnot(all(training_fold$refit_dates == nrow(train)), all(training_fold$test_dates == nrow(test)),
    all(training_fold$inner_train_dates == sum(train$fold != training_fold$validation_fold[1])),
    all(training_fold$inner_validation_dates == sum(train$fold == training_fold$validation_fold[1])))
  errors[[length(errors) + 1]] <- tibble(fold = fold_number, glm_max_probability_difference = error)
}

# Independently recompute pooled metrics from exported probabilities -------
recomputed <- predictions |> group_by(model) |>
  summarise(brier = mean((probability - mist_present)^2),
    log_loss = -mean(mist_present * log(pmax(probability, 1e-12)) +
      (1 - mist_present) * log(pmax(1 - probability, 1e-12))),
    auc = (sum(rank(probability)[mist_present == 1]) - sum(mist_present) * (sum(mist_present) + 1) / 2) /
      (sum(mist_present) * sum(mist_present == 0)), .groups = "drop")
metrics <- left_join(recomputed, scores, by = "model", suffix = c("_r", "_python"))
stopifnot(max(abs(metrics$brier_r - metrics$brier_python)) < 1e-7,
  max(abs(metrics$log_loss_r - metrics$log_loss_python)) < 1e-7,
  max(abs(metrics$auc_r - metrics$auc_python)) < 1e-12)
write_csv(bind_rows(errors), file.path(out, "verification.csv"))
cat("Verified all model cohorts, fold audit, independent GLM probabilities and pooled metrics.\n")
