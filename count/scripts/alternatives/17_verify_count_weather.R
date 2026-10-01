library(dplyr)
library(readr)
library(here)

# Check the paired recent-year experiment -------------------------------
out <- here("count", "intermediate-data")
cohort <- read_csv(file.path(out, "count_weather_cohort.csv"), show_col_types = FALSE)
predictions <- read_csv(file.path(out, "count_weather_predictions.csv"), show_col_types = FALSE)
scores <- read_csv(file.path(out, "count_weather_scores.csv"), show_col_types = FALSE)
annual <- read_csv(file.path(out, "count_annual_forward_scores.csv"), show_col_types = FALSE)
m2_formula <- formula(readRDS(here("count", "model", "count_annual_models.rds"))$M2)
stopifnot(!grepl("cloud_base", paste(deparse(m2_formula), collapse = "")),
  !anyDuplicated(cohort$ringing_date),
  all(is.finite(cohort$previous_rain_log)),
  setequal(unique(predictions$season), 2014:2023),
  setequal(unique(predictions$model), c("M2", "M2_previous_rain")),
  all(predictions |> count(ringing_date) |> pull(n) == 2),
  nrow(predictions) == 284)
reference <- scores |> filter(model == "M2") |> pull(mean_poisson_deviance)
annual_reference <- annual |> filter(model == "M2", period == "2014–2023") |>
  pull(mean_poisson_deviance)
stopifnot(length(annual_reference) == 1,
  abs(reference - annual_reference) < 1e-6)
cat("Verified paired 2014–2023 M2/prior-rain scores and baseline parity.\n")
