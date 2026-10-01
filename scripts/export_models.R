library(mgcv)
library(jsonlite)
library(dplyr)
library(here)

# Export fitted count effects for shared browser and Node prediction -------
model <- readRDS(here("count", "model", "model.rds"))
fit <- model$fit
reference <- fit$model[1, ]
reference[names(model$reference_weather)] <- model$reference_weather
reference$season <- 2023
reference$season_day <- 24
reference$moon_distance_from_new_moon <- 3
reference$bush_period <- factor("front_bush", levels = levels(fit$model$bush_period))
reference_matrix <- predict(fit, reference, type = "lpmatrix", discrete = FALSE)
variables <- c("season_day", "moon_distance_from_new_moon", names(model$reference_weather))
effects <- list()
for (variable in variables) {
  limits <- range(fit$model[[variable]])
  if (variable == "season_day") limits <- c(1, 85)
  if (variable == "moon_distance_from_new_moon") limits <- c(0, 15)
  grid <- reference[rep(1, 2001), ]
  grid[[variable]] <- seq(limits[1], limits[2], length.out = nrow(grid))
  difference <- sweep(predict(fit, grid, type = "lpmatrix", discrete = FALSE), 2, reference_matrix[1, ], "-")
  eta <- as.numeric(difference %*% coef(fit))
  se <- sqrt(rowSums((difference %*% fit$Vp) * difference))
  histogram <- hist(fit$model[[variable]], breaks = 24, plot = FALSE)
  effects[[variable]] <- list(min = limits[1], max = limits[2],
    reference = reference[[variable]][1], eta = eta, se = se,
    histogram = list(x = histogram$mids, count = histogram$counts))
}
dir.create(here("site", "models"), showWarnings = FALSE)
direction <- (atan2(-fit$model$wind_u_10m_mean_ms, -fit$model$wind_v_10m_mean_ms) * 180 / pi + 360) %% 360
direction_histogram <- hist(direction, breaks = seq(0, 360, by = 15), plot = FALSE)
write_json(list(version = model$version, training = model$training,
  reference_eta = as.numeric(predict(fit, reference, type = "link", discrete = FALSE)),
  theta = fit$family$getTheta(TRUE), effects = effects,
  historical_reference = model$historical_reference,
  direction_histogram = list(x = direction_histogram$mids, count = direction_histogram$counts),
  interpolation = "Linear interpolation of 2001 samples per fitted effect"),
  here("site", "models", "count.json"), auto_unbox = TRUE, digits = 12, dataframe = "rows")

# Preserve the frozen CNN weights and preprocessing without conversion -----
metadata <- fromJSON(here("mist", "model", "metadata.json"))
members <- lapply(metadata$seeds, function(seed)
  fromJSON(here("mist", "model", paste0("cnn_seed_", seed, ".json")), simplifyVector = FALSE))
write_json(list(metadata = metadata, members = members),
  here("site", "models", "mist.json"), auto_unbox = TRUE, digits = 16)

# Reference predictions cover observed inputs and the full sandbox range ---
set.seed(73)
verification <- fit$model
random <- reference[rep(1, 500), ]
for (variable in variables) random[[variable]] <- runif(nrow(random), effects[[variable]]$min, effects[[variable]]$max)
verification <- bind_rows(verification, random)
issued <- reference[rep(1, 15), ]
issued$surface_pressure_mean_hpa <- seq(909, 936, length.out = nrow(issued))
issued$temperature_2m_mean_c <- seq(16, 25, length.out = nrow(issued))
verification <- bind_rows(verification, issued)
verification$season <- 2023
verification$bush_period <- reference$bush_period
expected <- as.numeric(predict(fit, verification, type = "response", discrete = FALSE))
write_json(list(input = verification |> select(all_of(variables)), expected = expected,
  low = qnbinom(0.1, mu = expected, size = fit$family$getTheta(TRUE)),
  high = qnbinom(0.9, mu = expected, size = fit$family$getTheta(TRUE))),
  here("count", "intermediate-data", "javascript_reference.json"),
  auto_unbox = TRUE, digits = 16, dataframe = "rows")
cat("Exported count effects, mist weights and prediction references.\n")
