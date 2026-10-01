library(dplyr)
library(readr)
library(mgcv)
library(plotly)
library(htmltools)
library(here)

# Read the exact artifact used by the forecast -----------------------------
model <- readRDS(here("count", "model", "model.rds"))
fit <- model$fit
data <- read_csv(here("count", "intermediate-data", "training.csv"), show_col_types = FALSE) |>
  mutate(bush_period = factor(bush_period, levels = c("back_bush", "front_bush")))
predictions <- read_csv(here("count", "intermediate-data", "forecast_validation_predictions.csv"), show_col_types = FALSE)
scores <- read_csv(here("count", "intermediate-data", "forecast_validation_scores.csv"), show_col_types = FALSE)
annual <- read_csv(here("count", "intermediate-data", "forecast_validation_seasons.csv"), show_col_types = FALSE)
stopifnot(nrow(data) == model$training$n_dates,
  isTRUE(all.equal(as.numeric(predict(fit, newdata = data, type = "response")), as.numeric(fitted(fit)))))

# Interactive figures -----------------------------------------------------
chart <- function(widget, height = 360) {
  widget <- plotly_build(widget)
  widget$width <- "100%"
  widget$height <- height
  widget$x$layout$height <- height
  widget$x$layout$margin <- list(l = 75, r = 25, b = 75, t = 25)
  widget$sizingPolicy$browser$fill <- FALSE
  tags$div(class = "chart", config(widget, responsive = TRUE, displaylogo = FALSE))
}
score_table <- function(x, columns) {
  tags$div(class = "table-scroll", tags$table(
    tags$thead(tags$tr(lapply(unname(columns), tags$th))),
    tags$tbody(lapply(seq_len(nrow(x)), function(i) tags$tr(lapply(names(columns), function(column) {
      value <- x[[column]][i]
      tags$td(if (is.numeric(value)) {
        if (column %in% c("dates", "seasons", "calibration_dates", "ranked_seasons", "horizon", "improved_seasons")) format(value, trim = TRUE)
        else if (column == "interval_80_coverage") sprintf("%.1f%%", 100 * value)
        else sprintf("%.2f", value)
      } else as.character(value))
    }))))))
}
effect_plot <- function(fit, variable, label) {
  reference <- data[1, ]
  for (column in all.vars(formula(fit))) {
    if (is.numeric(data[[column]])) reference[[column]] <- median(data[[column]])
  }
  reference$bush_period <- factor("front_bush", levels = levels(data$bush_period))
  grid <- reference[rep(1, 100), ]
  grid[[variable]] <- seq(quantile(data[[variable]], 0.05),
    quantile(data[[variable]], 0.95), length.out = 100)
  difference <- sweep(predict(fit, newdata = grid, type = "lpmatrix"),
    2, predict(fit, newdata = reference, type = "lpmatrix")[1, ], "-")
  log_ratio <- as.numeric(difference %*% coef(fit))
  se <- sqrt(rowSums((difference %*% fit$Vp) * difference))
  curve <- tibble(x = if (variable == "rain_log") expm1(grid[[variable]]) else grid[[variable]],
    ratio = exp(log_ratio), low = exp(log_ratio - 1.96 * se), high = exp(log_ratio + 1.96 * se))
  plot_ly(curve, x = ~x, showlegend = FALSE) |>
    add_lines(y = ~high, line = list(color = "transparent"), hoverinfo = "skip") |>
    add_lines(y = ~low, fill = "tonexty", fillcolor = "rgba(22,123,117,.15)",
      line = list(color = "transparent"), hoverinfo = "skip") |>
    add_lines(y = ~ratio, line = list(color = "#167b75", width = 2),
      text = ~sprintf("%s: %.2f<br>Multiplier %.2f<br>95%% interval %.2f–%.2f",
        label, x, ratio, low, high), hoverinfo = "text") |>
    layout(xaxis = list(title = list(text = label, standoff = 15), automargin = TRUE),
      yaxis = list(title = list(text = "Catch multiplier", standoff = 15), automargin = TRUE),
      shapes = list(list(type = "line", x0 = min(curve$x), x1 = max(curve$x),
        y0 = 1, y1 = 1, line = list(color = "#adbab5", dash = "dot"))))
}

year_reference <- data[1, ]
for (column in all.vars(formula(fit))) {
  if (is.numeric(data[[column]])) year_reference[[column]] <- median(data[[column]])
}
year_reference$bush_period <- factor("front_bush", levels = levels(data$bush_period))
year_reference$season <- 2023
year_grid <- year_reference[rep(1, 150), ]
year_grid$season <- seq(min(data$season), max(data$season), length.out = 150)
difference <- sweep(predict(fit, newdata = year_grid, type = "lpmatrix"),
  2, predict(fit, newdata = year_reference, type = "lpmatrix")[1, ], "-")
year_grid$ratio <- exp(as.numeric(difference %*% coef(fit)))
year_se <- sqrt(rowSums((difference %*% fit$Vp) * difference))
year_grid$low <- year_grid$ratio * exp(-1.96 * year_se)
year_grid$high <- year_grid$ratio * exp(1.96 * year_se)
future_years <- tibble(season = 2023:2028, ratio = 1)
year_plot <- plot_ly(year_grid, x = ~season, showlegend = FALSE) |>
  add_lines(y = ~high, line = list(color = "transparent"), hoverinfo = "skip") |>
  add_lines(y = ~low, fill = "tonexty", fillcolor = "rgba(184,118,40,.15)",
    line = list(color = "transparent"), hoverinfo = "skip") |>
  add_lines(y = ~ratio, name = "Historical fit", line = list(color = "#b87628")) |>
  add_lines(data = future_years, x = ~season, y = ~ratio, inherit = FALSE,
    name = "Forecast: held at 2023", line = list(color = "#172d32", dash = "dash")) |>
  layout(xaxis = list(title = "Season (continuous smooth)"),
    yaxis = list(title = "Year multiplier relative to 2023"))
bush_grid <- year_reference[rep(1, 2), ]
bush_grid$bush_period <- factor(c("back_bush", "front_bush"), levels = levels(data$bush_period))
bush_difference <- sweep(predict(fit, newdata = bush_grid, type = "lpmatrix"),
  2, predict(fit, newdata = bush_grid[1, ], type = "lpmatrix")[1, ], "-")
bush_grid$ratio <- exp(as.numeric(bush_difference %*% coef(fit)))
bush_grid$se <- sqrt(rowSums((bush_difference %*% fit$Vp) * bush_difference))
bush_grid$low <- bush_grid$ratio * exp(-1.96 * bush_grid$se)
bush_grid$high <- bush_grid$ratio * exp(1.96 * bush_grid$se)
bush_plot <- plot_ly(bush_grid, x = ~bush_period, y = ~ratio, type = "scatter", mode = "markers",
  marker = list(color = "#b87628", size = 10),
  error_y = list(type = "data", symmetric = FALSE, array = bush_grid$high - bush_grid$ratio,
    arrayminus = bush_grid$ratio - bush_grid$low)) |>
  layout(xaxis = list(title = "Bush category"), yaxis = list(title = "Multiplier versus back bush"))


weather_variables <- tibble(variable = c("rain_log", "wind_speed_10m_mean_ms",
  "temperature_2m_mean_c", "surface_pressure_mean_hpa", "total_cloud_cover_mean",
  "relative_humidity_mean_pct", "wind_u_10m_mean_ms", "wind_v_10m_mean_ms"),
  label = c("Rain 00–08 (mm)", "Wind speed (m/s)", "Temperature (°C)", "Pressure (hPa)",
    "Cloud fraction", "Humidity (%)", "East–west wind (m/s)", "North–south wind (m/s)"))
weather_plots <- lapply(seq_len(nrow(weather_variables)), function(i)
  chart(effect_plot(fit, weather_variables$variable[i], weather_variables$label[i])))
data$estimated <- as.numeric(predict(fit, newdata = data, type = "response"))
annual_fit <- data |> group_by(season) |>
  summarise(observed_total = sum(total_birds_ringed), estimated_total = sum(estimated),
    dates = n(), .groups = "drop")
fitted_totals <- plot_ly(annual_fit, x = ~season) |>
  add_lines(y = ~observed_total, name = "Observed", line = list(color = "#172d32")) |>
  add_lines(y = ~estimated_total, name = "Full-model fitted", line = list(color = "#167b75")) |>
  layout(xaxis = list(title = "Season"), yaxis = list(title = "Birds on included operated dates"),
    legend = list(orientation = "h"))
forecast_totals <- plot_ly(annual, x = ~season) |>
  add_lines(y = ~observed_total, name = "Observed", line = list(color = "#172d32")) |>
  add_lines(y = ~predicted_total, name = "Next-season predicted", line = list(color = "#167b75")) |>
  layout(xaxis = list(title = "Held-out season", tickmode = "linear", dtick = 1),
    yaxis = list(title = "Birds on included operated dates"), legend = list(orientation = "h"))
daily_plot <- plot_ly(predictions, x = ~observed, y = ~predicted, type = "scatter", mode = "markers",
  text = ~paste("Date:", ringing_date, "<br>Observed:", observed, "<br>Predicted:", round(predicted)),
  hoverinfo = "text", marker = list(color = "#167b75", opacity = 0.6)) |>
  layout(xaxis = list(title = "Observed birds"), yaxis = list(title = "Predicted birds"),
    shapes = list(list(type = "line", x0 = 0, y0 = 0, x1 = max(predictions$observed),
      y1 = max(predictions$observed), line = list(color = "#adbab5", dash = "dot"))))
theta <- fit$family$getTheta(TRUE)
distribution <- tibble(count = 0:3500, probability = dnbinom(count, mu = 500, size = theta))
pmf_plot <- plot_ly(distribution, x = ~count, y = ~probability, type = "scatter", mode = "lines",
  line = list(color = "#167b75")) |>
  layout(xaxis = list(title = "Possible daily count"), yaxis = list(title = "Probability"), showlegend = FALSE)

# Model description and validation ---------------------------------------
css <- 'html{scroll-behavior:smooth}body{margin:0;background:#fff;color:#243137;font:16px/1.55 Arial,Helvetica,sans-serif}main{max-width:1060px;margin:0 auto;padding:28px 24px 60px}h1{font-size:2rem;line-height:1.25}h2{margin:40px 0 18px;border-bottom:1px solid #ddd;padding-bottom:10px}h3{font-size:1.05rem;margin:24px 0 10px}nav{display:flex;flex-wrap:wrap;gap:16px;margin-bottom:25px}a{color:#167b75}ul{padding-left:23px}li{margin:6px 0}.equation{padding:16px;background:#f5f7f8;overflow-x:auto;margin:18px 0}math{font-size:1.12rem}.table-scroll{overflow-x:auto;margin:18px 0}table{border-collapse:collapse;width:100%;font-size:.9rem}th,td{padding:9px 12px;text-align:left;border-bottom:1px solid #ddd}th{background:#f5f7f8;white-space:nowrap}.caption{font-size:.9rem;color:#56636a}.chart{display:block;width:100%;min-width:0;margin:0 0 22px;overflow:hidden}.chart-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));column-gap:24px}.html-widget{display:block;max-width:100%}.chart-grid .chart{margin-bottom:18px}@media(max-width:750px){main{padding:20px 12px}.chart-grid{grid-template-columns:1fr}h1{font-size:1.6rem}}'
page <- tags$html(lang = "en", tags$head(tags$meta(charset = "utf-8"),
  tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
  tags$title("Ngulia count model — description and validation"), tags$style(HTML(css))),
  tags$body(tags$main(
    tags$h1("Daily capture count: model description and validation"),
    tags$nav(tags$a(href = "#model", "Full model"), tags$a(href = "#effects", "Model effects"),
      tags$a(href = "#annual", "Annual levels"), tags$a(href = "#validation", "Validation"),
      tags$a(href = "#forecast", "Live forecast"), tags$a(href = "alternatives.html", "Alternatives and tests")),
    tags$p("This page describes the single count model used by the forecast. Every fitted effect, annual fitted total and count distribution below comes from the same saved forecast artifact. Held-out validation refits that complete formula using earlier years only."),
    tags$h2(id = "model", "The full model"),
    tags$p("The expected number of captured birds is conditional on ringing taking place. A negative-binomial generalized additive model combines calendar, moon, weather, historical bush layout and year in one fit. Counts exclude targeted swallow/martin captures."),
    tags$pre(paste(deparse(formula(fit)), collapse = "\n")),
    tags$p("The log link makes contributions multiplicative on expected count. Penalized smooths describe season day, distance from new moon, log(1 + morning rain), wind speed, temperature, pressure and historical year. Cloud fraction, humidity, wind components and bush layout enter as linear or categorical terms. The year term is a shrinkage cubic spline with basis dimension six, rather than a global polynomial."),
    tags$p(sprintf("Training includes %d operated dates in %d seasons from 1977–2023, including %d documented zero-catch dates. Dates with unknown or mixed bush layout or missing inputs are excluded; documented single-layout dates in the 1994–1995 transition are retained.",
      nrow(data), n_distinct(data$season), sum(data$total_birds_ringed == 0))),
    tags$p("The year and layout terms absorb some historical variation in effort and population. They do not distinguish those mechanisms or measure net-hours. Weather effects are conditional associations after accounting for these terms; correlated weather variables and historical changes prevent a causal interpretation."),
    tags$h2(id = "effects", "Calendar, moon and weather effects from the full model"),
    tags$p(class = "caption", "Each curve varies one input over its central 90% while holding every other input at its training median and layout at front bush. Multipliers are relative to the same median reference. Shading shows pointwise 95% coefficient intervals, not daily prediction intervals. These are the complete model's effects, not fits of separate reduced models."),
    tags$div(class = "chart-grid", chart(effect_plot(fit, "season_day", "Season day (20 October = 1)")),
      chart(effect_plot(fit, "moon_distance_from_new_moon", "Days from new moon"))),
    tags$div(class = "chart-grid", weather_plots),
    tags$h2(id = "annual", "Historical annual level and layout"), chart(year_plot), chart(bush_plot),
    tags$p("The historical year curve is relative to 2023 at fixed weather and front-bush layout. The dashed extension shows the live rule: future predictions keep this term at its fitted 2023 value. Layout and era overlap, so the bush contrast is a predictive association. Holding the year effect avoids carrying the spline's right-edge slope indefinitely into the future; it assumes the latest fitted level remains a useful baseline."),
    tags$h3("Observed and fitted annual totals"), chart(fitted_totals, 420),
    tags$p(class = "caption", "Both lines sum over exactly the same included operated dates. The fitted line uses each date's actual historical weather, year and layout. It describes the full training fit and is not an independent validation or an estimate of unobserved calendar dates."),
    tags$h2(id = "validation", "Next-season validation: 2014–2023"),
    tags$p("For each test season, fit the complete model on all eligible earlier seasons. Then predict that season assuming front bush, with the year term held at the last training year, exactly as in production. No current-season counts enter these predictions. Earlier seasons inform training, but only 2014–2023 enter this assessment."),
    score_table(scores, c(dates = "Dates", seasons = "Seasons", mean_absolute_error = "Mean absolute error (birds)",
      mean_poisson_deviance = "Poisson deviance", observed_mean = "Observed mean", predicted_mean = "Predicted mean",
      interval_80_coverage = "80% range coverage")),
    chart(forecast_totals, 420),
    tags$p(class = "caption", "Annual predictions sum the held-out forecasts over the same observed operated dates. They do not forecast how many dates will be operated. This is the deployed year-level rule, unlike the historical spline-extrapolation benchmark retained in the alternatives report."),
    chart(daily_plot, 400),
    tags$p(sprintf("Mean absolute error is %.0f birds per date. Predicted and observed means are %.0f and %.0f birds; the nominal 80%% count range covers %.1f%% of observations. Poisson deviance measures error in the expected count; it does not change the negative-binomial fitting distribution.",
      scores$mean_absolute_error, scores$predicted_mean, scores$observed_mean, 100 * scores$interval_80_coverage)),
    tags$p("Validation uses historical ERA5 weather, not weather forecasts issued before each ringing date. Candidate selection used this historical record, so these results are retrospective evidence rather than an untouched final test. Recent seasons are few, and large annual variation remains. Independent operational accuracy requires new counts matched to archived issued forecasts."),
    tags$h2(id = "forecast", "How the live forecast uses this model"),
    tags$p("The shared pipeline requests hourly Open-Meteo ECMWF weather. It derives the same eight daily weather inputs, fixes layout to front bush and holds year at 2023 for later seasons. Calendar and moon terms follow the forecast date. The saved model is used without daily retraining or live annual adjustment; mist is forecast separately."),
    tags$p("The website shows expected count and a central 80% negative-binomial count range. That range includes fitted residual count variability, but excludes uncertainty in weather forecasts, model coefficients and a changing future annual level. The full-season reference curve uses median training weather."),
    chart(pmf_plot, 320),
    tags$p(class = "caption", sprintf("Example: 500 expected birds with the full model's fitted dispersion θ = %.2f. Daily counts can vary widely around the expected value.", theta)),
    tags$p("Annual live updates, alternative year baselines, previous-morning rain and neural-network screens are described separately in ",
      tags$a(href = "alternatives.html", "Count model alternatives and tests"), ". None runs in the published forecast."),
    tags$p("Reproduce from the repository root:"),
    tags$pre("Rscript count/scripts/train.R\nRscript count/scripts/validate.R\nRscript count/scripts/build_report.R"),
    tags$p(class = "caption", paste("Forecast artifact:", model$version,
      "· Inputs: raw-data/daily_coverage.csv · Saved model: count/model/model.rds · Validation tables: count/intermediate-data/"))
  )))
save_html(page, file = here("count", "reports", "model.html"), libdir = "lib")
cat("Wrote count model description and validation\n")
