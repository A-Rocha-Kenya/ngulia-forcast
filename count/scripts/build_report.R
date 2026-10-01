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
  layout(xaxis = list(title = "Season"),
    yaxis = list(title = "Year multiplier relative to 2023"))
bush_grid <- year_reference[rep(1, 2), ]
bush_grid$bush_period <- factor(c("back_bush", "front_bush"), levels = levels(data$bush_period))
bush_difference <- sweep(predict(fit, newdata = bush_grid, type = "lpmatrix"),
  2, predict(fit, newdata = bush_grid[1, ], type = "lpmatrix")[1, ], "-")
bush_grid$ratio <- exp(as.numeric(bush_difference %*% coef(fit)))
bush_grid$se <- sqrt(rowSums((bush_difference %*% fit$Vp) * bush_difference))
bush_grid$low <- bush_grid$ratio * exp(-1.96 * bush_grid$se)
bush_grid$high <- bush_grid$ratio * exp(1.96 * bush_grid$se)
bush_plot <- plot_ly(bush_grid, x = ~factor(bush_period, labels = c("Back bush", "Front bush")), y = ~ratio, type = "scatter", mode = "markers",
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
  layout(xaxis = list(title = "Test season", tickmode = "linear", dtick = 1),
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
css <- 'html{scroll-behavior:smooth}body{margin:0;background:#fff;color:#243137;font:16px/1.55 Arial,Helvetica,sans-serif}main{max-width:1060px;margin:0 auto;padding:28px 24px 60px}h1{font-size:2rem;line-height:1.25}h2{margin:40px 0 18px;border-bottom:1px solid #ddd;padding-bottom:10px}h3{font-size:1.05rem;margin:24px 0 10px}nav{display:flex;flex-wrap:wrap;gap:16px;margin-bottom:25px}a{color:#167b75}ul{padding-left:23px}li{margin:6px 0}.equation{padding:16px;background:#f5f7f8;overflow-x:auto;margin:18px 0}math{font-size:1.12rem}.table-scroll{overflow-x:auto;margin:18px 0}table{border-collapse:collapse;width:100%;font-size:.9rem}th,td{padding:9px 12px;text-align:left;border-bottom:1px solid #ddd}th{background:#f5f7f8;white-space:nowrap}.caption{font-size:.9rem;color:#56636a}.chart{display:block;width:100%;min-width:0;margin:0 0 22px;overflow:hidden}.chart-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));column-gap:24px}.html-widget{display:block;max-width:100%}.report-lead{font-size:1.1rem}.report-note{padding:18px;background:#f5f7f8;border-left:3px solid #167b75;margin:24px 0}.report-note p{margin-bottom:0}.report-metrics{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:16px;margin:24px 0}.report-metrics>div{padding:18px;background:#f5f7f8}.report-metrics p{font-size:.9rem}.report-metrics strong{font-size:1.6rem}details{margin:20px 0}summary{cursor:pointer;color:#167b75}.chart-grid .chart{margin-bottom:18px}@media(max-width:750px){main{padding:20px 12px}.chart-grid,.report-metrics{grid-template-columns:1fr}h1{font-size:1.6rem}}'
page <- tags$html(lang = "en", tags$head(tags$meta(charset = "utf-8"),
  tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
  tags$title("Count model | Ngulia"), tags$style(HTML(css))),
  tags$body(tags$main(
    tags$h1("How many birds might we catch?"),
    tags$p(class = "report-lead", "The count forecast combines decades of ringing records with the date, moon and weather to estimate the catch on a day when ringing takes place. Here is how the model works, what we have checked, and what to expect from it in a new season."),
    tags$nav(tags$a(href = "#model", "The model"), tags$a(href = "#effects", "Date, moon & weather"),
      tags$a(href = "#annual", "Historical baseline"), tags$a(href = "#fit-check", "Fit check"),
      tags$a(href = "#validation", "Next-season test"), tags$a(href = "#forecast", "The live forecast")),
    tags$h2(id = "model", "1. Learn from past ringing days"),
    tags$p("The question is simple: given the conditions on a particular day, how many birds should we expect to capture? This is a forecast of the ringing catch, rather than the total number of birds migrating through Ngulia. It assumes ringing takes place and excludes targeted swallow and martin captures."),
    tags$p("We use a negative-binomial generalized additive model (GAM). The GAM learns flexible curves: the relationship with rain or the time of season does not have to be a straight line. The negative-binomial distribution allows actual catches to vary widely around the expected count, as they do in the ringing records."),
    tags$ul(
      tags$li(tags$strong("Date and moon"), " describe recurring patterns within the migration season."),
      tags$li(tags$strong("Weather"), " adjusts the expected catch for that day's rain, wind, temperature, pressure, cloud and humidity."),
      tags$li(tags$strong("Year and bush layout"), " account for longer-term differences in the historical records.")),
    tags$p(sprintf("The saved model learns from %s ringing dates across %d seasons, from 1977 to 2023. This includes %d documented days with no catch. Each day's inputs enter the same model together.",
      format(nrow(data), big.mark = ",", trim = TRUE), n_distinct(data$season), sum(data$total_birds_ringed == 0))),
    tags$details(tags$summary("Model formula and training details"),
      tags$pre(paste(trimws(deparse(formula(fit)), which = "right"), collapse = "\n")),
      tags$p("A log link combines the effects as multipliers on the expected count. Penalized smooths describe season day, distance from new moon, log(1 + morning rain), wind speed, temperature, pressure and historical year. Cloud fraction, humidity and the two wind components are linear terms; bush layout is categorical. The historical year term is a shrinkage cubic spline with basis dimension six."),
      tags$p("Dates with unknown or mixed bush layout or missing inputs are excluded. Documented single-layout dates in the 1994–1995 transition are retained. Every fitted curve on this page comes from the saved model used by the forecast.")),

    tags$h2(id = "effects", "2. How do the date, moon and weather change the catch?"),
    tags$p("These curves show how the model adjusts its expected count when one input changes and the others stay fixed. They help us understand what the model has learned before asking whether its predictions work in a later season."),
    tags$div(class = "report-note", tags$strong("How to read a multiplier"),
      tags$p("A value of 1 means the same expected catch as the reference conditions. A value of 2 means twice the expected catch; 0.5 means half. For example, a multiplier of 1.5 would turn a reference catch of 400 birds into 600, with all other inputs unchanged.")),
    tags$p(class = "caption", "The reference uses median training inputs and front-bush layout. Each curve spans the central 90% of its input's observed values. Shading shows uncertainty in the fitted effect (pointwise 95% coefficient intervals); it is different from the range of possible catches on a future day."),
    tags$h3("Timing within the season and the moon cycle"),
    tags$p("The first curve describes the pattern through the ringing season. The second asks how the expected catch changes with distance from new moon, after accounting for the date and weather."),
    tags$div(class = "chart-grid", chart(effect_plot(fit, "season_day", "Season day (20 October = 1)")),
      chart(effect_plot(fit, "moon_distance_from_new_moon", "Days from new moon"))),
    tags$h3("Weather on the ringing day"),
    tags$p("Read these curves in the same way: above 1 raises the expected catch relative to the reference, below 1 lowers it. Weather is summarised from midnight to 08:00 in Ngulia local time: rain is the total, and the other inputs are averages. The last two curves describe the east–west and north–south components of wind."),
    tags$div(class = "chart-grid", weather_plots),
    tags$p("These are associations in the historical records. Weather variables are related to one another, and ringing conditions have changed over time. A curve therefore describes the model's adjustment, rather than proving that changing that one weather variable causes a change in catch."),

    tags$h2(id = "annual", "3. Put today's forecast in its historical context"),
    tags$p("The same date and weather have not always corresponded to the same catch. The model includes a year effect to represent changes in the overall catch level, and a bush-layout term to account for the different historical ringing locations."),
    tags$h3("What happens to the year effect after 2023?"),
    tags$p("The solid curve shows the historical year effect relative to 2023, with weather and layout held fixed. A value of 2 means twice the 2023 expected catch under the same conditions. For future seasons, the forecast keeps this effect at the fitted 2023 level, shown by the dashed line."),
    chart(year_plot),
    tags$p("This gives the forecast a fixed recent baseline. It avoids extending the historical curve's end slope indefinitely into the future, but assumes the 2023 level remains relevant. The model does not yet adjust that baseline using catches from the new season."),
    tags$h3("Why include bush layout?"),
    tags$p("The comparison below is relative to back-bush layout. The live forecast assumes front bush. Because layout and historical era overlap, this difference can also reflect changes in effort or bird abundance; it should not be read as an isolated effect of moving the nets."),
    chart(bush_plot),
    tags$p(class = "caption", "Year and layout help describe historical variation, but do not separate changes in bird populations from changes in ringing effort. The model does not measure net-hours."),

    tags$h2(id = "fit-check", "4. First check: can the model describe the records it learned from?"),
    tags$p("We add the fitted daily counts within each season and compare them with the recorded catches. This checks whether the model can describe the broad historical pattern using each day's recorded weather, year and layout."),
    chart(fitted_totals, 420),
    tags$p("Both lines cover exactly the same included ringing dates. A close match is useful, but the model has already seen these catches during training. To assess prediction, we need to test it on a season whose catches were left out."),
    tags$p(class = "caption", "These totals exclude dates outside the training data. They do not estimate catches on days when ringing did not take place."),

    tags$h2(id = "validation", "5. The harder test: predict a season before seeing its catches"),
    tags$p("We repeat the following exercise for each season from 2014 to 2023. This mimics starting a new season with only the earlier ringing records available."),
    tags$ol(
      tags$li(tags$strong("Train on the past."), " For the 2014 test, use eligible seasons up to 2013; for the 2015 test, use seasons up to 2014, and so on."),
      tags$li(tags$strong("Predict the next season."), " Use its date, moon and historical weather, assume front bush, and keep the year effect at the last training season's level, following the live forecast rule."),
      tags$li(tags$strong("Compare with the catches."), " Only after making the predictions do we use that season's observed counts to score them.")),
    tags$p(sprintf("Together, these tests cover %d ringing dates across %d seasons. No catches from a test season enter that season's fitted model.", scores$dates, scores$seasons)),
    tags$h3("What do the results tell us?"),
    tags$div(class = "report-metrics",
      tags$div(tags$p("Average daily error"), tags$strong(sprintf("%.0f birds", scores$mean_absolute_error)),
        tags$p("Average distance between the predicted and observed catch, ignoring the direction of the error.")),
      tags$div(tags$p("Average predicted / observed"), tags$strong(sprintf("%.0f / %.0f", scores$predicted_mean, scores$observed_mean)),
        tags$p(sprintf("Predictions average %.0f%% above the recorded daily catch across the test dates.", 100 * (scores$predicted_mean / scores$observed_mean - 1)))),
      tags$div(tags$p("Days inside the 80% range"), tags$strong(sprintf("%.1f%%", 100 * scores$interval_80_coverage)),
        tags$p("An 80% range aims to include about 8 out of 10 catches. Here it includes slightly more."))),
    tags$p("The average error is large compared with the average observed catch. The forecast gives a broad expectation, and substantial misses remain possible. Covering more than 80% of observations does not by itself show that the ranges are precise or well calibrated."),
    tags$h3("Does it get the seasonal totals right?"),
    tags$p("The lines below sum predictions and observations over the same ringing dates in each test season. They show whether the model carries the catch level forward successfully when it has not seen that season's catches."),
    chart(forecast_totals, 420),
    tags$p(sprintf("Predicted totals are above observed totals in %d of the %d test seasons. The model also misses the high catch in 2017. A new season can differ substantially from the baseline carried forward from earlier years.",
      sum(annual$predicted_total > annual$observed_total), nrow(annual))),
    tags$p(class = "caption", "Totals cover observed ringing dates only. This test does not predict how many days will be operated in a future season."),
    tags$h3("How close are the individual daily predictions?"),
    tags$p("Each dot is one test date. A dot on the dashed diagonal would be an exact match. Above the line means too many birds predicted; below it means too few. The spread shows how much day-to-day variation remains unexplained."),
    chart(daily_plot, 400),
    tags$details(tags$summary("Detailed validation scores"),
      score_table(scores, c(dates = "Dates", seasons = "Seasons", mean_absolute_error = "Mean absolute error (birds)",
        mean_poisson_deviance = "Poisson deviance", observed_mean = "Observed mean", predicted_mean = "Predicted mean",
        interval_80_coverage = "80% range coverage")),
      tags$p("Mean absolute error is the average absolute difference in birds. Poisson deviance is an additional score for error in the expected count; smaller is better. Its name does not change the model's negative-binomial fitting distribution.")),
    tags$div(class = "report-note", tags$strong("What this test still leaves to check"),
      tags$p("We used historical ERA5 weather, rather than weather forecasts issued before each ringing day. This tests the count model with reconstructed weather inputs, so it does not measure errors introduced by the live weather forecast. The historical record also informed model selection; it is not an untouched final test.")),

    tags$h2(id = "forecast", "6. What to expect from the live forecast"),
    tags$p("For each forecast date, the website uses hourly Open-Meteo ECMWF weather to calculate the same eight daily weather inputs. The date and moon follow that day; layout is fixed to front bush and the year effect stays at 2023. The saved model then produces an expected count and a range of possible catches. Mist is predicted by a separate model."),
    tags$h3("An expected count is not a promise"),
    tags$p("A forecast of 500 birds means an expected catch of 500 under those inputs. It does not mean the model expects every similar day to produce exactly 500. The example below shows the distribution of possible catches around that mean."),
    chart(pmf_plot, 320),
    tags$p(sprintf("In this illustration, an expected count of 500 gives a central 80%% range of about %.0f–%.0f birds. The wide, asymmetric range reflects the variation in catches learned from the records.",
      qnbinom(0.1, mu = 500, size = theta), qnbinom(0.9, mu = 500, size = theta))),
    tags$p("The website's 80% range includes this fitted day-to-day count variability. It does not include uncertainty in the weather forecast, fitted coefficients or the baseline for a future season. The full-season reference curve uses median training weather, so it shows a typical-weather pattern rather than a weather forecast for every day."),
    tags$h3("The next check: follow a new season"),
    tags$p("For a new season, we need to match each archived forecast as issued with the recorded catch on days when ringing actually takes place. We can then check daily errors, whether predictions remain systematically high or low, and how often catches fall inside the published range. That will assess the weather forecast and count model together under operational conditions."),
    tags$p(tags$span("The published forecast uses historical records through 2023 and does not adjust its annual baseline from live catches. Experiments with annual updates, alternative baselines, previous-morning rain and neural networks are described in "),
      tags$a(href = "alternatives.html", "Count model alternatives and tests"), "; they are not used in the published forecast."),
    tags$details(tags$summary("Reproduce this report"),
      tags$p("Run from the repository root:"),
      tags$pre("Rscript count/scripts/train.R\nRscript count/scripts/validate.R\nRscript count/scripts/build_report.R"),
      tags$p(class = "caption", paste("Forecast artifact:", model$version,
        "· Inputs: raw-data/daily_coverage.csv · Saved model: count/model/model.rds · Validation tables: count/intermediate-data/")))
  )))
save_html(page, file = here("count", "reports", "model.html"), libdir = "lib")
cat("Wrote count model description and validation\n")
