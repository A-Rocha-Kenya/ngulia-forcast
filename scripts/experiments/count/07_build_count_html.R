library(dplyr)
library(readr)
library(tidyr)
library(mgcv)
library(plotly)
library(htmltools)
library(here)

# Read reviewed cohort, models, and held-out predictions -------------------
result_dir <- here("validation", "research")
data <- read_csv(file.path(result_dir, "count_annual_model_data.csv"), show_col_types = FALSE) |>
  mutate(bush_period = factor(bush_period, levels = c("back_bush", "front_bush")),
    year_id = factor(season))
fits <- readRDS(file.path(result_dir, "count_annual_models.rds"))
scores <- read_csv(file.path(result_dir, "count_annual_scores.csv"), show_col_types = FALSE)
forward <- read_csv(file.path(result_dir, "count_annual_forward_predictions.csv"), show_col_types = FALSE)
forward_scores <- read_csv(file.path(result_dir, "count_annual_forward_scores.csv"), show_col_types = FALSE)
whole_predictions <- read_csv(file.path(result_dir, "count_annual_predictions.csv"), show_col_types = FALSE)
within_predictions <- read_csv(file.path(result_dir, "count_within_predictions.csv"), show_col_types = FALSE)
m3b_forward <- read_csv(file.path(result_dir, "count_m3b_forward_predictions.csv"), show_col_types = FALSE)
m3b_scores <- read_csv(file.path(result_dir, "count_m3b_forward_scores.csv"), show_col_types = FALSE)
m3b_updated_scores <- read_csv(file.path(result_dir, "count_m3b_updated_scores.csv"), show_col_types = FALSE)
m3b_recent <- read_csv(file.path(result_dir, "count_m3b_full_fit_recent_levels.csv"), show_col_types = FALSE)
m3b_paired <- read_csv(file.path(result_dir, "count_m3b_paired_differences.csv"), show_col_types = FALSE)
m3b_artifact <- readRDS(file.path(result_dir, "count_m3b_research_model.rds"))
transition <- read_csv(file.path(result_dir, "count_transition_audit.csv"), show_col_types = FALSE)
within_scores <- read_csv(file.path(result_dir, "count_within_scores.csv"), show_col_types = FALSE)
within_annual <- read_csv(file.path(result_dir, "count_within_season_scores.csv"), show_col_types = FALSE)
updated_scores <- read_csv(file.path(result_dir, "count_annual_updated_scores.csv"), show_col_types = FALSE)
updated_prior_scores <- read_csv(file.path(result_dir, "count_annual_updated_prior_scores.csv"), show_col_types = FALSE)
paired <- read_csv(file.path(result_dir, "count_annual_paired_differences.csv"), show_col_types = FALSE)
parameters <- read_csv(file.path(result_dir, "count_annual_parameters.csv"), show_col_types = FALSE)
weather_scores <- read_csv(file.path(result_dir, "count_weather_scores.csv"), show_col_types = FALSE)
weather_paired <- read_csv(file.path(result_dir, "count_weather_paired.csv"), show_col_types = FALSE)
weather_seasons <- read_csv(file.path(result_dir, "count_weather_season_scores.csv"), show_col_types = FALSE)
colors <- c(M0 = "#54728a", M1 = "#167b75", M2 = "#b87628", M3a = "#a04d86", M3b = "#536ca9")

# Reused interactive components -----------------------------------------
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
metric_chart <- function(x, metric, label) {
  plot_ly(x, x = ~model, y = x[[metric]], type = "bar",
    text = formatC(x[[metric]], digits = 2, format = "f"), textposition = "auto",
    marker = list(color = unname(colors[x$model]))) |>
    layout(xaxis = list(title = "Model"), yaxis = list(title = label), showlegend = FALSE)
}
equation <- function(x) tags$div(class = "equation", HTML(paste0(
  '<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">', x, '</math>')))
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

# First figure: raw daily catch by season, with observed mean --------------
annual_raw <- data |> group_by(season) |>
  summarise(dates = n(), total = sum(total_birds_ringed), mean = mean(total_birds_ringed),
    median = median(total_birds_ringed), .groups = "drop")
raw_count <- plot_ly(data, x = ~season, y = ~total_birds_ringed,
  type = "scatter", mode = "markers", name = "Daily count",
  marker = list(size = 5, color = "#167b75", opacity = 0.45),
  text = ~paste("Date:", ringing_date, "<br>Count:", total_birds_ringed,
    "<br>Bush:", bush_period), hoverinfo = "text") |>
  add_trace(data = annual_raw, x = ~season, y = ~mean, type = "scatter", mode = "lines+markers",
    name = "Annual daily mean", line = list(color = "#172d32", width = 2),
    text = ~sprintf("Season %d<br>Mean %.0f<br>Total %.0f<br>%d dates", season, mean, total, dates),
    hoverinfo = "text", inherit = FALSE) |>
  layout(xaxis = list(title = "Season"), yaxis = list(title = "Birds per date"),
    legend = list(orientation = "h"), height = 430)
coverage_plot <- plot_ly(data |> count(season, bush_period), x = ~season, y = ~n,
  color = ~bush_period, colors = c("#54728a", "#b87628", "#a46999"), type = "bar") |>
  layout(barmode = "stack", xaxis = list(title = "Season"), yaxis = list(title = "Dates"),
    legend = list(orientation = "h"), height = 300)

# Conditional NB distribution using the fitted M1 dispersion ---------------
theta <- fits$M1$family$getTheta(TRUE)
illustration <- tibble(count = 0:3500, probability = dnbinom(count, mu = 500, size = theta))
pmf_plot <- plot_ly(illustration, x = ~count, y = ~probability, type = "scatter", mode = "lines",
  line = list(color = "#b87628", width = 2), showlegend = FALSE) |>
  layout(xaxis = list(title = "Possible daily count", automargin = TRUE),
    yaxis = list(title = "Probability per integer count", automargin = TRUE))

# M0 season/moon figures; M1 weather figures; M2 trend/layout figures -------
m0_effects <- tags$div(class = "chart-grid",
  chart(effect_plot(fits$M0, "season_day", "Days from 20 October")),
  chart(effect_plot(fits$M0, "moon_distance_from_new_moon", "Days from new moon")))
weather_variables <- tibble(variable = c("rain_log", "wind_speed_10m_mean_ms",
    "temperature_2m_mean_c", "surface_pressure_mean_hpa",
    "total_cloud_cover_mean", "relative_humidity_mean_pct", "wind_u_10m_mean_ms", "wind_v_10m_mean_ms"),
  label = c("Rain 00–08 (mm)", "Wind speed (m/s)", "Temperature (°C)", "Pressure (hPa)",
    "Cloud fraction", "Humidity (%)", "East–west wind (m/s)", "North–south wind (m/s)"))
weather_plots <- lapply(seq_len(nrow(weather_variables)), function(i)
  effect_plot(fits$M1, weather_variables$variable[i], weather_variables$label[i]))
m1_effects <- tags$div(class = "chart-grid", lapply(weather_plots, chart))
year_reference <- data[1, ]
for (column in all.vars(formula(fits$M2))) {
  if (is.numeric(data[[column]])) year_reference[[column]] <- median(data[[column]])
}
year_reference$bush_period <- factor("front_bush", levels = levels(data$bush_period))
year_reference$season <- 2023
year_grid <- year_reference[rep(1, 150), ]
year_grid$season <- seq(min(data$season), max(data$season), length.out = 150)
difference <- sweep(predict(fits$M2, newdata = year_grid, type = "lpmatrix"),
  2, predict(fits$M2, newdata = year_reference, type = "lpmatrix")[1, ], "-")
year_grid$ratio <- exp(as.numeric(difference %*% coef(fits$M2)))
year_se <- sqrt(rowSums((difference %*% fits$M2$Vp) * difference))
year_grid$low <- year_grid$ratio * exp(-1.96 * year_se)
year_grid$high <- year_grid$ratio * exp(1.96 * year_se)
year_plot <- plot_ly(year_grid, x = ~season, showlegend = FALSE) |>
  add_lines(y = ~high, line = list(color = "transparent"), hoverinfo = "skip") |>
  add_lines(y = ~low, fill = "tonexty", fillcolor = "rgba(184,118,40,.15)",
    line = list(color = "transparent"), hoverinfo = "skip") |>
  add_lines(y = ~ratio, line = list(color = "#b87628")) |>
  layout(xaxis = list(title = "Season (continuous smooth)"),
    yaxis = list(title = "Year multiplier relative to 2023"), height = 340)
bush_grid <- year_reference[rep(1, 2), ]
bush_grid$bush_period <- factor(c("back_bush", "front_bush"), levels = levels(data$bush_period))
bush_difference <- sweep(predict(fits$M2, newdata = bush_grid, type = "lpmatrix"),
  2, predict(fits$M2, newdata = bush_grid[1, ], type = "lpmatrix")[1, ], "-")
bush_grid$ratio <- exp(as.numeric(bush_difference %*% coef(fits$M2)))
bush_grid$se <- sqrt(rowSums((bush_difference %*% fits$M2$Vp) * bush_difference))
bush_grid$low <- bush_grid$ratio * exp(-1.96 * bush_grid$se)
bush_grid$high <- bush_grid$ratio * exp(1.96 * bush_grid$se)
bush_plot <- plot_ly(bush_grid, x = ~bush_period, y = ~ratio, type = "scatter", mode = "markers",
  marker = list(color = "#b87628", size = 10),
  error_y = list(type = "data", symmetric = FALSE, array = bush_grid$high - bush_grid$ratio,
    arrayminus = bush_grid$ratio - bush_grid$low)) |>
  layout(xaxis = list(title = "Bush category"), yaxis = list(title = "Multiplier versus back bush"), height = 320)

# Annual totals are always sums over the same included recorded dates ------
annual_total_plot <- function(prediction_data, model_name, known_year = NULL) {
  annual <- prediction_data |> filter(model == model_name) |> group_by(season) |>
    summarise(dates = n(), observed_total = sum(observed), estimated_total = sum(predicted), .groups = "drop")
  result <- plot_ly(annual, x = ~season) |>
    add_lines(y = ~observed_total, name = "Observed total", line = list(color = "#172d32", width = 2),
      text = ~sprintf("Season %d<br>Observed %s<br>%d dates", season,
        format(observed_total, big.mark = ","), dates), hoverinfo = "text") |>
    add_lines(y = ~estimated_total, name = paste(model_name, "estimated total"),
      line = list(color = colors[[model_name]], width = 2),
      text = ~sprintf("Season %d<br>Predicted %s<br>%d dates", season,
        format(round(estimated_total), big.mark = ","), dates), hoverinfo = "text")
  if (!is.null(known_year)) {
    known <- known_year |> filter(model == model_name) |> group_by(season) |>
      summarise(known_total = sum(predicted), .groups = "drop")
    result <- result |> add_lines(data = known, x = ~season, y = ~known_total,
      name = "M3a with other dates known", inherit = FALSE,
      line = list(color = "#bd88a8", dash = "dot"))
  }
  result |> layout(xaxis = list(title = "Season"),
    yaxis = list(title = "Total birds on included recorded dates"), legend = list(orientation = "h"))
}

# M3a deviations and M3b's recent adjusted levels -------------------------
annual_grid <- year_reference[rep(1, n_distinct(data$season)), ]
annual_grid$season <- sort(unique(data$season))
annual_grid$year_id <- factor(annual_grid$season, levels = levels(data$year_id))
annual_effect <- predict(fits$M3a, newdata = annual_grid, type = "terms",
  terms = "s(year_id)", se.fit = TRUE)
annual_grid$ratio <- exp(as.numeric(annual_effect$fit))
annual_grid$low <- exp(as.numeric(annual_effect$fit - 1.96 * annual_effect$se.fit))
annual_grid$high <- exp(as.numeric(annual_effect$fit + 1.96 * annual_effect$se.fit))
annual_effect_plot <- plot_ly(annual_grid, x = ~season, y = ~ratio, type = "scatter", mode = "markers",
  marker = list(color = colors[["M3a"]], size = 8),
  error_y = list(type = "data", symmetric = FALSE, array = annual_grid$high - annual_grid$ratio,
    arrayminus = annual_grid$ratio - annual_grid$low),
  text = ~sprintf("Season %d<br>Annual multiplier %.2f<br>95%% interval %.2f–%.2f",
    season, ratio, low, high), hoverinfo = "text") |>
  layout(xaxis = list(title = "Season"), yaxis = list(title = "Deviation from smooth annual level"))
m3b_recent <- m3b_recent |> mutate(level = exp(log_level), point_size = 6 + 30 * weight)
recent_plot <- plot_ly(m3b_recent, x = ~season, y = ~level,
  type = "scatter", mode = "lines+markers", name = "Weather-adjusted annual level",
  line = list(color = colors[["M3b"]]), marker = list(size = m3b_recent$point_size),
  text = ~sprintf("Season %d<br>Adjusted level %.2f<br>Weight %.1f%%<br>%d dates",
    season, level, 100 * weight, dates), hoverinfo = "text") |>
  add_lines(data = tibble(season = 2014:2026, level = m3b_artifact$prior$expected_level),
    x = ~season, y = ~level, name = "Recent weighted baseline",
    line = list(color = "#172d32", dash = "dash"), inherit = FALSE) |>
  layout(xaxis = list(title = "Season"),
    yaxis = list(title = "Multiplier relative to shared weather/calendar response"),
    legend = list(orientation = "h"))

# Matched validation plots and score tables ---------------------------------
metric_plots <- lapply(c("mean_poisson_deviance", "log_rmse", "mean_season_spearman"), function(metric) {
  label <- c(mean_poisson_deviance = "Poisson deviance ↓", log_rmse = "Log-count RMSE ↓",
    mean_season_spearman = "Season rank correlation ↑")[[metric]]
  metric_chart(scores, metric, label)
})
within_annual_plot <- plot_ly(within_annual, x = ~season, y = ~exp(annual_log_error),
  color = ~model, colors = unname(colors[c("M2", "M3a")]), type = "scatter", mode = "lines+markers",
  text = ~sprintf("Season %d<br>%s<br>Predicted/observed annual mean %.2f",
    season, model, exp(annual_log_error)), hoverinfo = "text") |>
  layout(xaxis = list(title = "Season"), yaxis = list(title = "Predicted / observed mean catch"),
    legend = list(orientation = "h"))
learning_curve <- m3b_updated_scores |> filter(calibration_dates <= 10) |>
  mutate(model = factor(model, levels = c("M2", "M3b prior", "M3b updated")))
learning_plot <- plot_ly(learning_curve, x = ~calibration_dates, y = ~mean_poisson_deviance,
  color = ~model, colors = c(colors[["M2"]], "#8ea4d0", colors[["M3b"]]),
  type = "scatter", mode = "lines+markers",
  text = ~sprintf("%s<br>%d observed dates<br>%d test dates across %d seasons<br>Deviance %.1f",
    model, calibration_dates, dates, seasons, mean_poisson_deviance), hoverinfo = "text") |>
  layout(xaxis = list(title = "Current-year dates already observed", tickvals = 0:10),
    yaxis = list(title = "Deviance on subsequent dates ↓"), legend = list(orientation = "h"))
weather_coefficients <- bind_rows(lapply(c("M2", "M3a"), function(name) {
  coefficients <- summary(fits[[name]])$p.table
  tibble(model = name, term = c("total_cloud_cover_mean", "relative_humidity_mean_pct",
    "wind_u_10m_mean_ms", "wind_v_10m_mean_ms"),
    log_multiplier = coefficients[c("total_cloud_cover_mean", "relative_humidity_mean_pct",
      "wind_u_10m_mean_ms", "wind_v_10m_mean_ms"), "Estimate"])
}))

# Scientific explanation beside the figures -------------------------------
eta0 <- '<mi>log</mi><mo>(</mo><mi>μ</mi><mo>)</mo><mo>=</mo><msub><mi>η</mi><mn>0</mn></msub><mo>=</mo><msub><mi>β</mi><mn>0</mn></msub><mo>+</mo><msub><mi>f</mi><mi>D</mi></msub><mo>(</mo><mtext>season day</mtext><mo>)</mo><mo>+</mo><msub><mi>f</mi><mi>L</mi></msub><mo>(</mo><mtext>moon distance</mtext><mo>)</mo>'
eta1 <- '<msub><mi>η</mi><mn>1</mn></msub><mo>=</mo><msub><mi>η</mi><mn>0</mn></msub><mo>+</mo><mtext>daily ERA5 weather terms</mtext>'
eta2 <- '<msub><mi>η</mi><mn>2</mn></msub><mo>=</mo><msub><mi>η</mi><mn>1</mn></msub><mo>+</mo><mi>g</mi><mo>(</mo><mtext>year</mtext><mo>)</mo><mo>+</mo><msub><mi>β</mi><mi>F</mi></msub><mi>I</mi><mo>(</mo><mtext>front</mtext><mo>)</mo>'
eta3a <- '<msub><mi>η</mi><mtext>3a</mtext></msub><mo>=</mo><msub><mi>η</mi><mn>2</mn></msub><mo>+</mo><msub><mi>b</mi><mi>y</mi></msub><mo>,</mo><mspace width="1em"/><msub><mi>b</mi><mi>y</mi></msub><mo>∼</mo><mi>N</mi><mo>(</mo><mn>0</mn><mo>,</mo><msubsup><mi>σ</mi><mtext>year</mtext><mn>2</mn></msubsup><mo>)</mo>'
eta3b <- '<msub><mi>η</mi><mtext>3b</mtext></msub><mo>=</mo><msub><mi>η</mi><mn>2</mn></msub><mo>−</mo><mi>g</mi><mo>(</mo><mtext>year</mtext><mo>)</mo><mo>+</mo><msub><mi>a</mi><mtext>current year</mtext></msub>'
css <- 'html{scroll-behavior:smooth}body{margin:0;background:#fff;color:#243137;font:16px/1.55 Arial,Helvetica,sans-serif}main{max-width:1060px;margin:0 auto;padding:28px 24px 60px}h1{font-size:2rem;line-height:1.25}h2{margin:40px 0 18px;border-bottom:1px solid #ddd;padding-bottom:10px}h3{font-size:1.05rem;margin:24px 0 10px}nav{display:flex;flex-wrap:wrap;gap:16px;margin-bottom:25px}a{color:#167b75}ul{padding-left:23px}li{margin:6px 0}.equation{padding:16px;background:#f5f7f8;overflow-x:auto;margin:18px 0}math{font-size:1.12rem}.table-scroll{overflow-x:auto;margin:18px 0}table{border-collapse:collapse;width:100%;font-size:.9rem}th,td{padding:9px 12px;text-align:left;border-bottom:1px solid #ddd}th{background:#f5f7f8;white-space:nowrap}.caption{font-size:.9rem;color:#56636a}.chart{display:block;width:100%;min-width:0;margin:0 0 22px;overflow:hidden}.chart-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));column-gap:24px}.html-widget{display:block;max-width:100%}.chart-grid .chart{margin-bottom:18px}@media(max-width:750px){main{padding:20px 12px}.chart-grid{grid-template-columns:1fr}h1{font-size:1.6rem}}'

recent_one <- filter(m3b_scores, horizon == 1, period == "2014–2023")
recent_two <- filter(m3b_scores, horizon == 2, period == "2014–2023")
recent_three <- filter(m3b_scores, horizon == 3, period == "2014–2023")
learning_selected <- m3b_updated_scores |> filter(calibration_dates %in% c(0, 5, 10))
weather_recent <- weather_scores |>
  filter(period == "2014–2023") |>
  select(model, dates, mean_poisson_deviance, mean_absolute_error, predicted_mean)
weather_recent$model <- recode(weather_recent$model,
  M2_previous_rain = "M2 + prior-morning rain")
lag_seasons <- weather_seasons |>
  filter(season >= 2014, model %in% c("M2", "M2_previous_rain")) |>
  select(season, model, deviance) |>
  pivot_wider(names_from = model, values_from = deviance) |>
  mutate(difference = M2_previous_rain - M2)
weather_plot <- plot_ly(lag_seasons, x = ~season, y = ~difference,
  type = "bar", marker = list(color = "#167b75")) |>
  layout(xaxis = list(title = "Forecast season", tickmode = "linear", dtick = 1),
    yaxis = list(title = "Prior-morning rain − M2 deviance (lower is better)"),
    showlegend = FALSE)
weather_uncertainty <- weather_paired |>
  filter(period == "2014–2023") |>
  mutate(model = "Prior-morning rain") |>
  select(model, period, improved_seasons, deviance_difference, low, high)
page <- tags$html(lang = "en", tags$head(tags$meta(charset = "utf-8"),
  tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
  tags$title("Ngulia daily catch: annual levels and live updates"), tags$style(HTML(css))),
  tags$body(tags$main(
    tags$h1("Daily count models at Ngulia"),
    tags$nav(tags$a(href = "#raw", "Raw counts"), tags$a(href = "#m0", "M0"),
      tags$a(href = "#m1", "M1"), tags$a(href = "#m2", "M2"),
      tags$a(href = "#weather", "Weather tests"),
      tags$a(href = "#m3a", "M3a"), tags$a(href = "#m3b", "M3b"),
      tags$a(href = "#validation", "Validation"), tags$a(href = "#live", "Live updates")),
    tags$p("In these retrospective tests, M2 extrapolates its historical smooth-year trend. The live forecast holds that term at 2023 for later seasons. M3a adds a random annual deviation but has no information about an unseen year's direction. M3b shares M2's fitted weather response, removes the year smooth from the forecast, uses recent adjusted annual levels for the new-year baseline, and updates that baseline as counts arrive."),
    tags$p(sprintf("In 2014–2023 forward tests, M3b's one-year deviance was %.1f versus %.1f for M2. At two and three years ahead it was %.1f versus %.1f and %.1f versus %.1f. These historical ERA5 results are exploratory; none assesses issued-weather errors.",
      recent_one$mean_poisson_deviance[recent_one$model == "M3b"], recent_one$mean_poisson_deviance[recent_one$model == "M2"],
      recent_two$mean_poisson_deviance[recent_two$model == "M3b"], recent_two$mean_poisson_deviance[recent_two$model == "M2"],
      recent_three$mean_poisson_deviance[recent_three$model == "M3b"], recent_three$mean_poisson_deviance[recent_three$model == "M2"])),
    tags$h2(id = "raw", "Recorded daily counts and coverage"), chart(raw_count, 420),
    tags$p(sprintf("%d dates, %d seasons (1977–2023), including %d documented operated zeros. Counts exclude targeted swallow/martin captures. Annual totals below sum only these same included dates; they are not estimates for unobserved calendar dates.",
      nrow(data), n_distinct(data$season), sum(data$total_birds_ringed == 0))),
    chart(coverage_plot, 300),
    tags$p(class = "caption", sprintf("1994–1995: %d back-only/front-only dates retained. Eighteen mixed dates and four unclassified dates are excluded.", sum(transition$included))),
    tags$h3("Negative-binomial count variability"), chart(pmf_plot, 330),
    tags$p(class = "caption", sprintf("Conditional example for 500 expected birds using M1's fitted dispersion θ = %.2f.", theta)),
    tags$h2(id = "m0", "M0 · Season and moon"), equation(eta0), m0_effects,
    tags$p(class = "caption", "Effects are catch multipliers at otherwise fixed inputs; shading shows pointwise 95% coefficient intervals."),
    tags$h2(id = "m1", "M1 · Add daily ERA5 weather"), equation(eta1),
    tags$p("Eight daily ERA5 weather inputs, each available from the Open-Meteo ECMWF forecast or derivable from its hourly wind speed and direction. Wind speed and its components are correlated; these curves are conditional associations."), m1_effects,
    tags$h2(id = "m2", "M2 · Historical year smooth and bush layout"), equation(eta2),
    tags$p("The year term is a penalized cubic spline (basis dimension six). Extrapolation carries its fitted right-edge slope beyond 2023; increasing spline flexibility is not an annual forecasting solution."),
    chart(year_plot, 350), chart(bush_plot, 320),
    tags$p(class = "caption", "The layout contrast overlaps with era and is a predictive association rather than a measured effort correction."),
    tags$h3("M2 annual totals · held-out seasons"),
    chart(annual_total_plot(whole_predictions, "M2"), 400),
    tags$p(class = "caption", "One prediction per included date from five whole-season folds; the annual estimates use historical ERA5 weather for those same dates. Lines do not represent an independently forecast number of operated dates."),
    tags$h2(id = "weather", "M2 + prior-morning rain · Rolling next-year tests"),
    tags$p("Every tested weather input can be requested from the Open-Meteo ECMWF feed or derived from its hourly values. M2 uses eight daily weather inputs and has no cloud-base term. Only 2014–2023 seasons are evaluated here; earlier seasons can still enter each model's training set. Every test season is fitted only on earlier years and all candidates predict the same operated dates using historical ERA5 weather."),
    score_table(weather_recent, c(model = "Candidate", dates = "Dates",
      mean_poisson_deviance = "Deviance ↓", mean_absolute_error = "Mean absolute error ↓",
      predicted_mean = "Predicted mean")),
    tags$p(sprintf("On recent dates, M2 has deviance %.1f and mean absolute error %.0f birds per date. Adding prior-morning rain gives %.1f and %.0f. These are paired comparisons on the same held-out dates.",
      weather_scores$mean_poisson_deviance[weather_scores$model == "M2" & weather_scores$period == "2014–2023"],
      weather_scores$mean_absolute_error[weather_scores$model == "M2" & weather_scores$period == "2014–2023"],
      weather_scores$mean_poisson_deviance[weather_scores$model == "M2_previous_rain" & weather_scores$period == "2014–2023"],
      weather_scores$mean_absolute_error[weather_scores$model == "M2_previous_rain" & weather_scores$period == "2014–2023"])),
    score_table(weather_uncertainty, c(model = "Candidate", period = "Test period",
      improved_seasons = "Seasons improved", deviance_difference = "Deviance difference",
      low = "95% lower", high = "95% upper")),
    chart(weather_plot, 350),
    tags$p(class = "caption", "Each bar is prior-morning rain minus M2 for one fully held-out season. Negative bars favor rain. The paired season-bootstrap interval conditions on this already explored candidate."),
    tags$p("Earlier cloud, overnight and hourly CNN screens are summarized in ",
      tags$a(href = "../../research/count/contenders.md", "the contender notes"), "."),
    tags$p("Operational audit: a live query for Ngulia returned temperature, humidity, precipitation, pressure, cloud and wind, including yesterday when requested with past_days=1. The API accepted a cloud_base field name but returned only null values, so cloud base was excluded from all active count models. See ",
      tags$a(href = "https://open-meteo.com/en/docs/ecmwf-api", "ECMWF API documentation"), "."),
    tags$p("Historical issued forecasts are for operational evaluation, not required to train these models: the fits above use ERA5 and observed counts. Open-Meteo's archived ECMWF individual runs start in March 2024, after the latest count labels in this dataset, so they cannot reconstruct the 2014–2023 forecasts as issued. The scheduled website workflow archives its selected M2 forecast; later count observations are still needed to score issued-weather accuracy. See ",
      tags$a(href = "https://open-meteo.com/en/docs/single-runs-api", "Single Runs API availability"), "."),
    tags$p("Decision: M2 is the published count model. Prior-morning rain improves the retrospective average, but its season-level interval includes no gain. Revisit it after obtaining new observed counts and comparable issued forecasts. Older years remain useful for fitting, but are not part of the weather-model choice score here."),
    tags$h2(id = "m3a", "M3a · Smooth year plus annual random intercept"), equation(eta3a),
    tags$p(sprintf("M3a retains M2's weather, layout and smooth year trend, then adds a partially pooled annual intercept. The fitted annual log-scale SD is %.2f. That intercept may combine effort, population variation and other annual changes.",
      parameters$annual_sd[parameters$model == "M3a"])),
    chart(annual_effect_plot, 360),
    tags$p(class = "caption", "Annual deviations relative to M3a's smooth trend. Pointwise intervals exclude variance-component uncertainty."),
    tags$details(tags$summary("Weather coefficients in M2 and M3a"),
      tags$p("Linear coefficients are conditional log catch associations per unit. M3a's annual intercept does not make every weather effect exclusively within-year."),
      score_table(weather_coefficients, c(model = "Model", term = "Weather input", log_multiplier = "Log multiplier"))),
    tags$h3("M3a annual totals · unseen versus known years"),
    chart(annual_total_plot(whole_predictions, "M3a", within_predictions), 400),
    tags$p(class = "caption", "Solid model line: whole seasons held out, so the annual intercept is unknown. Dotted line: chronological date blocks held out while other dates from the same year estimate its intercept. Both are evaluated on the same included dates; the known-year test is retrospective."),
    tags$h2(id = "m3b", "M3b · Shared weather response and recent annual baseline"), equation(eta3b),
    tags$ul(tags$li("Fit M2 to historical dates so its calendar, moon and weather terms use the long record. The historical smooth year term helps prevent annual level changes from being assigned to weather."),
      tags$li("For each historical season, remove the fitted year-smooth term and compare observed totals with the resulting weather/calendar expectations. This estimates an adjusted annual catch level."),
      tags$li("Use the most recent 10 annual levels, giving their weights a five-year half-life. This defines a constant pre-season level for future years rather than extending the spline's slope. The weights apply only to the annual baseline, not to the weather fit."),
      tags$li("Treat remaining annual variation as a Gaussian distribution on log level. New observed counts update that distribution through the negative-binomial likelihood; the weather response remains fixed.")),
    tags$p("The year smooth is therefore still used when estimating weather effects from historical data. It is not a future population-trend forecast in M3b. A stable recent annual baseline is an explicit assumption, and the smooth alone cannot prove that weather associations come only from within-year variation."),
    chart(recent_plot, 370),
    tags$p(class = "caption", sprintf("Marker size shows recency weight. The dashed baseline is the weighted expected annual multiplier %.2f from 2014–2023; the uncertainty SD on log scale is %.2f. This does not assert that future effort or population size will be constant.",
      m3b_artifact$prior$expected_level, m3b_artifact$prior$annual_sd)),
    tags$h3("M3b annual totals · one-year-ahead forecast"),
    chart(annual_total_plot(filter(m3b_forward, horizon == 1), "M3b"), 400),
    tags$p(class = "caption", "Each season 1997–2023 is forecast using only earlier seasons. Annual totals sum predictions over the same observed operated dates, using retrospective ERA5 weather."),
    tags$h2(id = "validation", "Validation · Different questions need different splits"),
    tags$p("Whole-season folds test transfer to seasons unseen by training. Within-year blocks test prediction when some dates from that year are already known. Rolling origins train only through the previous season and test one, two or three years ahead; they are the relevant check for 2025+ use."),
    tags$h3("Whole-season folds · M0 through M3a"),
    tags$div(class = "chart-grid", chart(metric_plots[[1]], 320), chart(metric_plots[[3]], 320)),
    score_table(scores, c(model = "Model", mean_poisson_deviance = "Deviance ↓",
      annual_log_rmse = "Annual mean log error ↓", within_log_rmse = "Centered daily log error ↓",
      mean_negative_binomial_log_loss = "Predictive log loss ↓", interval_80_coverage = "80% coverage")),
    score_table(filter(paired, evaluation == "whole-season"),
      c(candidate = "Compared with M2", difference = "Deviance difference", low = "95% lower", high = "95% upper")),
    tags$p(class = "caption", "Paired season-bootstrap intervals condition on the already explored models. Negative differences favor the candidate."),
    tags$h3("Known-year date blocks · M2 and M3a"),
    tags$div(class = "chart-grid",
      chart(metric_chart(within_scores, "annual_log_rmse", "Annual mean log error ↓"), 320),
      chart(metric_chart(within_scores, "within_log_rmse", "Centered daily log error ↓"), 320)),
    score_table(within_scores, c(model = "Model", mean_poisson_deviance = "Deviance ↓",
      mean_season_spearman = "Within-season rank ↑", annual_log_rmse = "Annual mean log error ↓",
      within_log_rmse = "Centered daily log error ↓")),
    tags$p("M3a improves the known-year level but slightly worsens centered daily error and rank. The date-block fit can use later dates from that season, so it does not simulate live updates."),
    tags$h3("Rolling forecasts · M2 versus M3b"),
    score_table(m3b_scores, c(model = "Model", horizon = "Years ahead", period = "Test period",
      dates = "Dates", mean_poisson_deviance = "Deviance ↓", annual_log_rmse = "Annual mean log error ↓",
      predicted_mean = "Predicted mean", observed_mean = "Observed mean",
      mean_predictive_log_loss = "Predictive log loss ↓", interval_80_coverage = "80% coverage")),
    score_table(m3b_paired, c(horizon = "Years ahead", period = "Test period",
      difference = "M3b − M2 deviance", low = "95% lower", high = "95% upper")),
    tags$p(class = "caption", "Every paired 95% season-bootstrap interval spans zero. The M3b year-level rule was proposed after examining historical count results, so these estimates are exploratory. All tests use ERA5 weather rather than archived issued forecasts."),
    tags$h2(id = "live", "Live annual updates · One new count at a time"),
    tags$p("Before the season, M3b forecasts with its recent annual baseline. For each completed operated date, enter the comparable non-swallow count and matched weather. Its negative-binomial likelihood updates only the current annual level; no whole-model refit is needed. Predict the remaining dates with the posterior average annual multiplier. Missing dates are never entered as zero."),
    chart(learning_plot, 390),
    score_table(learning_selected, c(model = "Model", calibration_dates = "Known dates", dates = "Remaining test dates",
      seasons = "Seasons", mean_poisson_deviance = "Deviance ↓", annual_log_rmse = "Annual mean log error ↓",
      predicted_mean = "Predicted mean", observed_mean = "Observed mean", interval_80_coverage = "80% coverage")),
    tags$p(class = "caption", "All three lines are evaluated on identical subsequent dates at each known-date count. Moving right changes the test cohort; compare lines vertically, not the slope of one line alone. The full day-by-day trajectory is saved in count_m3b_updated_scores.csv."),
    tags$p("The M2 specification is published for count forecasts with front-bush layout. This comparison remains retrospective: it uses historically selected operated dates, incomplete historical zero coverage and reanalysis weather. The live forecast holds M2's year term at its 2023 value for later seasons; that operational extrapolation rule and issued-weather accuracy have not been retrospectively scored."),
    tags$p(class = "caption", "Rebuild: run 08_compare_m0_m2.R, 10_verify_count_annual.R, 12_compare_m3b.R, 14_verify_count_m3b.R, 15_compare_count_weather.R, 17_verify_count_weather.R, and 07_build_count_html.R with Rscript. Keep count_model_report_lib/ beside this HTML file.")
  )))
save_html(page, file = file.path(result_dir, "count_model_report.html"), libdir = "count_model_report_lib")
cat("Wrote M2/M3a/M3b interactive count report\n")
