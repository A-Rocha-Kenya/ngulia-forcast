library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)
library(plotly)
library(htmltools)
library(here)

# Read saved observations and validation results ---------------------------
out <- here("mist", "intermediate-data")
labels <- read_csv(file.path(out, "labels.csv"), show_col_types = FALSE)
means <- read_csv(file.path(out, "night_means.csv"), show_col_types = FALSE)
predictions <- read_csv(file.path(out, "predictions.csv"), show_col_types = FALSE)
scores <- read_csv(file.path(out, "scores.csv"), show_col_types = FALSE)
fold_scores <- read_csv(file.path(out, "fold_scores.csv"), show_col_types = FALSE)
differences <- read_csv(file.path(out, "paired_differences.csv"), show_col_types = FALSE)
summary <- fromJSON(file.path(out, "run_summary.json"))
config <- summary$config
models <- scores$model
model_names <- c("Historical frequency", "Average weather", "Hourly model (used live)")
colors <- c("#7c8995", "#246b93", "#167b75")
logistic <- filter(scores, model == models[2])
cnn <- filter(scores, model == models[3])
paired <- filter(differences, baseline == models[2], metric == "brier")
wins <- fold_scores |> select(model, fold, brier) |> pivot_wider(names_from = model, values_from = brier)
wins <- sum(wins[[models[3]]] < wins[[models[2]]])

# Keep the research summaries and independent full-data GLM check ----------
calibration <- predictions |> mutate(bin = cut(probability, seq(0, 1, .1), include.lowest = TRUE)) |>
  group_by(model, bin) |> summarise(dates = n(), predicted = mean(probability),
    observed = mean(mist_present), .groups = "drop")
annual_predictions <- predictions |> group_by(model, season) |>
  summarise(predicted = mean(probability), observed = mean(mist_present), dates = n(), .groups = "drop")
write_csv(calibration, file.path(out, "calibration.csv"))
write_csv(annual_predictions, file.path(out, "annual_predictions.csv"))
fit_data <- left_join(labels, means, by = "ringing_date")
fit <- glm(reformulate(config$features, "mist_present"), data = fit_data, family = binomial())
coefficients <- read_csv(file.path(out, "logistic_coefficients.csv"), show_col_types = FALSE)
stopifnot(max(abs(unname(coef(fit)) - coefficients$coefficient)) < 1e-4)
saveRDS(fit, here("mist", "model", "logistic_effects.rds"))

# Two figures: model comparison and probability calibration ----------------
chart <- function(widget, height = 380) {
  widget <- plotly_build(widget)
  widget$width <- "100%"
  widget$height <- height
  widget$x$layout$height <- height
  widget$x$layout$margin <- list(l = 85, r = 30, b = 85, t = 35)
  widget$sizingPolicy$browser$fill <- FALSE
  tags$div(class = "chart", plotly::config(widget, responsive = TRUE, displaylogo = FALSE))
}
score_plot <- plot_ly(scores, x = ~brier, y = ~factor(model, levels = rev(models),
  labels = c("Hourly model<br>(used live)", "Average<br>weather", "Historical<br>frequency")),
  type = "bar", orientation = "h", marker = list(color = colors),
  text = ~sprintf("%.4f", brier), textposition = "auto", hoverinfo = "x+y") |>
  layout(xaxis = list(title = "Brier score", rangemode = "tozero"),
    yaxis = list(title = "", automargin = TRUE), showlegend = FALSE)
calibration_plot <- plot_ly(filter(calibration, model == models[3]), x = ~predicted, y = ~observed,
  type = "scatter", mode = "lines+markers", line = list(color = "#167b75"),
  marker = list(color = "#167b75", size = 8),
  text = ~sprintf("%d test dates<br>Average forecast: %.0f%%<br>Mist recorded: %.0f%%", dates, 100 * predicted, 100 * observed),
  hoverinfo = "text") |>
  layout(xaxis = list(title = "Forecast chance of mist", range = c(0, 1), tickformat = ".0%"),
    yaxis = list(title = "How often mist was recorded", range = c(0, 1), tickformat = ".0%"),
    showlegend = FALSE,
    shapes = list(list(type = "line", x0 = 0, x1 = 1, y0 = 0, y1 = 1,
      line = list(color = "#aaa", dash = "dot"))))

# A short story, with research detail available on demand ------------------
css <- 'body{margin:0;background:#fff;color:#243137;font:16px/1.6 Arial,Helvetica,sans-serif}main{max-width:1120px;margin:auto;padding:30px 24px 65px}h1{font-size:2rem;line-height:1.25}h2{margin:42px 0 18px;border-bottom:1px solid #ddd;padding-bottom:9px}h3{font-size:1.15rem;margin:26px 0 10px}nav{display:flex;flex-wrap:wrap;gap:14px;margin:20px 0}a{color:#167b75}li{margin:6px 0}.caption{color:#56636a;font-size:.9rem}.chart{display:block;width:100%;min-width:0;margin:0 0 24px;overflow:hidden}.html-widget{display:block;max-width:100%}.report-lead{font-size:1.1rem}.report-note{padding:18px;background:#f5f7f8;border-left:3px solid #167b75;margin:24px 0}.report-note p{margin-bottom:0}.table-wrap{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:.9rem}th,td{padding:9px;border-bottom:1px solid #ddd;text-align:left}pre{white-space:pre-wrap;overflow-wrap:anywhere;background:#f5f7f8;padding:16px;font-size:.85rem}details{margin:18px 0}summary{cursor:pointer;color:#167b75}@media(max-width:700px){main{padding:18px 12px}h1{font-size:1.6rem}}'
page <- tags$html(lang = "en", tags$head(tags$meta(charset = "utf-8"),
  tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
  tags$title("How the Ngulia mist forecast works"), tags$style(HTML(css))),
  tags$body(tags$main(
    tags$h1("Will there be mist?"),
    tags$p(class = "report-lead", "The mist forecast follows the weather through the night to estimate the chance of mist being recorded at Ngulia. Here is what that number means, how the model learns, and how we have checked it."),
    tags$nav(tags$a(href = "#model", "The model"), tags$a(href = "#validation", "Season checks"),
      tags$a(href = "#probabilities", "Reading the probabilities"), tags$a(href = "#forecast", "The live forecast")),
    tags$div(class = "report-note", tags$strong("What does 70% mean?"),
      tags$p("The model estimates a 7-in-10 chance of any recorded mist under those weather conditions. Light or patchy mist counts too. The number does not tell us how thick the mist will be, how long it will last, or how many birds will be caught.")),

    tags$h2(id = "model", "1. Learn the weather patterns of misty nights"),
    tags$p(sprintf("The model learns from %s dates with field observations across %d seasons, from %d to %d. Each observation says whether mist was recorded. About %.0f%% of these dates had some mist; this describes the recorded sample, rather than every night at Ngulia.",
      format(summary$dates, big.mark = ",", trim = TRUE), summary$seasons, summary$first_season,
      summary$last_season, 100 * summary$observed_frequency)),
    tags$p("For each ringing date, it reads twelve hourly weather records, from 21:00 the previous evening to 08:00 that morning, in Ngulia local time. The inputs are cloud cover, humidity, east–west and north–south wind, temperature, and the gap between temperature and dew point."),
    tags$p("A small convolutional neural network (CNN) learns patterns across neighbouring hours and combines them into one probability. It retains the sequence of weather changes rather than reducing the whole night to averages. The forecast averages three trained networks to give its final estimate."),
    tags$details(tags$summary("A little more about the model"),
      tags$p("The input has 12 hours and six weather channels. Dew-point depression is the temperature minus dew point, derived consistently from temperature and humidity. All inputs are standardised using training data."),
      tags$p("Each network has two short convolution layers and 449 fitted parameters. Training treats any recorded mist as present, including light, sustained and unspecified mist. Nights without an observation or with incomplete weather are excluded. Bird counts, bush layout and calendar or moon terms do not enter this model."),
      tags$p("The 21:00–08:00 window was retained because longer histories brought little benefit or performed worse. The full-data ensemble supplies the live forecast; the checks below use separate predictions for seasons left out of training."),
      tags$p(tags$a(href = "../selection_notes.md", "Model selection record"), tags$span(" · "),
        tags$a(href = "../config.json", "Training configuration"))),

    tags$h2(id = "validation", "2. Check it on seasons left out of training"),
    tags$p("A model can look convincing on the observations it learned from. To test it, we divide the seasons into five groups, train on four groups and predict the remaining group. Repeating this gives a test prediction for every included date. A season's observations never appear in both its training and test data."),
    tags$p("We compare three approaches on exactly the same dates:"),
    tags$ul(
      tags$li(tags$strong("Historical frequency:"), " use the fraction of training dates with mist, with no weather input."),
      tags$li(tags$strong("Average weather:"), " use a simple logistic model of the six weather inputs averaged over the night."),
      tags$li(tags$strong("Hourly model:"), " use the weather sequence. This is the CNN used by the website.")),
    tags$p("The figure uses the Brier score: a measure of error in the predicted probabilities. Lower is better, and zero would be perfect. A confident forecast that turns out wrong receives a larger penalty."),
    chart(score_plot, 300),
    tags$p(sprintf("The hourly model scores %.4f, compared with %.4f for average weather: a %.1f%% reduction in probability error. It performs better in all %d season groups. This supports using it over the simpler comparison models, while leaving room for mistakes.",
      cnn$brier, logistic$brier, 100 * (1 - cnn$brier / logistic$brier), wins)),
    tags$div(class = "report-note", tags$strong("This is not yet a next-season test"),
      tags$p("The season groups mix earlier and later years, so a model can learn from years later than its test season. This checks predictions for omitted seasons. A chronological test using only the past, like the count report, still needs to be done.")),
    tags$details(tags$summary("Detailed scores and validation method"),
      tags$div(class = "table-wrap", tags$table(
        tags$thead(tags$tr(tags$th("Approach"), tags$th("Brier ↓"), tags$th("Log loss ↓"), tags$th("AUC ↑"))),
        tags$tbody(lapply(seq_len(nrow(scores)), function(i) tags$tr(tags$td(model_names[i]),
          tags$td(sprintf("%.4f", scores$brier[i])), tags$td(sprintf("%.4f", scores$log_loss[i])),
          tags$td(sprintf("%.4f", scores$auc[i]))))))),
      tags$p("Log loss is another measure of probability error, with a strong penalty for confident mistakes. AUC measures how well the model ranks mist dates above no-mist dates; 0.5 is chance ranking and 1 is perfect. AUC is not the percentage of forecasts that are correct."),
      tags$p(sprintf("The hourly-minus-average-weather Brier difference is %+.4f, with a paired season-bootstrap 95%% interval of [%+.4f, %+.4f]. This resamples whole seasons to retain dependence between dates. It describes uncertainty in these saved comparisons, not uncertainty from the earlier model search.",
        paired$difference, paired$low, paired$high)),
      tags$p("The five season groups are fixed. Scaling, fitting and early stopping use training data only; test observations are excluded. Pooled scores give each date equal weight. The model was selected after exploratory comparisons on this historical record, so these scores are not an untouched final test."),
      tags$p("The hourly CNN also allows more flexible relationships than the average-weather model. Its improvement cannot be attributed to hour ordering alone."),
      tags$p(tags$a(href = "../intermediate-data/scores.csv", "Download scores (CSV)"))),

    tags$h2(id = "probabilities", "3. Do the probabilities match how often mist occurs?"),
    tags$p("If dates forecast around 70% have mist about seven times out of ten, that probability is useful. The figure below groups the hourly model's test forecasts by their predicted chance and compares them with how often mist was actually recorded."),
    tags$p("Dots near the dashed diagonal indicate agreement. Below it, the forecast chance was too high; above it, it was too low. Hover over a dot to see how many dates support it."),
    chart(calibration_plot, 400),
    tags$p("The probabilities do not match perfectly. Small groups of dates can also give unstable comparisons. The plot is a check on average behaviour across similar forecasts, rather than a guarantee for any single night."),
    tags$p("Light or patchy mist is harder to distinguish than sustained mist in these historical checks. Both still count as mist for the forecast. The model predicts what observers recorded, and the records do not provide a precise measurement of visibility or mist intensity."),
    tags$p(class = "caption", "Each dot summarises a 10-percentage-point forecast group. All forecasts shown are from seasons left out of fitting; no uncertainty bars are shown."),

    tags$h2(id = "forecast", "4. What remains to check in a new season?"),
    tags$p("The live forecast uses issued Open-Meteo ECMWF hourly weather, including the preceding evening, and the saved three-network model. It reports a chance of mist separately from the expected bird count."),
    tags$p("The historical checks above use reconstructed ERA5 weather. They do not measure the accuracy of the issued weather forecast or how the model performs in today's conditions. The current dataset has no observed mist labels after 2013, so recent performance remains unmeasured."),
    tags$p("For a new season, we need to record whether mist occurs and match each observation with the archived forecast as it was issued. That will let us check probability error and whether a forecast such as 70% still corresponds to mist on roughly seven out of ten comparable dates."),
    tags$details(tags$summary("Reproduce the report and analysis"),
      tags$p("To rebuild this page from saved results, run from the repository root:"),
      tags$pre("Rscript mist/scripts/03_build_report.R\npython3 scripts/publish_reports.py"),
      tags$p("To repeat data preparation, model training and validation, with the local ERA5 archive available:"),
      tags$pre("python3 -m venv .venv-mist\n.venv-mist/bin/python -m pip install -r mist/requirements.txt\nMIST_PYTHON=.venv-mist/bin/python bash mist/scripts/run.sh"),
      tags$p("The saved configuration, season groups, inputs and run manifests retain the research audit. Model training and validation remain separate from the daily forecast refresh."),
      tags$p(tags$a(href = "../../README.md#mist-probability", "Model and pipeline documentation"))),
    tags$p(class = "caption", "Sources: curated field observations, the Ngulia ERA5 hourly archive and fixed season groups. The live forecast uses the saved model weights; this page does not retrain the CNN.")
  )))
save_html(page, file = here("mist", "reports", "model.html"), libdir = "mist_model_report_lib")
capture.output(sessionInfo(), file = file.path(out, "r_report_session.txt"))
print(scores, width = Inf)
cat("Wrote mist model story and validation report.\n")
