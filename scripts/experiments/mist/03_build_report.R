library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(jsonlite)
library(plotly)
library(htmltools)
library(DT)
library(here)

# Read the single comparison output contract -------------------------------
out <- here("validation", "research", "mist")
labels <- read_csv(file.path(out, "labels.csv"), show_col_types = FALSE)
hourly <- read_csv(file.path(out, "hourly.csv"), show_col_types = FALSE)
means <- read_csv(file.path(out, "night_means.csv"), show_col_types = FALSE)
predictions <- read_csv(file.path(out, "predictions.csv"), show_col_types = FALSE)
scores <- read_csv(file.path(out, "scores.csv"), show_col_types = FALSE)
fold_scores <- read_csv(file.path(out, "fold_scores.csv"), show_col_types = FALSE)
category_scores <- read_csv(file.path(out, "category_scores.csv"), show_col_types = FALSE)
differences <- read_csv(file.path(out, "paired_differences.csv"), show_col_types = FALSE)
training <- read_csv(file.path(out, "training.csv"), show_col_types = FALSE)
reference <- read_csv(file.path(out, "deployed_reference_scores.csv"), show_col_types = FALSE)
summary <- fromJSON(file.path(out, "run_summary.json"))
inference <- fromJSON(file.path(out, "inference_check.json"))
config <- summary$config
models <- scores$model
colors <- c("#7c8995", "#246b93", "#167b75")
category_names <- c(none = "No mist", light_patchy = "Light / patchy", good = "Good / sustained",
  present_unspecified = "Presence unspecified")
feature_names <- c(cloud = "Cloud cover (fraction)", humidity = "Relative humidity (%)",
  u = "East–west wind u (m/s)", v = "North–south wind v (m/s)",
  temperature = "Temperature (°C)", depression = "Dew-point depression (°C)")
null <- filter(scores, model == models[1])
logistic <- filter(scores, model == models[2])
cnn <- filter(scores, model == models[3])
paired <- filter(differences, baseline == models[2], metric == "brier")
wins <- fold_scores |> select(model, fold, brier) |> pivot_wider(names_from = model, values_from = brier)
near_ties <- sum(abs(wins[[models[3]]] - wins[[models[2]]]) < 1e-4)
wins <- sum(wins[[models[3]]] < wins[[models[2]]])

# Recompute reporting summaries from date-level held-out predictions -------
annual <- labels |> count(season, mist_observation) |>
  mutate(category = unname(category_names[mist_observation]))
calibration <- predictions |> mutate(bin = cut(probability, seq(0, 1, .1), include.lowest = TRUE)) |>
  group_by(model, bin) |> summarise(dates = n(), predicted = mean(probability),
    observed = mean(mist_present), .groups = "drop")
annual_predictions <- predictions |> group_by(model, season) |>
  summarise(predicted = mean(probability), observed = mean(mist_present), dates = n(), .groups = "drop")
roc <- predictions |> group_by(model, probability) |>
  summarise(positive = sum(mist_present), negative = sum(mist_present == 0), .groups = "drop") |>
  group_by(model) |> arrange(desc(probability), .by_group = TRUE) |>
  mutate(tpr = cumsum(positive) / sum(positive), fpr = cumsum(negative) / sum(negative)) |> ungroup()
roc <- bind_rows(tibble(model = models, probability = 1, tpr = 0, fpr = 0), roc)
write_csv(calibration, file.path(out, "calibration.csv"))
write_csv(annual_predictions, file.path(out, "annual_predictions.csv"))

# Responsive Plotly widgets; build before setting the final heights --------
chart <- function(widget, height = 380) {
  widget <- plotly_build(widget)
  widget$width <- "100%"
  widget$height <- height
  widget$x$layout$height <- height
  widget$x$layout$margin <- list(l = 85, r = 30, b = 85, t = 35)
  widget$sizingPolicy$browser$fill <- FALSE
  tags$div(class = "chart", plotly::config(widget, responsive = TRUE, displaylogo = FALSE))
}
coverage_plot <- plot_ly(annual, x = ~season, y = ~n, color = ~category, type = "bar") |>
  layout(barmode = "stack", xaxis = list(title = "Season"), yaxis = list(title = "Observed dates"),
    legend = list(orientation = "h", y = 1.15))
metric_plots <- list()
for (metric in c("brier", "log_loss", "auc")) {
  metric_plots[[metric]] <- plot_ly(scores, x = ~model, y = scores[[metric]], type = "bar",
    marker = list(color = colors), text = sprintf("%.4f", scores[[metric]]), textposition = "auto") |>
    layout(xaxis = list(title = "", tickangle = -15),
      yaxis = list(title = switch(metric, brier = "Brier ↓", log_loss = "Log loss ↓", auc = "ROC AUC ↑")),
      showlegend = FALSE)
}
metric_plot <- subplot(metric_plots, nrows = 3, titleY = TRUE, margin = .04)
fold_plot <- plot_ly(fold_scores, x = ~factor(fold), y = ~brier, color = ~factor(model, levels = models),
  colors = colors, type = "scatter", mode = "lines+markers",
  text = ~paste(dates, "test dates"), hoverinfo = "x+y+name+text") |>
  layout(xaxis = list(title = "Held-out whole-season fold"), yaxis = list(title = "Brier score (lower is better)"),
    legend = list(orientation = "h", y = 1.15))
calibration_plot <- plot_ly(calibration, x = ~predicted, y = ~observed,
  color = ~factor(model, levels = models), colors = colors, type = "scatter", mode = "lines+markers",
  text = ~paste(dates, "dates"), hoverinfo = "x+y+name+text") |>
  layout(xaxis = list(title = "Mean held-out probability", range = c(0, 1)),
    yaxis = list(title = "Observed mist frequency", range = c(0, 1)),
    legend = list(orientation = "h", y = 1.15),
    shapes = list(list(type = "line", x0 = 0, x1 = 1, y0 = 0, y1 = 1,
      line = list(color = "#aaa", dash = "dot"))))
roc_plot <- plot_ly(roc, x = ~fpr, y = ~tpr, color = ~factor(model, levels = models), colors = colors,
  type = "scatter", mode = "lines", text = ~sprintf("Threshold %.2f", probability), hoverinfo = "x+y+name+text") |>
  layout(xaxis = list(title = "False-positive rate", range = c(0, 1)),
    yaxis = list(title = "Mist detection rate", range = c(0, 1)),
    legend = list(orientation = "h", y = 1.15),
    shapes = list(list(type = "line", x0 = 0, x1 = 1, y0 = 0, y1 = 1,
      line = list(color = "#aaa", dash = "dot"))))
annual_plot <- plot_ly(annual_predictions, x = ~season, y = ~predicted,
  color = ~factor(model, levels = models), colors = colors, type = "scatter", mode = "lines") |>
  add_trace(data = filter(annual_predictions, model == models[1]), x = ~season, y = ~observed,
    inherit = FALSE, type = "scatter", mode = "markers", name = "Observed", marker = list(color = "#333")) |>
  layout(xaxis = list(title = "Season"), yaxis = list(title = "Mist frequency / mean probability", range = c(0, 1)),
    legend = list(orientation = "h", y = 1.15))
category_plot <- plot_ly(filter(category_scores, category != "present_unspecified"),
  x = ~unname(category_names[category]), y = ~brier, color = ~factor(model, levels = models), colors = colors,
  type = "bar", text = ~paste(dates, "dates"), hoverinfo = "x+y+name+text") |>
  layout(barmode = "group", xaxis = list(title = "Historical observation category"),
    yaxis = list(title = "Binary Brier score within category"), legend = list(orientation = "h", y = 1.15))
probability_plot <- plot_ly(filter(predictions, mist_observation != "present_unspecified"),
  x = ~unname(category_names[mist_observation]), y = ~probability, color = ~factor(model, levels = models),
  colors = colors, type = "box", boxpoints = FALSE) |>
  layout(boxmode = "group", xaxis = list(title = "Historical observation category"),
    yaxis = list(title = "Held-out mist probability", range = c(0, 1)), legend = list(orientation = "h", y = 1.15))

# Logistic effects are full-data descriptions, separate from CV results ----
fit_data <- left_join(labels, means, by = "ringing_date")
fit <- glm(reformulate(config$features, "mist_present"), data = fit_data, family = binomial())
coefficients <- read_csv(file.path(out, "logistic_coefficients.csv"), show_col_types = FALSE)
stopifnot(max(abs(unname(coef(fit)) - coefficients$coefficient)) < 1e-4)
saveRDS(fit, file.path(out, "logistic_effects.rds"))
effects <- list()
reference_inputs <- fit_data[1, ]
for (feature in config$features) reference_inputs[[feature]] <- median(fit_data[[feature]])
for (feature in config$features) {
  grid <- reference_inputs[rep(1, 80), ]
  grid[[feature]] <- seq(quantile(fit_data[[feature]], .05), quantile(fit_data[[feature]], .95), length.out = 80)
  prediction <- predict(fit, grid, type = "link", se.fit = TRUE)
  curve <- tibble(value = grid[[feature]], probability = plogis(prediction$fit),
    low = plogis(prediction$fit - 1.96 * prediction$se.fit), high = plogis(prediction$fit + 1.96 * prediction$se.fit))
  effects[[feature]] <- plot_ly(curve, x = ~value, showlegend = FALSE) |>
    add_lines(y = ~high, line = list(color = "transparent"), hoverinfo = "skip") |>
    add_lines(y = ~low, fill = "tonexty", fillcolor = "rgba(36,107,147,.15)",
      line = list(color = "transparent"), hoverinfo = "skip") |>
    add_lines(y = ~probability, line = list(color = colors[2])) |>
    layout(xaxis = list(title = unname(feature_names[feature]), automargin = TRUE),
      yaxis = list(title = "Mist probability", range = c(0, 1)))
}

# Native mathematical notation without a network dependency ---------------
math <- function(content) HTML(paste0('<math xmlns="http://www.w3.org/1998/Math/MathML" display="block"><mrow>', content, '</mrow></math>'))
bernoulli <- math('<msub><mi>Y</mi><mi>d</mi></msub><mo>∼</mo><mi>Bernoulli</mi><mo>(</mo><msub><mi>p</mi><mi>d</mi></msub><mo>)</mo>')
null_equation <- math('<msub><mover><mi>p</mi><mo>^</mo></mover><mi>d</mi></msub><mo>=</mo><mfrac><mn>1</mn><msub><mi>n</mi><mi>train</mi></msub></mfrac><munder><mo>∑</mo><mrow><mi>i</mi><mo>∈</mo><mi>train</mi></mrow></munder><msub><mi>y</mi><mi>i</mi></msub>')
logistic_equation <- math('<mi>log</mi><mo>(</mo><mfrac><msub><mi>p</mi><mi>d</mi></msub><mrow><mn>1</mn><mo>−</mo><msub><mi>p</mi><mi>d</mi></msub></mrow></mfrac><mo>)</mo><mo>=</mo><msub><mi>β</mi><mn>0</mn></msub><mo>+</mo><munderover><mo>∑</mo><mrow><mi>j</mi><mo>=</mo><mn>1</mn></mrow><mn>6</mn></munderover><msub><mi>β</mi><mi>j</mi></msub><msub><mover><mi>x</mi><mo>¯</mo></mover><mrow><mi>d</mi><mi>j</mi></mrow></msub><mo>,</mo><mspace width="1em"/><msub><mover><mi>x</mi><mo>¯</mo></mover><mrow><mi>d</mi><mi>j</mi></mrow></msub><mo>=</mo><mfrac><mn>1</mn><mn>12</mn></mfrac><munderover><mo>∑</mo><mrow><mi>h</mi><mo>=</mo><mrow><mo>−</mo><mn>3</mn></mrow></mrow><mn>8</mn></munderover><msub><mi>x</mi><mrow><mi>d</mi><mi>h</mi><mi>j</mi></mrow></msub>')
cnn_equation <- math('<msub><mover><mi>p</mi><mo>^</mo></mover><mi>d</mi></msub><mo>=</mo><mfrac><mn>1</mn><mn>3</mn></mfrac><munderover><mo>∑</mo><mrow><mi>s</mi><mo>=</mo><mn>1</mn></mrow><mn>3</mn></munderover><mi>σ</mi><mo>(</mo><msub><mi>b</mi><mi>s</mi></msub><mo>+</mo><msubsup><mi>w</mi><mi>s</mi><mi>T</mi></msubsup><mi>vec</mi><mo>(</mo><msubsup><mi>H</mi><mi>d</mi><mrow><mn>2</mn><mo>,</mo><mi>s</mi></mrow></msubsup><mo>)</mo><mo>)</mo>')
convolution_equation <- math('<msup><mi>H</mi><mi>ℓ</mi></msup><mo>=</mo><mi>ReLU</mi><mo>(</mo><msup><mi>Conv</mi><mi>ℓ</mi></msup><mo>(</mo><msup><mi>H</mi><mrow><mi>ℓ</mi><mo>−</mo><mn>1</mn></mrow></msup><mo>)</mo><mo>)</mo><mo>,</mo><mspace width="1em"/><msup><mi>H</mi><mn>0</mn></msup><mo>=</mo><mi>standardize</mi><mo>(</mo><msub><mi>X</mi><mi>d</mi></msub><mo>)</mo>')
loss_equation <- math('<mi>L</mi><mo>=</mo><mo>−</mo><mfrac><mn>1</mn><mi>n</mi></mfrac><munderover><mo>∑</mo><mrow><mi>d</mi><mo>=</mo><mn>1</mn></mrow><mi>n</mi></munderover><mo>[</mo><msub><mi>y</mi><mi>d</mi></msub><mi>log</mi><mo>(</mo><msub><mi>p</mi><mi>d</mi></msub><mo>)</mo><mo>+</mo><mo>(</mo><mn>1</mn><mo>−</mo><msub><mi>y</mi><mi>d</mi></msub><mo>)</mo><mi>log</mi><mo>(</mo><mn>1</mn><mo>−</mo><msub><mi>p</mi><mi>d</mi></msub><mo>)</mo><mo>]</mo>')

# Scientific report: methods, results, limitations and exact reproduction --
css <- 'body{margin:0;background:#fff;color:#243137;font:16px/1.6 Arial,Helvetica,sans-serif}main{max-width:1120px;margin:auto;padding:30px 24px 65px}h1{font-size:2rem;line-height:1.25}h2{margin:42px 0 18px;border-bottom:1px solid #ddd;padding-bottom:9px}h3{font-size:1.15rem;margin:26px 0 10px}nav{display:flex;flex-wrap:wrap;gap:14px;margin:20px 0}a{color:#167b75}li{margin:6px 0}.equation{padding:14px;background:#f5f7f8;overflow-x:auto;margin:18px 0}math{font-size:1.12rem}.caption{color:#56636a;font-size:.9rem}.chart{display:block;width:100%;min-width:0;margin:0 0 24px;overflow:hidden}.html-widget{display:block;max-width:100%}.architecture{padding:18px;background:#f5f7f8;line-height:2}.table-wrap{overflow-x:auto}table.methods{border-collapse:collapse;width:100%}table.methods th,table.methods td{padding:9px;border-bottom:1px solid #ddd;text-align:left}pre{white-space:pre-wrap;overflow-wrap:anywhere;background:#f5f7f8;padding:16px;font-size:.85rem}details{margin:18px 0}summary{cursor:pointer;color:#167b75}@media(max-width:700px){main{padding:18px 12px}h1{font-size:1.6rem}}'
score_table <- scores |> select(Model = model, Dates = dates, Brier = brier, `Log loss` = log_loss,
  `ROC AUC` = auc, `Mean probability` = mean_probability)
score_widget <- datatable(score_table, rownames = FALSE, options = list(dom = "t", ordering = TRUE, scrollX = TRUE),
  caption = "Pooled held-out scores on identical dates. Click a column heading to sort.") |>
  formatRound(c("Brier", "Log loss", "ROC AUC", "Mean probability"), 4)
feature_table <- tags$table(class = "methods", tags$thead(tags$tr(tags$th("Channel"), tags$th("Definition"))),
  tags$tbody(tags$tr(tags$td("Cloud"), tags$td("ERA5 total cloud cover, fraction 0–1")),
    tags$tr(tags$td("Humidity"), tags$td("Relative humidity from 2 m temperature and dew point; clipped 0–100%")),
    tags$tr(tags$td("u / v"), tags$td("10 m wind components, m/s; positive eastward / northward")),
    tags$tr(tags$td("Temperature"), tags$td("2 m air temperature, °C")),
    tags$tr(tags$td("Depression"), tags$td("Temperature minus dew point reconstructed from clipped RH, °C; ≥0"))))
page <- tags$html(tags$head(tags$meta(charset = "utf-8"), tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
  tags$title("Ngulia mist forecast: reproducible model comparison"), tags$style(HTML(css))),
  tags$body(tags$main(
    tags$h1("Mist probability forecasting at Ngulia"),
    tags$p("A reproducible comparison of climatology, logistic regression and an hourly convolutional neural network."),
    tags$nav(tags$a(href = "#abstract", "Abstract"), tags$a(href = "#data", "Data"), tags$a(href = "#models", "Models"),
      tags$a(href = "#validation", "Validation"), tags$a(href = "#results", "Results"),
      tags$a(href = "#labels", "Light and sustained mist"), tags$a(href = "#reproduction", "Reproduction")),
    tags$h2(id = "abstract", "Abstract"),
    tags$p(sprintf("We predict any recorded mist on %s labelled dates from %d seasons (%d–%d). All models share the same cohort, weather source and five whole-season folds. Training-only climatology, a six-mean logistic model and a three-member hourly CNN yield Brier scores %.4f, %.4f and %.4f, respectively. The CNN changes Brier by %.1f%% relative to the matched twelve-hour logistic baseline and improves %d of five folds.",
      format(summary$dates, big.mark = ","), summary$seasons, summary$first_season, summary$last_season,
      null$brier, logistic$brier, cnn$brier, 100 * (1 - cnn$brier / logistic$brier), wins)),
    tags$p("This is an exploratory retrospective comparison with ERA5 reanalysis. The architecture was chosen after earlier screens on these observations; the comparison is not an independent final test. The selected CNN is now used by the website, with issued-weather accuracy still to be assessed."),
    tags$p("Selected model: six-channel binary CNN using 21:00 the previous evening through 08:00 on the ringing date. Climatology and matched logistic means are retained only as evaluation baselines."),
    tags$h2(id = "data", "1. Data and forecast target"),
    tags$div(class = "equation", bernoulli),
    tags$ul(tags$li("One output per ringing date: Y = 1 for light/patchy, good/sustained or unspecified recorded mist; Y = 0 for no recorded mist."),
      tags$li(sprintf("%d good/sustained, %d light/patchy, %d no-mist and %d unspecified-presence dates. Presence prevalence: %.1f%%.",
        sum(labels$mist_observation == "good"), sum(labels$mist_observation == "light_patchy"), sum(labels$mist_observation == "none"),
        sum(labels$mist_observation == "present_unspecified"), 100 * summary$observed_frequency)),
      tags$li("Only field labels through season 2013 are eligible. Missing labels are excluded; ERA5-derived classifications are not labels. Bird count, net configuration and operation filters do not enter this analysis."),
      tags$li("Every included date has all six channels from 21:00 the previous day through 08:00 East Africa Time, inclusive: twelve hourly values per ringing date. Complete cases are selected once for all three models. The cohort audit records exclusions.")),
    chart(coverage_plot), tags$p(class = "caption", "Figure 1. Observation coverage by season and original category. Coverage is uneven and does not represent every calendar date."),
    tags$h3("A common weather input"), tags$div(class = "table-wrap", feature_table),
    tags$p("The input array is date × 12 hours × 6 channels, from the existing Ngulia ERA5 point archive. M1 averages the hours; M2 retains their order. M0 shares the evaluation dates but intentionally ignores weather. Thus M1–M2 changes the representation and model flexibility without changing weather channels or data availability."),
    tags$p("Humidity uses the same Magnus conversion as the daily baseline. Dew-point depression is reconstructed from clipped humidity so the same transform can be applied to issued temperature/RH. This avoids tiny negative depressions from rounded ERA5 values. The overlapping 00:00–08:00 cloud, humidity and u means are checked against the existing dataset to within 10⁻⁶."),
    tags$h2(id = "models", "2. Model specifications and implementation"),
    tags$h3("M0 — training climatology"), tags$div(class = "equation", null_equation),
    tags$p("The null model estimates the training-set fraction of dates with recorded mist, then assigns that probability to every test date. It has one parameter, no weather input and no information from test labels. A whole-dataset average would leak the held-out labels and is not used."),
    tags$h3("M1 — logistic regression on six night means"), tags$div(class = "equation", logistic_equation),
    tags$p("Seven fitted coefficients: an intercept and one coefficient for each weather mean. The log odds are additive and linear. Fitting minimizes unweighted Bernoulli negative log likelihood without regularization. Predictors are standardized with training-only mean and standard deviation for numerical optimization; the reported coefficients are transformed back to physical units."),
    tags$p("Python/SciPy BFGS fits the comparison model. An independent R binomial GLM verifies the full-data coefficients to within 10⁻⁴ and supplies the descriptive effect intervals below. Correlated humidity, temperature and depression can make individual coefficients difficult to interpret."),
    tags$div(tags$h3("Six fitted logistic effects and coefficients"),
      tags$p(sprintf("Full-data intercept %.4f; channel coefficients: %s.", coefficients$coefficient[1],
        paste(sprintf("%s %+.4f", coefficients$term[-1], coefficients$coefficient[-1]), collapse = "; "))),
      lapply(config$features, function(feature) tagList(tags$h3(unname(feature_names[feature])), chart(effects[[feature]], 340))),
      tags$p(class = "caption", "Figure 2. Full-data conditional curves over each input's central 90%, holding the other means at their medians. Bands are pointwise 95% model intervals. These describe the fitted logistic model; they are not held-out validation or causal effects. Some combinations of correlated predictors may be uncommon.")),
    tags$h3("M2 — small hourly CNN (the ANN candidate)"),
    tags$div(class = "architecture", "12 hours × 6 channels → channel standardization → Conv1D (6→8, width 3) → ReLU → Conv1D (8→8, width 3) → ReLU → flatten 8×12 → dropout 0.1 → linear (96→1) → sigmoid → average 3 seeds"),
    tags$div(class = "equation", convolution_equation, cnn_equation),
    tags$ul(tags$li("σ is the inverse logit, σ(z) = 1 / (1 + exp(−z)). Each convolution has stride 1 and one zero-padding position at each end; time length remains twelve. No pooling, recurrence, dilation or U-Net is used. Two layers give a five-hour local receptive field; the final dense layer combines all time positions."),
      tags$li("Each network has 449 trainable parameters (152 + 200 + 97). Three fixed seeds, 73/173/273, form an ensemble by averaging probabilities: 1,347 weights total. The best seed is never selected."),
      tags$li("One centre and scale per channel, estimated across training dates and hours. Dropout is active during training and disabled for inference. The final bias starts at training prevalence on the logit scale.")),
    tags$h3("Training objective and stopping"), tags$div(class = "equation", loss_equation),
    tags$ul(tags$li("Logistic and CNN training use the same binary target and unweighted Bernoulli likelihood. No label smoothing, category weights or class balancing is applied."),
      tags$li(sprintf("CNN: CPU PyTorch; AdamW learning rate %g, weight decay %g, shuffled batches of %d; at most %d epochs. Early stopping monitors inner-validation Brier score, improvement >%g and patience %d epochs.",
        config$learning_rate, config$weight_decay, config$batch_size, config$max_epochs, config$stopping_tolerance, config$patience)),
      tags$li("Stopping is chosen without outer-test labels. For each outer fold, the next numbered fold is inner validation; the remaining three folds train the stopping model. A fresh model is then fitted on all four outer-training folds for that seed's selected epoch count.")),
    tags$h2(id = "validation", "3. Comparable evaluation"),
    tags$ul(tags$li("Five frozen whole-season folds from season_folds.csv (original seed 73). A season never appears in both training and test. Each model issues exactly one held-out probability for each eligible date."),
      tags$li("The outer fold has no role in scaling, fitting or stopping. The three model scores are computed by the same Python function from the same date-level output schema."),
      tags$li("Pooled metrics weight each observed date equally. Uncertainty resamples whole seasons (2,000 paired bootstrap draws), retaining each sampled season's dates and the paired predictions. It describes variation in these fixed CV results; it does not account for prior candidate selection or retraining uncertainty."),
      tags$li("Random season folds can train on years later than a test season. They compare across-season generalization, not an operational y+1 forecast. No second forecast check is added here; a future untouched season or a prespecified chronological evaluation is needed before making a final promotion claim.")),
    tags$h3("Metrics and how to read them"),
    tags$ul(tags$li("Primary — Brier: mean (p − y)². Lower is better; zero is perfect. It rewards accurate probabilities and penalizes confident mistakes. Brier skill versus climatology = 1 − model Brier / M0 Brier."),
      tags$li("Log loss: the mean negative Bernoulli log likelihood above, with natural logarithms and probabilities clipped to [10⁻¹², 1 − 10⁻¹²] only for scoring. Lower is better; confident wrong forecasts receive a strong penalty."),
      tags$li("ROC AUC: fraction of mist/no-mist pairs correctly ranked, with half credit for tied probabilities. Higher is better (0.5 chance, 1 perfect). It measures discrimination, not calibration."),
      tags$li("Calibration: fixed 0.1-wide probability bins; mean forecast versus observed fraction. On the diagonal is calibrated, below is overprediction, above is underprediction. Hover to inspect bin size. No within-bin uncertainty bars are shown because dates within a season are dependent."),
      tags$li("No classification threshold is selected. The website output remains a probability; accuracy at 0.5 is not the selection criterion. Pooled M0 AUC need not be exactly 0.5 because its training prevalence varies between folds.")),
    tags$h2(id = "results", "4. Results and improvement versus complexity"),
    chart(metric_plot, 850), tags$p(class = "caption", "Figure 3. Identical-cohort held-out probability scores. Brier and log loss should decrease; AUC should increase."),
    tags$div(class = "table-wrap", score_widget),
    tags$ul(tags$li(sprintf("CNN versus six-mean logistic: Brier difference %+.4f; paired season bootstrap 95%% interval [%+.4f, %+.4f]. Brier change %.1f%%; log-loss change %.1f%%; AUC change %+.3f.",
      paired$difference, paired$low, paired$high, 100 * (1 - cnn$brier / logistic$brier),
      100 * (1 - cnn$log_loss / logistic$log_loss), cnn$auc - logistic$auc)),
      tags$li(sprintf("Logistic and CNN Brier skill versus M0: %.1f%% and %.1f%%. CNN improves %d/5 outer folds relative to matched logistic.",
        100 * (1 - logistic$brier / null$brier), 100 * (1 - cnn$brier / null$brier), wins)),
      tags$li(sprintf("%d fold comparison(s) have an absolute Brier change below 0.0001 and is practically a tie; a strict win count should not be read as five equally strong improvements.", near_ties)),
      tags$li(sprintf("Deployed three-mean, 00:00–08:00 logistic reference on the same dates/folds: Brier %.4f, log loss %.4f, AUC %.3f. This is kept in a separate audit output; M1 intentionally uses all six channels and the same twelve-hour CNN window for the primary comparison.",
        reference$brier, reference$log_loss, reference$auc)),
      tags$li(sprintf("Complexity: M0 one probability; M1 seven coefficients; M2 three × 449 parameters and hourly preprocessing. No extra weather provider or spatial locations are required. Full-fit CNN JSON files total %.1f kB; NumPy inference matches PyTorch within %.2g on the saved sample.",
        inference$artifact_bytes / 1000, inference$max_probability_difference))),
    chart(fold_plot), tags$p(class = "caption", "Figure 4. Fold-specific Brier scores reveal whether the pooled improvement is consistent across omitted seasons."),
    chart(calibration_plot), tags$p(class = "caption", "Figure 5. Calibration on held-out probabilities; sparse bins should be interpreted cautiously."),
    chart(roc_plot), tags$p(class = "caption", "Figure 6. ROC curves sweep all thresholds; ranking is complementary to probability scores."),
    chart(annual_plot), tags$p(class = "caption", "Figure 7. Mean held-out probabilities and observed presence by season, using labelled dates only. Coverage differences affect annual averages."),
    tags$h2(id = "labels", "5. Light versus sustained mist: retain information without changing the target"),
    tags$p("The source distinguishes good mist lasting at least two hours and light/patchy mist lasting at least one hour. These are coarse persistence/patchiness categories, not calibrated measurements of visibility or intensity. Both are positive evidence of mist. Binary prediction discards that distinction during training but the original labels are preserved for every date."),
    tags$p(sprintf("Relative to M1, CNN category Brier changes are %+.4f for good/sustained mist, %+.4f for light/patchy mist and %+.4f for no mist. Category scores are diagnostics of the binary forecast; light/patchy observations remain challenging.",
      category_scores$brier[category_scores$model == models[3] & category_scores$category == "good"] - category_scores$brier[category_scores$model == models[2] & category_scores$category == "good"],
      category_scores$brier[category_scores$model == models[3] & category_scores$category == "light_patchy"] - category_scores$brier[category_scores$model == models[2] & category_scores$category == "light_patchy"],
      category_scores$brier[category_scores$model == models[3] & category_scores$category == "none"] - category_scores$brier[category_scores$model == models[2] & category_scores$category == "none"])),
    chart(category_plot), tags$p(class = "caption", "Figure 8. Binary Brier score stratified by the original category. Within each presence category y = 1; within no mist y = 0. This is a diagnostic, not a separate category-classification score. The single unspecified date is omitted from this plot but retained in primary scores."),
    chart(probability_plot), tags$p(class = "caption", "Figure 9. Held-out probability distributions by original category. Boxes show quartiles and median; whiskers follow the standard 1.5 × IQR rule."),
    tags$ul(tags$li("Recommended current design: train on any recorded mist and evaluate the light/good/no-mist strata alongside the pooled metrics. This preserves a clear, calibrated binary forecast objective."),
      tags$li("Do not encode light = 0.5 or use stronger positive weights in the primary likelihood: the output would target an invented severity average or a weighted population rather than the probability of any recorded mist."),
      tags$li("Category-supervised training was tested and discarded: its binary gain was marginal and it worsened light/patchy forecasts. The retained model uses binary supervision; original categories remain in the evaluation data."),
      tags$li("Weather/label disagreement does not identify a human mistake. Modelling observer reliability requires independent information such as repeated ratings with observer IDs or visibility measurements; the current single label per night cannot separate observer error from unmeasured local weather.")),
    tags$h2("6. Interpretation and deployment boundary"),
    tags$p("The 21:00–08:00 model was selected for performance versus complexity: extending to 18:00 added only a marginal gain, while 18/24-hour histories and fewer channels did not help. A compact selection record is retained in research/mist/selection_notes.md. Future observed seasons and saved issued forecasts are needed for an independent operational assessment."),
    tags$p("The comparison measures predictability of historical recorded mist, not a perfectly observed physical fog state. ERA5 grid resolution, sparse field recording, and the coarse category definition limit what can be learned. The CNN gain cannot be attributed solely to time ordering: M1–M2 also changes nonlinear flexibility. Earlier exploratory mean-ANN comparisons were weak, but that is not a controlled ablation in this standardized run."),
    tags$p("All six channels are obtained or derived from the issued ECMWF hourly weather, including the preceding evening. Frozen weights, feature order, hour order, timezone and scaling are in model/mist/. The daily forecast uses NumPy inference without PyTorch. ERA5 evaluation does not measure issued-weather performance."),
    tags$h2(id = "reproduction", "7. Reproduce the analysis"),
    tags$p("Run from the ngulia-forcast repository root. R handles source preparation and the interactive report; Python owns all comparison fitting and metrics. This keeps one evaluation implementation while retaining an inspectable R research workflow."),
    tags$pre("python3 -m venv .venv-mist\n.venv-mist/bin/python -m pip install -r research/mist/requirements.txt\nMIST_PYTHON=.venv-mist/bin/python bash scripts/experiments/mist/run.sh"),
    tags$p(tags$a(href = "../../research/mist/README.md", "Pipeline documentation"), " · ",
      tags$a(href = "../../research/mist/config.json", "Exact experiment configuration"), " · ",
      tags$a(href = "mist/predictions.csv", "Held-out predictions (CSV)"), " · ",
      tags$a(href = "mist/scores.csv", "Scores (CSV)"), " · ",
      tags$a(href = "mist/verification.csv", "Independent checks (CSV)")),
    tags$ul(tags$li("R packages: dplyr, tidyr, readr, tibble, lubridate, jsonlite, here, plotly, htmltools and DT (listed in DESCRIPTION). The fixed folds are already saved in validation/research/season_folds.csv."),
      tags$li("The default hourly archive is in the sibling ngulia-dataset repository. Set NGULIA_ERA5_ARCHIVE to another compatible ZIP and NGULIA_EXPERIMENT_DATA to another compatible daily CSV when required; preparation checks the 00:00–08:00 subset against its daily means."),
      tags$li("Edit research/mist/config.json for explicit settings. New comparisons must preserve the target, folds, eligible dates and scoring definitions, or receive a separate comparison group. Negative relative hours refer to the preceding calendar day; the existing-dataset parity check uses the 00:00–08:00 subset."),
      tags$li("Outputs: mist/labels.csv, hourly.csv, cohort_audit.csv, predictions.csv, scores.csv, fold_scores.csv, category_scores.csv, calibration.csv, paired_differences.csv, training.csv, seed_scores.csv and verification.csv. Full-data research artifacts are separate from held-out predictions."),
      tags$li("input_manifest.csv hashes the raw inputs and config; run_manifest.csv hashes exported inputs and analysis sources. Session/runtime versions and the effective config are saved with the run. Frozen files and package versions support reruns; exact neural results can still vary across platforms.")),
    tags$details(tags$summary("Current runtime and training audit"),
      tags$pre(sprintf("Python %s · NumPy %s · SciPy %s · PyTorch %s\nCPU; one thread; deterministic algorithms enabled\nComparison + full-data refit elapsed %.1f seconds\nOuter-refit stopping epochs: min %d, median %.0f, max %d",
        summary$python, summary$numpy, summary$scipy, summary$torch, summary$elapsed_seconds,
        min(training$stopping_epoch), median(training$stopping_epoch), max(training$stopping_epoch)))),
    tags$h2("References"),
    tags$ul(tags$li(tags$a(href = "https://arxiv.org/abs/1803.01271", "Bai, Kolter & Koltun (2018). An Empirical Evaluation of Generic Convolutional and Recurrent Networks for Sequence Modeling."),
        " Motivates considering temporal convolutions; the small two-layer network here is not their full TCN architecture."),
      tags$li(tags$a(href = "https://jmlr.org/papers/v11/raykar10a.html", "Raykar et al. (2010). Learning From Crowds."),
        " Multiple-annotator reliability models require repeated annotator information, which this comparison does not have."),
      tags$li(tags$a(href = "https://docs.pytorch.org/docs/2.14/notes/randomness.html", "PyTorch reproducibility documentation."),
        " Seeds and deterministic algorithms reduce variation; exact reproducibility across platforms/releases is not guaranteed.")),
    tags$p(class = "caption", "Sources: curated daily_coverage.csv, existing Ngulia ERA5 hourly archive, and fixed season_folds.csv. The production CNN uses the frozen full-data ensemble, not these held-out predictions.")
  )))
save_html(page, file = here("validation", "research", "mist_model_report.html"), libdir = "mist_model_report_lib")
capture.output(sessionInfo(), file = file.path(out, "r_report_session.txt"))
print(scores, width = Inf)
cat("Wrote standardized mist comparison report.\n")
