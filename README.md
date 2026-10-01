# Ngulia daily forecast

[Open the forecast website](https://a-rocha-kenya.github.io/ngulia-forcast/) · [Explore the forecast](https://a-rocha-kenya.github.io/ngulia-forcast/explore/)

The website predicts daily capture count and the probability of mist at Ngulia. Both models use the same issued Open-Meteo ECMWF hourly forecast, with separate inputs and predictions. The scheduled pipeline refreshes the website twice daily during the ringing season and archives issued forecasts for later assessment.

## Daily capture count

**[Understand the count model](https://a-rocha-kenya.github.io/ngulia-forcast/count/)** for the description of the full forecast model, its interactive effect plots and next-season validation.

The forecast estimates the number of birds captured **assuming ringing takes place at front bush**. A negative-binomial generalized additive model combines season timing, moon phase, morning rain, wind, temperature, pressure, cloud cover and humidity. Historical net layout and a smooth year effect account for changes in capture level across the training record.

Training uses 1,033 operated dates in 47 seasons from 1977–2023, including documented zero-catch operation dates. For future seasons, the year effect stays at its fitted 2023 level. The forecast shows an expected count and an 80% count range; that range does not include uncertainty in the issued weather or the fitted coefficients. The full-season reference curve uses typical historical weather.

Rolling evaluation on 142 operated dates in 2014–2023 gives a mean absolute error of **485 birds per date**. Adding previous-morning rain or an annual level that updates from current-season observations gave uncertain improvements, so the published model remains the simpler choice. The separate [alternatives report](https://a-rocha-kenya.github.io/ngulia-forcast/count/alternatives.html) documents these exploratory comparisons; [selection notes](count/selection_notes.md) summarize the other tests.

The evaluation uses historical ERA5 weather. The rolling assessment holds the year effect at the last training year, matching the production rule. Accuracy with issued ECMWF weather still needs assessment using new observations. The model currently does not update from live capture counts.

To train the count model and refresh both website predictions, run from the repository root with the R dependencies in `DESCRIPTION` and Node.js 22 or later installed:

```sh
Rscript count/scripts/train.R
Rscript scripts/export_models.R
node scripts/verify_javascript.mjs
Rscript scripts/update_forecast.R
```

The shared update script writes `site/data/forecast.json`. The Explore page at `/explore/` runs interactively from the exported model files; it does not shift issued weather onto invented dates. [Data provenance](raw-data/provenance.md) describes the curated snapshot and eligibility rules.

## Explore the forecast and shared prediction

Choose a season and ringing date using native selectors, previous/next-day buttons, or the season calendar. Expand the calendar to compare ten seasons; its colour matrix uses timing and moon effects with fixed typical weather. Weather plots show fitted catch effects and observed distributions. Panels are ordered by the ratio of largest to smallest predicted catch across the central 90% of each input's observed distribution (approximated from its histogram), holding other weather at typical values. This is a display order based on model sensitivity, rather than a validated ranking of causal importance. Drag the plot markers to change the scenario; focused figures also respond to arrow keys. Rain uses a log(1 + rain) x axis to include dry conditions. The catch estimate, 80% count range and mist probability update together; the moon card is omitted. Reset restores typical weather, and a copied scenario URL preserves the date and all selected inputs.

`site/prediction.js` is the single count and mist prediction implementation used by the browser and the daily update via `scripts/predict.mjs`. R and Python retain model fitting and research validation. `scripts/export_models.R` writes versioned model artifacts to `site/models/`: count effects sampled at 2,001 points with linear interpolation, and the unchanged frozen CNN weights and preprocessing. Front-bush layout and the 2023 year effect remain fixed. JavaScript implements the same negative-binomial count quantiles. Export uses exact GAM prediction (`discrete = FALSE`), avoiding the small batch-dependent approximation from `bam`'s default discretized prediction.

Plot controls cover observed training ranges. Wind speed and direction jointly change the two wind components; their combined effect curves do not show a confidence band. Other shaded bands are 95% marginal fitted-effect intervals, distinct from the 80% predictive count range. Independently selected conditions need not represent an observed weather combination. Mist uses a hypothetical constant 12-hour profile from 21:00–08:00 and derives dew-point depression from temperature and humidity. Date, pressure and rain do not enter the CNN.

The workflow regenerates model artifacts after training and checks JavaScript against 1,548 R prediction cases (observed covariates, random combinations, and issued-weather extrapolation) plus frozen NumPy mist references before publishing. For the larger 200-night mist comparison, run `python3 scripts/export_mist_reference.py` (NumPy required) and `node scripts/verify_javascript.mjs --full`. Prediction references are verification data, not a second production implementation.

The forecast update groups weather by Ngulia local date and includes only dates with complete 00:00–08:00 count inputs and a complete 21:00–08:00 mist window. A date can be omitted when the weather response lacks the previous evening. The Explore page does not correct the separate ERA5/issued-weather elevation mismatch or the historical UTC/local-date grouping issue; those require a coordinated data rebuild and model assessment.

<details>
<summary>Reproduce the count model report</summary>

```sh
Rscript count/scripts/train.R
Rscript count/scripts/validate.R
Rscript count/scripts/build_report.R
```

`count/` owns the scripts, saved model, intermediate data and reports. Inputs come from `raw-data/daily_coverage.csv`. The main report is `count/reports/model.html`; the separate `count/reports/alternatives.html` preserves comparisons with annual adjustments and additional weather features. Their scripts are under `count/scripts/alternatives/`, with `count/scripts/build_alternatives_report.R` assembling that report.

</details>

## Mist probability

**[Understand the mist model](https://a-rocha-kenya.github.io/ngulia-forcast/mist/)** for the model description, interactive figures, calibration and validation.

The forecast gives the probability of **any recorded mist**, including light or patchy mist. It averages three small convolutional neural networks using hourly cloud cover, humidity, wind components, temperature and dew-point depression from **21:00 on the previous evening through 08:00 on the ringing date**, in Ngulia local time.

Training uses 1,184 observed dates in 43 seasons through 2013. Five whole-season validation folds give a **Brier score of 0.1675** and **AUC of 0.8103**, compared with a Brier score of 0.1845 for logistic regression using average weather. The model guide explains the comparison and probability calibration, with detailed validation scores available to expand; [selection notes](mist/selection_notes.md) explain the tested windows and variants.

The network weights and preprocessing metadata are saved in `mist/model/`. The shared forecast update runs their exported copies through `site/prediction.js`; daily inference needs no Python or neural-network training. Mist probability is displayed independently of the count estimate. The NumPy predictor remains a research reference for parity checks.

This assessment also uses ERA5 weather. There are no observed mist labels after 2013 in the current dataset, so recent accuracy and performance with issued ECMWF weather remain unmeasured.

<details>
<summary>Reproduce the mist assessment</summary>

Install `mist/requirements.txt` in a Python environment, then run:

```sh
MIST_PYTHON=/path/to/python bash mist/scripts/run.sh
```

The ERA5 hourly ZIP is stored locally in `raw-data/` (excluded from Git), copied from `ngulia-dataset`; override its location with `NGULIA_ERA5_ARCHIVE`. Fixed season folds are saved in `mist/season_folds.csv`, and generated results are under `mist/intermediate-data/`. Rebuild the report alone with `Rscript mist/scripts/03_build_report.R`.

</details>

`mist/` owns its scripts, model artifacts, intermediate data and report (`mist/reports/model.html`). The shared `scripts/update_forecast.R` fetches weather and runs both models; `scripts/publish_reports.py` includes their saved HTML reports in the website. Model training and validation run separately from the daily forecast refresh.

The published reports reuse the header, navigation and footer from `site/index.html`
and the theme variables in `site/styles.css`. `site/reports.css` provides the report
layout, tables and responsive figures. Publishing also applies the shared palette to
Plotly widgets while preserving their data and interactivity. Refresh the website
copies after rebuilding a report or editing the shared menu or palette:

```sh
python3 scripts/publish_reports.py
python3 -m http.server 8765 --directory site
```

Open `http://localhost:8765/count/` or `http://localhost:8765/mist/`.
The main menu uses `/` (Live), `/explore/` (Explore), `/count/` (Count model),
and `/mist/` (Mist model). Publishing generates the Explore page from the shared
homepage and gives each model an `index.html`; the existing `model.html` links
remain available.
The deployment workflow already runs the publishing step; no Quarto or Vite build is
needed for these existing `htmltools` reports.
