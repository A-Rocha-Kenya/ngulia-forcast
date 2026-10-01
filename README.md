# Ngulia daily forecast

[Open the forecast website](https://a-rocha-kenya.github.io/ngulia-forcast/) · [View the demo](https://a-rocha-kenya.github.io/ngulia-forcast/?demo=1)

The website predicts daily capture count and the probability of mist at Ngulia. Both models use the same issued Open-Meteo ECMWF hourly forecast, with separate inputs and predictions. The scheduled pipeline refreshes the website twice daily during the ringing season and archives issued forecasts for later assessment.

## Daily capture count

**[Read the count model report](https://a-rocha-kenya.github.io/ngulia-forcast/count/model.html)** for the description of the full forecast model, its interactive effect plots and next-season validation.

The forecast estimates the number of birds captured **assuming ringing takes place at front bush**. A negative-binomial generalized additive model combines season timing, moon phase, morning rain, wind, temperature, pressure, cloud cover and humidity. Historical net layout and a smooth year effect account for changes in capture level across the training record.

Training uses 1,033 operated dates in 47 seasons from 1977–2023, including documented zero-catch operation dates. For future seasons, the year effect stays at its fitted 2023 level. The forecast shows an expected count and an 80% count range; that range does not include uncertainty in the issued weather or the fitted coefficients. The full-season reference curve uses typical historical weather.

Rolling evaluation on 142 operated dates in 2014–2023 gives a mean absolute error of **485 birds per date**. Adding previous-morning rain or an annual level that updates from current-season observations gave uncertain improvements, so the published model remains the simpler choice. The separate [alternatives report](https://a-rocha-kenya.github.io/ngulia-forcast/count/alternatives.html) documents these exploratory comparisons; [selection notes](count/selection_notes.md) summarize the other tests.

The evaluation uses historical ERA5 weather. The rolling assessment holds the year effect at the last training year, matching the production rule. Accuracy with issued ECMWF weather still needs assessment using new observations. The model currently does not update from live capture counts.

To train the count model and refresh both website predictions, run from the repository root with the R dependencies in `DESCRIPTION` and Python NumPy installed:

```sh
Rscript count/scripts/train.R
Rscript scripts/update_forecast.R
```

The shared update script writes `site/data/forecast.json`. Set `NGULIA_DEMO=1` to write example in-season dates using current weather. [Data provenance](raw-data/provenance.md) describes the curated snapshot and eligibility rules.

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

**[Read the mist model report](https://a-rocha-kenya.github.io/ngulia-forcast/mist/model.html)** for the model description, interactive figures, calibration and validation.

The forecast gives the probability of **any recorded mist**, including light or patchy mist. It averages three small convolutional neural networks using hourly cloud cover, humidity, wind components, temperature and dew-point depression from **21:00 on the previous evening through 08:00 on the ringing date**, in Ngulia local time.

Training uses 1,184 observed dates in 43 seasons through 2013. Five whole-season validation folds give a **Brier score of 0.1675** and **AUC of 0.8103**, compared with a Brier score of 0.1845 for logistic regression using average weather. The report includes fold, calibration and mist-category diagnostics; [selection notes](mist/selection_notes.md) explain the tested windows and variants.

The network weights and preprocessing metadata are saved in `mist/model/`. The shared forecast update runs them through `mist/scripts/predict.py` using NumPy; daily inference needs no neural-network training. Mist probability is displayed independently of the count estimate.

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

Open `http://localhost:8765/count/model.html` or `http://localhost:8765/mist/model.html`.
The deployment workflow already runs the publishing step; no Quarto or Vite build is
needed for these existing `htmltools` reports.
