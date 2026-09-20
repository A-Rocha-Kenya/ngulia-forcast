# Ngulia catch forecast

An independent, reproducible daily catch-opportunity forecast for the Ngulia ringing season. The project trains from its own copy of the curated daily dataset, retrieves ECMWF IFS weather forecasts through Open-Meteo, and publishes a static dashboard with GitHub Actions and GitHub Pages.

The forecast is a planning aid. It estimates relative catch opportunity conditional on ringing taking place; it is not an abundance estimate or a promise of a particular number of birds.

## How it works

1. `scripts/train_model.R` fits two negative-binomial GAMs to historical positive-catch dates: date + moon, and date + moon + forecast-available weather.
2. `scripts/update_forecast.R` retrieves 15 days of hourly ECMWF IFS 0.25° forecasts from Open-Meteo and summarizes 00:00–08:00 Africa/Nairobi time.
3. The forecast is written to `site/data/forecast.json` and displayed by the static dashboard in `site/`.
4. `.github/workflows/update-forecast.yml` runs twice daily during the ringing season, archives every issued forecast, and deploys the site to GitHub Pages.

## Local run

Install the packages listed in `DESCRIPTION`, then run from the repository root:

```r
source("scripts/train_model.R")
source("scripts/update_forecast.R")
```

Preview the site with:

```sh
python3 -m http.server 8000 --directory site
```

Then open <http://localhost:8000>.

## Model interpretation

The score is indexed so that 100 represents the mean fitted opportunity across historical positive-catch dates. The operational site emphasizes relative opportunity and the weather adjustment rather than absolute catch because historical operation, lighting, net configuration, staffing, and zero-catch coverage are incomplete.

Season-blocked validation is stored in `validation/forecast_model_cv.csv`. Realised ERA5 weather is used there as a best-case proxy for a perfect forecast. Every live forecast is archived so lead-time-specific performance can be evaluated prospectively.

In the current blocked validation, adding direct weather reduces log-RMSE from 1.854 to 1.750 (5.6%) and log-MAE from 1.396 to 1.307 (6.4%). The highest-ranked 20% of dates contain 38.0% of historical catch, compared with 33.7% for date + moon alone. These values describe positive-catch dates and should not be read as performance for predicting whether ringing occurs.

## Data provenance

`data/daily_coverage.csv` is a snapshot copied on 2026-09-20 from the Ngulia data workspace at `A-Rocha-Kenya/Ngulia`. It contains one row per calendar date in the ringing-season scaffold and includes curated catch, calendar, lunar, operational, and ERA5 variables. The forecast scripts select only the columns needed for training.

The dataset metadata specifies CC BY 4.0. Retain attribution to the Ngulia Ringing Project when redistributing it. Open-Meteo weather data and ECMWF model output remain subject to their respective attribution and use terms.
