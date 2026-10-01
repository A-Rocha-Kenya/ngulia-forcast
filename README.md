# Ngulia daily forecast

The website publishes **one count model and one mist model** from a shared Open-Meteo ECMWF hourly forecast. Count and mist are fitted independently; their probabilities and expected counts are displayed together, not multiplied.

| Output | Published model | Historical assessment | Main limitation |
|---|---|---|---|
| Expected daily catch, conditional on operation at front bush | M2 negative-binomial GAM: season day, moon, eight weather inputs, net layout and a smooth historical year term | Rolling 2014–2023 evaluation: mean Poisson deviance 522.7; mean absolute error 483 birds on 142 operated dates | Unmeasured annual effort/population variation; issued-weather error unmeasured |
| Probability of any recorded mist | Three-member six-channel hourly CNN, previous 21:00 through 08:00 | Five whole-season folds through 2013: Brier 0.1675, log loss 0.5045, AUC 0.8103 on 1,184 dates | No field labels after 2013; issued-weather error unmeasured |

The count model is fit through 2023. For future seasons the live forecast holds its fitted year term at 2023, avoiding an unchecked extrapolation of the historical smooth. This operational rule is explicit in the JSON model metadata; it has not been scored in the retrospective comparison. The mist ensemble is frozen through 2013. Neither model updates from current-year observations. A catch forecast assumes the front-bush layout and a ringing date; it does not predict whether ringing will occur.

## Run the live pipeline

From the repository root, with the R packages in `DESCRIPTION` and Python NumPy installed:

```sh
Rscript scripts/production/count/train.R
Rscript scripts/production/update_forecast.R
```

The first script writes the ignored count fit `model/count_m2.rds`. The mist weights and metadata are committed in `model/mist/`; no PyTorch or training is needed for daily forecasts. The second script fetches one issued hourly weather response, builds count and mist inputs, and writes `site/data/forecast.json`. `NGULIA_DEMO=1` writes `site/data/forecast.demo.json` with live weather mapped to in-season example dates. GitHub Actions runs the same pipeline twice daily during the ringing window and archives issued public forecasts.

To preview the example forecast at any time, open `?demo=1` on the GitHub Pages site.

## Research and alternatives

- [Count assessment and contenders](research/count/README.md) — M2 validation, M3 annual adjustments and prior-morning rain.
- [Mist assessment and contenders](research/mist/README.md) — retained CNN, matched logistic and climatology baselines.
- [Count HTML report](validation/research/count_model_report.html) and [mist HTML report](validation/research/mist_model_report.html) — retrospective figures and detailed methods.

Both reports are also available on the website: [count report](https://a-rocha-kenya.github.io/ngulia-forcast/reports/count_model_report.html) and [mist report](https://a-rocha-kenya.github.io/ngulia-forcast/reports/mist_model_report.html). Deployment copies the committed reports and their interactive chart libraries into the site; research fitting is not rerun by this publishing step.

The reports use historical ERA5 weather. They do not establish accuracy of an issued online forecast. The production scripts are under `scripts/production/`; research reruns are under `scripts/experiments/count/` and `scripts/experiments/mist/`. Older discarded code and bulky outputs were removed from the repository working tree; the compact selection findings are in the research notes.
