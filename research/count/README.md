# Count: published M2 and alternatives

**Published model:** M2 negative-binomial GAM trained on 1,033 dates in 47 seasons (1977–2023). It predicts catch conditional on an operated ringing date and front-bush layout. Predictors are season day, moon phase, 00:00–08:00 rain, wind speed, temperature, pressure, cloud fraction, humidity, wind u/v, net layout and a shrinkage-penalized year smooth. `scripts/production/count/train.R` fits the exact evaluated M2 formula. `scripts/production/update_forecast.R` applies it to issued ECMWF weather. It holds the year term at 2023 beyond the training period; this operational extrapolation has not been separately validated.

**Assessment:** whole-season folds, chronological held-out blocks within known years and rolling forecasts from earlier years are in [the count report](../../validation/research/count_model_report.html). For model choice, prioritize rolling 2014–2023: M2 deviance 522.7 and mean absolute error 483 on 142 operated dates. This is retrospective ERA5 evaluation, not online forecast accuracy. The historical total and daily catch vary greatly between seasons; M2 has no live annual-level update.

**Contenders:** [prior-morning rain](contenders.md) lowers recent deviance to 496.4, but its season-bootstrap interval includes no gain. M3b can update a year-level multiplier from observed current-year counts; its one-year retrospective gain is uncertain, and its two- and three-year results are worse than M2. M3a annual intercept helps when a year's level is already known but does not improve unseen-year prediction. These are not in the public forecast.

Rerun the active comparisons from the repository root:

```sh
Rscript scripts/experiments/count/08_compare_m0_m2.R
Rscript scripts/experiments/count/10_verify_count_annual.R
Rscript scripts/experiments/count/12_compare_m3b.R
Rscript scripts/experiments/count/14_verify_count_m3b.R
Rscript scripts/experiments/count/15_compare_count_weather.R
Rscript scripts/experiments/count/17_verify_count_weather.R
Rscript scripts/experiments/count/07_build_count_html.R
```

Inputs are `data/daily_coverage.csv`; outputs are `validation/research/count_*` and the HTML report. Current count pipeline uses no ANN, mist probability, team size or unmeasured effort estimate.
