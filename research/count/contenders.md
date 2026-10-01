# Count contender: prior-morning rain

The sole active weather extension adds previous calendar day's 00:00–08:00 rain to published M2. Open-Meteo ECMWF can supply it with `past_days=1`. The 2014–2023 rolling comparison uses only earlier seasons for each fit and scores both models on the same 142 operated dates: M2 deviance 522.7, mean absolute error 483; with prior rain, deviance 496.4, mean absolute error 464. Six of ten seasons improve. The paired season-bootstrap 95% interval for deviance change is −64.6 to 12.8, so this evidence does not justify another live input.

The M3a/M3b annual-level comparisons are in the [main count report](../../validation/research/count_model_report.html). Earlier exploratory cloud, overnight, interaction and hourly CNN tests did not establish a reliable improvement beyond the simple M2. In the earlier 2014–2023 CNN screen, its deviance was 482.7 versus 496.4 for prior rain, but it improved only five of ten seasons and the paired interval spanned zero. Cloud base was removed because an ECMWF API check returned no usable values.

No research candidate runs in the scheduled workflow. Public issued forecasts are archived under `forecast-archive/` for comparison with later observed counts. The latest local count labels are from 2023; a prospective skill score is therefore not yet available.
