# Retrospective model assessments

- [Count report](count_model_report.html): published M2 specification versus M0/M1 and annual-level alternatives M3a/M3b; prior-morning rain is a contender. The live M2 forecast holds the historical year term at 2023 for future seasons.
- [Mist report](mist_model_report.html): published three-member 21:00–08:00 CNN versus climatology and matched logistic baselines.

These results use observed historical labels and ERA5 weather. They do not score issued ECMWF forecasts. Outputs under `count_*` and `mist/` are research products; the live pipeline uses `model/count_m2.rds` and committed `model/mist/` weights. See [model research](../../research/README.md) and the repository [README](../../README.md) for current deployment.
