# Mist: published hourly CNN and evaluation baselines

**Published model:** a binary three-member CNN using six hourly channels (cloud fraction, humidity, wind u/v, temperature and dew-point depression) from 21:00 on the preceding evening through 08:00 on the ringing date. The target is any recorded mist. Frozen full-data weights and preprocessing metadata are committed under `model/mist/`; `scripts/production/mist/predict.py` applies them to the issued ECMWF hourly response. The ensemble was trained on 1,184 observed dates in 43 seasons through 2013. No newer labels are available locally, so the forecast cannot currently be assessed against recent observed mist.

**Assessment:** five fixed whole-season held-out folds give Brier 0.1675, log loss 0.5045 and AUC 0.8103. The matched six-mean logistic has Brier 0.1845 and climatology 0.2371. This is exploratory selection on ERA5 weather, not a prospective issued-weather score. The [mist report](../../validation/research/mist_model_report.html) includes calibration, fold and original-category checks. [Selection notes](selection_notes.md) summarize the other tested windows and variants. The old three-variable logistic is a historical reference, not a live model.

The live forecast shares only the issued weather fetch, calendar and JSON output with count. Mist uses its own hourly window, frozen weights and NumPy inference; count uses a separately fitted GAM and daily weather aggregates. The scheduled job needs NumPy but not PyTorch.

To reproduce the research comparison, install `research/mist/requirements.txt` in a Python environment, then run:

```sh
MIST_PYTHON=/path/to/python bash scripts/experiments/mist/run.sh
```

The existing sibling `ngulia-dataset` provides the ERA5 hourly ZIP. Override it with `NGULIA_ERA5_ARCHIVE`. The fixed folds are in `validation/research/season_folds.csv`. Outputs are in `validation/research/mist/`; rebuilding the HTML alone uses `Rscript scripts/experiments/mist/03_build_report.R`.
