# Training data

`daily_coverage.csv` was copied from `data/04_curated/daily_coverage.csv` in the Ngulia research workspace on 2026-09-20. The source working tree was based on commit `7cd6189f5cf1db6d61d5f95984b51a1c2d835229` and included the reconciled daily-covariate work current on that date.

The forecast uses only positive-catch dates with complete values for:

- total catch excluding targeted swallow and martin captures;
- season day and distance from new moon;
- 00:00–08:00 precipitation;
- 10 m wind speed;
- 2 m temperature; and
- surface pressure.

No observed mist or operational field is required by the live model.

