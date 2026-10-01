# Forecast data provenance

`daily_coverage.csv` is a snapshot of `ngulia-dataset/data/04_curated/daily_coverage.csv`, refreshed on 2026-09-28. The refresh adds the reviewed historical bush/night-net configurations and incorporates the corrected 2020-11-18 non-swallow count (1,750). It contains 4,707 daily rows, including dates with no ringing. No-operation zeros are not observed zero-catch forecast targets.

The published count model uses 1,033 eligible operated dates across 47 seasons, 1977–2023. The binary mist cohort has 1,184 field-labelled dates through season 2013; any recorded mist type counts as presence. Count and mist have separate eligibility rules and models.

Historical daily weather columns are ERA5-derived. Count training uses this snapshot; mist training used the ERA5 hourly archive in the sibling `ngulia-dataset` project, with frozen weights in `mist/model/`. The live forecast uses issued ECMWF weather. Forecast-versus-reanalysis differences have not yet been quantified.

The count comparison restricts training to seasons **1977 onward** and recovers 1994–1995 from recorded daily bush sites. It includes eleven documented operated zeros. Stable periods use the reviewed configuration; transition dates are back-only or front-only. Eighteen mixed-use dates and four unclassified dates are excluded from every candidate. Daily deployment does not quantify net-metres or hours; mixed use is not treated as a known front/back proportion. Mist eligibility remains independent of this count filter.
