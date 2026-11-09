# Live count forecast pressure audit

Checked the published forecast generated at 2026-11-09 11:38:03 UTC. All 15 issued daily pressures are below the entire count training range. This produces a strong downward shift through the fitted pressure effect, independently of the other weather effects.

| Quantity | Pressure (hPa) |
| --- | ---: |
| Training minimum | 923.66 |
| Training median | 927.48 |
| Training maximum | 930.36 |
| Issued forecast range | 908.9–911.8 |

Relative to training-median pressure, the issued pressure multiplies expected catch by 0.133–0.182: an 82–87% reduction. The smooth pressure term has approximately one effective degree of freedom, so its extrapolation continues the fitted slope well outside the observed range.

| Ringing date | Published expected catch | Same forecast with training-median pressure |
| --- | ---: | ---: |
| 2026-11-09 | 35.5 | 194.7 |
| 2026-11-10 | 56.0 | 324.2 |
| 2026-11-11 | 22.6 | 152.3 |
| 2026-11-12 | 22.0 | 160.0 |

These counterfactual values isolate the pressure contribution; they are not corrected or validated forecasts. Calculations use exact R GAM prediction differences and the rounded published daily inputs and counts.

The production request in `scripts/update_forecast.R` leaves elevation unspecified. A separate request to the same ECMWF endpoint returned an elevation of 951 m and grid coordinates −3.0, 38.25. Open-Meteo documents that the default elevation comes from a 90 m terrain model and that surface pressure is calculated using elevation, temperature and mean sea-level pressure: [ECMWF API documentation](https://open-meteo.com/en/docs/ecmwf-api). Training uses ERA5 surface pressure from the historical archive. Different terrain representations are a plausible explanation for the systematic offset; the ERA5 grid elevation and the residual weather-model bias still need direct verification.

The browser displays `expected_catch` directly. The shared prediction checks passed 1,548 R count cases with maximum relative error 0.00000223 and no differences in count interval endpoints. This isolates a weather-input compatibility problem rather than a JavaScript conversion problem.

The initial audit established the need for a common pressure/elevation basis for training and issued weather. Changing API elevation also changes other weather inputs, so the follow-up below assesses both count and mist. The historical UTC/local-date grouping issue documented in the README remains a separate data-rebuild concern. Recent observed catches are needed to measure actual current-season forecast bias; this audit establishes the pressure-driven suppression without those observations.

Reproduce from the repository root:

```sh
Rscript count/scripts/alternatives/19_audit_live_pressure.R
node scripts/verify_javascript.mjs
```

The audit saves the downloaded forecast and a daily pressure-effect table under `count/intermediate-data/` (excluded from Git). Re-running downloads the currently published forecast.

## Elevation alignment and assessment

The original hourly archive identifies the ERA5 grid point as −3.0°, 38.25°. Its surface geopotential was independently retrieved from the [NCAR GDEX ERA5 invariant fields](https://tds.gdex.ucar.edu/thredds/catalog/files/d633000/e5.oper.invariant/197901/catalog.html): variable `Z[0][372][153]` in `e5.oper.invariant.128_129_z.ll025sc.1979010100_1979010100.nc` is 7,485.0767 m²/s². Dividing by standard gravity (9.80665 m/s²) gives 763.2654 m. This agrees with the historical API's rounded native ERA5 elevation of 763 m. The reference and source are saved in `raw-data/weather_reference.json`.

`scripts/update_forecast.R` now explicitly requests this elevation for the shared ECMWF feed, aligning the temperature and pressure elevation basis for both models. The API returns the same weather grid coordinates under the default and aligned requests. Forecast metadata records the returned elevation and training reference. Model weights, historical training data and year handling are unchanged. No median substitution, clipping or fitted constant offset is applied.

The historical check compares API ERA5 weather at 951 m and 763.2654 m against the original hourly archive at identical UTC timestamps, 2023-10-20 through 2024-01-12 (2,040 hours). Using hourly timestamps avoids mixing this check with the separate daily-date grouping issue.

| Error against original archive | 951 m | 763.27 m |
| --- | ---: | ---: |
| Mean pressure error (hPa) | −17.98 | +1.84 |
| Pressure RMSE (hPa) | 17.99 | 1.91 |
| Mean temperature error (°C) | −1.225 | −0.025 |
| Temperature RMSE (°C) | 1.225 | 0.038 |

This validates a substantial improvement in elevation compatibility. It does not eliminate differences in how the API derives pressure: the residual +1.84 hPa is material for this pressure-sensitive model. It also does not estimate issued ECMWF weather error. No empirical offset was inferred from this single season or applied to production.

A paired comparison of current ECMWF requests, passed through the complete production pipeline, gives the following first four dates. The current weather run differs from the earlier published snapshot above, so use the paired rows here to isolate the elevation change.

| Date | Catch at 951 m | Catch at 763.27 m | Mist at 951 m | Mist at 763.27 m |
| --- | ---: | ---: | ---: | ---: |
| 2026-11-09 | 34.0 | 432.9 | 85.5% | 81.0% |
| 2026-11-10 | 16.9 | 220.3 | 27.1% | 30.4% |
| 2026-11-11 | 21.9 | 275.8 | 64.0% | 63.7% |
| 2026-11-12 | 20.3 | 257.9 | 46.5% | 51.4% |

Across 15 forecast dates, catch increases by 12.6–13.1 times. Mist changes by −4.5 to +11.3 percentage points; pressure does not enter mist, so these changes result from the temperature/dew-point features. These are sensitivity results, not evidence that actual catches will increase by this factor or that mist calibration has improved. Current-season observations are still required for that assessment.

Run the complete comparison with the local ERA5 archive available:

```sh
Rscript count/scripts/alternatives/20_assess_weather_elevation.R
```

The script saves both hourly weather responses, both forecast outputs, a paired daily comparison, and historical input errors under `count/intermediate-data/`. The concise historical error table is retained in Git. For offline replay, use `NGULIA_WEATHER_FILE` and `NGULIA_FORECAST_OUTPUT` with `scripts/update_forecast.R`. The scheduled workflow already runs this script and watches changes under `raw-data/` and `scripts/`, so no workflow changes are needed.
