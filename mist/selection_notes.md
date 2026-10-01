# Mist model selection record

Retained model: binary six-channel CNN, **21:00 previous day–08:00 ringing date**, inclusive (12 hourly values), averaged over seeds 73/173/273. It has 449 parameters per member. It was selected for performance versus complexity, rather than the absolute smallest observed loss.

All screens used the same 1,184 field-labelled dates in 43 seasons through 2013, five fixed whole-season folds, training-only scaling and inner validation for stopping. Inputs were existing Ngulia ERA5 hourly weather. Architecture/optimizer settings were those in the active pipeline. Scores below are exploratory; selection used these observations and uncertainty does not account for that search.

| Candidate | Brier ↓ | Log loss ↓ | AUC ↑ | Decision |
|---|---:|---:|---:|---|
| 00:00–08:00, six channels | 0.172011 | 0.517741 | 0.799044 | Replaced in research |
| **Previous 21:00–08:00, six channels** | **0.167520** | **0.504500** | **0.810297** | **Retained** |
| Previous 18:00–08:00, six channels | 0.167278 | 0.504197 | 0.811781 | Only 0.14% Brier gain beyond retained window |
| Previous 15:00–08:00, six channels | 0.171897 | 0.516712 | 0.800480 | Longer history did not help |
| Previous 09:00–08:00, six channels | 0.172973 | 0.519795 | 0.794852 | Worse than retained window in all five folds |
| 00:00–08:00, category-supervised | 0.170664 | 0.515916 | 0.805171 | Marginal, uncertain gain; worsened light/patchy forecasts |
| 00:00–08:00, cloud/RH/u only | 0.178926 | 0.533730 | 0.781267 | Removing channels hurt skill |

The retained window improves Brier 2.6% relative to the nine-hour CNN; four of five folds and all three paired seed runs improve. Its paired season-bootstrap Brier difference versus nine hours was −0.00449, with a conditional 95% interval [−0.00863, −0.00049]. The previous 18:00 window added three more hours and 24 more parameters/member for a marginal average gain; the interval comparisons do not establish a statistical difference between those two windows.

Discarded experiment scripts, configurations, weights, detailed outputs and separate HTML reports were removed during cleanup. This compact record preserves the selection rationale. The single retained pipeline remains reproducible; climatology and matched logistic means remain evaluation baselines. The selected CNN now supplies the website's mist probability; the former three-variable logistic remains only a retrospective reference.
