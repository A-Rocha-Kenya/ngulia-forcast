#!/usr/bin/env bash
set -euo pipefail
# Run from the repository root; install research/mist/requirements.txt first.
Rscript scripts/experiments/mist/01_prepare_data.R
"${MIST_PYTHON:-python3}" scripts/experiments/mist/02_compare_models.py
"${MIST_PYTHON:-python3}" scripts/experiments/mist/predict_numpy.py validation/research/mist/artifacts
Rscript scripts/experiments/mist/03_build_report.R
Rscript scripts/experiments/mist/04_verify.R
