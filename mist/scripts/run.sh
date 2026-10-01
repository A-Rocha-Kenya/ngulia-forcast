#!/usr/bin/env bash
set -euo pipefail
# Run from the repository root; install mist/requirements.txt first.
Rscript mist/scripts/01_prepare_data.R
"${MIST_PYTHON:-python3}" mist/scripts/02_compare_models.py
"${MIST_PYTHON:-python3}" mist/scripts/predict_numpy.py mist/model/research mist/intermediate-data/inference_check.json
Rscript mist/scripts/03_build_report.R
Rscript mist/scripts/04_verify.R
