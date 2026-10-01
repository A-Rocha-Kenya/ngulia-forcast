"""Compare shared JavaScript inference with frozen NumPy inference on observed nights."""
import csv
import json
import sys
from pathlib import Path
import numpy as np

root = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(root / 'mist/scripts'))
from predict_numpy import predict

metadata = json.loads((root / 'mist/model/metadata.json').read_text())
with (root / 'mist/intermediate-data/hourly.csv').open() as handle:
    rows = list(csv.DictReader(handle))[:12 * 200]
raw = np.array([[float(row[feature]) for feature in metadata['features']] for row in rows]).reshape(-1, 12, 6)
artifacts = [json.loads((root / f'mist/model/cnn_seed_{seed}.json').read_text()) for seed in metadata['seeds']]
(root / 'mist/intermediate-data/javascript_reference.json').write_text(json.dumps(
    dict(hours=raw.tolist(), probability=(100 * predict(artifacts, raw)).tolist())))
(root / 'mist/model/inference_reference.json').write_text(json.dumps(
    dict(hours=raw[:10].tolist(), probability=(100 * predict(artifacts, raw[:10])).tolist())))
