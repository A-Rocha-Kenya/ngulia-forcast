"""Apply the frozen three-member mist CNN to issued hourly weather."""
import csv
import json
import sys
from pathlib import Path
import numpy as np


def predict_member(artifact, raw):
    values = (raw - np.array(artifact['centre'])) / np.array(artifact['spread'])
    weights = {key: np.array(value) for key, value in artifact['state'].items()}
    for layer in ['extract.0', 'extract.2']:
        kernel = weights[layer + '.weight'].shape[-1]
        padded = np.pad(values, ((0, 0), (kernel // 2, kernel // 2), (0, 0)))
        windows = np.lib.stride_tricks.sliding_window_view(padded, kernel, axis=1)
        values = np.maximum(0, np.einsum('btck,ock->bto', windows, weights[layer + '.weight'])
                            + weights[layer + '.bias'])
    hidden = values.transpose(0, 2, 1).reshape(len(raw), -1)
    eta = hidden @ weights['output.1.weight'].T + weights['output.1.bias']
    return 1 / (1 + np.exp(-eta.ravel()))


input_file, output_file, artifact_dir = map(Path, sys.argv[1:4])
metadata = json.loads((artifact_dir / 'metadata.json').read_text())
with input_file.open() as handle:
    rows = list(csv.DictReader(handle))
dates = list(dict.fromkeys(row['weather_valid_date'] for row in rows))
raw = np.array([[[float(row[feature]) for feature in metadata['features']]
                 for row in rows if row['weather_valid_date'] == date]
                for date in dates])
assert raw.shape == (len(dates), len(metadata['relative_hours']), len(metadata['features']))
assert all([int(row['hour']) for row in rows if row['weather_valid_date'] == date]
           == metadata['relative_hours'] for date in dates)
artifacts = [json.loads((artifact_dir / f'cnn_seed_{seed}.json').read_text())
             for seed in metadata['seeds']]
probability = np.mean([predict_member(artifact, raw) for artifact in artifacts], axis=0)
with output_file.open('w', newline='') as handle:
    writer = csv.writer(handle)
    writer.writerow(['weather_valid_date', 'mist_probability_pct'])
    writer.writerows(zip(dates, 100 * probability))
