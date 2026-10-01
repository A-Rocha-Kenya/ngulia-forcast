"""Research CNN inference with NumPy only; raw shape is nights × 12 hours × 6 features."""
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


def predict(artifacts, raw):
    return np.mean([predict_member(artifact, raw) for artifact in artifacts], axis=0)


if __name__ == '__main__':
    directory = Path(sys.argv[1])
    artifacts = [json.loads(path.read_text()) for path in sorted(directory.glob('cnn_seed_*.json'))]
    sample = np.load(directory / 'inference_sample.npz')
    difference = float(np.max(np.abs(predict(artifacts, sample['raw']) - sample['probability'])))
    assert difference < 1e-6
    result = dict(members=len(artifacts), nights=len(sample['raw']), max_probability_difference=difference,
        artifact_bytes=sum(path.stat().st_size for path in directory.glob('cnn_seed_*.json')))
    (directory.parent / 'inference_check.json').write_text(json.dumps(result, indent=2))
    print(json.dumps(result, indent=2))
