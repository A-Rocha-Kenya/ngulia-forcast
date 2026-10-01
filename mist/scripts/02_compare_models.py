"""Compare climatology, mean-weather logistic regression and an hourly CNN on fixed season folds."""
import hashlib
import json
import platform
import time
from pathlib import Path

import numpy as np
import scipy
import torch
from modeling import read_csv, write_csv, score, logistic_fit, logistic_predict, standardized, train_network

# Read the common cohort and explicit experiment settings ------------------
ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'count/intermediate-data/mist'
CONFIG = json.loads((ROOT / 'mist/config.json').read_text())
FEATURES = CONFIG['features']
SEEDS = CONFIG['seeds']
torch.set_num_threads(1)
torch.use_deterministic_algorithms(True)

labels = sorted(read_csv(OUT / 'labels.csv'), key=lambda row: row['ringing_date'])
date_index = {row['ringing_date']: i for i, row in enumerate(labels)}
raw = np.empty((len(labels), len(CONFIG['relative_hours']), len(FEATURES)), dtype=np.float64)
for row in read_csv(OUT / 'hourly.csv'):
    raw[date_index[row['ringing_date']], CONFIG['relative_hours'].index(int(row['hour']))] = [float(row[v]) for v in FEATURES]
x = raw.astype(np.float32)
means = raw.mean(axis=1)
deployed_means = raw[:, np.array(CONFIG['relative_hours']) >= 0, :3].mean(axis=1)
y = np.array([int(row['mist_present']) for row in labels], dtype=np.float32)
folds = np.array([int(row['fold']) for row in labels])
seasons = np.array([int(row['season']) for row in labels])
fold_numbers = sorted(set(folds))
assert np.isfinite(raw).all() and len(date_index) == len(labels)
assert seasons.max() <= CONFIG['last_label_season']
assert all(len(set(folds[seasons == season])) == 1 for season in set(seasons))

# Outer season folds; inner training fold chooses CNN stopping epoch -------
started = time.perf_counter()
predictions, seed_predictions, training, reference_predictions = [], [], [], []
for fold in fold_numbers:
    train, test = folds != fold, folds == fold
    null = np.full(test.sum(), y[train].mean())
    logistic = logistic_fit(means[train], y[train])
    p_logistic = logistic_predict(logistic, means[test])
    # A separate reference reproduces the deployed three-variable baseline.
    core = logistic_fit(deployed_means[train], y[train])
    p_core = logistic_predict(core, deployed_means[test])
    validation_fold = fold_numbers[(fold_numbers.index(fold) + 1) % len(fold_numbers)]
    inner_train = train & (folds != validation_fold)
    inner_validation = folds == validation_fold
    ensemble = []
    for seed in SEEDS:
        inner_x, val_x, _, _ = standardized(x[inner_train], x[inner_validation])
        _, stopping_epoch = train_network(inner_x, torch.from_numpy(y[inner_train]), seed,
            validation=(val_x, torch.from_numpy(y[inner_validation])))
        train_x, test_x, _, _ = standardized(x[train], x[test])
        model, _ = train_network(train_x, torch.from_numpy(y[train]), seed, epochs=stopping_epoch)
        model.eval()
        with torch.no_grad():
            probability = torch.sigmoid(model(test_x)).numpy()
        ensemble.append(probability)
        training.append(dict(fold=int(fold), seed=seed, validation_fold=int(validation_fold),
            inner_train_dates=int(inner_train.sum()), inner_validation_dates=int(inner_validation.sum()),
            refit_dates=int(train.sum()), test_dates=int(test.sum()), stopping_epoch=stopping_epoch,
            parameters=sum(parameter.numel() for parameter in model.parameters())))
        for index, p in zip(np.flatnonzero(test), probability):
            seed_predictions.append(dict(seed=seed, **labels[index], probability=float(p)))
    for name, probability in [('M0 climatology', null), ('M1 logistic means', p_logistic),
                              ('M2 hourly CNN', np.mean(ensemble, axis=0))]:
        for index, p in zip(np.flatnonzero(test), probability):
            predictions.append(dict(model=name, **labels[index], probability=float(p)))
    for index, p in zip(np.flatnonzero(test), p_core):
        reference_predictions.append(dict(model='Deployed logistic (3 means)', **labels[index], probability=float(p)))
    print(f'Fold {fold}: all three models evaluated', flush=True)

write_csv(OUT / 'predictions.csv', predictions)
write_csv(OUT / 'seed_predictions.csv', seed_predictions)
write_csv(OUT / 'training.csv', training)
write_csv(OUT / 'deployed_reference_predictions.csv', reference_predictions)
models = ['M0 climatology', 'M1 logistic means', 'M2 hourly CNN']
pooled_scores, fold_scores, category_scores, probabilities = [], [], [], {}
for name in models:
    records = sorted([r for r in predictions if r['model'] == name], key=lambda r: r['ringing_date'])
    probability = np.array([r['probability'] for r in records])
    probabilities[name] = probability
    assert len(records) == len(labels) and np.isfinite(probability).all() and ((probability > 0) & (probability < 1)).all()
    pooled_scores.append(dict(model=name, **score(y, probability)))
    for fold in fold_numbers:
        mask = folds == fold
        fold_scores.append(dict(model=name, fold=int(fold), **score(y[mask], probability[mask])))
    for category in sorted({r['mist_observation'] for r in labels}):
        mask = np.array([r['mist_observation'] == category for r in labels])
        category_scores.append(dict(model=name, category=category, **score(y[mask], probability[mask])))
write_csv(OUT / 'scores.csv', pooled_scores)
write_csv(OUT / 'fold_scores.csv', fold_scores)
write_csv(OUT / 'category_scores.csv', category_scores)
write_csv(OUT / 'deployed_reference_scores.csv', [dict(model='Deployed logistic (3 means)',
    **score(y, np.array([r['probability'] for r in sorted(reference_predictions, key=lambda r: r['ringing_date'])])))])
write_csv(OUT / 'seed_scores.csv', [dict(seed=seed, **score(y, np.array([r['probability'] for r in
    sorted([r for r in seed_predictions if r['seed'] == seed], key=lambda r: r['ringing_date'])]))) for seed in SEEDS])

# Paired season bootstrap: candidate choice remains exploratory ------------
intervals = []
for baseline, candidate in [(models[0], models[1]), (models[1], models[2])]:
    rng = np.random.default_rng(CONFIG['bootstrap_seed'])
    for metric in ['brier', 'log_loss']:
        if metric == 'brier':
            delta = (probabilities[candidate] - y) ** 2 - (probabilities[baseline] - y) ** 2
        else:
            def losses(p):
                p = np.clip(p, 1e-12, 1 - 1e-12)
                return -y * np.log(p) - (1 - y) * np.log1p(-p)
            delta = losses(probabilities[candidate]) - losses(probabilities[baseline])
        by_season = np.array([[delta[seasons == season].sum(), (seasons == season).sum()] for season in sorted(set(seasons))])
        draws = rng.integers(0, len(by_season), (CONFIG['bootstrap_replicates'], len(by_season)))
        samples = by_season[draws].sum(axis=1)
        low, high = np.quantile(samples[:, 0] / samples[:, 1], [0.025, 0.975])
        intervals.append(dict(baseline=baseline, candidate=candidate, metric=metric,
            difference=float(delta.mean()), low=float(low), high=float(high)))
write_csv(OUT / 'paired_differences.csv', intervals)

# Refit research artifacts on all observations; these are not CV predictions
artifact_dir = ROOT / 'mist/model/research'
artifact_dir.mkdir(exist_ok=True)
logistic = logistic_fit(means, y)
logistic['features'] = FEATURES
(artifact_dir / 'logistic.json').write_text(json.dumps(logistic, indent=2))
raw_coefficients = np.array(logistic['coefficients'][1:]) / logistic['spread']
intercept = logistic['coefficients'][0] - raw_coefficients @ logistic['centre']
write_csv(OUT / 'logistic_coefficients.csv', [dict(term='Intercept', coefficient=float(intercept))] +
    [dict(term=feature, coefficient=float(value)) for feature, value in zip(FEATURES, raw_coefficients)])
train_x, _, centre, spread = standardized(x, x)
members, full_epochs = [], []
for seed in SEEDS:
    epochs = int(np.median([row['stopping_epoch'] for row in training if row['seed'] == seed]))
    model, _ = train_network(train_x, torch.from_numpy(y), seed, epochs=epochs)
    model.eval()
    with torch.no_grad():
        members.append(torch.sigmoid(model(train_x)).numpy())
    artifact = dict(model='M2 hourly CNN', architecture='CNN', features=FEATURES, seed=seed,
        centre=centre.tolist(), spread=spread.tolist(), relative_hours=CONFIG['relative_hours'],
        timezone=CONFIG['timezone'], state={key: value.tolist() for key, value in model.state_dict().items()},
        epochs=epochs, last_label_season=CONFIG['last_label_season'])
    (artifact_dir / f'cnn_seed_{seed}.json').write_text(json.dumps(artifact))
    full_epochs.append(dict(seed=seed, epochs=epochs))
np.savez(artifact_dir / 'inference_sample.npz', raw=x[:32], probability=np.mean(members, axis=0)[:32])
write_csv(OUT / 'full_fit_training.csv', full_epochs)
summary = dict(dates=len(labels), seasons=len(set(seasons)), first_season=int(seasons.min()),
    last_season=int(seasons.max()), observed_frequency=float(y.mean()),
    elapsed_seconds=time.perf_counter() - started, python=platform.python_version(),
    numpy=np.__version__, scipy=scipy.__version__, torch=torch.__version__, config=CONFIG)
(OUT / 'run_summary.json').write_text(json.dumps(summary, indent=2))
paths = [Path(__file__), ROOT / 'mist/config.json', OUT / 'labels.csv', OUT / 'hourly.csv',
         ROOT / 'mist/scripts/01_prepare_data.R', ROOT / 'mist/scripts/03_build_report.R',
         ROOT / 'mist/scripts/predict_numpy.py', ROOT / 'mist/scripts/modeling.py', ROOT / 'mist/scripts/04_verify.R',
         ROOT / 'mist/scripts/run.sh', ROOT / 'mist/requirements.txt']
write_csv(OUT / 'run_manifest.csv', [dict(source=str(path.relative_to(ROOT)),
    sha256=hashlib.sha256(path.read_bytes()).hexdigest()) for path in paths])
(artifact_dir / 'metadata.json').write_text(json.dumps(dict(format_version=1,
    target=CONFIG['target'], features=FEATURES, units=['fraction', 'percent', 'm/s', 'm/s', 'degC', 'degC'],
    timezone=CONFIG['timezone'], relative_hours=CONFIG['relative_hours'], seeds=SEEDS,
    first_training_date=labels[0]['ringing_date'], last_training_date=labels[-1]['ringing_date'],
    input_manifest=read_csv(OUT / 'input_manifest.csv'), run_manifest=read_csv(OUT / 'run_manifest.csv'),
    runtime=summary, status='published_mist_cnn'), indent=2))
print(json.dumps(pooled_scores, indent=2), flush=True)
