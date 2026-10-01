"""Shared fitting and probability metrics for the main mist comparison and follow-up experiments."""
import csv
import json
from pathlib import Path
import numpy as np
from scipy.optimize import minimize
from scipy.special import expit
from scipy.stats import rankdata
import torch
from torch import nn

ROOT = Path(__file__).resolve().parents[2]
CONFIG = json.loads((ROOT / 'mist/config.json').read_text())
FEATURES = CONFIG['features']

def read_csv(path):
    with path.open() as handle:
        return list(csv.DictReader(handle))

def write_csv(path, rows):
    with path.open('w', newline='') as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)

def score(observed, probability):
    clipped = np.clip(probability, 1e-12, 1 - 1e-12)
    positives = int(observed.sum())
    negatives = len(observed) - positives
    auc = ((rankdata(probability)[observed == 1].sum() - positives * (positives + 1) / 2)
           / (positives * negatives)) if positives and negatives else None
    return dict(dates=len(observed), brier=float(np.mean((probability - observed) ** 2)),
                log_loss=float(-np.mean(observed * np.log(clipped) + (1 - observed) * np.log1p(-clipped))),
                auc=auc, mean_probability=float(probability.mean()), observed_frequency=float(observed.mean()))

def logistic_fit(values, observed):
    centre, spread = values.mean(0), values.std(0)
    design = np.column_stack([np.ones(len(values)), (values - centre) / spread])

    def objective(beta):
        eta = design @ beta
        return np.mean(np.logaddexp(0, eta) - observed * eta), design.T @ (expit(eta) - observed) / len(values)

    result = minimize(objective, np.zeros(design.shape[1]), jac=True, method='BFGS', options={'gtol': 1e-9})
    assert np.max(np.abs(objective(result.x)[1])) < 1e-7
    return dict(centre=centre.tolist(), spread=spread.tolist(), coefficients=result.x.tolist())

def logistic_predict(model, values):
    return expit(np.column_stack([np.ones(len(values)),
        (values - model['centre']) / model['spread']]) @ model['coefficients'])

class Network(nn.Module):
    def __init__(self, channels=len(FEATURES), hours=len(CONFIG['relative_hours'])):
        super().__init__()
        width = CONFIG['hidden_channels']
        kernel = CONFIG['kernel_size']
        self.extract = nn.Sequential(nn.Conv1d(channels, width, kernel, padding=kernel // 2), nn.ReLU(),
                                     nn.Conv1d(width, width, kernel, padding=kernel // 2), nn.ReLU())
        self.output = nn.Sequential(nn.Dropout(CONFIG['dropout']), nn.Linear(hours * width, 1))

    def forward(self, values):
        logits = self.output(self.extract(values.transpose(1, 2)).flatten(1))
        return logits.squeeze(1)

def standardized(train, test):
    centre, spread = train.mean((0, 1), keepdims=True), train.std((0, 1), keepdims=True)
    return torch.from_numpy((train - centre) / spread), torch.from_numpy((test - centre) / spread), centre, spread

def train_network(values, observed, seed, validation=None, epochs=CONFIG['max_epochs']):
    torch.manual_seed(seed)
    model = Network(values.shape[2], values.shape[1])
    with torch.no_grad():
        model.output[-1].bias.fill_(float(torch.logit(observed.mean())))
    optimizer = torch.optim.AdamW(model.parameters(), lr=CONFIG['learning_rate'], weight_decay=CONFIG['weight_decay'])
    loss_fn = nn.BCEWithLogitsLoss()
    best_score, best_epoch = np.inf, 1
    for epoch in range(1, epochs + 1):
        model.train()
        for batch in torch.randperm(len(observed)).split(CONFIG['batch_size']):
            optimizer.zero_grad()
            logits = model(values[batch])
            loss = loss_fn(logits, observed[batch])
            loss.backward()
            optimizer.step()
        if validation is not None:
            model.eval()
            with torch.no_grad():
                brier = float(((presence_probability(model, validation[0]) - validation[1]) ** 2).mean())
            if brier < best_score - CONFIG['stopping_tolerance']:
                best_score, best_epoch = brier, epoch
            if epoch - best_epoch >= CONFIG['patience']:
                break
    return model, best_epoch if validation is not None else epochs


def presence_probability(model, values):
    logits = model(values)
    return torch.sigmoid(logits)
