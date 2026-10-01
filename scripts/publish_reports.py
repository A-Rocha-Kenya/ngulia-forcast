"""Copy model reports and chart libraries into the Pages artifact."""
from pathlib import Path
import shutil

root = Path(__file__).resolve().parents[1]
github = 'https://github.com/A-Rocha-Kenya/ngulia-forcast/blob/main/'
for model in ('count', 'mist'):
    source = root / model / 'reports'
    destination = root / 'site' / model
    destination.mkdir(parents=True, exist_ok=True)
    for path in source.glob('*.html'):
        html = path.read_text().replace('../../README.md', github + 'README.md')
        html = html.replace('../selection_notes.md', github + model + '/selection_notes.md')
        html = html.replace('../config.json', github + model + '/config.json')
        html = html.replace('../intermediate-data/scores.csv', 'scores.csv')
        html = html.replace('<body>', '<body><p style="margin:16px"><a href="../">← Forecast</a></p>')
        (destination / path.name).write_text(html)
    for directory in source.iterdir():
        if directory.is_dir():
            shutil.copytree(directory, destination / directory.name, dirs_exist_ok=True)
shutil.copy2(root / 'mist/intermediate-data/scores.csv', root / 'site/mist/scores.csv')

# Preserve links to the previously published report URLs.
legacy = root / 'site/reports'
legacy.mkdir(exist_ok=True)
for model in ('count', 'mist'):
    (legacy / f'{model}_model_report.html').write_text(
        f'<!doctype html><html lang="en"><head><meta charset="utf-8">'
        f'<meta http-equiv="refresh" content="0;url=../{model}/model.html">'
        f'<title>{model.title()} model report</title></head><body>'
        f'<a href="../{model}/model.html">Open the {model} model report</a></body></html>')
