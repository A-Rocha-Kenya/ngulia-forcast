"""Copy the committed research reports into the Pages artifact."""
from pathlib import Path
import shutil

root = Path(__file__).resolve().parents[2]
source = root / 'validation/research'
destination = root / 'site/reports'
destination.mkdir(parents=True, exist_ok=True)

for model in ('count', 'mist'):
    name = f'{model}_model_report'
    html = (source / f'{name}.html').read_text()
    html = html.replace('../../research/',
        'https://github.com/A-Rocha-Kenya/ngulia-forcast/blob/main/research/')
    html = html.replace('<body>', '<body><p style="margin:16px"><a href="../">← Forecast</a></p>')
    (destination / f'{name}.html').write_text(html)
    shutil.copytree(source / f'{name}_lib', destination / f'{name}_lib', dirs_exist_ok=True)

(destination / 'mist').mkdir(exist_ok=True)
shutil.copy2(source / 'mist/scores.csv', destination / 'mist/scores.csv')
