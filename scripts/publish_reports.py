"""Publish model reports with the forecast site's shared layout and theme."""
import json
import re
from pathlib import Path
import shutil

root = Path(__file__).resolve().parents[1]
github = 'https://github.com/A-Rocha-Kenya/ngulia-forcast/blob/main/'
site = (root / 'site/index.html').read_text()
explore = root / 'site/explore'
explore.mkdir(exist_ok=True)
(explore / 'index.html').write_text(site.replace('<head>', '<head>\n    <base href="../">', 1)
    .replace('Live forecast | Ngulia', 'Explore forecast | Ngulia')
    .replace('Daily bird capture and mist forecasts for the Ngulia ringing season.',
             'Explore how dates and weather change the Ngulia bird capture and mist forecasts.'))
header = re.search(r'<header\b.*?</header>', site, re.DOTALL).group()
footer = re.search(r'<footer\b.*?</footer>', site, re.DOTALL).group()
theme = dict(re.findall(r'--([\w-]+):\s*([^;]+);', (root / 'site/styles.css').read_text()))
palette = {
    '#167b75': theme['cyan'], '#b87628': theme['amber'], '#172d32': theme['ink'],
    '#adbab5': theme['muted'], '#aaa': theme['muted'], '#333': theme['ink'],
    '#7c8995': theme['muted'], '#246b93': theme['amber'], '#54728a': theme['muted'],
    '#a04d86': theme['green'], '#536ca9': '#8ea4d0',
    '#66c2a5': theme['green'], '#fc8d62': theme['amber'], '#8da0cb': theme['muted'],
}
rgb_palette = {
    tuple(int(old[i:i + 2], 16) for i in (1, 3, 5)):
    ','.join(str(int(new[i:i + 2], 16)) for i in (1, 3, 5))
    for old, new in palette.items() if len(old) == 7
}


def recolor(value, color=False):
    if isinstance(value, dict):
        return {key: recolor(item, key in ('color', 'fillcolor', 'bgcolor')) for key, item in value.items()}
    if isinstance(value, list):
        return [recolor(item, color) for item in value]
    if color and isinstance(value, str):
        rgba = re.fullmatch(r'rgba\((\d+),(\d+),(\d+),([\d.]+)\)', value)
        if rgba and tuple(map(int, rgba.group(1, 2, 3))) in rgb_palette:
            return f'rgba({rgb_palette[tuple(map(int, rgba.group(1, 2, 3)))]},{rgba.group(4)})'
        return palette.get(value, value)
    return value


def theme_widget(match):
    widget = json.loads(match.group(2))
    if 'layout' in widget['x']:
        widget = recolor(widget)
        layout = widget['x']['layout']
        layout.update(paper_bgcolor=theme['panel-soft'], plot_bgcolor=theme['panel-soft'],
                      font={'color': theme['ink'], 'family': 'Inter, system-ui, sans-serif'})
        for key, axis in layout.items():
            if re.fullmatch(r'[xy]axis\d*', key):
                axis.update(color=theme['muted'], gridcolor=theme['line'],
                            zerolinecolor=theme['line'], linecolor=theme['line'])
        layout.setdefault('legend', {}).update(bgcolor='rgba(0,0,0,0)', font={'color': theme['ink']})
        layout['hoverlabel'] = {'bgcolor': theme['panel'], 'bordercolor': theme['line'], 'font': {'color': theme['ink']}}
    return match.group(1) + json.dumps(widget, ensure_ascii=False, separators=(',', ':')) + match.group(3)


for model in ('count', 'mist'):
    source = root / model / 'reports'
    destination = root / 'site' / model
    destination.mkdir(parents=True, exist_ok=True)
    for path in source.glob('*.html'):
        html = path.read_text().replace('../../README.md', github + 'README.md')
        html = html.replace('../selection_notes.md', github + model + '/selection_notes.md')
        html = html.replace('../config.json', github + model + '/config.json')
        html = html.replace('../intermediate-data/scores.csv', 'scores.csv')
        head = re.search(r'<head>(.*?)</head>', html, re.DOTALL).group(1)
        head = re.sub(r'<style>.*?</style>', '', head, flags=re.DOTALL)
        head += '\n<link rel="icon" href="../favicon.svg" type="image/svg+xml">'
        head += '\n<link rel="stylesheet" href="../styles.css">\n<link rel="stylesheet" href="../reports.css">'
        body = re.search(r'<body>(.*?)</body>', html, re.DOTALL).group(1)
        label = f'{model.title()} model' if path.name == 'model.html' else f'{model.title()} model comparisons'
        body = body.replace('<main>', f'<main class="report-content"><p class="report-kicker">{label}</p>', 1)
        body = body.replace('<nav>', '<nav class="report-toc" aria-label="Page sections">')
        body = body.replace('href="model.html"', 'href="./"')
        body = re.sub(r'(<script type="application/json" data-for="[^"]+">)(.*?)(</script>)', theme_widget, body, flags=re.DOTALL)
        navigation = re.sub(r'href="(?!https?://)([^"]*)"', r'href="../\1"', header)
        navigation = navigation.replace(f'href="../{model}/"', f'href="../{model}/" aria-current="page"')
        navigation = navigation.replace('Loading…', 'Model guide')
        # Keep widget dependencies in the head; publish one valid document shell.
        html = f'<!doctype html>\n<html lang="en"><head>{head}</head>\n<body class="report-page">\n{navigation}\n{body}\n{footer}\n</body></html>\n'
        (destination / path.name).write_text(html)
        if path.name == 'model.html':
            (destination / 'index.html').write_text(html)
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
        f'<meta http-equiv="refresh" content="0;url=../{model}/">'
        f'<title>{model.title()} model | Ngulia</title></head><body>'
        f'<a href="../{model}/">Open the {model} model</a></body></html>')
