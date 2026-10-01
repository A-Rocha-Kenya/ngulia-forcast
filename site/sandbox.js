import { calendar, countMean, predictCount, mistProbability, scenarioInput, scenarioHours, effect } from "./prediction.js";
import { updateForecastVisuals } from "./card-animation.js";

const format = new Intl.NumberFormat("en", { maximumFractionDigits: 1 });
const whole = new Intl.NumberFormat("en", { maximumFractionDigits: 0 });
const dateLabel = new Intl.DateTimeFormat("en", { day: "numeric", month: "short", year: "numeric", timeZone: "UTC" });
const ringingDateLabel = new Intl.DateTimeFormat("en", { day: "numeric", month: "short", timeZone: "UTC" });
const dayMs = 86400000;
const iso = date => date.toISOString().slice(0, 10);
const day = date => new Date(`${date}T12:00:00Z`);
const controls = [
  { key: "rain", variable: "rain_log", label: "Rain 00–08", unit: "mm", step: 0.01 },
  { key: "wind", variable: "wind_speed_10m_mean_ms", label: "Wind speed", unit: "m/s", step: 0.01 },
  { key: "direction", label: "Wind direction · from", unit: "°", min: 0, max: 360, step: 1 },
  { key: "temperature", variable: "temperature_2m_mean_c", label: "Temperature", unit: "°C", step: 0.01 },
  { key: "pressure", variable: "surface_pressure_mean_hpa", label: "Pressure", unit: "hPa", step: 0.01 },
  { key: "cloud", variable: "total_cloud_cover_mean", label: "Cloud cover", unit: "%", step: 0.001 },
  { key: "humidity", variable: "relative_humidity_mean_pct", label: "Humidity", unit: "%", step: 0.01 }
];

export async function loadSandbox() {
  const [count, mist] = await Promise.all(["count", "mist"].map(async name => {
    const response = await fetch(`models/${name}.json`);
    if (!response.ok) throw new Error(`Cannot load ${name} model`);
    return response.json();
  }));
  const today = new Date().toLocaleDateString("en-CA", { timeZone: "Africa/Nairobi" });
  const year = calendar(today).season;
  const currentYear = new Date(today).getUTCMonth() >= 5 ? year : year + 1;
  const parameters = new URLSearchParams(window.location.search);
  let date = parameters.get("date") || (calendar(today).season_day >= 1 && calendar(today).season_day <= 85 ? today : `${currentYear}-11-12`);
  const defaults = Object.fromEntries(controls.filter(c => c.variable).map(c => [c.key,
    c.key === "rain" ? Math.expm1(count.effects[c.variable].reference) : count.effects[c.variable].reference]));
  defaults.direction = (Math.atan2(-count.effects.wind_u_10m_mean_ms.reference, -count.effects.wind_v_10m_mean_ms.reference) * 180 / Math.PI + 360) % 360;
  let weather = Object.fromEntries(Object.entries(defaults).map(([key, value]) => [key, parameters.has(key) ? Number(parameters.get(key)) : value])), expanded = false;
  const years = Array.from({ length: 10 }, (_, i) => currentYear + i);
  if (!years.includes(calendar(date).season)) years.unshift(calendar(date).season);
  controls.forEach(c => {
    if (!c.variable) return;
    c.min = c.key === "rain" ? Math.expm1(count.effects[c.variable].min) : count.effects[c.variable].min;
    c.max = c.key === "rain" ? Math.expm1(count.effects[c.variable].max) : count.effects[c.variable].max;
  });
  // Compare catch variation across the central 90% of each observed input.
  controls.forEach(c => {
    const histogram = c.variable ? count.effects[c.variable].histogram : count.direction_histogram;
    const total = histogram.count.reduce((sum, n) => sum + n, 0);
    const limits = [0.05, 0.95].map(p => {
      let cumulative = 0;
      const value = histogram.x[histogram.count.findIndex(n => (cumulative += n) >= p * total)];
      return c.key === "rain" ? Math.expm1(value) : value;
    });
    const means = Array.from({ length: 101 }, (_, i) => countMean(count,
      scenarioInput(date, { ...defaults, [c.key]: limits[0] + i / 100 * (limits[1] - limits[0]) })));
    c.importance = Math.max(...means) / Math.min(...means);
  });
  controls.sort((a, b) => b.importance - a.importance);

  document.body.classList.add("sandbox-mode");
  document.querySelector("#update-label").textContent = "Interactive forecast";
  document.querySelector("#live-title").textContent = "Your scenario";
  document.querySelector(".moon-signal").hidden = true;
  document.querySelector(".weather-details").hidden = true;
  document.querySelector(".next-section").hidden = true;
  document.querySelector(".season-section").hidden = true;
  const section = document.createElement("section");
  section.className = "sandbox-controls";
  section.innerHTML = `<div class="sandbox-intro"><h2>Explore the forecast</h2>
    <p>Choose a ringing date and drag the weather markers. Both predictions update immediately.</p></div>
    <div class="calendar-heading">
      <label for="scenario-season">Season <select id="scenario-season">${years.map(y => `<option value="${y}">${y}–${String(y + 1).slice(-2)}</option>`).join("")}</select></label>
      <label for="scenario-date">Ringing date <select id="scenario-date"></select></label>
      <div class="date-stepper"><button id="date-previous" type="button" aria-label="Previous ringing date">‹</button><button id="date-next" type="button" aria-label="Next ringing date">›</button></div>
      <button id="calendar-expand" type="button" aria-expanded="false" aria-controls="sandbox-calendar"><span>Show other years</span><svg viewBox="0 0 24 24" aria-hidden="true"><path d="m6 9 6 6 6-6"/></svg></button></div>
    <div id="sandbox-calendar" class="sandbox-calendar"></div>
    <div class="calendar-key"><span>Lower catch</span><i></i><span>Higher catch</span><span>◇ New moon</span></div>
    <div class="weather-heading"><h2>Shape the weather</h2><div class="sandbox-actions"><button id="scenario-copy" type="button">Copy scenario link</button><button id="weather-reset" type="button">Reset typical weather</button></div></div>
    <p class="sandbox-note">Panels run from larger to smaller effects on the predicted catch across typical observed weather, with other conditions held fixed. Curves show the catch multiplier relative to typical weather. Shading is the 95% fitted-effect interval; wind curves combine speed and direction effects and have no interval. Histograms show observed training values. Drag the amber marker to change a value.</p>
    <div id="scenario-weather" class="scenario-weather">${controls.map(c => `<article class="effect-card">
      <div class="effect-heading"><span>${c.label}</span><output id="value-${c.key}"></output></div>
      <svg id="plot-${c.key}" class="effect-plot" viewBox="0 0 380 240" role="slider" tabindex="0" aria-label="${c.label}" aria-valuemin="${c.min}" aria-valuemax="${c.max}"></svg>
    </article>`).join("")}</div>
    `;
  document.querySelector("main").append(section);

  function formatValue(c, value) { return `${format.format(c.key === "cloud" ? value * 100 : value)} ${c.unit}`; }

  function drawCalendar() {
    const season = calendar(date).season;
    const shown = expanded ? [...years].reverse() : [season];
    const rows = shown.map(y => Array.from({ length: 85 }, (_, i) => {
      const d = iso(new Date(day(`${y}-10-20`).getTime() + i * dayMs));
      return { date: d, ...calendar(d), mean: countMean(count, scenarioInput(d, defaults)) };
    }));
    const max = Math.max(...rows.flat().map(r => r.mean));
    const left = 58, width = 960, cell = (width - left - 12) / 85, rowHeight = 66;
    const color = value => {
      const stops = [[20, 40, 56], [45, 147, 158], [246, 203, 91], [243, 133, 77]];
      const p = value / max * 2.999, k = Math.floor(p), t = p - k;
      return `rgb(${stops[k].map((v, i) => Math.round(v + t * (stops[Math.min(k + 1, 3)][i] - v))).join(",")})`;
    };
    const guides = [...Array.from({ length: 17 }, (_, i) => i * 5), 84].map(i => {
      const label = new Intl.DateTimeFormat("en", { day: "2-digit", month: "short", timeZone: "UTC" }).format(day(rows[0][i].date));
      return `<line class="calendar-guide" x1="${left + i * cell}" x2="${left + i * cell}" y1="0" y2="${shown.length * rowHeight}"/><text x="${left + i * cell}" y="${shown.length * rowHeight + 20}" text-anchor="middle">${label}</text>`;
    }).join("") + `<line class="december-guide" x1="${left + 42 * cell}" x2="${left + 42 * cell}" y1="0" y2="${shown.length * rowHeight}"/>`;
    document.querySelector("#sandbox-calendar").innerHTML = `<svg viewBox="0 0 ${width} ${shown.length * rowHeight + 32}" aria-label="Season timing and moon opportunity calendar">${rows.map((row, j) => {
      const y = j * rowHeight;
      const selected = row.findIndex(r => r.date === date);
      return `<g class="calendar-row" data-year="${shown[j]}" tabindex="0" role="slider" aria-label="${shown[j]} ringing date" aria-valuemin="1" aria-valuemax="85" aria-valuenow="${selected >= 0 ? selected + 1 : 1}" aria-valuetext="${selected >= 0 ? date : row[0].date}">
        <text x="44" y="${y + 37}" text-anchor="end">${shown[j]}</text>
        ${row.map((r, i) => `<rect x="${left + i * cell}" y="${y + 4}" width="${cell + 0.2}" height="50" fill="${color(r.mean)}"><title>${dateLabel.format(day(r.date))}: ${whole.format(r.mean)} birds with typical weather</title></rect>`).join("")}
        ${row.filter((r, i) => Math.abs(r.moon_days_from_new_moon) < 0.5).map(r => `<text class="new-moon" x="${left + (row.indexOf(r) + 0.5) * cell}" y="${y + 39}" text-anchor="middle">◆</text>`).join("")}
        ${selected >= 0 ? `<rect class="selected-day" x="${left + selected * cell}" y="${y + 1}" width="${cell}" height="56"/>` : ""}
        <rect class="calendar-hit" x="${left}" y="${y}" width="${85 * cell}" height="${rowHeight - 8}"/>
      </g>`;
    }).join("")}${guides}</svg>`;
    document.querySelectorAll(".calendar-row").forEach(row => {
      row.addEventListener("click", event => {
        const svg = row.ownerSVGElement, bounds = svg.getBoundingClientRect();
        const i = Math.max(0, Math.min(84, Math.floor(((event.clientX - bounds.left) * width / bounds.width - left) / cell)));
        date = iso(new Date(day(`${row.dataset.year}-10-20`).getTime() + i * dayMs));
        updateDate();
      });
      row.addEventListener("keydown", event => {
        if (!["ArrowLeft", "ArrowRight", "Home", "End"].includes(event.key)) return;
        event.preventDefault();
        const old = Number(row.getAttribute("aria-valuenow"));
        const i = event.key === "Home" ? 0 : event.key === "End" ? 84 : Math.max(0, Math.min(84, old - 1 + (event.key === "ArrowRight" ? 1 : -1)));
        const rowYear = row.dataset.year;
        date = iso(new Date(day(`${rowYear}-10-20`).getTime() + i * dayMs));
        updateDate();
        document.querySelector(`.calendar-row[data-year="${rowYear}"]`).focus();
      });
    });
  }

  function axisValue(c, position) {
    return c.key === "rain" ? Math.expm1(Math.log1p(c.min) + position * (Math.log1p(c.max) - Math.log1p(c.min)))
      : c.min + position * (c.max - c.min);
  }
  function axisPosition(c, value) {
    return c.key === "rain" ? (Math.log1p(value) - Math.log1p(c.min)) / (Math.log1p(c.max) - Math.log1p(c.min))
      : (value - c.min) / (c.max - c.min);
  }

  function drawEffect(c) {
    const svg = document.querySelector(`#plot-${c.key}`);
    const input = scenarioInput(date, weather);
    const baseline = scenarioInput(date, { ...weather, [c.key]: defaults[c.key] });
    const base = countMean(count, baseline);
    const curve = Array.from({ length: 101 }, (_, i) => {
      const value = axisValue(c, i / 100);
      const changed = scenarioInput(date, { ...weather, [c.key]: value });
      const ratio = countMean(count, changed) / base;
      const se = c.variable ? effect(count, c.variable, changed[c.variable], "se") : 0;
      return { value, ratio, low: ratio * Math.exp(-1.96 * se), high: ratio * Math.exp(1.96 * se) };
    });
    const ymax = Math.max(1.2, ...curve.map(r => r.high));
    const x = value => 44 + axisPosition(c, value) * 322;
    const y = value => 155 - value / ymax * 140;
    const line = field => curve.map((r, i) => `${i ? "L" : "M"}${x(r.value).toFixed(2)},${y(r[field]).toFixed(2)}`).join(" ");
    const histogram = c.variable ? count.effects[c.variable].histogram : count.direction_histogram;
    const histogramMax = Math.max(...histogram.count);
    const hist = histogram.x.map((v, i) => {
      const value = c.key === "rain" ? Math.expm1(v) : v;
      const next = c.key === "rain" ? Math.expm1(histogram.x[i + 1] ?? v + (v - histogram.x[i - 1])) : histogram.x[i + 1];
      const barWidth = c.key === "rain" ? Math.max(2, x(next) - x(value)) : 322 / histogram.x.length;
      return `<rect class="histogram-bar" x="${Math.max(44, x(value) - barWidth / 2)}" y="${188 - histogram.count[i] / histogramMax * 21}" width="${Math.min(barWidth, 366 - Math.max(44, x(value) - barWidth / 2))}" height="${histogram.count[i] / histogramMax * 21}"/>`;
    }).join("");
    const selectedX = x(weather[c.key]);
    svg.innerHTML = `<defs><clipPath id="clip-${c.key}"><rect x="44" y="10" width="322" height="185"/></clipPath></defs>
      ${[0, ymax / 2, ymax].map(v => `<line class="effect-grid" x1="44" x2="366" y1="${y(v)}" y2="${y(v)}"/><text x="37" y="${y(v) + 4}" text-anchor="end">${format.format(v)}×</text>`).join("")}
      <g clip-path="url(#clip-${c.key})"><line class="effect-reference" x1="44" x2="366" y1="${y(1)}" y2="${y(1)}"/>
      ${c.key !== "wind" && c.key !== "direction" ? `<path class="effect-band" d="${line("high")} ${[...curve].reverse().map(r => `L${x(r.value)},${y(r.low)}`).join(" ")} Z"/>` : ""}
      <path class="effect-curve" d="${line("ratio")}"/>${hist}
      <line class="effect-selection" x1="${selectedX}" x2="${selectedX}" y1="10" y2="192"/>
      <circle class="effect-handle" cx="${selectedX}" cy="${y(countMean(count, input) / base)}" r="6"/></g>
      <text class="distribution-label" x="44" y="164">Observed distribution</text>
      ${(c.key === "rain" ? [0, 0.5, 1, 2, 5, 10, c.max] : [c.min, axisValue(c, 0.5), c.max]).map(value => `<text x="${x(value)}" y="209" text-anchor="middle">${format.format(c.key === "cloud" ? value * 100 : value)}</text>`).join("")}
      <text x="205" y="232" text-anchor="middle">${c.unit}${c.key === "rain" ? " · log(1 + rain) scale" : ""}</text>
      <rect class="effect-hit" x="44" y="10" width="322" height="182"/>`;
    document.querySelector(`#value-${c.key}`).textContent = formatValue(c, weather[c.key]);
    svg.setAttribute("aria-valuenow", weather[c.key]);
    svg.setAttribute("aria-valuetext", formatValue(c, weather[c.key]));
  }

  function updateOutput() {
    const input = scenarioInput(date, weather), prediction = predictCount(count, input);
    const mistPercent = mistProbability(mist, scenarioHours(input));
    updateForecastVisuals(prediction.expected_catch, mistPercent);
    document.querySelector("#primary-date").textContent = dateLabel.format(day(date));
    document.querySelector("#primary-catch").textContent = whole.format(prediction.expected_catch);
    document.querySelector("#catch-range").textContent = `Likely range ${whole.format(prediction.catch_low)}–${whole.format(prediction.catch_high)} birds`;
    const reference = count.historical_reference.filter(r => Math.abs(r.season_day - input.season_day) <= 7);
    document.querySelector("#catch-context").textContent = `Higher than ${whole.format(100 * reference.filter(r => r.total_birds_ringed <= prediction.expected_catch).length / reference.length)}% of historical catches around this date.`;
    document.querySelector("#mist-value").textContent = whole.format(mistPercent);
    document.querySelector(".mist-meter").style.setProperty("--mist-percent", `${mistPercent}%`);
    const url = new URL(window.location.href);
    url.searchParams.set("date", date);
    Object.entries(weather).forEach(([key, value]) => url.searchParams.set(key, String(value)));
    window.history.replaceState(null, "", url);
  }

  function updateWeather() { controls.forEach(drawEffect); updateOutput(); }
  function updateDate() {
    const season = calendar(date).season;
    document.querySelector("#scenario-season").value = season;
    document.querySelector("#scenario-date").innerHTML = Array.from({ length: 85 }, (_, i) => {
      const value = iso(new Date(day(`${season}-10-20`).getTime() + i * dayMs));
      return `<option value="${value}">${ringingDateLabel.format(day(value))}</option>`;
    }).join("");
    document.querySelector("#scenario-date").value = date;
    document.querySelector("#date-previous").disabled = calendar(date).season_day === 1;
    document.querySelector("#date-next").disabled = calendar(date).season_day === 85;
    drawCalendar(); updateOutput();
  }
  controls.forEach(c => {
    const svg = document.querySelector(`#plot-${c.key}`);
    const drag = event => {
      const bounds = svg.getBoundingClientRect();
      weather[c.key] = axisValue(c, Math.max(0, Math.min(1, ((event.clientX - bounds.left) * 380 / bounds.width - 44) / 322)));
      updateWeather();
    };
    svg.addEventListener("keydown", event => {
      if (!["ArrowLeft", "ArrowRight", "Home", "End"].includes(event.key)) return;
      event.preventDefault();
      weather[c.key] = event.key === "Home" ? c.min : event.key === "End" ? c.max
        : axisValue(c, Math.max(0, Math.min(1, axisPosition(c, weather[c.key]) + (event.key === "ArrowRight" ? 0.01 : -0.01))));
      updateWeather();
    });
    svg.addEventListener("pointerdown", event => { event.preventDefault(); svg.setPointerCapture(event.pointerId); drag(event); });
    svg.addEventListener("pointermove", event => { if (svg.hasPointerCapture(event.pointerId)) drag(event); });
    svg.addEventListener("pointerup", event => svg.releasePointerCapture(event.pointerId));
  });
  document.querySelector("#scenario-date").addEventListener("change", event => {
    date = event.target.value; updateDate();
  });
  document.querySelector("#scenario-season").addEventListener("change", event => {
    date = iso(new Date(day(`${event.target.value}-10-20`).getTime() + (calendar(date).season_day - 1) * dayMs));
    updateDate();
  });
  for (const [id, offset] of [["date-previous", -1], ["date-next", 1]]) {
    document.querySelector(`#${id}`).addEventListener("click", () => {
      date = iso(new Date(day(date).getTime() + offset * dayMs)); updateDate();
    });
  }
  document.querySelector("#calendar-expand").addEventListener("click", event => {
    expanded = !expanded; event.currentTarget.setAttribute("aria-expanded", expanded);
    event.currentTarget.querySelector("span").textContent = expanded ? "Collapse years" : "Show other years"; drawCalendar();
  });
  document.querySelector("#weather-reset").addEventListener("click", () => { weather = { ...defaults }; updateWeather(); });
  document.querySelector("#scenario-copy").addEventListener("click", async event => {
    await navigator.clipboard.writeText(window.location.href);
    event.target.textContent = "Scenario link copied";
  });
  updateDate(); updateWeather();
}
