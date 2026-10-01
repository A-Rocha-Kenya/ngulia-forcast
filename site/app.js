import { loadSandbox } from "./sandbox.js?v=2";
import { updateForecastVisuals } from "./card-animation.js";

const whole = new Intl.NumberFormat("en", { maximumFractionDigits: 0 });
const oneDecimal = new Intl.NumberFormat("en", { maximumFractionDigits: 1 });
const weekday = new Intl.DateTimeFormat("en", { weekday: "short", timeZone: "UTC" });
const dayMonth = new Intl.DateTimeFormat("en", { day: "numeric", month: "short", timeZone: "UTC" });
const longDate = new Intl.DateTimeFormat("en", { weekday: "long", day: "numeric", month: "long", timeZone: "UTC" });
const chartDate = new Intl.DateTimeFormat("en", { day: "numeric", month: "short", year: "numeric", timeZone: "UTC" });
const monthName = new Intl.DateTimeFormat("en", { month: "short", timeZone: "UTC" });
const exploreMode = /\/explore\/(?:index\.html)?$/.test(window.location.pathname) || new URLSearchParams(window.location.search).get("demo") === "1";
document.querySelector(exploreMode ? "#nav-explore" : "#nav-live").setAttribute("aria-current", "page");
document.title = `${exploreMode ? "Explore forecast" : "Live forecast"} | Ngulia`;

const parseDate = value => new Date(`${value}T12:00:00Z`);

function moonPhase(signedDays) {
  const distance = Math.abs(signedDays);
  if (distance <= 1.5) return { icon: "🌑", label: "New moon" };
  if (distance >= 13.5) return { icon: "🌕", label: "Full moon" };
  if (signedDays > 9.5) return { icon: "🌔", label: "Waxing gibbous" };
  if (signedDays > 5.5) return { icon: "🌓", label: "First quarter" };
  if (signedDays > 0) return { icon: "🌒", label: "Waxing crescent" };
  if (signedDays < -9.5) return { icon: "🌖", label: "Waning gibbous" };
  if (signedDays < -5.5) return { icon: "🌗", label: "Last quarter" };
  return { icon: "🌘", label: "Waning crescent" };
}

function moonSymbol(signedDays) {
  const radius = 22;
  const centre = 25;
  const phaseAngle = signedDays >= 0
    ? Math.min(Math.PI, signedDays / 14.765 * Math.PI)
    : 2 * Math.PI - Math.min(Math.PI, Math.abs(signedDays) / 14.765 * Math.PI);
  const waxing = phaseAngle <= Math.PI;
  const distanceFromNew = waxing ? phaseAngle : 2 * Math.PI - phaseAngle;
  const terminatorFactor = Math.cos(distanceFromNew);
  const terminator = [];
  const limb = [];

  for (let i = 0; i <= 28; i += 1) {
    const localY = -radius + 2 * radius * i / 28;
    const halfWidth = Math.sqrt(Math.max(0, radius * radius - localY * localY));
    const terminatorX = waxing ? terminatorFactor * halfWidth : -terminatorFactor * halfWidth;
    terminator.push([centre + terminatorX, centre + localY]);
    limb.push([centre + (waxing ? halfWidth : -halfWidth), centre + localY]);
  }

  const points = [...terminator, ...limb.reverse()];
  const path = points.map(([x, y], index) => `${index ? "L" : "M"}${x.toFixed(2)},${y.toFixed(2)}`).join(" ") + " Z";
  return `<svg viewBox="0 0 50 50" role="img" aria-label="${moonPhase(signedDays).label}">
    <circle class="moon-disc" cx="25" cy="25" r="22"></circle>
    <path class="moon-light" d="${path}"></path>
    <circle class="moon-outline" cx="25" cy="25" r="22"></circle>
  </svg>`;
}

function weatherMarkup(day) {
  const items = [
    ["Rain 00–08", `${oneDecimal.format(day.total_precipitation_00_08_mm)} mm`],
    ["Low cloud", `${whole.format(day.low_cloud_cover_mean_pct)}%`],
    ["Humidity", `${whole.format(day.relative_humidity_mean_pct)}%`],
    ["Wind", `${oneDecimal.format(day.wind_speed_10m_mean_ms)} m/s`],
    ["Temperature", `${oneDecimal.format(day.temperature_2m_mean_c)}°C`],
    ["Pressure", `${whole.format(day.surface_pressure_mean_hpa)} hPa`]
  ];
  return items.map(([label, value]) => `<div class="weather-item"><span>${label}</span><strong>${value}</strong></div>`).join("");
}

function setLive(day) {
  updateForecastVisuals(day?.expected_catch ?? 0, day?.mist_probability_pct ?? 0);
  if (!day) {
    document.querySelector("#primary-date").textContent = "Outside the forecast window";
    document.querySelector("#primary-catch").textContent = "—";
    document.querySelector("#catch-range").textContent = "Live predictions begin 15 days before the season.";
    document.querySelector("#catch-context").textContent = "";
    document.querySelector("#moon-phase").textContent = "—";
    document.querySelector("#mist-value").textContent = "—";
    document.querySelector("#weather-grid").innerHTML = "";
    return;
  }

  const phase = moonPhase(day.moon_days_from_new_moon);
  document.querySelector("#primary-date").textContent = longDate.format(parseDate(day.date));
  document.querySelector("#primary-catch").textContent = whole.format(day.expected_catch);
  document.querySelector("#catch-range").textContent = `Likely range ${whole.format(day.catch_low)}–${whole.format(day.catch_high)} birds`;
  document.querySelector("#catch-context").textContent = `Higher than ${whole.format(day.historical_percentile)}% of historical catches around this date.`;
  document.querySelector("#moon-icon").innerHTML = moonSymbol(day.moon_days_from_new_moon);
  document.querySelector("#moon-phase").textContent = phase.label;
  document.querySelector("#mist-value").textContent = whole.format(day.mist_probability_pct);
  document.querySelector(".mist-meter").style.setProperty("--mist-percent", `${Math.max(0, Math.min(100, day.mist_probability_pct))}%`);
  document.querySelector("#weather-grid").innerHTML = weatherMarkup(day);
}

function renderNextDays(days) {
  const container = document.querySelector("#next-days");
  if (!days.length) {
    container.innerHTML = `<p class="empty-state">No live weather forecast is available for the next three ringing dates.</p>`;
    return;
  }

  container.innerHTML = days.map(day => {
    return `<article class="day-card">
      <div class="day-date"><strong>${weekday.format(parseDate(day.date))}</strong><span>${dayMonth.format(parseDate(day.date))}</span></div>
      <div class="day-catch"><strong>${whole.format(day.expected_catch)} birds</strong><span>${whole.format(day.catch_low)}–${whole.format(day.catch_high)} likely</span></div>
      <div class="day-signals">
        <span class="day-moon">${moonSymbol(day.moon_days_from_new_moon)}</span>
        <span class="day-mist"><strong>${whole.format(day.mist_probability_pct)}%</strong><span>mist</span></span>
      </div>
    </article>`;
  }).join("");
}

function renderSeasonChart(values, displayDate) {
  const container = document.querySelector("#season-chart");
  if (!values.length) {
    container.innerHTML = `<p class="empty-state">Season outlook unavailable.</p>`;
    return;
  }

  const width = 720;
  const height = 255;
  const margin = { top: 20, right: 10, bottom: 34, left: 38 };
  const plotWidth = width - margin.left - margin.right;
  const plotHeight = height - margin.top - margin.bottom;
  const liveValues = values.filter(d => d.expected_catch != null);
  const rawMax = Math.max(...values.map(d => d.baseline_expected_catch), ...liveValues.map(d => d.catch_high), 1);
  const roughStep = rawMax / 2;
  const magnitude = 10 ** Math.floor(Math.log10(roughStep));
  const normalizedStep = roughStep / magnitude;
  const tickStep = (normalizedStep <= 1 ? 1 : normalizedStep <= 2 ? 2 : normalizedStep <= 5 ? 5 : 10) * magnitude;
  const maxValue = Math.ceil(rawMax / tickStep) * tickStep;
  const x = index => margin.left + index * plotWidth / (values.length - 1);
  const y = value => margin.top + plotHeight - Math.min(value, maxValue) * plotHeight / maxValue;
  const path = (rows, field) => {
    const points = rows.map(d => [x(values.indexOf(d)), y(d[field])]);
    if (points.length < 2) return "";
    return points.reduce((commands, point, index) => {
      if (index === 0) return [`M${point[0].toFixed(1)},${point[1].toFixed(1)}`];
      const previous = points[index - 1];
      const before = points[index - 2] || previous;
      const after = points[index + 1] || point;
      const control1 = [previous[0] + (point[0] - before[0]) / 6, previous[1] + (point[1] - before[1]) / 6];
      const control2 = [point[0] - (after[0] - previous[0]) / 6, point[1] - (after[1] - previous[1]) / 6];
      commands.push(`C${control1[0].toFixed(1)},${control1[1].toFixed(1)} ${control2[0].toFixed(1)},${control2[1].toFixed(1)} ${point[0].toFixed(1)},${point[1].toFixed(1)}`);
      return commands;
    }, []).join(" ");
  };
  const baselinePath = path(values, "baseline_expected_catch");
  const livePath = liveValues.length > 1 ? path(liveValues, "expected_catch") : "";

  let rangeBand = "";
  if (liveValues.length > 1) {
    const upper = liveValues.map(d => `${x(values.indexOf(d)).toFixed(1)},${y(d.catch_high).toFixed(1)}`);
    const lower = [...liveValues].reverse().map(d => `${x(values.indexOf(d)).toFixed(1)},${y(d.catch_low).toFixed(1)}`);
    rangeBand = `<polygon class="range-band" points="${[...upper, ...lower].join(" ")}"></polygon>`;
  }

  const yTicks = Array.from({ length: Math.round(maxValue / tickStep) + 1 }, (_, index) => index * tickStep);
  const grid = yTicks.map(value => `<g><line class="grid" x1="${margin.left}" x2="${width - margin.right}" y1="${y(value)}" y2="${y(value)}"></line><text x="${margin.left - 7}" y="${y(value) + 4}" text-anchor="end">${whole.format(value)}</text></g>`).join("");
  const monthIndices = values.reduce((out, row, index) => {
    const month = parseDate(row.date).getUTCMonth();
    if (!out.some(item => item.month === month)) out.push({ month, index, label: monthName.format(parseDate(row.date)) });
    return out;
  }, []);
  const firstYear = parseDate(values[0].date).getUTCFullYear();
  const monthLabels = monthIndices.map((item, index) => {
    const year = parseDate(values[item.index].date).getUTCFullYear();
    const label = index === 0 || year !== parseDate(values[monthIndices[index - 1]?.index]?.date || values[0].date).getUTCFullYear()
      ? `${item.label} ${year}`
      : item.label;
    return `<text x="${x(item.index)}" y="${height - 8}" text-anchor="${index === monthIndices.length - 1 ? "end" : "start"}">${label}</text>`;
  }).join("");
  const todayIndex = values.findIndex(row => row.date === displayDate);
  const todayMarker = todayIndex >= 0 ? `
    <line class="today-line" x1="${x(todayIndex)}" x2="${x(todayIndex)}" y1="${margin.top}" y2="${margin.top + plotHeight}"></line>
    <text class="today-label" x="${x(todayIndex) + 5}" y="${margin.top + 11}">Today</text>` : "";

  container.innerHTML = `<svg viewBox="0 0 ${width} ${height}" role="img" aria-labelledby="season-chart-title season-chart-desc">
    <title id="season-chart-title">Predicted catch across the ringing season</title>
    <desc id="season-chart-desc">Typical-weather M2 count for the full season, with issued-weather expected catch and an 80 percent range where live weather is available.</desc>
    ${grid}
    ${rangeBand}
    <path class="baseline-path" d="${baselinePath}"></path>
    ${livePath ? `<path class="live-path" d="${livePath}"></path>` : ""}
    <line class="axis-line" x1="${margin.left}" x2="${width - margin.right}" y1="${margin.top + plotHeight}" y2="${margin.top + plotHeight}"></line>
    ${todayMarker}
    ${monthLabels}
    <text class="season-year" x="${width - margin.right}" y="12" text-anchor="end">${firstYear}–${String(firstYear + 1).slice(-2)} season</text>
    <g class="chart-hover" hidden>
      <line class="hover-guide" y1="${margin.top}" y2="${margin.top + plotHeight}"></line>
      <circle class="hover-marker baseline-marker" r="4"></circle>
      <circle class="hover-marker live-marker" r="4"></circle>
      <g class="hover-tooltip">
        <rect width="174" height="58" rx="7"></rect>
        <text class="hover-date" x="10" y="17"></text>
        <text class="hover-live" x="10" y="34"></text>
        <text class="hover-baseline" x="10" y="50"></text>
      </g>
    </g>
    <rect class="hover-overlay" x="${margin.left}" y="${margin.top}" width="${plotWidth}" height="${plotHeight}" tabindex="0" aria-label="Explore daily catch predictions"></rect>
  </svg>`;

  const svg = container.querySelector("svg");
  const overlay = svg.querySelector(".hover-overlay");
  const hover = svg.querySelector(".chart-hover");
  const updateHover = clientX => {
    const bounds = svg.getBoundingClientRect();
    const svgX = (clientX - bounds.left) * width / bounds.width;
    const index = Math.max(0, Math.min(values.length - 1, Math.round((svgX - margin.left) * (values.length - 1) / plotWidth)));
    const row = values[index];
    const pointX = x(index);
    const tooltipX = Math.min(width - margin.right - 174, Math.max(margin.left, pointX + 10));
    const liveAvailable = row.expected_catch != null;
    hover.removeAttribute("hidden");
    hover.querySelector(".hover-guide").setAttribute("x1", pointX);
    hover.querySelector(".hover-guide").setAttribute("x2", pointX);
    hover.querySelector(".baseline-marker").setAttribute("cx", pointX);
    hover.querySelector(".baseline-marker").setAttribute("cy", y(row.baseline_expected_catch));
    hover.querySelector(".live-marker").toggleAttribute("hidden", !liveAvailable);
    if (liveAvailable) {
      hover.querySelector(".live-marker").setAttribute("cx", pointX);
      hover.querySelector(".live-marker").setAttribute("cy", y(row.expected_catch));
    }
    hover.querySelector(".hover-tooltip").setAttribute("transform", `translate(${tooltipX} ${margin.top + 6})`);
    hover.querySelector(".hover-date").textContent = chartDate.format(parseDate(row.date));
    hover.querySelector(".hover-live").textContent = liveAvailable ? `Weather: ${whole.format(row.expected_catch)} birds` : "Weather: unavailable";
    hover.querySelector(".hover-baseline").textContent = `Typical weather: ${whole.format(row.baseline_expected_catch)} birds`;
    overlay.setAttribute("aria-label", `${chartDate.format(parseDate(row.date))}. ${liveAvailable ? `Weather forecast ${whole.format(row.expected_catch)} birds. ` : ""}Typical-weather forecast ${whole.format(row.baseline_expected_catch)} birds.`);
  };
  overlay.addEventListener("pointermove", event => updateHover(event.clientX));
  overlay.addEventListener("pointerdown", event => updateHover(event.clientX));
  overlay.addEventListener("pointerleave", () => { hover.setAttribute("hidden", ""); });
  overlay.addEventListener("focus", () => updateHover(overlay.getBoundingClientRect().left + overlay.getBoundingClientRect().width / 2));
  overlay.addEventListener("blur", () => { hover.setAttribute("hidden", ""); });
}

async function loadForecast() {
  try {
    if (exploreMode) { await loadSandbox(); return; }
    const response = await fetch(`data/forecast.json?v=${Date.now()}`, { cache: "no-store" });
    if (!response.ok) throw new Error(`Forecast request failed: ${response.status}`);
    const data = await response.json();
    const generated = new Date(data.generated_at);
    document.querySelector("#update-label").textContent = data.forecast.length
        ? `ECMWF live · updated ${generated.toLocaleString("en", { day: "numeric", month: "short", hour: "2-digit", minute: "2-digit" })}`
        : `Forecast starts ${dayMonth.format(parseDate(data.status.next_season_start))}`;
    setLive(data.forecast[0]);
    renderNextDays(data.forecast.slice(1, 6));
    const displayDate = new Date().toLocaleDateString("en-CA", { timeZone: data.source.timezone });
    renderSeasonChart(data.season_outlook || [], displayDate);
    const monthDay = displayDate.slice(5);
    if (monthDay > "01-12" && monthDay < "10-20") {
      const nextStart = `${displayDate.slice(0, 4)}-10-20`;
      document.querySelector("#season-modal-message").textContent = `The ringing season runs from 20 October to 12 January. The next season starts ${chartDate.format(parseDate(nextStart))}.`;
      document.querySelector("#season-modal").showModal();
    }
  } catch (error) {
    document.querySelector("#update-label").textContent = "Unavailable";
    document.querySelector("#primary-date").textContent = "Forecast unavailable";
    document.querySelector("#catch-range").textContent = "Please try again shortly.";
    console.error(error);
  }
}

loadForecast();
