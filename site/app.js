const number = new Intl.NumberFormat("en", { maximumFractionDigits: 0 });
const oneDecimal = new Intl.NumberFormat("en", { maximumFractionDigits: 1 });
const shortDate = new Intl.DateTimeFormat("en", { weekday: "short", day: "numeric", month: "short", timeZone: "UTC" });
const longDate = new Intl.DateTimeFormat("en", { weekday: "long", day: "numeric", month: "long", timeZone: "UTC" });

const parseDate = value => new Date(`${value}T12:00:00Z`);
const moonLabel = distance => distance <= 1 ? "new moon" : `${number.format(distance)} d from new moon`;
const reliabilityLabel = value => `${value[0].toUpperCase()}${value.slice(1)} confidence`;

function setPrimary(day) {
  const panel = document.querySelector("#primary-panel");
  panel.classList.remove("loading");

  if (!day) {
    document.querySelector("#primary-date").textContent = "Outside the live forecast window";
    document.querySelector("#primary-score").textContent = "—";
    document.querySelector("#score-context").textContent = "Weather-adjusted scores appear when the ringing season enters the 15-day forecast range.";
    document.querySelector("#conditions").innerHTML = `
      <div class="condition"><span>Next update</span><strong>Twice daily</strong></div>
      <div class="condition"><span>Forecast window</span><strong>15 days</strong></div>
      <div class="condition"><span>Night period</span><strong>00–08 EAT</strong></div>`;
    return;
  }

  document.querySelector("#primary-date").textContent = longDate.format(parseDate(day.date));
  document.querySelector("#primary-score").textContent = number.format(day.opportunity_index);
  const direction = day.weather_adjustment_pct >= 0 ? "above" : "below";
  document.querySelector("#score-context").textContent = `${number.format(Math.abs(day.weather_adjustment_pct))}% ${direction} the date-and-moon baseline · ${reliabilityLabel(day.reliability)}.`;
  document.querySelector("#conditions").innerHTML = `
    <div class="condition"><span>Rain 00–08</span><strong>${oneDecimal.format(day.total_precipitation_00_08_mm)} mm</strong></div>
    <div class="condition"><span>Low cloud</span><strong>${number.format(day.low_cloud_cover_mean_pct)}%</strong></div>
    <div class="condition"><span>Humidity</span><strong>${number.format(day.relative_humidity_mean_pct)}%</strong></div>
    <div class="condition"><span>Wind</span><strong>${oneDecimal.format(day.wind_speed_10m_mean_ms)} m/s</strong></div>
    <div class="condition"><span>Temperature</span><strong>${oneDecimal.format(day.temperature_2m_mean_c)}°C</strong></div>
    <div class="condition"><span>Moon</span><strong>${moonLabel(day.moon_distance_from_new_moon)}</strong></div>`;
}

function renderForecast(days, nextSeasonStart) {
  const chart = document.querySelector("#forecast-chart");
  const empty = document.querySelector("#forecast-empty");

  if (!days.length) {
    chart.hidden = true;
    empty.hidden = false;
    empty.textContent = `The next ringing season begins ${longDate.format(parseDate(nextSeasonStart))}. Live weather scores will appear when it enters the ECMWF forecast window.`;
    return;
  }

  const maxScore = Math.max(160, ...days.flatMap(day => [day.opportunity_index, day.baseline_index]));
  chart.innerHTML = days.map(day => {
    const weatherHeight = Math.max(4, 100 * day.opportunity_index / maxScore);
    const baselineHeight = Math.max(4, 100 * day.baseline_index / maxScore);
    return `<article class="forecast-day" role="listitem" title="${reliabilityLabel(day.reliability)}">
      <div class="date">${shortDate.format(parseDate(day.date))}</div>
      <div class="bar-stage">
        <div class="bar baseline" style="height:${baselineHeight}%"></div>
        <div class="bar weather" style="height:${weatherHeight}%"></div>
      </div>
      <div class="day-score">${number.format(day.opportunity_index)}<small>${moonLabel(day.moon_distance_from_new_moon)}</small></div>
    </article>`;
  }).join("");
}

function renderTiming(days) {
  document.querySelector("#timing-strip").innerHTML = days.slice(0, 14).map(day => `
    <article class="timing-day" role="listitem">
      <span>${shortDate.format(parseDate(day.date))}</span>
      <strong>${number.format(day.baseline_index)}</strong>
      <span>baseline</span>
      <span class="moon">${moonLabel(day.moon_distance_from_new_moon)}</span>
    </article>`).join("");
}

async function loadForecast() {
  try {
    const response = await fetch(`data/forecast.json?v=${Date.now()}`, { cache: "no-store" });
    if (!response.ok) throw new Error(`Forecast request failed: ${response.status}`);
    const data = await response.json();
    const generated = new Date(data.generated_at);
    document.querySelector("#update-label").textContent = `Updated ${generated.toLocaleString("en", { dateStyle: "medium", timeStyle: "short" })}`;
    document.querySelector("#season-chip").textContent = data.forecast.length ? "Live season forecast" : `Season starts ${shortDate.format(parseDate(data.status.next_season_start))}`;
    setPrimary(data.forecast[0]);
    renderForecast(data.forecast, data.status.next_season_start);
    renderTiming(data.timing_outlook);
  } catch (error) {
    document.querySelector("#update-label").textContent = "Forecast unavailable";
    document.querySelector("#season-chip").textContent = "Update pending";
    document.querySelector("#primary-panel").classList.remove("loading");
    document.querySelector("#score-context").textContent = "The latest forecast could not be loaded. Please try again shortly.";
    document.querySelector("#forecast-empty").hidden = false;
    document.querySelector("#forecast-empty").textContent = error.message;
  }
}

loadForecast();

