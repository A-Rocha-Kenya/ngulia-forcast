// Shared by the sandbox and scheduled forecast. Model fitting stays in R/Python.
export function effect(model, variable, value, field = "eta") {
  const curve = model.effects[variable];
  const position = (value - curve.min) / (curve.max - curve.min) * (curve[field].length - 1);
  const index = Math.max(0, Math.min(curve[field].length - 2, Math.floor(position)));
  return curve[field][index] + (position - index) * (curve[field][index + 1] - curve[field][index]);
}

export function calendar(date) {
  const day = new Date(`${date}T12:00:00Z`);
  const year = day.getUTCFullYear() - (day.getUTCMonth() < 5 ? 1 : 0);
  const age = ((day - new Date("2000-01-06T18:14:00Z")) / 86400000 % 29.530588853 + 29.530588853) % 29.530588853;
  const signed = age <= 29.530588853 / 2 ? age : age - 29.530588853;
  return { season: year, season_day: Math.round((day - new Date(`${year}-10-20T12:00:00Z`)) / 86400000) + 1,
    moon_days_from_new_moon: signed, moon_distance_from_new_moon: Math.round(Math.abs(signed)) };
}

export function countMean(model, input) {
  return Math.exp(model.reference_eta + Object.keys(model.effects).reduce((sum, key) => sum + effect(model, key, input[key]), 0));
}

function logGamma(z) {
  const c = [676.5203681218851, -1259.1392167224028, 771.3234287776531, -176.6150291621406, 12.507343278686905, -0.13857109526572012, 9.984369578019572e-6, 1.5056327351493116e-7];
  if (z < 0.5) return Math.log(Math.PI) - Math.log(Math.sin(Math.PI * z)) - logGamma(1 - z);
  z -= 1;
  let x = 0.9999999999998099;
  c.forEach((value, i) => { x += value / (z + i + 1); });
  const t = z + 7.5;
  return 0.9189385332046727 + (z + 0.5) * Math.log(t) - t + Math.log(x);
}

function betaFraction(a, b, x) {
  let c = 1, d = 1 - (a + b) * x / (a + 1);
  d = 1 / (Math.abs(d) < 1e-30 ? 1e-30 : d);
  let h = d;
  for (let m = 1; m <= 200; m += 1) {
    for (const aa of [m * (b - m) * x / ((a + 2 * m - 1) * (a + 2 * m)),
      -(a + m) * (a + b + m) * x / ((a + 2 * m) * (a + 2 * m + 1))]) {
      d = 1 + aa * d; c = 1 + aa / c;
      d = 1 / (Math.abs(d) < 1e-30 ? 1e-30 : d);
      if (Math.abs(c) < 1e-30) c = 1e-30;
      const delta = d * c;
      h *= delta;
      if (aa < 0 && Math.abs(delta - 1) < 3e-12) return h;
    }
  }
  return h;
}

function betaCdf(x, a, b) {
  const scale = Math.exp(logGamma(a + b) - logGamma(a) - logGamma(b) + a * Math.log(x) + b * Math.log1p(-x));
  return x < (a + 1) / (a + b + 2) ? scale * betaFraction(a, b, x) / a
    : 1 - scale * betaFraction(b, a, 1 - x) / b;
}

export function countQuantile(probability, mean, theta) {
  const p = theta / (theta + mean);
  let low = -1, high = Math.max(1, Math.ceil(mean));
  while (betaCdf(p, theta, high + 1) < probability) high *= 2;
  while (high - low > 1) {
    const middle = Math.floor((high + low) / 2);
    if (betaCdf(p, theta, middle + 1) >= probability) high = middle;
    else low = middle;
  }
  return high;
}

export function predictCount(model, input) {
  const mean = countMean(model, input);
  return { expected_catch: mean, catch_low: countQuantile(0.1, mean, model.theta),
    catch_high: countQuantile(0.9, mean, model.theta) };
}

export function mistProbability(model, hours) {
  return 100 * model.members.reduce((sum, member) => {
    let values = hours.map(row => row.map((value, i) => (value - member.centre[0][0][i]) / member.spread[0][0][i]));
    for (const layer of ["extract.0", "extract.2"]) {
      const weights = member.state[`${layer}.weight`];
      const bias = member.state[`${layer}.bias`];
      values = values.map((row, time) => weights.map((channels, output) => Math.max(0,
        channels.reduce((total, kernel, channel) => total + kernel.reduce((s, w, k) =>
          s + w * (values[time + k - Math.floor(kernel.length / 2)]?.[channel] ?? 0), 0), bias[output]))));
    }
    const flat = values[0].flatMap((_, channel) => values.map(row => row[channel]));
    const eta = member.state["output.1.weight"][0].reduce((sum, weight, i) => sum + weight * flat[i], member.state["output.1.bias"][0]);
    return sum + 1 / (1 + Math.exp(-eta));
  }, 0) / model.members.length;
}

export function scenarioInput(date, weather) {
  const angle = weather.direction * Math.PI / 180;
  return { ...calendar(date), rain_log: Math.log1p(weather.rain), wind_speed_10m_mean_ms: weather.wind,
    temperature_2m_mean_c: weather.temperature, surface_pressure_mean_hpa: weather.pressure,
    total_cloud_cover_mean: weather.cloud, relative_humidity_mean_pct: weather.humidity,
    wind_u_10m_mean_ms: -weather.wind * Math.sin(angle), wind_v_10m_mean_ms: -weather.wind * Math.cos(angle) };
}

export function scenarioHours(input) {
  const temperature = input.temperature_2m_mean_c;
  const gamma = Math.log(input.relative_humidity_mean_pct / 100) + 17.67 * temperature / (temperature + 243.5);
  const depression = Math.max(0, temperature - 243.5 * gamma / (17.67 - gamma));
  return Array.from({ length: 12 }, () => [input.total_cloud_cover_mean, input.relative_humidity_mean_pct,
    input.wind_u_10m_mean_ms, input.wind_v_10m_mean_ms, temperature, depression]);
}
