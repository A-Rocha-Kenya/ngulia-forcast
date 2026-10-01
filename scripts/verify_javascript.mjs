import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { predictCount, mistProbability, calendar } from "../site/prediction.js";

const read = path => JSON.parse(readFileSync(new URL(`../${path}`, import.meta.url)));
const model = read("site/models/count.json");
const reference = read("count/intermediate-data/javascript_reference.json");
const predictions = reference.input.map(row => predictCount(model, row));
const relativeError = Math.max(...predictions.map((p, i) => Math.abs(p.expected_catch / reference.expected[i] - 1)));
const rangeError = Math.max(...predictions.flatMap((p, i) => [Math.abs(p.catch_low - reference.low[i]), Math.abs(p.catch_high - reference.high[i])]));
assert(relativeError < 0.0005, `Count mean relative error ${relativeError}`);
assert(rangeError <= 1, `Count range discrepancy ${rangeError} birds`);
assert.equal(calendar("2026-11-12").season_day, 24);
assert.equal(calendar("2027-01-12").season_day, 85);
const mist = read("site/models/mist.json");
const mistReference = read(process.argv.includes("--full") ? "mist/intermediate-data/javascript_reference.json" : "mist/model/inference_reference.json");
const mistError = Math.max(...mistReference.hours.map((hours, i) => Math.abs(mistProbability(mist, hours) - mistReference.probability[i])));
assert(mistError < 1e-8, `Mist probability discrepancy ${mistError}`);
console.log(JSON.stringify({ count_cases: predictions.length, maximum_relative_count_error: relativeError,
  maximum_count_range_error_birds: rangeError, mist_cases: mistReference.hours.length,
  maximum_mist_error_percentage_points: mistError }, null, 2));
