import { readFileSync, writeFileSync } from "node:fs";
import { calendar, predictCount, mistProbability } from "../site/prediction.js";

const [mode, inputPath, outputPath] = process.argv.slice(2);
const model = mode === "calendar" ? null : JSON.parse(readFileSync(new URL(`../site/models/${mode}.json`, import.meta.url)));
const input = JSON.parse(readFileSync(inputPath));
const result = mode === "calendar" ? input.map(row => calendar(row.date))
  : mode === "count" ? input.map(row => predictCount(model, row))
  : input.map(row => ({ weather_valid_date: row.weather_valid_date, mist_probability_pct: mistProbability(model, row.hours) }));
writeFileSync(outputPath, JSON.stringify(result));
