library(dplyr)
library(readr)
library(tibble)
library(jsonlite)
library(here)

# Check the actual ECMWF feed at Ngulia with yesterday included ------------
variables <- c("temperature_2m", "relative_humidity_2m", "dewpoint_2m",
  "precipitation", "surface_pressure", "cloud_cover", "wind_speed_10m",
  "wind_direction_10m", "cloud_base")
query <- c(latitude = -3.0142286, longitude = 38.2108675,
  hourly = paste(variables, collapse = ","), models = "ecmwf_ifs025",
  timezone = "Africa/Nairobi", forecast_days = 15, past_days = 1,
  wind_speed_unit = "ms")
url <- paste0("https://api.open-meteo.com/v1/ecmwf?",
  paste(names(query), vapply(query, URLencode, character(1), reserved = TRUE),
    sep = "=", collapse = "&"))
response <- fromJSON(url)
audit <- tibble(variable = variables,
  unit = vapply(variables, function(name) response$hourly_units[[name]], character(1)),
  available_hours = vapply(variables,
    function(name) sum(!is.na(response$hourly[[name]])), integer(1)),
  requested_hours = length(response$hourly$time),
  first_hour = first(response$hourly$time), last_hour = last(response$hourly$time),
  checked_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
stopifnot(all(audit$available_hours[audit$variable != "cloud_base"] > 0))
write_csv(audit, here("validation", "research", "count_weather_api_audit.csv"))
print(audit, width = Inf)
