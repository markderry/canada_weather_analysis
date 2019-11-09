library(weathercan)
library(plyr)
library(data.table)


stations <- data.table(weathercan::stations)

start_end <- stations[!is.na(start) & !is.na(end), list(min_start = min(start), max_end = max(end))]
long_term_stations <- stations[start<start_end$min_start+50 & end > start_end$max_end-10 & interval == "day", ]

long_term_stations[, list(station_id, start, end)]

station_data <- ddply(.data = long_term_stations[, list(station_id)], .variables = .(station_id, interval = "day"), .fun = weather_dl)
station_data <- data.table(station_data)

station_data[station_name==stations[1]]
station_data_calgary <- data.table(weather_dl(station_ids = "27211", start = "1920-01-01", interval = "day"))




# plots
library(plotly)
library(ggplot2)

stations <- station_data[, unique(station_name)]

plot_ly(station_data, x = ~decade, y = ~temp, color = ~year_in_decade, type = "box") %>%
  layout(boxmode = "group")
plot_ly(station_data, x = ~decade, y = ~temp, type = "box") %>%
  layout(boxmode = "group")

station_data[station_name==stations[1]]

plot_ly(station_data[station_name==stations[1]], x = ~decade, y = ~temp, type = "box", name = stations[1])
plot_ly(station_data[station_name==stations[2]], x = ~decade, y = ~temp, type = "box", name = stations[2])


ggplot(data = station_data[station_name == stations[5],], aes(x = as.numeric(year), y = temp, colour = station_name)) + 
  # geom_line() + 
  geom_smooth() +
  facet_wrap(~ month)

ggplot(data = station_data, aes(x = as.numeric(year), y = temp, colour = station_name)) + 
  # geom_line() + 
  geom_smooth() +
  facet_wrap(~ station_name)

ggplot(data = station_data_calgary, aes(x = as.numeric(year), y = max_temp, colour = station_name)) + 
  # geom_line() + 
  geom_smooth() +
  facet_wrap(~ month)




stations[station_name %like% "CALGARY"]
stations[prov == "AB"]
stations[end > 2016 & start <1930, station_name]

stations[end > 2016 & start < 1930 & interval == "day", station_name]

# station_data <- weather_dl(station_ids = stations[end > 2016 & start < 1930 & interval == "day", station_id][1], start = "1920-01-01", end = "2017-01-01", interval = "day")

station_data_calgary <- data.table(weather_dl(station_ids = "27211", start = "1920-01-01", interval = "day"))
station_data_calgary[, decade := (floor((as.numeric(year))/10))*10]
station_data_calgary[, year2 := as.character(as.numeric(year)-decade)]
