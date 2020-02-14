library(jsonlite)
library(magrittr)
library(foreach)
library(lubridate)
library(dplyr)

history <- fromJSON("watch-history.json") %>% select(-subtitles, -header, -description, -products) %>%
  mutate(time = gsub("[TZ]", "", time)) %>%
  mutate(time = as.POSIXct(
    strptime(time, "%Y-%m-%d%H:%M:%S")))

history %<>% filter(time > as.POSIXct("2018-12-31 23:59:59"))
history %<>% filter(time < as.POSIXct("2020-01-01 00:00:00"))
history <- history[order(as.POSIXct(history$time), decreasing = FALSE),]

sessions <- data.frame(start=as.POSIXct(character()),
                       end=as.POSIXct(character()),
                       duration=as.numeric())


for(i in 1:length(history$time)){
  return <- data.frame(start=history$time[i], 
                       end=history$time[i+1], 
                       duration=as.numeric(difftime(history$time[i+1], history$time[i]), units="secs"))
  sessions <- rbind(sessions, return)
}

sessions %<>% filter(as.difftime(duration, units = "secs") < as.difftime(40, units = "mins"))
sum(sessions$duration)
mean.difftime(sessions$duration)

bitrate_bytes_hour <- 1650000000
energy_term1 <- 1.805
energy_term2 <- 1073741824

sessions %<>% mutate(energy = bitrate_bytes_hour * (duration / 3600) * (energy_term1 / energy_term2),
         cO2_g = energy * 475,
         cO2_kg = cO2_g / 1000,
         month = as.ordered(month(start)))

summary(sessions$month)
summary(sessions$cO2_g)           
summary(sessions$duration)

aggregate(sessions$cO2_kg, by=list(Category=sessions$month), FUN=sum)
sum(sessions$cO2_kg)
sum(sessions$duration) / 3600
