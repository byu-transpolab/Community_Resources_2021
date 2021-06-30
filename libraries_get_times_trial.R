library(otpr)
library(tidyverse)
library(sf)
library(otpr)
library(opentripplanner)






#try <- otp_get_times(otpcon = otpcon, fromPlace = c(40.24106, -111.6577), toPlace = c(40.23453, -111.6349), mode = "TRANSIT", detail = TRUE, includeLegs = TRUE)


libraries <- st_read("data/libraries.geojson") %>%
  st_transform(4326) %>%
  rename(id = ID)

get_bglatlong <- function(){
  bgcentroid <- read_csv("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG49.txt")
  
  
  st_as_sf(bgcentroid, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
    mutate(id = str_c(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)) %>%
    filter(COUNTYFP == "049") %>%
    select(id, POPULATION)
}


tib <- libraries %>%
  st_centroid() %>% 
  st_transform(4326) %>%
  mutate(
    LATITUDE  = st_coordinates(.)[, 2],
    LONGITUDE = st_coordinates(.)[, 1],
  ) %>%
  transmute(id = as.character(id), LONGITUDE, LATITUDE)   %>%
  st_set_geometry(NULL)

tib

total <- nrow(tib)

path_otp <- otp_dl_jar(file.path("OTP"), cache = TRUE)
log2 <- otp_setup(otp = path_otp, dir = "otp")
otpcon <- otp_connect()
on.exit(otp_stop(warn = FALSE))

for (i in 1:total) {
  response <- otp_get_times(otpcon, fromPlace = c(tib[i,]$LATITUDE, tib[i, ]$LONGITUDE), toPlace = c(40.24106, -111.6577), mode = "TRANSIT",detail = TRUE)
  # If response is OK update dataframe
  if (response$errorId == "OK") {
    tib[i, "status"]<- response$errorId
    tib[i, "duration"]<- response$itineraries$duration
    tib[i, "walktime"]<- response$itineraries$walkTime
    tib[i, "transitTime"]<- response$itineraries$transitTime
    tib[i, "waitingtime"]<- response$itineraries$waitingTime
    tib[i, "transfers"]<- response$itineraries$transfers
  }else {
    # record error
    tib[i, "status"]<- response$errorId
  }
}
