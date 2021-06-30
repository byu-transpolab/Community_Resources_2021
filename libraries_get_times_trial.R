library(otpr)
library(tidyverse)
library(sf)
library(otpr)
library(opentripplanner)
library(progress)






#try <- otp_get_times(otpcon = otpcon, fromPlace = c(40.24106, -111.6577), toPlace = c(40.23453, -111.6349), mode = "TRANSIT", detail = TRUE, includeLegs = TRUE)


libraries <- st_read("data/libraries.geojson") %>%
  st_transform(4326) %>%
  rename(id = ID)

bgcentroid <- read_csv("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG49.txt")
  
  
bg <- st_as_sf(bgcentroid, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  mutate(id = str_c(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)) %>%
  filter(COUNTYFP == "049") %>%
  select(id, POPULATION)

big <- bg %>%
  st_centroid() %>% 
  st_transform(4326) %>%
  mutate(
    LATITUDE  = st_coordinates(.)[, 2],
    LONGITUDE = st_coordinates(.)[, 1],
  ) %>%
  transmute(id = as.character(id), LONGITUDE, LATITUDE)   %>%
  st_set_geometry(NULL)
 

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
totalbg <- nrow(big)
pb <- progress_bar$new(total = total, format = "(:spin)[:bar]:percent")#progress bar
routes <- data.frame("a", "b", "c", "d", "e", "f", "g", "h")
names(routes) <- c("FromPlace", "ToPlace", "status", "duration", "walktime", "transitTime", "waitingtime", "transfers")
k<- 1

path_otp <- otp_dl_jar(file.path("OTP"), cache = TRUE)
log2 <- otp_setup(otp = path_otp, dir = "otp")
otpcon <- otp_connect()
on.exit(otp_stop(warn = FALSE))

for (i in 1:total) {
  for (j in 1:totalbg) {
    response <- otp_get_times(otpcon, fromPlace = c(tib[i,]$LATITUDE, tib[i, ]$LONGITUDE), toPlace = c(big[j,]$LATITUDE, big[j,]$LONGITUDE), mode = "TRANSIT",detail = TRUE)
    # If response is OK update dataframe
    if (response$errorId == "OK") {
        routes[k, "FromPlace"]<- tib[i,]$id
        routes[k, "ToPlace"]<- big[j,]$id
        routes[k, "status"]<- response$errorId
        routes[k, "duration"]<- response$itineraries$duration
        routes[k, "walktime"]<- response$itineraries$walkTime
        routes[k, "transitTime"]<- response$itineraries$transitTime
        routes[k, "waitingtime"]<- response$itineraries$waitingTime
        routes[k, "transfers"]<- response$itineraries$transfers
        k<-k+1
    }else {
      # record error
      routes[, "status"]<- response$errorId
    }
  }
}
