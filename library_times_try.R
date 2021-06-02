library(tidyverse)
library(sf)
library(opentripplanner)            # Load Package
library(tidyr)
library(tmap)
library(RcppSimdJson)
library(rgdal)
library(proj4)

libraries <- st_read("C:/Users/stuck/Documents/Transpo Research/Libraries/Library_Points.geojson") %>%
    st_transform(4326)  %>%
      mutate(
        LATITUDE  = st_coordinates(.)[, 2],
        LONGITUDE = st_coordinates(.)[, 1],
      )

libraries <- st_cast(libraries, "POINT")

path_data <- file.path("C:/Users/Public", "OTP")
dir.create(path_data)

path_otp <- otp_dl_jar(path_data, cache = FALSE)

otp_dl_demo(path_data)

log1 <- otp_build_graph(otp = path_otp, dir =  path_data)

log2 <- otp_setup(otp = path_otp, dir = path_data)

otpcon <- otp_connect()

head(libraries)

toPlace   = libraries[rep(seq(1, 10), times = nrow(libraries)),]
fromPlace = libraries[rep(seq(1, 10), each  = nrow(libraries)),]

routes <- otp_plan(otpcon = otpcon,
                   fromPlace = fromPlace,
                   toPlace = toPlace,
                   fromID = fromPlace$NAME,
                   toID = toPlace$NAME,
                   mode = c("CAR"),
                   get_geometry = FALSE)
routes <- routes[,c("fromPlace","toPlace","duration")]
# Use the tidyr package to go from long to wide format
routes_matrix <- tidyr::pivot_wider(routes, 
                                    names_from = "toPlace", 
                                    values_from = "duration")

