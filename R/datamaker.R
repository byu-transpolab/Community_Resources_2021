library(sf)

#' Get block groups
#' 
#' @return spatial points sf object from Census Bureau population weighted centroids
get_bglatlong <- function(){
  bgcentroid <- read_csv("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG49.txt")
  
  st_as_sf(bgcentroid, coords = c("LONGITUDE", "LATITUDE"))
}


get_osmbpf <- function(){
  if(!file.exists(raw_file)){
    download.file("https://byu.box.com/shared/static/nkf9nh63oqp501ech59urzzi738sio5l.zip", raw_file)
  }
}

get_parks <- function(file){
  
}


get_libraries <- function(){
  
}

get_groceries <- function(){
  
}


calculate_times <- function(landuse, bgcentroids){
  
  #here's where your OTP stuff goes. Might want to make sure the data is there in another function
}