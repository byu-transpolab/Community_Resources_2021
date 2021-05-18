library(sf)
library("rjson")

#' Get block groups
#' 
#' @return spatial points sf object from Census Bureau population weighted centroids
get_bglatlong <- function(){
  bgcentroid <- read_csv("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG49.txt")
  
  st_as_sf(bgcentroid, coords = c("LONGITUDE", "LATITUDE"))
}


get_osmbpf <- function(){
  download.file("https://byu.box.com/shared/static/nkf9nh63oqp501ech59urzzi738sio5l.zip", destfile = "/data")
}

get_parks <- function(file){
  parks <- fromJSON(file = "https://byu.box.com/s/s4wqbp34lczsmpirrhtxhifpp10whr2o")
  parks_df <- as.data.frame(parks)
  park_boundaries <- parks %>%
    ungroup() %>%
    select(id) %>%
    st_simplify(dTolerance = 100, preserveTopology = TRUE) %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON") %>%
    st_cast("LINESTRING", group_or_split = TRUE)
  point_samples <- park_boundaries %>%
    st_line_sample(density = 1/500)
  park_points <- st_sf(id = 
                         park_boundaries$id, geometry = point_samples) %>%
    st_as_sf() %>%
    st_cast(to = "POINT")

  write_points_file <- function(points, file){
    points %>%
      st_transform(4326) %>%
      mutate(
        LATITUDE = st_coordinates(.)[,2],
        LONGITUDE = st_coordinates(.)[,1]
      ) %>%
      st_set_geometry(NULL) %>%
      write_csv(file)
  }
  park_latlon <- park_points %>% write_points_file("points.csv")
}


get_libraries <- function(){
  libraries <- fromJSON(file = "https://byu.box.com/s/sn5rc75whklvik6ub83zxgc1esztoc28")
  libraries_latlon <- libraries %>% write_points_file("lib_points.csv")
  libraries_latlon_col <- libraries_latlon[,c(31,32)]
   }
}

get_groceries <- function(){
  
}


calculate_times <- function(landuse, bgcentroids){
  
  path_data <- file.path("C:/Users/Public", "OTP")
  dir.create(path_data)
  
  path_otp <- otp_dl_jar(path_data, cache = FALSE)
  
  otp_dl_demo(path_data)
  
  log1 <- otp_build_graph(otp = path_otp, dir =  path_data)
  
  log2 <- otp_setup(otp = path_otp, dir = path_data)
  
  otpcon <- otp_connect()
  
  amenities <- sf::st_read(landuse)
  bgpoints <- sf::st_read(bgcentroids)
  head(amenities)
  
  toPlace   = amenities[rep(seq(1, 10), times = nrow(amenities)),]
  fromPlace = amenities[rep(seq(1, 10), each  = nrow(amenities)),]
  
  routes <- otp_plan(otpcon = otpcon,
                     fromPlace = fromPlace,
                     toPlace = toPlace,
                     fromID = fromPlace$geo_code,
                     toID = toPlace$geo_code,
                     mode = c("WALK"),
                     get_geometry = FALSE)
  routes <- routes[,c("fromPlace","toPlace","duration")]
  # Use the tidyr package to go from long to wide format
  routes_matrix <- tidyr::pivot_wider(routes, 
                                      names_from = "toPlace", 
                                      values_from = "duration")
  #here's where your OTP stuff goes. Might want to make sure the data is there in another function
}