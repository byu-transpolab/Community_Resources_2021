#' Get block groups
#' 
#' @return spatial points sf object from Census Bureau population weighted centroids
get_bglatlong <- function(){
  bgcentroid <- read_csv("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG49.txt")
    
  
  st_as_sf(bgcentroid, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
    mutate(id = str_c(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)) %>%
    filter(COUNTYFP == "049") %>%
    select(id, POPULATION)
}

#' Get GTFS File for OpenTripPlanner
#' 
#' @param file to data file
#' @return Stores a GTFS file in the appropriate location
#' 
get_gtfs <- function(path){
  if(!file.exists(path)){
    # originally from UTA: June 4 2021
    download.file("https://gtfsfeed.rideuta.com/gtfs.zip",
                  destfile = path)
  } else {
    message(path, " already available")
  }
  return(path) # to use file target, need to return path to data. 
}


#' Get OSM PBF file for OpenTripPlanner
#' 
#' @param file to data file
#' @return Stores and unzips a PBF file in the data folder
#' 
get_osmbpf <- function(path){
  if(!file.exists(path)){
    stop(path, " not available")
  } else {
    message(path, " already available")
  }
  
  
  return(path) # to use file target, need to return path to data. 
}


#' Get parks polygons data
#' 
#' @param file A geojson file with parks data
#' @param crs Projected CRS to use for data; 
#' @return An sf object with parks as polygons with attributes
#' 
#' @details This dataset is small enough that we can just keep thd ata directly in git
#' 
get_parks <- function(file, crs){
  st_read(file) %>%
    st_transform(crs) 
}

#' Get points along park polygons
#' 
#' @param park_polygons An sf data frame with polygons
#' @param density Rate at which to sample points along the boundary.  density
#'   should be in units of the  projection.
#'   
#' @return A sf dataframe with points along the boundary
#' 
#' @details The parks are big enough we want to get distances to each point along
#' the boundary, rather than just a centroid.
make_park_points <- function(park_polygons, density, crs){
  
  # turn polygon boundaries into linestrings
  suppressWarnings(
    park_boundaries <- park_polygons %>%
      ungroup() %>%
      select(id) %>%
      # simplify boundaries
      st_simplify(dTolerance = 100, preserveTopology = TRUE) %>%
      st_cast("MULTIPOLYGON") %>% 
      st_cast("POLYGON") %>% 
      st_cast("LINESTRING", group_or_split = TRUE)
  )
  
  # sample points along lines
  point_samples <- park_boundaries %>%
    st_line_sample(density = 1/500)
  
  # make a dataset of points
  suppressWarnings(
    park_points <- st_sf(id = park_boundaries$id, geometry = point_samples) %>%
      st_as_sf() %>%
      st_cast(to = "POINT")
  )
  
  park_points 
}

#' Get Libraries Data
#' 
#' @param file Path to libraries geojson file
#' @param crs Projected CRS to use for data; 
#' @return sf data frame with 
#' 
get_libraries <- function(file, crs){
  st_read(file) %>%
    st_transform(crs) %>%
    rename(id = ID)
}

#' Get Groceries Data
#' 
#' @param file Path to libraries geojson file
#' @param crs Projected CRS to use for data; 
#' @return sf data frame with groceries data
#' 
get_groceries <- function(file, crs){
  st_read(file) %>%
    st_transform(crs)
}

make_groceries_points <- function(grocery_polygons){
  grocery_points <- gCentroid(polygons, byid = TRUE)
  
  suppressWarnings(
    grocery_points <- st_sf(id = grocery_polygons$id, geometry = grocery_points) %>%
      st_as_sf() %>%
      st_cast(to = "POINT")
  )
  
  grocery_points
}

#' Function to get lat / long from sf data as matrix
#' 
#' @param sfc A simple features collection
#' @return A data frame with three columns, id, LATITUDE and LONGITUDE
#' 
#' @details If sfc is a polygon, will first calculate the centroid.
#' 
get_latlong <- function(sfc){
  
  tib <- sfc %>%
    st_centroid() %>% 
    st_transform(4326) %>%
    mutate(
      LATITUDE  = st_coordinates(.)[, 2],
      LONGITUDE = st_coordinates(.)[, 1],
    ) %>%
    transmute(id = as.character(id), LONGITUDE, LATITUDE)   %>%
    st_set_geometry(NULL)
  
  tib
}

make_graph <- function(graph_dir){
  path_otp <- otp_dl_jar(cache = TRUE)
  otp_build_graph(otp = path_otp, dir = "otp")
  return(file.path(graph_dir, "Graph.obj"))
}




#' Calculate multimodal travel times between bgcentroids and destinations
#' 
#' @param landuse Destination features
#' @param bgcentroid Population-weighted blockgroup centroid
#' @param osmpbf path to OSM pbf file
#' 
calculate_times <- function(landuse, bgcentroid, graph){
  
  # start connection to OTP
  path_otp <- otp_dl_jar(file.path("OTP"),cache = TRUE)
  log2 <- otp_setup(otp = path_otp, dir = "otp")
  otpcon <- otp_connect()
  on.exit(otp_stop(warn = FALSE))
  
  
  # get lat / long for the landuse and the centroids
  ll <- get_latlong(landuse)
  bg <- get_latlong(bgcentroid)
  
  # make a complete table with combination of to and froms
  expanded <- expand_grid(fromid = ll$id, toid = bg$id) %>%
    left_join(ll %>% rename(fromid = id, fromlng = LONGITUDE, fromlat = LATITUDE), 
              by = c("fromid")) %>%
    left_join(bg %>% rename(toid = id, tolng = LONGITUDE, tolat = LATITUDE), 
              by = c("toid"))
  
  # Get distance between each ll and each bg
  routes <- lapply(c("CAR", "WALK","TRANSIT"), function(mode){
    total <- nrow(tib)
    totalbg <- nrow(big)
    routes <- data.frame("a", "b", "c", "d", "e", "f", "g", "h")
    names(routes) <- c("FromPlace", "ToPlace", "status", "duration", "walktime", "transitTime", "waitingtime", "transfers")
    k<- 1
    
    for (i in 1:total) {
      for (j in 1:totalbg) {
        response <- otp_get_times(otpcon, fromPlace = c(tib[i,]$LATITUDE, tib[i, ]$LONGITUDE), toPlace = c(big[j,]$LATITUDE, big[j,]$LONGITUDE), mode = mode, detail = TRUE)
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
  }
}


  

