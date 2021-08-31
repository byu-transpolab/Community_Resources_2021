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
      st_cast(to = "POINT")%>%
      slice_head(n=2)
  )
  
  park_points 
}

get_park_attributes <- function(parks){
  park_attributes <- parks%>%
    select(id, playground)
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
    st_transform(crs)%>%
    rename(id = SITE_NAME)%>%
    slice_head(n=3)
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
  #expanded <- expand_grid(fromid = ll$id, toid = bg$id) %>%
    #left_join(ll %>% rename(fromid = id, fromlng = LONGITUDE, fromlat = LATITUDE), 
    #          by = c("fromid")) %>%
    #left_join(bg %>% rename(toid = id, tolng = LONGITUDE, tolat = LATITUDE), 
    #          by = c("toid"))
  
  # Get distance between each ll and each bg
  modes <- c("CAR","BUS")
  total_lu <- nrow(ll)
  total_bg <- nrow(bg)
  
  origin <- lapply(1:total_lu, function(i){
    ll_latlong <- c(ll[i,]$LATITUDE, ll[i, ]$LONGITUDE)
  
    
    destinations <- lapply(1:total_bg, function(j){
      bg_latlong <- c(bg[j,]$LATITUDE, bg[j, ]$LONGITUDE)
      
      
      times <- lapply(modes, function(mode){
        
        o <- otp_get_times(
          otpcon, 
          fromPlace = ll_latlong, toPlace = bg_latlong,
          mode = mode, detail = TRUE
        )
        
        o$errorId <- as.character(o$errorId)
        
        o
      })
      
      names(times) <- modes
      
      times %>% bind_rows(.id = "mode")
    })

    
    names(destinations) <- bg$id
    
    destinations %>% bind_rows(.id = "destination")
    
  })


  
  names(origin) <- ll$id
  
 parktimes <- 
    origin %>% 
    bind_rows(.id = "origin") %>%
    select(origin, destination, mode, itineraries) %>%
    mutate(duration = itineraries$duration) %>%
    select(origin, destination, mode, duration) %>%
    group_by(origin, destination, mode)%>%
    
    arrange(duration, .by_group = TRUE) %>%
      slice(1)

  
}
  
  #routes <- lapply(c("TRANSIT"), function(mode){
    #total <- nrow(ll)
    #totalbg <- nrow(bg)
    #times <- data.frame("a", "b", "c", "d", "e", "f", "g", "h")
    #names(times) <- c("FromPlace", "ToPlace", "status", "duration", "walktime", "transitTime", "waitingtime", "transfers")
    #k<- 1
    
    #for (i in 1:total) {
      #for (j in 1:totalbg) {
        #response <- otp_get_times(otpcon, fromPlace = c(ll[i,]$LATITUDE, ll[i, ]$LONGITUDE), toPlace = c(bg[j,]$LATITUDE, bg[j,]$LONGITUDE), mode = "TRANSIT", detail = TRUE)
        # If response is OK update dataframe
        #if (response$errorId == "OK") {
          #times[k, "FromPlace"]<- ll[i,]$id
          #times[k, "ToPlace"]<- bg[j,]$id
          #times[k, "status"]<- response$errorId
          #times[k, "duration"]<- response$itineraries$duration
          #times[k, "walktime"]<- response$itineraries$walkTime
          #times[k, "transitTime"]<- response$itineraries$transitTime
          #times[k, "waitingtime"]<- response$itineraries$waitingTime
          #times[k, "transfers"]<- response$itineraries$transfers
          #k<-k+1
        #}else {
          # record error
          #times[, "status"]<- response$errorId
        #}
      #}
    #}
    #response
  #})
#}


read_sl_data <- function(file){
  read.csv(file)
  
  libraries
} 

