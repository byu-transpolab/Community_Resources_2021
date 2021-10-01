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
    st_transform(crs)  %>%
    mutate(id = as.character(id))
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
      group_by(id)%>%
      ungroup()
  )
  
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
    transmute(id = Name) 
}

#' Get Groceries shape information
#' 
#' @param file Path to groceries geojson file
#' @param data Path to groceries survey data
#' @param crs Projected CRS to use for data; 
#' @return sf data frame with groceries data
#' 
get_groceries <- function(file, data, crs){
  # read shape information
  gj <- st_read(file) %>%
    st_transform(crs) %>%
    filter(st_is(., c("MULTIPOLYGON"))) %>%
    rename(id = SITE_NAME) %>%
    filter(!duplicated(id))
  
  # read survey data 
  gd <- read_spss("data/NEMS-S_UC2021_brief.sav") %>%
    transmute(
      id = STORE_ID,
      type = as_factor(STORE_T, levels = "labels"),
      type2 = STORE_T_3_TEXT,
      pharmacy = STORE_T2_3Rx_2 == 1,
      ethnic = STORE_T2_4ETH == 1,
      merch = STORE_T2_6GEN == 1,
      registers = REGISTERS,
      selfchecko = SELFCHECKOUT,
      total_registers = REGISTERS_TOT
    )
  
  inner_join(gj, gd, by = "id")
  
}


groceries_map <- function(groceries){
  
  pal <- colorFactor("Dark2", groceries$type)
  
  leaflet(groceries %>% st_centroid() %>% st_transform(4326)) %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addCircles(color = ~pal(type), radius = ~(total_registers* 10))

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
    suppressWarnings(st_centroid()) %>% # will always warn for constant geometry
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
#' @param graph path to OSM pbf file
#' 
#' @return A tibble with times between Block groups and resources by multiple modes
#' 
#' @details Parallelized, will use parallel::detectCores() - 1
#' 
calculate_times <- function(landuse, bgcentroid, graph, landuselimit = NULL, bglimit = NULL){
  
  # start connection to OTP
  path_otp <- otp_dl_jar(file.path("OTP"),cache = TRUE)
  log2 <- otp_setup(otp = path_otp, dir = "otp")
  otpcon <- otp_connect()
  on.exit(otp_stop(warn = FALSE))
  
  
  # get lat / long for the landuse and the centroids
  ll <- get_latlong(landuse)
  bg <- get_latlong(bgcentroid)
  
  # limit the number of cells for the time calculations (for debugging)
  if(!is.null(landuselimit)) ll <- ll %>% sample_n(landuselimit)
  if(!is.null(bglimit)) bg <- bg %>% sample_n(bglimit)
  
  # Get distance between each ll and each bg
  # loop through the land use points
  alltimes <- lapply(1:nrow(ll), function(i){
    ll_latlong <- c(ll[i,]$LATITUDE, ll[i, ]$LONGITUDE)
    
    message("Getting travel times for land use id", ll[i, ]$id)
    # loop through the block groups
    lapply(1:nrow(bg), function(j){
      bg_latlong <- c(bg[j,]$LATITUDE, bg[j, ]$LONGITUDE)
      
      # loop through the modes
      modes <- c("CAR","TRANSIT", "WALK")
      lapply(modes, function(mode){
        o <- otp_get_times(
          otpcon, 
          fromPlace = bg_latlong, toPlace = ll_latlong,

          mode = mode, 
          date = "10-05-2021", time = "08:00:00", detail = TRUE, includeLegs = TRUE
        )
        
        o$errorId <- as.character(o$errorId)
        
        o
      }) %>%
        set_names(modes) %>%
        bind_rows(.id = "mode")
      
    }) %>%
      set_names(bg$id) %>%
      bind_rows(.id = "blockgroup")

    
  }) %>%
    set_names(ll$id) %>%
    bind_rows(.id = "resource")
  
  # Do a little bit of cleanup
  alltimes %>% 
    bind_rows(.id = "origin") %>%
    select(blockgroup, resource, mode, itineraries) %>%
    transmute(resource, blockgroup, mode, 
              duration = itineraries$duration, 
              transfers = itineraries$transfers,
              walktime = itineraries$walkTime,
              waitingTime = itineraries$waitingTime, 
              transitTime = itineraries$transitTime) %>%
    # keep only the shortest itinerary by origin / destination / mode
    group_by(resource, blockgroup, mode) %>%
    arrange(duration, .by_group = TRUE) %>%
    slice(1)
  
}
  


#' Get American Community Survey data for the study.
#' 
#' @param state Which state to pull for
#' @param county Which county(ies) to pull
#' 
get_acsdata <- function(state = "UT", county = "Utah") {
  variables <- c(
    "population" = "B02001_001", # TOTAL: RACE
    "housing_units" = "B25001_001", # HOUSING UNITS
    "households" = "B19001_001", #HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)
    # Hispanic or Latino Origin by Race
    "white" = "B03002_003",
    "black" = "B03002_004",
    "asian" = "B03002_006",
    "hispanic" = "B03002_012",
    #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)
    "income" = "B19013_001",
    # FAMILY TYPE BY PRESENCE AND AGE OF RELATED CHILDREN
    "children_c06" = "B11004_004", # married under 6 only
    "children_c6+" = "B11004_005", # married under 6 and older
    "children_m06" = "B11004_011", # male under 6 only
    "children_m6+" = "B11004_012", # male under 6 and older
    "children_f06" = "B11004_017", # female under 6 only
    "children_f6+" = "B11004_018", # female under 6 and older
    #HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)
    "inc_0010" = "B19001_002",  "inc_1015" = "B19001_003", "inc_1520" = "B19001_004",
    "inc_2025" = "B19001_005", "inc_2530" = "B19001_006", "inc_3035" = "B19001_007",
    "inc_125"  = "B19001_015", "inc_150"  = "B19001_016", "inc_200"  = "B19001_017"
  )
  
  get_acs(geography = "block group", variables = variables,
                 state = state, county = county, geometry = TRUE) %>%
    select(-moe) %>%
    spread(variable, estimate) %>%
    # area is in m^2, change to km^2
    mutate(area = as.numeric(st_area(geometry) * 1e-6)) %>%
    transmute(
      geoid = GEOID,
      group = 1,
      population, households, housing_units, 
      density = households / area,
      income,
      # many of the variables come in raw counts, but we want to consider
      # them as shares of a relevant denominator.
      children = 100 * ( children_c06 + `children_c6+` + 
                           children_m06 + `children_m6+` + 
                           children_f06 + `children_f6+`) / households,
      lowincome    = 100 * (inc_0010 + inc_1015 + inc_1520 + inc_2530 +
                              inc_3035) / households,
      highincome   = 100 * (inc_125 + inc_150 + inc_200) / households,
      black        = 100 * black / population,
      asian        = 100 * asian / population,
      hispanic     = 100 * hispanic / population,
      white        = 100 * white / population
    ) %>%
    filter(population > 0) %>%
    st_set_geometry(NULL) %>%
    as_tibble()
}
