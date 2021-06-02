library(tidyverse)
library(sf)


park_polygons <- st_read("data/parks.geojson") 
  
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
  
park_latlon <- park_points %>%
    st_transform(4326) #I changed the crs number, the other one wasn't working, I don't think it was the right one
  
  library(opentripplanner)            # Load Package
  library(tidyr)
  library(tmap)
  library(RcppSimdJson)
  
path_data <- file.path("C:/Users/Public", "OTP") #path needs to have format OTP/graphs/default
dir.create(path_data)

path_otp <- otp_dl_jar(path_data, cache = FALSE)

log1 <- otp_build_graph(otp = path_otp, dir =  path_data)

log2 <- otp_setup(otp = path_otp, dir = path_data)
  
  otpcon <- otp_connect()
  
  head(park_latlon)

  toPlace   = park_latlon[rep(seq(1, 10), times = nrow(park_latlon)),]
  fromPlace = park_latlon[rep(seq(1, 10), each  = nrow(park_latlon)),]
  
  routes <- otp_plan(otpcon = otpcon,
                     fromPlace = fromPlace,
                     toPlace = toPlace,
                     fromID = as.character(fromPlace$id), #takes the id column, only accepts character
                     toID = as.character(toPlace$id), #same as previous
                     mode = c("WALK","BUS"),
                     get_geometry = FALSE)
  routes <- routes[,c("fromPlace","toPlace","duration")]
  # Use the tidyr package to go from long to wide format
  routes_matrix <- tidyr::pivot_wider(routes, 
                                      names_from = "toPlace", 
                                      values_from = "duration")


