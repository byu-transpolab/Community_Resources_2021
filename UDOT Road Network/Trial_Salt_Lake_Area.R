path_otp <- otp_dl_jar("C:/Users/stuck/Documents/Transpo Research/Community_Resources/UDOT Road Network",cache = TRUE)
log2 <- otp_setup(otp = path_otp, dir = "otp")
otpcon <- otp_connect()
on.exit(otp_stop(warn = FALSE))

ll <- get_latlong(landuse)
bg <- get_latlong(bgcentroid)

# Get distance between each ll and each bg
total_lu <- nrow(ll)
total_bg <- nrow(bg)


# loop through the land use points
alltimes <- lapply(1:total_lu, function(i){
  ll_latlong <- c(ll[i,]$LATITUDE, ll[i, ]$LONGITUDE)
  
  # loop through the block groups
  lapply(1:total_bg, function(j){
    bg_latlong <- c(bg[j,]$LATITUDE, bg[j, ]$LONGITUDE)
    
    # loop through the modes
    modes <- c("CAR","BUS")
    lapply(modes, function(mode){
      o <- otp_get_times(
        otpcon, 
        fromPlace = bg_latlong, toPlace = ll_latlong,
        
        mode = mode, 
        date = "08-02-2021", time = "08:00:00", detail = TRUE, includeLegs = TRUE
      )
      
      o$errorId <- as.character(o$errorId)
      
      o
    }) %>%
      set_names(modes) %>%
      bind_rows(.id = "mode")
    
  }) %>%
    set_names(bg$id) %>%
    bind_rows(.id = "blockgroup")
  
  
} %>%
  set_names(ll$id) %>%
  bind_rows(.id = "resource"))

# Do a little bit of cleanup
alltimes %>% 
  bind_rows(.id = "origin") %>%
  select(blockgroup, resource, mode, itineraries) %>%
  mutate(duration = itineraries$duration) %>%
  select(resource, blockgroup, mode, duration) %>%
  group_by(resource, blockgroup, mode) %>%
  arrange(duration, .by_group = TRUE) %>%
  slice(1)

}