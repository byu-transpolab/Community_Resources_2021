#' Get streetlight data
#' 
#' @param folder Path to folder containing streetlight data files
#' @param landuse One of "libraries", "parks" or "groceries" indicating which 
#'   land use streetlight file to download
#'   
#' @details We are unable to distribute the streetlight data files. This function
#'   prompts the user to navigate to the folder on box, download the file, and
#'   save it in the proper place before moving forward.
#' 
#' 
get_sl_data <- function(path, landuse){
  
  urls <- list(
    "libraries" = "https://byu.box.com/s/1mia30bkv7cd9yg7c3m8ijz98ajnxwky",
    "parks" = "",
    "groceries" = "https://byu.box.com/s/25dmjoho7uje6dxl50sse6rclaaep42w"
  )
  
  files <- list(
    "libraries" = "160756_Libraries_home_block_groups_all.csv",
    "parks" = "",
    "groceries" = "256055_Grocery_Stores_Utah_County_2019_home_block_groups_all.csv"
  )
  
  if(!file.exists(path)){
    message(str_c(
      "You need to download a file on Box.\n",
      "1. Go to ", urls[[landuse]], "\n",
      "2. Log in to Box, and download the file.\n", 
      "3. Copy the file to ", path, "\n",
      "4. Run tar_make() again."))
    stop(path, " not Found")
  } else {
    message(path, " available")
  }
  return(path) # to use file target, need to return path to data. 
  
}

#' Read and clean up Streetlight data file
#' 
#' @param path to a SL data file
#' @return a tibble with cleaned and organized data
read_sl_data <- function(path){
  read_csv(
    path, 
    col_types = list(
      `Zone ID` = col_character(),
      `Average Daily Zone Traffic (StL Volume)` = col_number(),
      `Percent by Home Location` = col_double()
    ))   %>%
    transmute(
      geoid = ifelse(`Block Group ID` == "N/A", NA, `Block Group ID`),
      dest = `Zone Name`,
      dest_id = `Zone ID`,
      home_work = `Home and Work Filter`,
      intersection = `Intersection Type`,
      day  = `Day Type`,
      time = `Day Part`,
      volume = `Average Daily Zone Traffic (StL Volume)`,
      percent = `Percent by Home Location`,
      flow = volume * percent
    )   %>%
    filter(!is.na(geoid))
}


#' Make a leaflet plot of flows between blockgroups and a destination
#' 
#' @param sldata A cleaned streetlight data tibble.
#' @param destinations A character vector of destination names
#' @param days Days to include in the analysis
#' @param times Times to include in the analysis
plot_streetlight <- function(sldata, destinations, days = "1: Weekday (M-Th)", 
                             times = "0: All Day (12am-12am)"){
  
  bg <- tigris::block_groups("UT", "Utah", class = "sf") %>%
    st_transform(4326)
  
  lib_flows <- inner_join(
    bg %>% select(GEOID),
    sldata %>% 
      filter(dest %in% destinations) %>%
      filter(day %in% days, time %in% times) %>%
      select(GEOID = geoid, dest,  flow),
    by = "GEOID"
  ) 
  
  pal <- colorNumeric("Reds", domain = lib_flows$flow)
  
  ggplot(lib_flows, aes(fill = flow)) + 
    geom_sf() + 
    scale_fill_viridis_b()
}


make_tlfd <- function(path){
  tlfd <- read_csv(
    path,
    col_types = list(
      `Type of Travel` = col_character(),
      `Home and Work Filter` = col_character(),
      `Intersection Type` = col_character(),
      `Zone ID` = col_character(),
      `Zone Name` = col_character(),
      `Day Type` = col_character(),
      `Day Part` = col_character(),
      `Average Daily Zone Traffic (StL Volume)` = col_number(),
      `Percent Home less than 1 mi` = col_number(),
      `Percent Home 1 to 3 mi` = col_number(),
      `Percent Home 3 to 5 mi` = col_number(),
      `Percent Home 5 to 10 mi` = col_number(),
      `Percent Home 10 to 25 mi` = col_number(),
      `Percent Home 25 to 50 mi` = col_number(),
      `Percent Home 50 to 100 mi` = col_number(),
      `Percent Home more than 100 mi` = col_number()
    )) %>%
    filter(`Zone Name` %in% 
             c("Brigham Young University - Harold B. Lee Library")) %>%
    filter(`Day Type` %in% "1: Weekday (M-Th)") %>%
    filter(`Day Part` %in% "0: All Day (12am-12am)") %>%
    select(
      end = `Intersection Type`,  
      volume = `Average Daily Zone Traffic (StL Volume)`,
      `Percent Home less than 1 mi`:`Percent Home more than 100 mi`) %>%
    pivot_longer(`Percent Home less than 1 mi`:`Percent Home more than 100 mi`,
                 names_to = "bin", values_to = "percent") %>%
    mutate(
      bin = gsub("Percent Home ", "", bin),
      bin = factor(bin, levels = c("less than 1 mi", "1 to 3 mi", "3 to 5 mi",
                                   "5 to 10 mi", "10 to 25 mi", "25 to 50 mi",   
                                   "50 to 100 mi", "more than 100 mi")),
      flow = volume * percent
    )
  
  
  ggplot( tlfd, aes(x = bin, y = flow, fill = end) ) +
    geom_bar(stat = "identity")
  
    
}
