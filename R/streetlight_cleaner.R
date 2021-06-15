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
    "libraries" = "https://byu.box.com/s/1mia30bkv7cd9yg7c3m8ijz98ajnxwky"
  )
  
  files <- list(
    "libraries" = "160756_Libraries_home_block_groups_all.csv"
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