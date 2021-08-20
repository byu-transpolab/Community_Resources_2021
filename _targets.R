library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/datamaker.R")
source("R/streetlight_cleaner.r")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf","opentripplanner", "rstudioapi",
                            "otpr", "leaflet", "tidycensus"))

this_crs <- 3560 # http://epsg.io/3560-1746 Utah North usft

otp_path <- "otp/graphs/default/"

# End this file with a list of target objects.
list(
  # block group centroids
  tar_target(bgcentroid, get_bglatlong()),
  tar_target(acsdata, get_acsdata(state = "UT", county = "Utah")),
  
  # OTP Routing data ==========
  # OSM data for OTP is already constructed and committed. But this allows
  # us to bring it into the targets stream
  tar_target(osmpbf, get_osmbpf(file.path(otp_path, "osm.pbf")), format = "file"),
  tar_target(gtfs,   get_gtfs(file.path(otp_path, "gtfs.zip")), format = "file"),
  tar_target(graph,  make_graph(otp_path), format = "file"),
  
  # Parks ============
  tar_target(park_polygons, get_parks("data/parks.geojson", this_crs)),
  tar_target(park_points, make_park_points(park_polygons, 1/500, this_crs)),
  tar_target(park_times, calculate_times(park_points, bgcentroid, graph)),
  # streetlight ----
  tar_target(sl_parks_csv, get_sl_data("data/streetlight_parks.zip", "parks"),
             format = "file"),
  tar_target(sl_parks, read_sl_data(sl_parks_csv)),
  
  # Groceries =====================
  tar_target(groceries, get_groceries("data/groceries.geojson", this_crs)),
  tar_target(grocery_times, calculate_times(groceries, bgcentroid)),
  # streetlight ----
  tar_target(sl_grocery_csv, get_sl_data("data/streetlight_groceries.csv", "groceries"),
             format = "file"),
  tar_target(sl_grocery, read_sl_data(sl_grocery_csv)),
  # choice data and models ---
  
  
  
  
  # Libraries ======================
  tar_target(libraries, get_libraries("data/libraries.geojson", this_crs)),
  tar_target(library_times, calculate_times(libraries, bgcentroid, graph)),
  # streetlight ----
  tar_target(sl_libraries_csv, get_sl_data("data/streetlight_libraries.csv", "libraries"),
             format = "file"),
  tar_target(sl_libraries, read_sl_data(sl_libraries_csv)),
  tar_target(lee_plot, plot_streetlight(sl_libraries, "Brigham Young University - Harold B. Lee Library")),
  # choice data and models ---
  tar_target(library_choicedata, )
  
  
  
  # combination df
  #tar_target(all_data, make_all_data(park_times, library_times, grocery_times))
  tar_target(dummy,1+1)
)
