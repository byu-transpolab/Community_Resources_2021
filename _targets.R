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
source("R/streetlight_cleaner.R")
source("R/choice_modeling.R")


# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf","opentripplanner", "rstudioapi",
                            "otpr", "leaflet", "tidycensus", "parallel", "haven", 
                            "mlogit", "jsonlite"))

this_crs <- 3560 # http://epsg.io/3560-1746 Utah North usft
bglimit <- NULL # This will limit the times calculation to this many random zones. Set to NULL for all

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
  tar_target(util_file, "data/mode_utilities.json", format = "file"),
  tar_target(utilities, read_utilities(util_file)),
  
  # Parks ============
  tar_target(park_polygons, get_parks("data/parks.geojson", this_crs)),
  tar_target(park_points, make_park_points(park_polygons, 1/500, this_crs)),
  tar_target(park_times, calculate_times(park_points, bgcentroid, graph, 
                                         bglimit = bglimit, shortcircuit = "data/park_times.rds")),
  tar_target(park_lsums, calculate_logsums(park_times, utilities)),
  # streetlight ----
  tar_target(sl_parks_csv, get_sl_data("data/streetlight_parks.zip", "parks"),
             format = "file"),
  tar_target(sl_parks, read_sl_data(sl_parks_csv)),
  # choice data and models ---
  tar_target(parks_estdata, make_estdata(sl_parks, park_lsums, park_polygons, acsdata, 
                                         n_obs = 10000, n_alts = 10)),
  tar_target(park_models, estimate_parkmodels(parks_estdata)),

  
  # Groceries =====================
  tar_target(groceries, get_groceries("data/groceries.geojson", "data/NEMS-S_UC2021_brief.sav", this_crs)),
  tar_target(grocery_times, calculate_times(groceries, bgcentroid, graph, bglimit = bglimit,
                                            shortcircuit = "data/grocery_times.rds")),
  tar_target(grocery_lsums, calculate_logsums(grocery_times, utilities)),
  # streetlight ----
  tar_target(sl_grocery_csv, get_sl_data("data/streetlight_groceries.csv", "groceries"),
             format = "file"),
  tar_target(sl_grocery, read_sl_data(sl_grocery_csv, "UT-")),
  # choice data and models ---
  tar_target(groceries_estdata, make_estdata(sl_grocery, grocery_lsums, groceries, acsdata,
                                             n_obs = 10000, n_alts = 10)),
  tar_target(grocery_models, estimate_grocerymodels(groceries_estdata)),
  tar_target(grocery_mod_rds, write_rds(grocery_models, "data/grocery_models.rds"), format = "rds"),
  tar_target(grocery_access, calculate_grocery_access(grocery_times, groceries, grocery_models)),

  
  
  
  
  # Libraries ======================
  tar_target(libgj, "data/libraries.geojson", format = "file"),
  tar_target(libraries, get_libraries(libgj, this_crs)),
  tar_target(library_times, calculate_times(libraries, bgcentroid, graph, bglimit = bglimit,
                                            shortcircuit = "data/library_times.rds")),
  tar_target(library_lsums, calculate_logsums(library_times, utilities)),
  # streetlight ----
  tar_target(sl_libraries_csv, get_sl_data("data/streetlight_libraries.csv", "libraries"),
             format = "file"),
  tar_target(sl_libraries, read_sl_data(sl_libraries_csv)),
  tar_target(lee_plot, plot_streetlight(sl_libraries, "Brigham Young University - Harold B. Lee Library")),
  # choice data and models ---
  tar_target(libraries_estdata, make_estdata(sl_libraries, library_lsums, libraries, acsdata,
                                             n_obs = 10000, n_alts = 10)),
  tar_target(library_models, estimate_librarymodels(libraries_estdata)),
  
  tar_target(dummy,1+1)
)
