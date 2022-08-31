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

options(java.parameters = "-Xmx8G")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf", "r5r", "rstudioapi",
                            "leaflet", "tidycensus", "haven", 
                            "mlogit", "jsonlite", "VGAM"))

this_crs <- 3560 # http://epsg.io/3560-1746 Utah North usft
bglimit <- NULL # This will limit the times calculation to this many random zones. Set to NULL for all

otp_path <- "r5"

# End this file with a list of target objects.
list(
  # block group centroids
  tar_target(bg, tigris::block_groups("UT", "Utah", year = 2019)),
  tar_target(bgcentroid, get_bglatlong()),
  tar_target(acsdata, get_acsdata(state = "UT", county = "Utah")),
  
  # OTP Routing data ==========
  # OSM data for OTP is already constructed and committed. But this allows
  # us to bring it into the targets stream
  tar_target(osmpbf, get_osmbpf(file.path(otp_path, "osm.pbf")), format = "file"),
  tar_target(gtfs,   get_gtfs(file.path(otp_path, "gtfs.zip")), format = "file"),
  tar_target(util_file, "data/mode_utilities.json", format = "file"),
  tar_target(utilities, read_utilities(util_file)),
  
  # Parks ============
  tar_target(park_polygons, get_parks("data/parks.geojson", this_crs)),
  tar_target(park_points, make_park_points(park_polygons, 1/500, this_crs)),
  tar_target(park_times, calculate_times(park_points, bgcentroid, osmpbf, gtfs,
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
  tar_target(park_access, calculate_access(park_models$`All - Logsum`, parks_estdata)),
  tar_target(pbin_access, get_binary_access(park_lsums, duration_WALK, 10)),

  
  # Groceries =====================
  tar_target(groceries, get_groceries("data/groceries.geojson", "data/NEMS-S_UC2021_brief.sav", this_crs)),
  tar_target(grocery_times, calculate_times(groceries, bgcentroid, osmpbf, gtfs,
                                            bglimit = bglimit,
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
  tar_target(grocery_access, calculate_access(grocery_models$`All - Logsum`, groceries_estdata)),
  tar_target(gbin_access, get_binary_access(grocery_lsums, duration_CAR, 5)),

  
  
  
  
  # Libraries ======================
  tar_target(libgj, "data/libraries.geojson", format = "file"),
  tar_target(libraries, get_libraries(libgj, this_crs)),
  tar_target(library_times, calculate_times(libraries, bgcentroid, osmpbf, gtfs,
                                            bglimit = bglimit,
                                            shortcircuit = "data/library_times.rds")),
  tar_target(library_lsums, calculate_logsums(library_times, utilities)),
  # streetlight ----
  tar_target(sl_libraries_csv, get_sl_data("data/streetlight_libraries.csv", "libraries"),
             format = "file"),
  tar_target(sl_libraries, read_sl_data(sl_libraries_csv)),
  # choice data and models ---
  tar_target(libraries_estdata, make_estdata(sl_libraries, library_lsums, libraries, acsdata,
                                             n_obs = 10000, n_alts = 10)),
  tar_target(library_models, estimate_librarymodels(libraries_estdata)),
  tar_target(library_access, calculate_access(library_models$`All - Logsum`, libraries_estdata)),
  tar_target(lbin_access, get_binary_access(library_lsums, duration_CAR, 10)),
  
  # aggregate accessibilities ==============
  tar_target(access_bin, accessbin_data(bgcentroid, gbin_access, lbin_access, pbin_access)),
  tar_target(access_ls , accessls_data(bgcentroid, grocery_access, library_access, park_access)),
  tar_target(no_buffer, make_nobuffer(access_bin, acsdata)),
  tar_target(no_logsum, make_nologsum(access_ls, acsdata)),
  
  # income map ===========
  tar_target(hh, read_rds("data/syn_hh.rds")), 
  tar_target(pp, read_rds("data/syn_pp.rds")),
  
  
  tar_target(dummy,1+1)
)








