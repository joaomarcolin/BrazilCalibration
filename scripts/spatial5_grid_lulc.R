## 2025 09 23
##
## This script extracts LULC classes for multiple years (1985, 1996, 2006, 2017, 2024)
## and calculates the area of each LULC class within each grid cell. LULC classes were
## reclassified from MapBiomas' data:
## 1: natural cover
## 2: pasture
## 3: temporary crops
## 4: perennial crops
## 5: forest plantation
## 6: non-vegetated

# (1) load grid and set output path ---------------------------------------
terraOptions(progress = 1) # shows progress bar
path_input_shp <- paste0("data_outputs/1_cell_grid/4_treat_grid/brazil_grid_tenure_",param_plot_km,"km.shp")
grid_sf        <- sf::st_read(path_input_shp, quiet=TRUE)
path_out_shp   <- paste0("data_outputs/1_cell_grid/5_grid_lulc/brazil_grid_lulc_",param_plot_km,"km.shp")

# (2) define function to extract LULC statistics --------------------------
# the main function must take "grid_sf" and a LUC raster for a given year and calculate
# plot-level LUC variables that should be added to "grid_sf" as new columns

get_lulc_vars <- function(my_grid, year) {
  
  start_time <- Sys.time()
  
  # load LULC raster for specified year
  lulc_path <- paste0("data_outputs/1_cell_grid/2_recode_lulc/lulc_",year,".tif")
  message("Processing: ", basename(lulc_path))
  lulc_rast <- terra::rast(lulc_path)
  
  # check coordinate reference systems
  if(sf::st_crs(my_grid) != sf::st_crs(lulc_rast)) {
    warning("CRS mismath. Reprojecting grid to raster CRS for extraction.")
    my_grid <- sf::st_transform(my_grid, sf::st_crs(lulc_rast))
  }
  
  # extract fractional coverage of each LULC class per grid cell
  # exact_extract returns a data.frame with 'cell_id' and columns for each class
  message("Running exact_extract...")
  class_coverage <- exactextractr::exact_extract(lulc_rast, my_grid,
                                                 fun="frac",         # calculate fraction of each LULC class
                                                 coverage_area=TRUE, # use actual pixel areas for edge cells
                                                 force_df=TRUE,
                                                 append_cols="cell_id",
                                                 max_cells_in_memory = .Machine$integer.max)
  
  # rename columns to inclue year suffix
  # all columns in my_grid must have, at most, 10-characters
  names(class_coverage)[2:7] <- paste0("area",1:6,"_",year)
  
  # join LULC data back to grid
  message("Merging with grid...")
  my_grid <- my_grid %>%
    dplyr::left_join(class_coverage, by="cell_id")
  
  # clean up memory and report timing
  rm(lulc_rast, class_coverage, lulc_path)
  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  cat("Year", year, "completed in", round(elapsed, 1), "minutes\n")
  return(my_grid)

  } # closes get_lulc_vars

# (3) processes all years -------------------------------------------------
## takes about 50 min per year
#years <- c("1985", "1995", "2006", "2017", "2024") ########################################################################
#
#tic("Processing all years")
#for (yr in years) {
#  tic(paste0("Processing ",yr))
#  grid_sf <- get_lulc_vars(grid_sf, year=yr)
#  toc()
#}
#toc()

tic("Processing 1985")
grid_sf <- get_lulc_vars(grid_sf, year="1985")
toc()

# (4) saves output --------------------------------------------------------

sf::st_write(grid_sf, path_out_shp, delete_layer=TRUE, quiet=TRUE)

rm(path_input_shp, path_out_shp, yr, years, get_lulc_vars)
