## This script extracts soybean attainable yields for 1981-2010
## and calculates plot productivity for soybean production

# (1) load grid, load raster and set output path --------------------------
terraOptions(progress = 1) # shows progress bar
# load grid
sf_grid <- sf::st_read(paste0("data_outputs/1_cell_grid/5_grid_lulc/brazil_grid_lulc_",param_plot_km,"km.shp"))
# load raster
soy_rast <- terra::rast("data_outputs/1_cell_grid/1_reproject/fao_brazil_soy_yield.tif")
# check coordinate reference systems
if(sf::st_crs(sf_grid) != sf::st_crs(soy_rast)) {
  warning("CRS mismath. Reprojecting grid to raster CRS for extraction.")
  soy_rast <- terra::project(soy_rast, terra::crs(vect(sf_grid)), method="bilinear")
}

# (2) process raster ------------------------------------------------------
# exact_extract returns a data.frame with 'cell_id' and columns for each class
plot_yield <- exactextractr::exact_extract(soy_rast, sf_grid,
                                           fun="mean",
                                           force_df=TRUE,
                                           append_cols="cell_id")
# FAO's yield is in kg/ha, I need values in tons/ha
names(plot_yield)[2] <- "FAO_soy_yield"
plot_yield$FAO_soy_yield <- plot_yield$FAO_soy_yield/1000
# replace NAs with 0s
plot_yield$FAO_soy_yield[is.na(plot_yield$FAO_soy_yield)] <- 0
# add to grid
sf_grid <- sf_grid %>%
  dplyr::left_join(plot_yield, by="cell_id")
# clean up
rm(plot_yield, soy_rast)

# (3) create and save final grid -------------------------------------------
# separate cell geometry and cell data
df_grid <- sf_grid %>% sf::st_drop_geometry()
sf_grid <- sf_grid %>% dplyr::select(cell_id, geometry)

# pivot longer (lulc variables)
df_grid <- df_grid %>%
  tidyr::pivot_longer(
    cols = matches("^area[1-6]_"),
    names_to = c(".value", "year"),
    names_pattern = "(area[1-6])_(\\d{4})"
  ) %>%
  dplyr::mutate(year=as.numeric(year))

# simplify LULC variables
df_grid <- df_grid %>%
  dplyr::transmute(
    cell_id, code_mun, code_biome, pub_area, protected, centroid_x, centroid_y, FAO_soy_yield, year,
    area_ha = area*100,
    cv_V    = area1,
    cv_I_C  = area2,
    cv_I_S  = area3 + area4,
    cv_I_F  = area5,
    cv_D    = pmax(1-cv_V-cv_I_C-cv_I_S-cv_I_F, 0)
  )
    
