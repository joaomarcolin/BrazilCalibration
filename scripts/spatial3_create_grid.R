# 2025 09 22
# This script creates a regular grid from Brazil's shapefile and parameter 'param_plot_km'

tic(paste("Create grid at",param_plot_km,"km resolution"))

# (1) load area that should be divided ------------------------------------
# consider the whole country
area_sf <- sf::st_read("data_outputs/1_cell_grid/1_reproject/brazil_EPSG5880.shp", quiet=TRUE) %>%
           dplyr::select(geometry)
  
# (2) Create grid template and initialize cells ---------------------------
area_rast <- terra::vect(area_sf)
cell_m <- param_plot_km * 1000
grid_template <- terra::rast(ext(area_rast), resolution = cell_m, crs=crs(area_rast))

# initialize grid with unique cell ids
grid_rast <- terra::init(grid_template, fun=function(x) 1:ncell(grid_template))

# mask grid to selected area's boundaries (remove cells outside of area_sf)
grid_rast <- terra::mask(grid_rast, area_rast)

# name the raster layer as 'cell_id'
names(grid_rast) <- "cell_id"

# convert raster grid (non NA cells) to polygon vector format for spatial operations
cat("Starting as.polygons:", format(Sys.time(), "%H:%M:%S"), "\n")
grid_polygons     <- terra::as.polygons(grid_rast)
cat("Finished as.polygons:", format(Sys.time(), "%H:%M:%S"), "\n")
grid_sf           <- sf::st_as_sf(grid_polygons)
cat("Finished st_as_sf:", format(Sys.time(), "%H:%M:%S"), "\n")
names(grid_sf)[1] <- "cell_id"

# (3) Check plot overlap with area shapefile ------------------------------
# create high-resolution raster (1km) for accurate area calculations
# rasterize() requires a resolution in meters and param_plot_km is in kilometers #########################################################

#if (param_plot_km>=3) {
#  area_rast2 <- terra::rasterize(
#                  area_rast,
#                  rast(ext(area_rast),
#                  res=1000,
#                  crs=crs(area_rast)),
#                  field=1
#                )
#  # calculate actual overlap area (in km2) between each grid cell and Brazil
#  grid_sf$overlap_area <- exactextractr::exact_extract(area_rast2, grid_sf,
#                                                       fun='sum',
#                                                       progress=TRUE)
#  grid_sf$plot_area <- param_plot_km^2
#  grid_sf$overlap_fraction <- grid_sf$overlap_area/grid_sf$plot_area
#  # with param_plot_km==5, 3983 out of the 347086 plots (1.15% of the total)
#  # have less than 75% of their area within Brazil.
#  # I treat these plots as background and ignore them
#  # identify cells with >75% overlap with Brazil's territory
#  valid_ids <- grid_sf$cell_id[grid_sf$overlap_fraction>0.75]
#  rm(area_rast2)
#} else {
  # if the plots are too small, just consider the centroid: if it's inside area_sf,
  # the cell is included
  centroids <- sf::st_centroid(grid_sf)
  inside    <- sf::st_within(centroids, area_sf, sparse=FALSE)[,1]
  grid_sf$overlap_area     <- as.numeric(inside)*param_plot_km^2
  grid_sf$plot_area        <- param_plot_km^2
  grid_sf$overlap_fraction <- as.numeric(inside)
  valid_ids <- grid_sf$cell_id[inside]
  rm(centroids, inside)
#} # closes if-else

# remove cells with <=75% overlap
grid_rast[!grid_rast %in% valid_ids] <- NA
# filter vector grid to keep only valid cells and select relevant columns
grid_sf <- grid_sf %>%
  dplyr::filter(overlap_fraction>0.75) %>%
  dplyr::select(cell_id,geometry,area=overlap_area)

# (4) saves grid outputs --------------------------------------------------
# sets output path for raster and vector formats
path_out_tif <- paste0("data_outputs/1_cell_grid/3_create_grid/brazil_grid_",param_plot_km,"km.tif")
path_out_shp <- paste0("data_outputs/1_cell_grid/3_create_grid/brazil_grid_",param_plot_km,"km.shp")
# saves .shp and .tif
sf::st_write(grid_sf, path_out_shp, quiet = TRUE, append=FALSE, delete_layer=TRUE)
terra::writeRaster(grid_rast, path_out_tif, overwrite = TRUE)
# removes auxiliary objects
rm(area_rast, area_sf, cell_m, grid_polygons,
   grid_template, path_out_shp, path_out_tif, valid_ids)

