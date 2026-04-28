# 2025 09 22
# This script creates a regular grid from Brazil's shapefile and parameter 'param_plot_km'

# (0) check if spatial settings are valid ---------------------------------
if (set_subset) {
  # check that set_subset_by has a valid value
  if (!set_subset_by %in% c("state", "biome", "both")) {
    stop("Invalid value for set_subset_by. When set_subset==TRUE, set_subset_by has to be 'state', 'biome' or 'both'.")
  }
  
  # establish valid values for state acronyms and biome names
  valid_states <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO",
                    "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR",
                    "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")
  valid_biomes <- c("amazon", "caatinga", "cerrado", "atlantic", "pampa","pantanal")
  
  # if subsetting by state
  if (set_subset_by=="state") {
    invalid_subset <- set_subset_area[!set_subset_area %in% valid_states]
    if (length(invalid_subset) > 0) {
      stop(paste("When set_subset_by==\"state\", set_subset_area must be a character vector of state acronyms. Invalid value:",
                 paste(invalid_subset, collapse=", ")))
    }
  }
  
  # if subsetting by biome
  if (set_subset_by=="biome") {
    invalid_subset <- set_subset_area[!set_subset_area %in% valid_biomes]
    if (length(invalid_subset) > 0) {
      stop(paste("When set_subset_by==\"biome\", set_subset_area must be a biome name. Invalid value:",
                 paste(invalid_subset, collapse=", ")))
    }
  }
  
  # if subsetting by both state and biome
  if (set_subset_by=="both" & length(set_subset_area)<2) {
    stop(paste("When set_subset_by==\"both\", set_subset_area must be a character vector with at least two elements: a biome name, followed by state acronyms."))
  } else if (set_subset_by=="both") {
    invalid_biome  <- !set_subset_area[1] %in% valid_biomes
    invalid_states <- set_subset_area[-1][!set_subset_area[-1] %in% valid_states]
    if (invalid_biome) {
      stop(paste("When set_subset_by==\"both\", the first element of set_subset_area must be a valid biome name."))
    } else if (length(invalid_states)>0) {
      stop(paste("When set_subset_by==\"both\", set_subset_area must only contain a biome name followed by state acronyms. Invalid value:",
                 paste(invalid_states, collapse=", ")))
    }
  }
  
} # close if (set_subset)


# (1) get area that should be divided -------------------------------------

if (set_subset==FALSE) {
  # consider the whole country
  area_sf <- sf::st_read("data_outputs/1_cell_grid/1_reproject/brazil_EPSG5880.shp") %>%
    dplyr::select(geometry)
  
} else if (set_subset==TRUE & set_subset_by=="state") {
  # consider only a subset of states
  area_sf <- sf::st_read("data_outputs/1_cell_grid/1_reproject/states_EPSG5880.shp") %>%
    dplyr::filter(state %in% set_subset_area) %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    dplyr::rename(geometry=x)
  
} else if (set_subset==TRUE & set_subset_by=="biome") {
  # consider only a biome
  area_sf <- sf::st_read("data_outputs/1_cell_grid/1_reproject/biomes_EPSG5880.shp") %>%
    dplyr::filter(name_biome %in% set_subset_area) %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    dplyr::rename(geometry=x)
  
} else if (set_subset==TRUE & set_subset_by=="both") {
  # get biome polygon
  area_biome_sf <- sf::st_read("data_outputs/1_cell_grid/1_reproject/biomes_EPSG5880.shp") %>%
    dplyr::filter(name_biome == set_subset_area[1]) %>%
    dplyr::select(geometry)
  # get states polygon
  area_state_sf <- sf::st_read("data_outputs/1_cell_grid/1_reproject/states_EPSG5880.shp") %>%
    dplyr::filter(state %in% set_subset_area[-1]) %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    dplyr::rename(geometry=x)
  # get overlap area
  area_sf <- st_intersection(area_biome_sf, area_state_sf) %>%
    st_cast("POLYGON") %>%
    mutate(area = st_area(geometry))
  n_polygons <- length(area_sf$geometry)
  # check if overlap is valid
  if (n_polygons==0) {
    # if there is no overlaping area, stop
    stop(paste("No overlap between biome", set_subset_area[1], "and states",
          paste(set_subset_area[-1], collapse=", ")))
    
  } else if (n_polygons==1) {
    # if there is exactly one contiguous overlap area, go on
    area_sf <- area_sf %>% dplyr::select(geometry)
    rm(area_biome_sf, area_state_sf)
    
  } else if (n_polygons>1) {
    # if there area multiple contiguous overlap area, pick the largest one
    total_area <- sum(area_sf$area)                  # get total overlap area
    area_sf <- area_sf %>% slice_max(area)           # keep only largest contiguous overlap
    remaining_area <- 100*area_sf$area[1]/total_area # get remaining area as a % of total overlap
    area_sf <- area_sf %>% dplyr::select(geometry)   # drop the polygon area column
    # print warning
    warning(paste0("The overlaping area between biome \"",
                   set_subset_area[1],
                   "\" and selected states (",
                   paste(set_subset_area[-1], collapse=", "),
                   ") is not contiguous. The cell grid will use the largest polygon, which covers ",
                   round(remaining_area,2),
                   "% of the total overlap area."
                  )
            )
    rm(total_area, remaining_area)
  }
  rm(n_polygons)
}

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
# rasterize() requires a resolution in meters and param_plot_km is in kilometers
if (param_plot_km>=3) {
  area_rast2 <- terra::rasterize(
                  area_rast,
                  rast(ext(area_rast),
                  res=1000,
                  crs=crs(area_rast)),
                  field=1
                )
  # calculate actual overlap area (in km2) between each grid cell and Brazil
  grid_sf$overlap_area <- exactextractr::exact_extract(area_rast2, grid_sf,
                                                       fun='sum',
                                                       progress=TRUE)
  grid_sf$plot_area <- param_plot_km^2
  grid_sf$overlap_fraction <- grid_sf$overlap_area/grid_sf$plot_area
  # with param_plot_km==5, 3983 out of the 347086 plots (1.15% of the total)
  # have less than 75% of their area within Brazil.
  # I treat these plots as background and ignore them
  # identify cells with >75% overlap with Brazil's territory
  valid_ids <- grid_sf$cell_id[grid_sf$overlap_fraction>0.75]
  rm(area_rast2)
} else {
  # if the plots are too small, just consider the centroid: if it's inside area_sf,
  # the cell is included
  centroids <- sf::st_centroid(grid_sf)
  inside    <- sf::st_within(centroids, area_sf, sparse=FALSE)[,1]
  grid_sf$overlap_area     <- as.numeric(inside)*param_plot_km^2
  grid_sf$plot_area        <- param_plot_km^2
  grid_sf$overlap_fraction <- as.numeric(inside)
  valid_ids <- grid_sf$cell_id[inside]
  rm(centroids, inside)
} # closes if-else

# remove cells with <=75% overlap
grid_rast[!grid_rast %in% valid_ids] <- NA
# filter vector grid to keep only valid cells and select relevant columns
grid_sf <- grid_sf %>%
  dplyr::filter(overlap_fraction>0.75) %>%
  dplyr::select(cell_id,geometry,area=overlap_area)

# (4) saves grid outputs --------------------------------------------------
# sets output path for raster and vector formats
path_out_tif <- paste0("data_outputs/1_cell_grid/3_create_grid/",set_name,"_grid_",param_plot_km,"km.tif")
path_out_shp <- paste0("data_outputs/1_cell_grid/3_create_grid/",set_name,"_grid_",param_plot_km,"km.shp")
# saves .shp and .tif
sf::st_write(grid_sf, path_out_shp, quiet = TRUE, append=FALSE, delete_layer=TRUE)
terra::writeRaster(grid_rast, path_out_tif, overwrite = TRUE)
# removes auxiliary objects
rm(area_rast, area_sf, cell_m, grid_polygons,
   grid_template, path_out_shp, path_out_tif, valid_ids)

