## 2025 09 22
## This script takes the regular grid for an area (from "create_grid.R") and
## assigns municipality codes, biome codes, the % of protected area and a binary
## variable equal to 1 if more than 50% of the cell's area is protected.
## This information is saved in a new shapefile.

# (0) loads cell grid -----------------------------------------------------
grid_vec  <- terra::vect(paste0("data_outputs/1_cell_grid/3_create_grid/",set_name,"_grid_",param_plot_km,"km.shp"))
grid_rast <- terra::rast(paste0("data_outputs/1_cell_grid/3_create_grid/",set_name,"_grid_",param_plot_km,"km.tif"))
grid_sf   <- sf::st_as_sf(grid_vec) 

# (1) assign municipality codes -------------------------------------------
# load municipality shapefile
mun_vec <- terra::vect("data_outputs/1_cell_grid/1_reproject/cities_EPSG5880.shp")
mun_sf  <- sf::st_as_sf(mun_vec)
# check coordinate reference systems match
if (sf::st_crs(grid_sf) != sf::st_crs(mun_sf)) {
  print("mun_sf and grid_sf have different projections")
  mun_sf <- sf::st_transform(mun_sf, st_crs(grid_sf))
}

# create a numeric ID for each municipality (rasterize needs numeric values)
mun_vec$mun_id_num <- as.integer(as.factor(mun_vec$code_mun))
mun_id_lookup <- data.frame(mun_id_num = mun_vec$mun_id_num,
                            code_mun   = mun_vec$code_mun) %>% base::unique()
# rasterize using that numeric ID
mun_rast <- terra::rasterize(mun_vec, grid_rast,
                             field="mun_id_num", fun="max")
# use exact_extract with fun="mode" to find the dominant municipality per cell
mun_values <- exactextractr::exact_extract(mun_rast, grid_sf,
                                           fun="mode",
                                           append_cols="cell_id",
                                           force_df=TRUE)

mun_values <- mun_values %>%
  dplyr::left_join(mun_id_lookup, by=c("mode"="mun_id_num")) %>%
  dplyr::select(cell_id, code_mun)

grid_sf <- grid_sf %>% 
  dplyr::left_join(mun_values, by="cell_id")

# remove the NA cells at the borders
grid_sf <- grid_sf[!is.na(grid_sf$code_mun),]

# removes auxiliary variables
rm(mun_vec, mun_sf, mun_rast, mun_values, mun_id_lookup)

# (2) assign biome code ---------------------------------------------------
# gets biomes shapefile
# biome codes:
# 1 amazon
# 2 caatinga
# 3 cerrado
# 4 atlantic
# 5 pampa
# 6 pantanal
# 7 islands

# load biomes shapefile
biomes_vec <- terra::vect("data_outputs/1_cell_grid/1_reproject/biomes_EPSG5880.shp")
biomes_vec$code_biome <- as.integer(biomes_vec$code_biome)
biomes_sf  <- sf::st_as_sf(biomes_vec)
# check coordinate reference systems match
if (sf::st_crs(grid_sf) != sf::st_crs(biomes_sf)) {
  print("biomes_sf and grid_sf have different projections")
  biomes_sf <- sf::st_transform(biomes_sf, st_crs(grid_sf))
}

# biome codes (cd_bm) are already small integers (1-7), so no lookup table needed
biomes_rast <- terra::rasterize(biomes_vec, grid_rast,
                                field="code_biome", fun="max")
biomes_values <- exactextractr::exact_extract(biomes_rast, grid_sf,
                                              fun="mode",
                                              append_cols="cell_id",
                                              force_df=TRUE)

biomes_values <- biomes_values %>%
  dplyr::rename(code_biome=mode) %>%
  dplyr::select(cell_id, code_biome)

grid_sf <- grid_sf %>%
  dplyr::left_join(biomes_values, by="cell_id")

# removes auxiliary objects (keep biomes_sf — needed for NA fallback in section 5)
rm(biomes_vec, biomes_rast, biomes_values)

# (3) assign protected area coverage --------------------------------------
protect_vec <- terra::vect("data_outputs/1_cell_grid/1_reproject/protected_areas_EPSG5880.shp")
protect_sf  <- sf::st_as_sf(protect_vec)
# check that coordinate reference systems match
if (sf::st_crs(grid_sf) != sf::st_crs(protect_sf)) {
  print("protect_sf and grid_sf have different projections")
  protect_sf <- sf::st_transform(protect_sf, st_crs(grid_sf))
}
# dissolve overlapping protected areas to prevent double-counting
protect_sf <- sf::st_union(protect_sf) %>%
  sf::st_simplify(dTolerance=250) %>% # simplify geometries
  sf::st_make_valid() %>%             # repair any self-intersections
  sf::st_cast("POLYGON") %>%          # break multipolygon back into individual polygons
  sf::st_as_sf()                      # convert back to sf object

# calculate intersection between grid cells and protected areas
protect_intersection <- sf::st_intersection(grid_sf, protect_sf)

# calculate area (sq km) of each protected fragment
protect_intersection$fragment_area <- as.numeric(st_area(protect_intersection)) /1e6

# summarize protected area by cell_id
protect_intersection <- protect_intersection %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(cell_id) %>%
  dplyr::summarize(pub_area = sum(fragment_area, na.rm=TRUE)) %>%
  dplyr::ungroup()

# updates grid_sf:
# joins protected area data back to grid
grid_sf <- grid_sf %>% 
  dplyr::left_join(protect_intersection, by="cell_id")
# set NA values (cells with no protected area) to 0
grid_sf$pub_area[is.na(grid_sf$pub_area)] <- 0
# max(grid_sf$area) is 25.00639 and max(grid_sf$pub_area) is 25.25068 due to some counting error
# I fix that to ensure that % of public area is between 0 and 1
grid_sf$pub_area <- base::pmin(grid_sf$pub_area, grid_sf$area)  #grid_sf$pub_area[grid_sf$pub_area>grid_sf$area] <- grid_sf$area[grid_sf$pub_area>grid_sf$area]
# convert protected area to fraction of cell area
grid_sf$pub_area <- grid_sf$pub_area/grid_sf$area

# create binary "protected" variable (1 if >50% protected area)
grid_sf$protected<-0
grid_sf$protected[grid_sf$pub_area>0.5] <- 1

# removes auxiliary objects
rm(protect_vec, protect_sf, protect_intersection)

# (4) save grid cell centroids --------------------------------------------
centroids          <- sf::st_centroid(grid_sf)
grid_sf$centroid_x <- sf::st_coordinates(centroids)[, 1]
grid_sf$centroid_y <- sf::st_coordinates(centroids)[, 2]
rm(centroids)

# (5) fill missing biome codes using nearest neighbor --------------------
# find cells with missing biome codes
na_cells <- which(is.na(grid_sf$code_biome))
# fill cell with the closest biome
if (length(na_cells) > 0) {
  message("Filling ", length(na_cells), " NA biome cells via nearest feature...")
  # get centroids of NA cells
  na_centroids <- sf::st_centroid(grid_sf[na_cells, ]) %>%
    dplyr::select(cell_id, geometry, centroid_x, centroid_y)
  # find the nearest biome
  nearest <- sf::st_join(na_centroids, biomes_sf, join = st_nearest_feature)
  # substitute values in grid_sf
  grid_sf$code_biome[na_cells] <- nearest$code_biome
}
rm(biomes_sf, na_centroids, nearest, na_cells)

# (6) save outputs --------------------------------------------------------
path_out_shp    <- paste0("data_outputs/1_cell_grid/4_treat_grid/",set_name,"_grid_tenure_",param_plot_km,"km.shp")
sf::st_write(grid_sf, path_out_shp, delete_layer=TRUE)
rm(grid_rast, grid_vec, path_out_shp)

# (7) print summary statistics --------------------------------------------
cat("\n=== SUMMARY ===\n")
cat("Grid size (rows): ", nrow(grid_sf), "\n", sep="")
cat("Cells with code_mun: ", sum(!is.na(grid_sf$code_mun)), "\n", sep="")
cat("Cells classified as protected: ", sum(grid_sf$protected == 1), "\n", sep="")
print(summary(select(st_drop_geometry(grid_sf),area, code_biome, pub_area, protected)))
rm(grid_sf)

