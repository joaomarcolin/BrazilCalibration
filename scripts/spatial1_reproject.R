## 2025 11 23
## this script reprojects all the spatial data to the same cartographic projection
##
## IBGE's shapefiles come  in EPSG:4674 (SIRGAS 2000 Geographic, Geocentric Reference System for the Americas);
## CNUC's shapefile  comes in EPSG:4674 (SIRGAS 2000 Geographic, Geocentric Reference System for the Americas);
## Funai's shapefile comes in EPSG:4674 (SIRGAS 2000 Geographic, Geocentric Reference System for the Americas);
## FAO GAEZ's raster comes in EPSG:4326 (WGS84 Geographic, World Geodetic System 1984);
## Mapbiomas' rasters come in EPSG:4326 (WGS84 Geographic, World Geodetic System 1984);
##
## They are all reprojected to EPSG5880 (metric projection).
##
## reprojecting IBGE's, MMA's and Funai's shapefiles is very quick, but reprojecting
## Mapbiomas' rasters take a very long time: about 3h30min per year (for the whole of Brazil)

# (1) IBGE ----------------------------------------------------------------
# (1.1) reproject from EPSG4674 (geographic) to EPSG5880 (metric)
# open original shapefile
cities_sf <- sf::st_read("data_inputs/IBGE/spatial/BR_Municipios_2024.shp")
states_sf <- sf::st_read("data_inputs/IBGE/spatial/BR_UF_2024.shp")
biomes_sf <- sf::st_read("data_inputs/IBGE/spatial/IBGE_biomas_v2025_dominios_vetor_e250K.shp")
# reproject
cities_sf <- sf::st_transform(cities_sf, crs=5880)
states_sf <- sf::st_transform(states_sf, crs=5880)
biomes_sf <- sf::st_transform(biomes_sf, crs=5880)

# (1.2) simplify and tidy biomes_sf
# in the original shapefile, Brazil is divided into 52 polygons for different
# natural formations, but I just need 7 polygons, one for each biome
biomes_sf <- biomes_sf %>%
  # drop natural formations' columns
  dplyr::select(
    code_biome = cd_bm,
    name_biome = nm_bm,
    geometry) %>%
  # merge polygons by biome
  dplyr::group_by(code_biome, name_biome) %>%
  dplyr::summarise(
    geometry = sf::st_union(geometry),
    .groups  = "drop"
  ) %>%
  # make sure polygons are valid
  sf::st_make_valid() %>%
  # recode biome names
  dplyr::mutate(
    name_biome = dplyr::case_match(
                   name_biome,
                   "Amazônia"        ~ "amazon",
                   "Caatinga"        ~ "caatinga",
                   "Cerrado"         ~ "cerrado",
                   "Mata Atlântica"  ~ "atlantic",
                   "Pampa"           ~ "pampa",
                   "Pantanal"        ~ "pantanal",
                   "Ilhas Oceânicas" ~ "islands"
                 )
    )

## plot biomes
#ggplot(biomes_sf) +
#  geom_sf(aes(fill = name_biome), color = "white", linewidth = 0.3) +
#  coord_sf(datum = NA) +   # drops the graticule and axis labels
#  scale_fill_brewer(palette = "Set2", name = "Biome") +
#  labs(title = "Brazilian Biomes") +
#  theme_minimal()

# (1.3) simplify and tidy cities_sf
cities_sf <- cities_sf %>%
  dplyr::select(
    code_mun = CD_MUN,
    name_mun = NM_MUN,
    state    = SIGLA_UF,
    geometry
  )
# there are three problematic polygons in cities_sf
#   "Área Operacional "Lagoa dos Patos"", CD_MUN=="4300002"
#   "Área Operacional "Lagoa Mirim"",     CD_MUN=="4300001"
#   "Boa Esperança do Norte",             CD_MUN=="5101837"
# the first two are not actually municipalities and should be ignores
cities_sf <- cities_sf %>%
  dplyr::filter(
    !(code_mun %in% c("4300001", "4300002"))
  )
# the third one is a municipality that was emancipated from
# Sorriso (code_mun=="5107925") in 2024 and should be merged with Sorriso
geom1 <- cities_sf$geometry[cities_sf$code_mun=="5101837"]    # get Boa Esperança do Norte's polygon
geom2 <- cities_sf$geometry[cities_sf$code_mun=="5107925"]    # get Sorriso's polygon
new_geom <- sf::st_union(geom1, geom2)                        # merge both polygons
cities_sf$geometry[cities_sf$code_mun=="5107925"] <- new_geom # substitute Sorriso's polygon with the merged polygon
cities_sf <- cities_sf[cities_sf$code_mun!="5101837", ]       # remove Boa Esperança do Norte's row
rm(geom1, geom2, new_geom)                                    # clean up
cities_sf <- cities_sf %>% sf::st_make_valid()                # make sure that the new polygon is valid

# (1.4) simplify and tidy states_sf
states_sf <- states_sf %>%
  dplyr::select(
    state = SIGLA_UF,
    geometry
  )

# (1.5) dissolve biome boundaries and get brazil_df
brazil_sf <- biomes_sf %>%
  # ignore islands
  dplyr::filter(name_biome != "islands") %>%
  # dissolve and merge
  sf::st_union() %>%
  sf::st_as_sf()

# (1.6) save reprojected shapefile and clean up
sf::st_write(cities_sf, "data_outputs/3_cell_grid/1_reproject/cities_EPSG5880.shp", delete_layer=TRUE)
sf::st_write(states_sf, "data_outputs/3_cell_grid/1_reproject/states_EPSG5880.shp", delete_layer=TRUE)
sf::st_write(brazil_sf, "data_outputs/3_cell_grid/1_reproject/brazil_EPSG5880.shp", delete_layer=TRUE)
sf::st_write(biomes_sf, "data_outputs/3_cell_grid/1_reproject/biomes_EPSG5880.shp", delete_layer=TRUE)
rm(cities_sf, states_sf, brazil_sf, biomes_sf)

# (2) Conservation Units and Indigenous Territories -----------------------
# open original shapefiles
cnuc_sf  <- sf::st_read("data_inputs/CNUC/cnuc_2025_08.shp")
funai_sf <- sf::st_read("data_inputs/FUNAI/tis_poligonaisPolygon.shp")

# reproject
cnuc_sf  <- sf::st_transform(cnuc_sf,  crs = 5880)
funai_sf <- sf::st_transform(funai_sf, crs = 5880)

# keep only the polygons
cnuc_sf  <- cnuc_sf  %>% dplyr::select(geometry) %>% sf::st_make_valid()
funai_sf <- funai_sf %>% dplyr::select(geometry) %>% sf::st_make_valid()

# dissolve and join all polygons
protected_areas_sf <- rbind(cnuc_sf, funai_sf) %>%
  sf::st_union() %>%         # merge all polygons
  sf::st_cast("POLYGON") %>% # split multipolygon into individual contiguous polygons
  sf::st_as_sf() %>%         # convert back to sf data.frame
  sf::st_make_valid() %>%    # make sure polygons have valid geometry
  dplyr::select(polygon = x) # only keep the polygons

# save output and clean up
sf::st_write(protected_areas_sf, "data_outputs/3_cell_grid/1_reproject/protected_areas_EPSG5880.shp", delete_layer=TRUE)
rm(cnuc_sf, funai_sf, protected_areas_sf)

# (3) FAO GAEZ attainable yield -------------------------------------------
# load data (for the whole world)
fao_rast <- terra::rast("data_inputs/FAO/ylHr_soy.tif")

# get Brazil's polygon to crop FAO's raster
brazil_sf <- sf::st_read("data_outputs/3_cell_grid/1_reproject/brazil_EPSG5880.shp") %>%
  # reproject to match FAO's projection
  sf::st_transform(crs=sf::st_crs(fao_rast))

# crops FAO's raster to Brazil's shapefile:
fao_brazil_rast <- terra::crop(fao_rast, brazil_sf)
# mask to keep only pixels within Brazil's boundary
fao_brazil_rast <- terra::mask(fao_brazil_rast, brazil_sf)
# reproject FAO's raster to EPSG:5880
fao_brazil_rast <- terra::project(fao_brazil_rast, y = "epsg:5880", method = "bilinear")
# save output
terra::writeRaster(fao_brazil_rast, "data_outputs/3_cell_grid/1_reproject/fao_brazil_soy_yield.tif", overwrite=TRUE)
# clean up
rm(fao_rast, brazil_sf, fao_brazil_rast)


# (4) Mapbiomas -----------------------------------------------------------
# reproject from EPSG4326 (geographic) to EPSG5880 (metric)
#
# this takes a terribly long time to run: about 4h min per year.
# do it only once and hope that you won't need to add more years

# auxiliary function to load, reproject, and save new .tif
reproject_mb_f <- function(year) {
  # set input and output paths
  input_path  <- paste0("data_inputs/Mapbiomas/brazil_coverage_",year,".tif")
  output_path <- paste0("data_outputs/3_cell_grid/1_reproject/mapbiomas",year,"_EPSG5880.tif")
  # load raster
  terra::rast(input_path) %>%
    # reproject raster
    terra::project(y = "epsg:5880", method = "near") %>%
    # save reprojected raster
    terra::writeRaster(output_path, overwrite = TRUE)
}

# load, reproject and save LULC rasters
years <- c(1985, 1995, 2006, 2017, 2024)
tic("Reproject Mapbiomas' rasters")
for (yr in years) {
  tic(paste("Reprojected and saved",yr,"raster."))
  reproject_mb_f(yr)
  toc()
}
toc()
rm(yr, years, reproject_mb_f)

