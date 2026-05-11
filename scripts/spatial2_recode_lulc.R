# 2025 11 23
# This script recodes MapBiomas' LULC classes

# (1) reclassification rule -----------------------------------------------

## unique raster values (for 1985, 1995, 2006, 2017, and 2024):
## 0  3  4  5  6  9 11 12 13 15 20 21 23 24 25 29 30 31 32 33 35 39 40 41 46 47 48 49 50 62 75
## to get a raster's unique values, uncomment and run the code bellow (this example uses the 1985 data):
#
#MB_1985 <- terra::rast("data_outputs/1_cell_grid/1_reproject/mapbiomas1985_EPSG5880.tif")
#print(MB_1985) ; terra::res(MB_1985) ; terra::ext(MB_1985) ; terra::crs(MB_1985) # summarize raster
#datatype(MB_1985)                                                                # check class (mapbiomas' rasters are "numeric")
#class_counts <- freq(MB_1985)                                                    # count number of pixels in each class (takes about 10min)
#unique(class_counts$value)                                                       # get unique values

reclass_matrix <- base::rbind(
  # first column defines original value, second column defines the new value
  c(0,0),                                  # 0: background
  c(3,1),c(4,1),c(5,1),c(6,1),c(11,1),     # 1: natural cover
  c(29,1),c(32,1),c(49,1),c(50,1),c(13,1), #    idem
  c(15,2),c(21,2),c(12,2),                 # 2: pasture
  c(20,3),c(39,3),c(40,3),c(41,3),c(62,3), # 3: temporary crops
  c(35,4),c(46,4),c(47,4),c(48,4),         # 4: perennial crops
  c(9,5),                                  # 5: forest plantation
  c(23,6),c(24,6),c(25,6),                 # 6: non-vegetated
  c(30,6),c(31,6),c(33,6),c(75,6)          #    idem
)

# (2) recode and save output ----------------------------------------------
# takes about 20 min per year

# auxiliary function to load, recode and save rasters
recode_mb_f <- function(year) {
  # set input and output path
  input_path  <- paste0("data_outputs/1_cell_grid/1_reproject/mapbiomas",year,"_EPSG5880.tif")
  output_path <- paste0("data_outputs/1_cell_grid/2_recode_lulc/lulc_",year,".tif")
  # load raster
  terra::rast(input_path) %>%
    # reclass
    terra::classify(reclass_matrix) %>%
    # save reclassified raster
    terra::writeRaster(
      output_path,
      overwrite = TRUE,
      NAflag    = 0,
      datatype  = "INT1U")
}

# load, recode and save LULC rasters
years <- c(1985, 1995, 2006, 2017, 2024)
tic("Recode Mapbiomas' rasters")
for (yr in years) {
  tic(paste("Recoded and saved",yr,"raster."))
  recode_mb_f(yr)
  toc()
}
toc()

# clean up
rm(yr, years, reclass_matrix, recode_mb_f)

# unique values by year:
# 1985: 0  3  4  5  6  9 11 12 13 15 20 21 23 24 25 29 30 31 32 33 35 39 40 41 46 47 48 49 50
# 1995: 0  3  4  5  6  9 11 12 13 15 20 21 23 24 25 29 30 31 32 33 35 39 40 41 46 47 48 49 50
# 2006: 0  3  4  5  6  9 11 12    15 20 21 23 24 25 29 30 31 32 33 35 39 40 41 46 47 48 49 50 62
# 2017: 0  3  4  5  6  9 11 12    15 20 21 23 24 25 29 30 31 32 33 35 39 40 41 46 47 48 49 50 62 75
# 2024: 0  3  4  5  6  9 11 12    15 20 21 23 24 25 29 30 31 32 33 35 39 40 41 46 47 48 49 50 62 75
#
# 0 and 13 show up in the data but aren't present in MapBiomas' variable dictionary
# 27 doesn't show up in the data but is present in MapBiomas' variable dictionary

