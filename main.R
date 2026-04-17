# 2026 04 17
# New version of the AgriCerradoCalibration project.
# This project deals with data from IBGE, Mapbiomas, IPEA
# and Paraná's Secretary of Agriculture to get a picture
# of the ongoing process of Land Use and Cover Change
# in the Brazilian Cerrado and the Brazilian Amazon.

rm(list=ls())

library(tidyverse)
library(terra)
library(sf)
library(exactextractr)
library(readxl)

# create output folders
if (!dir.exists("data_outputs"))                dir.create("data_outputs")
if (!dir.exists("data_outputs/1_census_data"))  dir.create("data_outputs/1_census_data")
if (!dir.exists("data_outputs/2_yearly_data"))  dir.create("data_outputs/2_yearly_data")
if (!dir.exists("data_outputs/3_geography"))    dir.create("data_outputs/3_geography")
if (!dir.exists("data_outputs/4_cell_grid"))    dir.create("data_outputs/4_cell_grid")
if (!dir.exists("results"))                     dir.create("results")

# report data sources
rmarkdown::render("data_sources.Rmd", output_file = "data_sources.html")

# settings
param_plot_km <- 1    # must be a positive integer
param_year    <- 1985 # must be 1985, 1995, 2006 or 2017

# (1) agriculture census data ---------------------------------------------
source("scripts/census1_load_data.R")      # load agriculture census data
source("scripts/census2_treat_data.R")     # tidy agriculture census data
source("scripts/census3_get_parameters.R") # calculate model parameters
# saves outputs
readr::write_csv(farm_area_by_class,        "data_outputs/1_census_data/farm_area_by_class.csv")
readr::write_csv(farm_area_by_class_2017,   "data_outputs/1_census_data/farm_area_by_class_2017.csv")
readr::write_csv(farm_number_by_class,      "data_outputs/1_census_data/farm_number_by_class.csv")
readr::write_csv(farm_number_by_class_2017, "data_outputs/1_census_data/farm_number_by_class_2017.csv")
readr::write_csv(workers_per_ha,            "data_outputs/1_census_data/workers_per_ha.csv")
readr::write_csv(area_by_activity,          "data_outputs/1_census_data/area_by_activity.csv")

# (2) yearly municipality-level data --------------------------------------
source("scripts/yearly1_load_mapbiomas.R")    # load Mapbiomas data
source("scripts/yearly2_load_PAM_PPM_PEVS.R") # load PAM and PPM data
source("scripts/yearly3_sort_amc.R")          # sort municipalities by AMC
source("scripts/yearly4_calculate_yield.R")   # calculate yield
source("scripts/yearly5_price_series.R")      # get yearly country-level variables
# save final data for calibration
readr::write_csv(MUN_data,      "data_outputs/2_yearly_data/municipality_data.csv")
readr::write_csv(yearly_prices, "data_outputs/2_yearly_data/prices.csv")

# (3) geographical analysis -----------------------------------------------


# (4) cell grid -----------------------------------------------------------
# creates output folders
if (!dir.exists("data_outputs/4_cell_grid/1_reproject"))     dir.create("data_outputs/4_cell_grid/1_reproject")
if (!dir.exists("data_outputs/4_cell_grid/2_reclass"))       dir.create("data_outputs/4_cell_grid/2_reclass")
if (!dir.exists("data_outputs/4_cell_grid/3_create_grid"))   dir.create("data_outputs/4_cell_grid/3_create_grid")
if (!dir.exists("data_outputs/4_cell_grid/4_treat_grid"))    dir.create("data_outputs/4_cell_grid/4_treat_grid")
if (!dir.exists("data_outputs/4_cell_grid/5_grid_lulc"))     dir.create("data_outputs/4_cell_grid/5_grid_lulc")
if (!dir.exists("data_outputs/4_cell_grid/6_complete_grid")) dir.create("data_outputs/4_cell_grid/6_complete_grid")
# create grid and calculate plot-level values
source("scripts/spatial1_reproject.R")     # reproject shapefiles and rasters
source("scripts/spatial2_reclass.R")       # reclass Mapbiomas' rasters
source("scripts/spatial3_create_grid.R")   # create grid
source("scripts/spatial4_treat_grid.R")    # identify farms
source("scripts/spatial5_grid_lulc.R")     # calculate plot-level LULC variables
source("scripts/spatial6_complete_grid.R") # generate final grid
# save final data for calibration
readr::write_csv(grid_vars, paste0("data_outputs/4_cell_grid/6_grid_complete/BR_grid_vars_",param_plot_km,"km.csv"))
sf::st_write(grid_geom,     paste0("data_outputs/4_cell_grid/6_grid_complete/BR_grid_geom_",param_plot_km,"km.shp"), delete_layer=TRUE)

# (5) report results ------------------------------------------------------
# define which states will be included in the report
# "SP" and "MG" also have significative Cerrado area but were not included
cerrado_states <- c("GO", "MT", "MS", "MA", "TO", "PI", "BA", "DF")
# render report
rmarkdown::render("report.Rmd", output_file = "report.html")




