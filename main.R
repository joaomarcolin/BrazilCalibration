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
library(tictoc)

# create output folders
if (!dir.exists("data_outputs"))                dir.create("data_outputs")
if (!dir.exists("data_outputs/1_cell_grid"))    dir.create("data_outputs/1_cell_grid")
if (!dir.exists("data_outputs/2_brazil_mun"))   dir.create("data_outputs/2_brazil_mun")
if (!dir.exists("data_outputs/3_census_data"))  dir.create("data_outputs/3_census_data")
if (!dir.exists("data_outputs/4_yearly_data"))  dir.create("data_outputs/4_yearly_data")
if (!dir.exists("data_outputs/5_final"))        dir.create("data_outputs/5_final")
if (!dir.exists("results"))                     dir.create("results")

# report data sources
rmarkdown::render("data_sources.Rmd", output_file = "data_sources.html", quiet=TRUE)

# Settings for generating the cell grid -----------------------------------
# The settings bellow are only used from section (3) on

param_plot_km <- 5        # must be a positive integer
#param_year    <- 1985     # must be 1985, 1995, 2006 or 2017
set_subset    <- FALSE    # if TRUE, the cell grid is constructed only for the set of states or the biome specified below
set_name      <- "brazil" # name the area of interest, used to save the final cell grid

if (set_subset) {
  # how to divide the country?
  set_subset_by   <- "state" # must be "state", "biome" or "both"
  # what is the relevant area?
  #   if subsetting by state, pick a vector of state acronyms - e.g., c("GO","MG","BA");
  #   if subsetting by biome, pick "cerrado" or "amazon"
  #   if subsetting by both,  pick a vector where the first element is a biome and the following elements are state acronyms
  set_subset_area <- c("GO", "MT", "MS", "MA", "TO", "PI", "BA", "DF")
}

## (1) cell grid -----------------------------------------------------------
## creates output folders
#if (!dir.exists("data_outputs/1_cell_grid/1_reproject"))     dir.create("data_outputs/1_cell_grid/1_reproject")
#if (!dir.exists("data_outputs/1_cell_grid/2_recode_lulc"))   dir.create("data_outputs/1_cell_grid/2_recode_lulc")
#if (!dir.exists("data_outputs/1_cell_grid/3_create_grid"))   dir.create("data_outputs/1_cell_grid/3_create_grid")
#if (!dir.exists("data_outputs/1_cell_grid/4_treat_grid"))    dir.create("data_outputs/1_cell_grid/4_treat_grid")
#if (!dir.exists("data_outputs/1_cell_grid/5_grid_lulc"))     dir.create("data_outputs/1_cell_grid/5_grid_lulc")
#if (!dir.exists("data_outputs/1_cell_grid/6_complete_grid")) dir.create("data_outputs/1_cell_grid/6_complete_grid")
### create grid and calculate plot-level values
##source("scripts/spatial1_reproject.R")     # reproject shapefiles and rasters # only need to run it once
##source("scripts/spatial2_recode_lulc.R")   # reclass Mapbiomas' rasters       # only need to run it once
#source("scripts/spatial3_create_grid.R")   # create grid
#source("scripts/spatial4_treat_grid.R")    # identify farms
#source("scripts/spatial5_grid_lulc.R")     # calculate plot-level LULC variables
#source("scripts/spatial6_complete_grid.R") # generate final grid
#
## save final data for calibration
#readr::write_csv(df_grid, paste0("data_outputs/1_cell_grid/6_complete_grid/df_",set_name,"_grid_",param_plot_km,"km.csv"))
#sf::st_write(sf_grid,     paste0("data_outputs/1_cell_grid/6_complete_grid/sf_",set_name,"_grid_",param_plot_km,"km.shp"), delete_layer=TRUE)

# (2) group municipalities ------------------------------------------------
# most data is available at the municipality-level, and many municipalities were
# created after 1985, so there are many NAs. I group municipalities into a few groups
# so data can be aggregated at the municipality-group-level consistently through 1985-2024
source("scripts/group_municipalities.R")
readr::write_csv(df_brazil_mun, "data_outputs/2_brazil_mun/df_brazil_mun.csv")

# (3) agriculture census data ---------------------------------------------
# data available at the state- and municipality-level for 1985, 1995, 2006 and 2017,
# though some variables are not available for every year or every aggregation level
source("scripts/census1_load_data.R")  # load agriculture census data
source("scripts/census2_treat_data.R") # tidy agriculture census data
# saves outputs
readr::write_csv(df_census_mun,   "data_outputs/3_census_data/df_census_mun.csv")
readr::write_csv(df_census_state, "data_outputs/3_census_data/df_census_state.csv")

# (4) yearly data ---------------------------------------------------------
# municipality-level time series for soybean output, soybean farming area,
# and cattle herd for 1985-2024;
# country-level time series for the real interest rate and the minimum wage in 1985-2024,
# as well as soybean and beef prices in 1995-2024
source("scripts/yearly1_load_mapbiomas.R") # load Mapbiomas data
source("scripts/yearly2_load_PAM_PPM.R")   # load PAM and PPM data and create 'df_yearly_mun' table with yearly municipality-level data
source("scripts/yearly3_price_data.R")     # get yearly country-level variables
# saves outputs
readr::write_csv(df_yearly_mun,    "data_outputs/4_yearly_data/df_yearly_mun.csv")
readr::write_csv(df_yearly_prices, "data_outputs/4_yearly_data/df_prices.csv")

# (5) report results ------------------------------------------------------
#source("scripts/census3_get_parameters.R") # calculate model parameters
#source("scripts/yearly3_sort_amc.R")          # sort municipalities by AMC
#source("scripts/yearly4_calculate_yield.R")   # calculate yield
#source("scripts/yearly5_price_series.R")      # get yearly country-level variables

# define which states will be included in the report
# "SP" and "MG" also have significant Cerrado area but were not included
report_states <- c("GO", "MT", "MS", "MA", "TO", "PI", "BA", "DF")

# render report
rmarkdown::render("report1.Rmd", output_file = "report1.html")
#rmarkdown::render("report2.Rmd", output_file = "report2.html")

# (6) final results -------------------------------------------------------
source("scripts/final1_data.R")           # group municipality-level data
source("scripts/final2_initialization.R") # generate final grid_df

# saves outputs
readr::write_csv(df_census_state,   "results/df_census_state.csv")
readr::write_csv(df_census_mun,     "results/df_census_mun.csv")
readr::write_csv(df_yearly_prices,  "results/df_yearly_prices.csv")
readr::write_csv(df_municipalities, "results/df_municipalities.csv")
readr::write_csv(df_grid,    paste0("results/grid_df_",set_name,"_",param_plot_km,"km.csv"))
#sf::st_write(sf_grid,        paste0("results/grid_sf_",set_name,"_",param_plot_km,"km.shp"), delete_layer=TRUE)

