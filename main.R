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
if (!dir.exists("results"))                     dir.create("results")

# report data sources
rmarkdown::render("data_sources.Rmd", output_file = "data_sources.html", quiet=TRUE)

# Settings for generating the cell grid -----------------------------------
# The settings bellow are only used from section (3) on
#
# I ran everything with set_subset==FALSE and param_plot_km set to 1 or 5, so 
# we have the full country at the 1km or 5km resolution. Since the spatial processing
# takes a long time to complete, I thought it was a good idea to make the code more flexible so that
# we can process a small part of the country. This may be useful if we ever need a higher resolution (<1km).
# In that case, you should pick: a biome, OR a set of states, OR a biome and a set of states - in this last case,
# the code creates the grid for the largest contiguous area of the overlap between the selected biome and the selected states.
#
# When working with the 1km or 5km resolution, it's best to just use the grid that is already created.
# If you need to look into a specific region, just use dplyr::filter() and select by municipality code, 
# state acronym or any other condition you see fit.

param_plot_km <- 5        # must be a positive integer
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

# (1) cell grid -----------------------------------------------------------
# creates output folders
if (!dir.exists("data_outputs/1_cell_grid/1_reproject"))     dir.create("data_outputs/1_cell_grid/1_reproject")
if (!dir.exists("data_outputs/1_cell_grid/2_recode_lulc"))   dir.create("data_outputs/1_cell_grid/2_recode_lulc")
if (!dir.exists("data_outputs/1_cell_grid/3_create_grid"))   dir.create("data_outputs/1_cell_grid/3_create_grid")
if (!dir.exists("data_outputs/1_cell_grid/4_treat_grid"))    dir.create("data_outputs/1_cell_grid/4_treat_grid")
if (!dir.exists("data_outputs/1_cell_grid/5_grid_lulc"))     dir.create("data_outputs/1_cell_grid/5_grid_lulc")
if (!dir.exists("data_outputs/1_cell_grid/6_complete_grid")) dir.create("data_outputs/1_cell_grid/6_complete_grid")
## create grid and calculate plot-level values
#source("scripts/spatial1_reproject.R")     # reproject shapefiles and rasters # only need to run it once
#source("scripts/spatial2_recode_lulc.R")   # reclass Mapbiomas' rasters       # only need to run it once
source("scripts/spatial3_create_grid.R")   # create grid
source("scripts/spatial4_treat_grid.R")    # identify farms
source("scripts/spatial5_grid_lulc.R")     # calculate plot-level LULC variables
source("scripts/spatial6_complete_grid.R") # generate final grid

# save final data for calibration
readr::write_csv(df_grid, paste0("data_outputs/1_cell_grid/6_complete_grid/df_",set_name,"_grid_",param_plot_km,"km.csv"))
sf::st_write(sf_grid,     paste0("data_outputs/1_cell_grid/6_complete_grid/sf_",set_name,"_grid_",param_plot_km,"km.shp"), delete_layer=TRUE)

# (2) group municipalities ------------------------------------------------
# most data is available at the municipality-level, and many municipalities were
# created after 1985, so there are many NAs. I group municipalities into a few groups
# so data can be aggregated at the municipality-group-level consistently through 1985-2024
source("scripts/group_municipalities.R") # takes about 5 min to run

readr::write_csv(df_brazil_mun, "data_outputs/2_brazil_mun/df_brazil_mun.csv")

# (3) agriculture census data ---------------------------------------------
# data available at the state- and municipality-level for 1985, 1995, 2006 and 2017,
# though some variables are not available for every year or every aggregation level
source("scripts/census1_load_data.R")  # load agriculture census data
source("scripts/census2_recode.R")     # recode variables
source("scripts/census3_mun_data.R")   # tidy municipality-level agriculture census data
source("scripts/census4_state_data.R") # tidy state-level agriculture census data
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
#source("scripts/yearly4_fix_yield.R")      # fixes yield (cattle per hectare)
# saves outputs
readr::write_csv(df_yearly_mun,    "data_outputs/4_yearly_data/df_yearly_mun.csv")
readr::write_csv(df_yearly_prices, "data_outputs/4_yearly_data/df_prices.csv")

# (5) report preliminary results ------------------------------------------
# define which states will be included in the report
# "SP" and "MG" also have significant Cerrado area but were not included
#
# if the grid was created by subsetting states, use set_subset_area as the vector of included states;
# otherwise, specify which states should be included in the report
report_states <- if (exists("set_subset_area")) set_subset_area else c("GO", "MT", "MS", "MA", "TO", "PI", "BA", "DF",
                                                                       "AC", "AP", "AM", "PA", "RO", "RR", "TO")
# render report
rmarkdown::render("report1.Rmd", output_file = "report1.html")

# (6) final results -------------------------------------------------------
included_states <- if (exists("set_subset_area")) set_subset_area else c("GO", "MT", "MS", "MA", "TO", "PI", "BA", "DF",
                                                                         "AC", "AP", "AM", "PA", "RO", "RR", "TO")
# manipulate data to prepare model initializetion
source("scripts/final_output.R")
# report results
rmarkdown::render("report2.Rmd", output_file = "report2.html")
# saves results
readr::write_csv(df_census_state,   "results/df_census_state.csv")
readr::write_csv(df_census_mun,     "results/df_census_mun.csv")
readr::write_csv(df_yearly_prices,  "results/df_yearly_prices.csv")
readr::write_csv(df_municipalities, "results/df_municipalities.csv")
readr::write_csv(df_grid,    paste0("results/grid_df_",set_name,"_",param_plot_km,"km.csv"))




# PREVIOUS (OUTDATED) SCRIPTS
#source("scripts/census3_get_parameters.R") # calculate model parameters
#source("scripts/yearly3_sort_amc.R")          # sort municipalities by AMC
#source("scripts/yearly4_calculate_yield.R")   # calculate yield
#source("scripts/yearly5_price_series.R")      # get yearly country-level variables

#sf::st_write(sf_grid,        paste0("results/grid_sf_",set_name,"_",param_plot_km,"km.shp"), delete_layer=TRUE)


