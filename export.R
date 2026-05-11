# This script processes the final results from main.R and generates the final objects
# that can be passed on to initialize the ABM. In order to do so, it crops the cell grid
# and the municipality-level data to an area of interest.
#
# The area of interest can be defined in three ways:
# (1) a biome;
# (2) a set of states;
# (3) the overlap between a biome and a set of states.
# The grid only keeps the largest available contiguous area.

rm(list=ls())
library(tidyverse)
library(sf)
library(tictoc)
library(igraph)

# Settings ----------------------------------------------------------------
# set cell size - must be 1 or 5
param_plot_km <- 5
# define type of area of interest - must be "state", "biome" or "both"
set_subset_by <- "state"
# define the area of interest:
#   if subsetting by state, pick a vector of state acronyms - e.g., c("GO","MG","BA");
#   if subsetting by biome, pick "cerrado" or "amazon"
#   if subsetting by both,  pick a vector where the first element is a biome and the following elements are state acronyms
set_aoi <- "GO" #c("cerrado", "MA", "TO", "PI", "BA")
# name the area of interest - will be used to save outputs
set_name <- "MATOPIBA"

# crop data to area of interest -------------------------------------------
tic(paste("Exporting", set_name, "data at", param_plot_km, "km resolution"))
source("scripts/export_crop_output.R")
toc()

# Exporting MATOPIBA data at 5 km resolution: 19.21 sec elapsed   (28,265 cells)
# Exporting MATOPIBA data at 1 km resolution: 376.79 sec elapsed (707,608 cells)
# Exporting GO data at 5 km resolution: 15.25 sec elapsed         (13,640 cells)
# Exporting GO data at 1 km resolution: 394.87 sec elapsed       (341,302 cells)

## report ------------------------------------------------------------------
#tic(paste("Rendering report:", set_name, "data at", param_plot_km, "km resolution"))
#rmarkdown::render("report2.Rmd", output_file = "report2.html")
#toc()

