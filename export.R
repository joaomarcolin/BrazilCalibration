# This script exports the processed data as objects that can be passed on to 
# initialize the ABM
if (!dir.exists("results"))                     dir.create("results")

# Settings for generating the cell grid (FROM main.R) ---------------------
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

# (0) check if spatial settings are valid (FROM spatial3_create_grid.R) ----
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


# (1) get area that should be divided (FROM spatial3_create_grid.R) -------

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
