# This script crops the cell grid and the yearly municipality-level of data
# to an area of interest defined in script "export.R". It requires arguments
# "param_plot_km", "set_subset_by", "set_aoi" and "aoi_name",
# defined in script "export.R"

# create the directory for storing the outputs
path_output <- paste0("initialization/",aoi_name,"_",param_plot_km,"km")
if (!dir.exists("initialization")) dir.create("initialization")
if (!dir.exists(path_output))      dir.create(path_output)

# (1) check if the area of interest is valid -------------------------------
# check that set_subset_by has a valid value
if (!set_subset_by %in% c("state", "biome", "both")) {
  stop("Invalid value for set_subset_by: accepted values are 'state', 'biome' or 'both'.")
}

# establish valid values for state acronyms and biome names
valid_states <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO",
                  "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR",
                  "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")
valid_biomes <- c("amazon", "caatinga", "cerrado", "atlantic", "pampa","pantanal")

# if subsetting by state, check if state acronyms are valid
if (set_subset_by=="state") {
  invalid_subset <- set_aoi[!set_aoi %in% valid_states]
  if (length(invalid_subset) > 0) {
    stop(paste("When set_subset_by==\"state\", set_aoi must be a character vector of state acronyms. Invalid value(s):",
               paste(invalid_subset, collapse=", ")))
  }
}

# if subsetting by biome, check if biome name is valid
if (set_subset_by=="biome") {
  invalid_subset <- set_aoi[!set_aoi %in% valid_biomes]
  if (length(invalid_subset) > 0) {
    stop(paste("When set_subset_by==\"biome\", set_aoi must be a biome name. Invalid value(s):",
               paste(invalid_subset, collapse=", ")))
  }
}

# if subsetting by both biome and states, check that biome name and state acronyms are valid
if (set_subset_by=="both" & length(set_aoi)<2) {
  stop(paste("When set_subset_by==\"both\", set_aoi must be a character vector with at least two elements: a biome name, followed by state acronyms."))
} else if (set_subset_by=="both") {
  invalid_biome  <- !set_aoi[1] %in% valid_biomes
  invalid_states <- set_aoi[-1][!set_aoi[-1] %in% valid_states]
  if (invalid_biome) {
    stop(paste("When set_subset_by==\"both\", the first element of set_aoi must be a valid biome name: 'cerrado' or 'amazon'."))
  } else if (length(invalid_states)>0) {
    stop(paste("When set_subset_by==\"both\", set_aoi must only contain a biome name followed by state acronyms. Invalid value(s):",
               paste(invalid_states, collapse=", ")))
  }
}

# clean up
rm(valid_biomes, valid_states)
if (exists("invalid_subset")) rm(invalid_subset)
if (exists("invalid_biome"))  rm(invalid_biome)
if (exists("invalid_states")) rm(invalid_states)

# (2) load data.frames ----------------------------------------------------
df_census_mun    <- read_csv(
  "data_outputs/results/df_census_mun.csv",
  col_types = cols(
    .default      = col_character(),
    legal_amazon  = col_logical(),
    year          = col_number(),
    farm_area_ha  = col_number(),
    n_farms       = col_number(),
    activity_area = col_number(),
    workers       = col_number()
  )
)

df_yearly_mun    <- read_csv(
  "data_outputs/results/df_yearly_mun.csv",
  col_types = cols(
    .default       = col_character(),
    legal_amazon   = col_logical(),
    year           = col_number(),
    pam_area_ha    = col_number(),
    pam_output_ton = col_number(),
    ppm_herd       = col_number(),
    mb_natcover_ha = col_number(),
    mb_nonveg_ha   = col_number(),
    mb_notobs_ha   = col_number(),
    mb_pasture_ha  = col_number(),
    mb_soybean_ha  = col_number(),
    mb_tcrop_ha    = col_number(),
    mb_forestry_ha = col_number(),
    mb_pcrop_ha    = col_number()
  )
)

df_yearly_prices <- read_csv(
  "data_outputs/results/df_yearly_prices.csv",
  col_types = cols(.default = col_number())
)


# (3) load and crop grid --------------------------------------------------
# (3.1) get path to grid's shapefile and dataframe
path_df <- paste0("data_outputs/results/grid_df_brazil_",param_plot_km,"km.csv")
path_sf <- paste0("data_outputs/results/grid_sf_brazil_",param_plot_km,"km.shp")

# (3.2) load data.frame and filter by area of interest and initialization years
df_grid <- readr::read_csv(
  path_df,
  col_types = cols(
    .default = col_number(),
    cell_id    = col_character(),
    code_mun   = col_character(),
    name_biome = col_character(),
    state      = col_character()
  )
)

if (set_subset_by == "state") {
  # if subsetting by state, only keep cells in the selected state(s)
  df_grid <- dplyr::filter(
    df_grid,
    state %in% set_aoi
  )
} else if (set_subset_by == "biome") {
  # if subsetting by biome, only keep cells in the selected biome(s)
  df_grid <- dplyr::filter(
    df_grid,
    name_biome %in% set_aoi
  )
} else if (set_subset_by == "both") {
  # if subsetting by both, retrieve biome name and state acronyms
  aoi_biome  <- set_aoi[1]
  aoi_states <- set_aoi[-1]
  # keep only cells in the selected biome and state(s)
  df_grid <- dplyr::filter(
    df_grid,
    name_biome  ==  aoi_biome,
    state      %in% aoi_states
  )
  # clean up
  rm(aoi_biome, aoi_states)
}

# (3.3) load shapefile and filter by area of interest
grid_sf <- sf::st_read(path_sf, quiet=TRUE) %>%
  dplyr::filter(cell_id %in% df_grid$cell_id)

# (3.4) only keep contiguous cells
# at this point, the cell grid may not be a contiguous area, especially
# if the area of interest was defined as the overlap of a biome and a set of states.
#
# to fix this, I only keep the largest contiguous area.
# first, find contigous groups of cells using a spatial graph
touches <- sf::st_touches(grid_sf)
# assign each cell to a contiguous group using a graph
graph  <- igraph::graph_from_adj_list(touches)
groups <- igraph::components(graph)$membership
# add group membership to grid_sf
grid_sf <- grid_sf %>%
  dplyr::mutate(group = groups)
# find the largest contiguous group
largest_group <- base::which.max(base::table(groups))
# keep only the largest contiguous group
grid_sf <- grid_sf %>%
  dplyr::filter(group == largest_group) %>%
  dplyr::select(-group)
df_grid <- df_grid %>%
  dplyr::filter(cell_id %in% grid_sf$cell_id)

# (3.5) clean up
rm(path_df, path_sf, touches, graph, groups, largest_group)

## histogram
#df_grid %>%
#  dplyr::filter(year==1995) %>%
#  ggplot(aes(x = theta_C)) +
#  geom_histogram(bins=50) +
#  # scale_x_log10() + # log scale to make it visible
#  theme_minimal()


# (4) summarise area of interest's yearly trends --------------------------

df_yearly_aoi <- df_yearly_mun %>%
  dplyr::filter(
    code_mun %in% df_grid$code_mun
  ) %>%
  dplyr::transmute(
    # keep grouping columns
    code_mun, name_mun, state,
    name_micro, name_rgi, name_meso, name_rgint,
    legal_amazon, name_biome, year,
    # get values for calculating farm yield
    area_S  = pam_area_ha,
    tons_S  = pam_output_ton,
    herd_C  = ppm_herd,
    area_C  = mb_pasture_ha,
    # get LULC variables
    mun_V   = mb_natcover_ha,
    mun_I   = mb_pasture_ha + mb_soybean_ha + mb_tcrop_ha + mb_pcrop_ha + mb_forestry_ha,
    mun_I_C = mb_pasture_ha,
    mun_I_S = mb_soybean_ha + mb_tcrop_ha + mb_pcrop_ha,
    mun_I_F = mb_forestry_ha,
    mun_D   = mb_nonveg_ha + mb_notobs_ha
  )

rm(df_yearly_mun)


# (5) summarise area of interest's agriculture census data ----------------
df_census_aoi <- df_census_mun %>%
  dplyr::filter(
    code_mun %in% df_grid$code_mun
  ) %>%
  dplyr::select(
    code_mun, name_mun, state,
    name_micro, name_rgi, name_meso, name_rgint,
    legal_amazon, name_biome, year,
    area_group, activity_group, day_group,
    farm_area_ha, n_farms, activity_area, workers
  )

rm(df_census_mun)

# (6) save outputs --------------------------------------------------------
readr::write_csv(df_census_aoi,    paste0(path_output,"/census_data_",aoi_name,".csv"))
readr::write_csv(df_yearly_aoi,    paste0(path_output,"/yearly_data_",aoi_name,".csv"))
readr::write_csv(df_yearly_prices, paste0(path_output,"/time_series.csv"))
readr::write_csv(df_grid,          paste0(path_output,"/cell_data_",aoi_name,"_",param_plot_km,"km.csv"))
sf::st_write(grid_sf,              paste0(path_output,"/cell_grid_",aoi_name,"_",param_plot_km,"km.shp"), delete_layer=TRUE, quiet=TRUE)
rm(path_output)
