# This script generates the final version of grid_df, to be passed on to 
# the ABM's initialization

# This script loads municipality-level geographic information
#
# we want to consider municipalities that were created after 1985 so the data
# is comparable throughout the period

# delete afterwards -------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(terra)
library(sf)
library(exactextractr)
library(readxl)
library(tictoc)

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

#included_states <- if (exists("set_subset_area")) set_subset_area else c("GO", "MT", "MS", "MA", "TO", "PI", "BA", "DF",
#                                                                         "AC", "AP", "AM", "PA", "RO", "RR", "TO")

included_states <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI",
                     "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", 
                     "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")

# (0) load inputs ---------------------------------------------------------
df_brazil_mun <- readr::read_csv(
                   "data_outputs/2_brazil_mun/df_brazil_mun.csv",
                   col_types = cols(.default = col_character())
                   ) %>%
                 dplyr::filter(
                   state %in% included_states
                   ) %>%
                 dplyr::select(
                   code_mun:name_biome
                   )

#df_census_state <- readr::read_csv(
#                     "data_outputs/3_census_data/df_census_state.csv",
#                     col_types = cols(
#                       .default      = col_character(),
#                       year          = col_number(),
#                       farm_area_ha  = col_number(),
#                       n_farms       = col_number(),
#                       activity_area = col_number(),
#                       workers       = col_number()
#                       )
#                     ) %>%
#                   dplyr::filter(
#                     state %in% included_states
#                     )

df_census_mun <- readr::read_csv(
                   "data_outputs/3_census_data/df_census_mun.csv",
                   col_types = cols(
                     .default      = col_character(),
                     legal_amazon  = col_logical(),
                     year          = col_number(),
                     farm_area_ha  = col_number(),
                     n_farms       = col_number(),
                     activity_area = col_number(),
                     workers       = col_number()
                     )
                   ) %>%
                 dplyr::filter(
                   state %in% included_states
                   )

df_yearly_mun <- readr::read_csv(
                   "data_outputs/4_yearly_data/df_yearly_mun.csv",
                   col_types = cols(
                     .default       = col_character(),
                     year           = col_number(),
                     legal_amazon   = col_logical(),
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
                   ) %>%
                 dplyr::filter(
                   state %in% included_states
                   )

df_yearly_prices <- readr::read_csv(
                      "data_outputs/4_yearly_data/df_prices.csv",
                      col_types = cols(
                        .default = col_number()
                        )
                      )

df_grid <- readr::read_csv(
             paste0("data_outputs/1_cell_grid/6_complete_grid/df_",set_name,"_grid_",param_plot_km,"km.csv"),
             col_types = cols(
               .default   = col_number(),
               cell_id    = col_character(),
               code_mun   = col_character(),
               code_biome = col_character()
               )
             ) %>%
           dplyr::filter(
             code_mun %in% df_brazil_mun$code_mun
             )

sf_grid <- sf::st_read(
             paste0("data_outputs/1_cell_grid/6_complete_grid/sf_",set_name,"_grid_",param_plot_km,"km.shp")
             ) %>%
           dplyr::filter(
             cell_id %in% df_grid$cell_id
             )

# (1) exclude municipalities not present in the cell grid -----------------
# this only makes a difference if the cell grid was created as the overlap of a set of states and a biome
df_grid <- df_grid %>% dplyr::filter(cell_id %in% sf_grid$cell_id)
df_brazil_mun <- df_brazil_mun %>% dplyr::filter(code_mun %in% df_grid$code_mun)
df_census_mun <- df_census_mun %>% dplyr::filter(code_mun %in% df_grid$code_mun)
df_yearly_mun <- df_yearly_mun %>% dplyr::filter(code_mun %in% df_grid$code_mun)

# (2) calculate cv_I and theta_S (tons of soybean / plot) -----------------
# I use FAO GAEZ's soy yield (tons/hectare) and substitute zeroes with the lowest available positive yield
df_grid$FAO_soy_yield[df_grid$FAO_soy_yield==0] <- min(df_grid$FAO_soy_yield[df_grid$FAO_soy_yield>0])
# theta_S is the max yield from the plot, so I multiply soy yield (tons/ha) by plot area (in hectares)
df_grid <- df_grid %>%
  dplyr::transmute(
    cell_id, code_mun, code_biome, pub_area, protected, centroid_x, centroid_y, year, area_ha,
    cv_V    = cv_V + cv_I_F,   # since AgriCerrado doesn't include forestry, I treat planted forests as natural cover
    cv_I    = cv_I_C + cv_I_S, # for the same reason, farming area is the sum of pasture and cropland (no planted forests)
    cv_I_C, cv_I_S,            # I keep cv_I_C and cv_I_S to decide plot type when initializing the model;
    cv_D,
    theta_S = FAO_soy_yield*area_ha
    )

# (3) calculate theta_C (tons of beef / plot) -----------------------------
# theta_C is calculated from PPM, and the number of municipalities change in 1985-2024
# so for each year (1985, 1995, 2006, 2017) I group municipalities to avoid NA values
# for cattle ranching outputs in municipalities created after 1985

# (3.1) calculate head of cattle per hectare by municipality group and year
# auxiliary function to summarise by municipality group
summarise_mun_yield_f <- function(yr) {
  # define grouping column
  group_col <- paste0("group_",yr)
  # summarise df_yearly_mun
  df_year <- df_yearly_mun %>%
    dplyr::filter(
      state %in% included_states,
      year == yr) %>%
    dplyr::select(
      state,
      all_of(group_col),
      ppm_herd,
      mb_pasture_ha
      ) %>%
    dplyr::group_by(
      state,
      across(all_of(group_col))
      ) %>%
    dplyr::summarise(
      across(everything(), ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
      ) %>%
    dplyr::mutate(
      area_C = mb_pasture_ha,
      herd_C = ppm_herd,
      )
  # calculate yield
  df_year$yield_C <- ifelse(df_year$area_C > 0,
                            df_year$herd_C / df_year$area_C,
                            0)
  # return summarised data.frame
  return(df_year)
} # close summarise_mun_yield_f(yr)

# summarise by municipality group - yield_C is the number of cattle per hectare
mun_1985_yield <- summarise_mun_yield_f("1985") # Brazil: 3823 municipalities, 263 have >2 cattle per ha
mun_1995_yield <- summarise_mun_yield_f("1995") # Brazil: 4469 municipalities, 395 have >2 cattle per ha
mun_2006_yield <- summarise_mun_yield_f("2006") # Brazil: 5547 municipalities, 450 have >2 cattle per ha
mun_2017_yield <- summarise_mun_yield_f("2017") # Brazil: 5547 municipalities, 558 have >2 cattle per ha
rm(summarise_mun_yield_f)

# (3.2) fix outliers
# assume that all municipalities with >2 cattle per hectares are outliers,
# identify all outliers and attribute to them the average yield of all non-outlier municipalities in the same state
#
# auxiliary function to correct outliers
fix_outliers_f <- function(df) {
  # add columns to identify outliers and regular municipalities
  df$outlier <- df$yield_C > 2
  df$regular <- !df$outlier
  # summarise herd and pasture area at state-level (only regular municipalities)
  df_state <- df %>%
    dplyr::filter(regular) %>%
    dplyr::select(state, herd_C, area_C) %>%
    dplyr::group_by(state) %>%
    dplyr::summarise(
      herd_C  = sum(herd_C, na.rm=TRUE),
      area_C  = sum(area_C, na.rm=TRUE),
      .groups = "drop"
      )
  # calculate state-level average yield of regular municipalities
  df_state$yield_C <- ifelse(df_state$area_C > 0,
                             df_state$herd_C / df_state$area_C,
                             0)
  # replace yield_C values for outlier municipalities
  for (idx in 1:length(df$yield_C)) {
    if (df$outlier[idx]) {
      idx_state       <- df$state[idx]
      idx_yield       <- df_state$yield_C[df_state$state==idx_state]
      df$yield_C[idx] <- idx_yield
    }
  }
  # tidy final data.frame
  df <- df %>%
    dplyr::select(
      any_of(c("group_1985", "group_1995", "group_2006", "group_2017")),
      yield_C
      )
  # return final data.frame
  return(df)
}

# substitute yield_C for all municipalities with >2 head of cattle per hectare
mun_1985_yield <- fix_outliers_f(mun_1985_yield) %>% dplyr::rename(`1985` = yield_C)
mun_1995_yield <- fix_outliers_f(mun_1995_yield) %>% dplyr::rename(`1995` = yield_C)
mun_2006_yield <- fix_outliers_f(mun_2006_yield) %>% dplyr::rename(`2006` = yield_C)
mun_2017_yield <- fix_outliers_f(mun_2017_yield) %>% dplyr::rename(`2017` = yield_C)
rm(fix_outliers_f)

## histogram
#mun_1985_yield %>%
#  ggplot(aes(x = yield_C)) +
#  geom_histogram(bins=50) +
#  theme_minimal()
#
## density plot
#mun_1985_yield %>%
#  ggplot(aes(x = yield_C)) +
#  geom_density() +
#  theme_minimal()

# (3.3) join to df_grid
# (a) associate grid cells with grouping cols
df_yield_C <- df_grid %>%
  dplyr::select(
    cell_id, code_mun
    ) %>%
  base::unique() %>%
  dplyr::left_join(
    dplyr::select(df_brazil_mun, code_mun:group_2017),
    by = "code_mun"
    ) 
# (b) retrieve yield_C for each initialization year
df_yield_C <- df_yield_C %>%
  dplyr::left_join(
    mun_1985_yield,
    by = "group_1985"
    ) %>%
  dplyr::left_join(
    mun_1995_yield,
    by = "group_1995"
    ) %>%
  dplyr::left_join(
    mun_2006_yield,
    by = "group_2006"
    ) %>%
  dplyr::left_join(
    mun_2017_yield,
    by = "group_2017"
    ) %>%
  dplyr::select(
    cell_id,
    `1985`:`2017`
    )
# (c) tidy data.frame
df_yield_C <- df_yield_C %>%
  tidyr::pivot_longer(
    cols      = -cell_id,
    names_to  = "year",
    values_to = "yield_C"
    ) %>%
  dplyr::mutate(
    year = as.numeric(year)
    )
# (d) join back to df_grid and calculate theta_C
df_grid <- df_grid %>%
  dplyr::left_join(
    df_yield_C,
    by = join_by(cell_id, year)
    ) %>%
  # yield_C is cattle per hectare, theta_C is tons of beef per plot
  dplyr::mutate(
    theta_C = (yield_C*0.4*0.2)*area_ha
    ) %>%
  dplyr::select(
    -yield_C
    )
# (e) clean up
rm(mun_1985_yield, mun_1995_yield,
   mun_2006_yield, mun_2017_yield,
   df_yield_C)



