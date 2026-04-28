# This script loads municipality-level geographic information
#
# we want to consider municipalities that were created after 1985 so the data
# is comparable throughout the period

# (1) load inputs ---------------------------------------------------------
df_brazil_mun    <- readr::read_csv(
                      "data_outputs/2_brazil_mun/df_brazil_mun.csv",
                      col_types = cols(
                        .default               = col_character(),
                        legal_amazon           = col_logical(),
                        amazon_area_km2        = col_number(),
                        atlantic_area_km2      = col_number(),
                        caatinga_area_km2      = col_number(),
                        cerrado_area_km2       = col_number(),
                        islands_area_km2       = col_number(),
                        pampa_area_km2         = col_number(),
                        pantanal_area_km2      = col_number(),
                        mun_area_km2           = col_number(),
                        mun_protected_area_km2 = col_number()
                        )
                      )

df_census_mun    <- readr::read_csv(
                      "data_outputs/3_census_data/df_census_mun.csv",
                      col_types = cols(
                        .default      = col_character(),
                        farm_area_ha  = col_number(),
                        n_farms       = col_number(),
                        activity_area = col_number(),
                        workers       = col_number()
                        )
                      )

df_census_state  <- readr::read_csv(
                      "data_outputs/3_census_data/df_census_state.csv",
                      col_types = cols(
                        .default      = col_character(),
                        farm_area_ha  = col_number(),
                        n_farms       = col_number(),
                        activity_area = col_number(),
                        workers       = col_number()
                        )
                      )

df_yearly_mun    <- readr::read_csv(
                      "data_outputs/4_yearly_data/df_yearly_mun.csv",
                      col_types = cols(
                        .default       = col_character(),
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

df_yearly_prices <- readr::read_csv(
                      "data_outputs/4_yearly_data/df_prices.csv",
                      col_types = cols(
                        .default = col_number(),
                        year     = col_character()
                        )
                      )

df_codes <- df_brazil_mun %>%
  dplyr::select(code_mun, group_1985, group_1995, group_2006, group_2017)

# (2) summarise data by municipality group --------------------------------

# auxiliary function to summarise data by municipality group
summarise_group_f <- function(yr) {
  # define grouping column
  group_col <- paste0("group_",yr)
  # summarise df_brazil_mun
  df_year1 <- df_brazil_mun %>%
    dplyr::select(
      all_of(group_col),
      legal_amazon:mun_protected_area_km2
    ) %>%
    dplyr::group_by(
      across(all_of(group_col))
    ) %>%
    dplyr::summarise(
      legal_amazon = any(legal_amazon),
      across(where(is.numeric), ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  # summarise df_yearly_mun
  df_year2 <- df_yearly_mun %>%
    dplyr::select(
      all_of(group_col),
      year:mb_pcrop_ha
    ) %>%
    dplyr::group_by(
      across(all_of(group_col)),
      year
    ) %>%
    dplyr::summarise(
      across(where(is.numeric), ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::transmute(across(all_of(group_col)),
                     year,
                     yield_C     = (ppm_herd*0.1*0.4)/mb_pasture_ha,
                     yield_S_pam = pam_output_ton/pam_area_ha,
                     yield_S_mb  = pam_output_ton/mb_soybean_ha)
  # join
  df_year <- df_codes %>%
    dplyr::left_join(df_year1,
                     by = group_col) %>%
    dplyr::left_join(df_year2, 
                     by = group_col) %>%
    dplyr::mutate(group = yr)
  # return summarized data.frame
  return(df_year)
} # close summarise_group_f(yr)

# summarise data by municipality group
df_1985 <- summarise_group_f("1985")
df_1995 <- summarise_group_f("1995")
df_2006 <- summarise_group_f("2006")
df_2017 <- summarise_group_f("2017")

# join in a single data.frame and clean up
df_municipalities <- rbind(df_1985, df_1995, df_2006, df_2017)
rm(df_1985, df_1995, df_2006, df_2017,
   df_brazil_mun, df_yearly_mun, df_codes,
   summarise_group_f)

# (3) deal with NA yield values -------------------------------------------



















