# This script generates the final version of grid_df, to be passed on to 
# the ABM's initialization


# (1) load inputs ---------------------------------------------------------

df_grid <- readr::read_csv(
                    paste0("data_outputs/1_cell_grid/6_complete_grid/df_",set_name,"_grid_",param_plot_km,"km.csv"),
                      col_types    = cols(
                        .default   = col_number(),
                        cell_id    = col_character(),
                        code_mun   = col_character(),
                        code_biome = col_character(),
                        year       = col_character()
                        )
                    )

#df_municipalities <- readr::read_csv(
#                       "results/df_municipalities.csv",
#                       col_types = cols(
#                         .default               = col_character(),
#                         legal_amazon           = col_logical(),
#                         amazon_area_km2        = col_number(),
#                         atlantic_area_km2      = col_number(),
#                         caatinga_area_km2      = col_number(),
#                         cerrado_area_km2       = col_number(),
#                         islands_area_km2       = col_number(),
#                         pampa_area_km2         = col_number(),
#                         pantanal_area_km2      = col_number(),
#                         mun_area_km2           = col_number(),
#                         mun_protected_area_km2 = col_number(),
#                         yield_C                = col_number(),
#                         yield_S_pam            = col_number(),
#                         yield_S_mb             = col_number()
#                         )
#                       )

#sf::st_write(sf_grid,     paste0("data_outputs/1_cell_grid/6_complete_grid/sf_",set_name,"_grid_",param_plot_km,"km.shp"), delete_layer=TRUE)


# (2) join data -----------------------------------------------------------
df_mun_data <- df_municipalities %>%
  dplyr::select(
    code_mun, state, group, year, legal_amazon,
    yield_C:yield_S_mb
    ) %>%
  dplyr::filter(
    year %in% c("1985", "1995", "2006", "2017", "2024")
    )

df_grid <- df_grid %>%
  dplyr::left_join(
    df_mun_data, 
    by = join_by(code_mun, year)
    ) %>%
  dplyr::select(
    cell_id, centroid_x, centroid_y,
    code_mun, state, code_biome, legal_amazon, protected,
    group, year,
    cv_V, cv_I_C, cv_I_S, cv_I_F, cv_D,
    yield_C, yield_S_pam, yield_S_mb,
    yield_S_FAO = FAO_soy_yield
    )

rm(df_mun_data)
