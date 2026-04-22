# This script deals with municipalities that are present in the cell grid
# but not in some of the other datasets - that is usually the case with municipalities
# created after 1985

df_brazil_mun    <- readr::read_csv("data_outputs/4_geography/municipalities.csv")
df_census_mun    <- readr::read_csv( "data_outputs/1_census_data/IBGE_agricensus_municipalities.csv")
df_yearly_mun    <- readr::read_csv(  "data_outputs/2_yearly_data/municipality_data.csv")
grid_df          <- readr::read_csv("data_outputs/3_cell_grid/6_complete_grid/brazil_grid_df_5km.csv")

# (1) check which municipalities are aways available ----------------------

# municipality codes from shapefiles (5570 unique values)
codes_all <- unique(df_brazil_mun$code_mun)

# municipality codes from agriculture census (4956 to 5563 unique values, depending on the year)
codes_census1995 <- unique(dplyr::filter(df_census_mun, year=="1995")$code_mun)
codes_census2006 <- unique(dplyr::filter(df_census_mun, year=="2006")$code_mun)
codes_census2017 <- unique(dplyr::filter(df_census_mun, year=="2017")$code_mun)
codes_census <- intersect(codes_census1995, intersect(codes_census2006, codes_census2017))
rm(df_census_mun, codes_census1995, codes_census2006, codes_census2017)

# municipality codes from yearly agriculture surveys (4104 to 5545 unique values, depending on the year)
# consider only pam and ppm values, check which municipalities show up in at least one of the surveys for a given year
df_yearly_mun <- df_yearly_mun %>% dplyr::select(code_mun, year, pam_area_ha, pam_output_ton, ppm_herd)
df_yearly_mun$is_available <- !(is.na(df_yearly_mun$pam_area_ha) &
                                is.na(df_yearly_mun$pam_output_ton) &
                                is.na(df_yearly_mun$ppm_herd))
# initialize list to be filled with unique available municipality codes for each survey year
list_yearly <- vector("list", length(1985:2024))
names(list_yearly) <- paste0("codes_survey",1985:2024)
# fill out list
for (yr in 1985:2024) {
  list_yearly[[paste0("codes_survey",yr)]] <- unique(dplyr::filter(df_yearly_mun, year==yr, is_available==TRUE)$code_mun)
}
codes_yearly <- Reduce(intersect, list_yearly)
rm(list_yearly, yr, df_yearly_mun)

# (2) identify problematic municipalities ---------------------------------
codes_missing <- codes_all[!codes_all%in%codes_yearly] # 1520

df_mun_missing <- df_brazil_mun %>% dplyr::filter(code_mun %in% codes_missing)
