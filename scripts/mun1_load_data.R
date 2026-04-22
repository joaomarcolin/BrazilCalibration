# This script sorts out municipality and amc codes
#
# we want to consider municipalities that were created after 1985 so the data
# is comparable throughout the period

# (1) load brazil_amc and brazil_mun tables -------------------------------
# load table with muncipality codes equivalence
brazil_mun_df <- read_csv("data_inputs/BdD/brasil_mun.csv", col_types = cols(.default = col_character())) %>%
  dplyr::select(
    code_mun   = id_municipio,              name_mun     = nome,
    state      = sigla_uf,                  code_micro   = id_microrregiao,
    name_micro = nome_microrregiao,         code_rgi     = id_regiao_imediata,
    name_rgi   = nome_regiao_imediata,      code_meso    = id_mesorregiao,
    name_meso  = nome_mesorregiao,          code_rgint   = id_regiao_intermediaria,
    name_rgint = nome_regiao_intermediaria, legal_amazon = amazonia_legal
  )

# load table with Áreas Minimamente Comparáveis - keep track of new municipalities
brazil_amc_df <- read_csv("data_inputs/BdD/brasil_amc.csv", col_types = cols(.default = col_character())) %>%
  # consider the period from 1980 to 2010
  dplyr::filter(ano_de=="1980" & ano_para=="2010") %>%
  # select and rename columns
  dplyr::select(
    code_mun  = id_municipio,
    code_amc  = id_amc)

# load municipalities shapefiles from 2024
brazil_sf <- st_read("data_outputs/3_cell_grid/1_reproject/cities_EPSG5880.shp")

# load biomes shapefiles
biomes_sf <- st_read("data_outputs/3_cell_grid/1_reproject/biomes_EPSG5880.shp")

# (2) calculate biome area per municipality -------------------------------

# intersect municipalities' and biomes' polygons
mun_biome_sf <- sf::st_intersection(brazil_sf, biomes_sf)

# compute the area (km2) of each intersection
mun_biome_df <- mun_biome_sf %>%
  dplyr::mutate(area_km2 = as.numeric(sf::st_area(geometry))/1e6) %>%
  sf::st_drop_geometry()

# pivot to wide format: one row per municipality
mun_biome_df <- mun_biome_df %>%
  dplyr::select(code_mun, name_biome, area_km2) %>%
  tidyr::pivot_wider(
    names_from  = name_biome,
    names_glue  = "{name_biome}_{.value}",
    names_sort  = TRUE,
    values_from = area_km2,
    values_fill = 0
  )

# clean up polygon data
rm(brazil_sf, biomes_sf, mun_biome_sf)

# (3) join everything in a single table -----------------------------------

df_brazil_mun <- brazil_amc_df %>%
  dplyr::full_join(
    brazil_mun_df,
    by = "code_mun"
  ) %>%
  dplyr::full_join(
    mun_biome_df, 
    by = "code_mun"
  ) %>%
  dplyr::mutate(
    mun_area_km2 = amazon_area_km2 + atlantic_area_km2 + caatinga_area_km2 + cerrado_area_km2 + 
                   islands_area_km2 + pampa_area_km2 + pantanal_area_km2
  )

# clean up
rm(brazil_amc_df, brazil_mun_df, mun_biome_df)


# (4) deal with missing code_amc values -----------------------------------

## five municipalities in 'df_brazil_mun' are not assigned to any AMC
#df_brazil_mun[is.na(df_brazil_mun$code_amc),c(1,3,4)]
##   code_mun  name_mun           state
## 1 1504752   Mojuí dos Campos   PA
## 2 4212650   Pescaria Brava     SC
## 3 4220000   Balneário Rincão   SC
## 4 4314548   Pinto Bandeira     RS
## 5 5006275   Paraíso das Águas  MS

# Mojuí dos Campos (PA) was emancipated from Santarém (code_amc="2019") in 2013
df_brazil_mun$code_amc[df_brazil_mun$code_mun=="1504752"] <- "2019"
# Pescaria Brava (SC) was emancipated from Laguna (code_amc="14399") in 2012
df_brazil_mun$code_amc[df_brazil_mun$code_mun=="4212650"] <- "14399"
# Balneário Rincão (SC) was emancipated from Içara (code_amc="14376") in 2013
df_brazil_mun$code_amc[df_brazil_mun$code_mun=="4220000"] <- "14376"
# Pinto Bandeira (RS) was emancipated from Bento Gonçalves (code_amc="15037") in 2013
df_brazil_mun$code_amc[df_brazil_mun$code_mun=="4314548"] <- "15037"
# Paraíso das Águas (MS) was emancipated from Chapadão do Sul (code_amc="1073") in 2012
df_brazil_mun$code_amc[df_brazil_mun$code_mun=="5006275"] <- "1073"

## also, one AMC in 'df_brazil_mun' is associated with two different states
#dplyr::filter(df_brazil_mun, code_amc=="11052")[,1:6]
## that is because Ataléia (MG), code_mun="3104700", was mistakenly included in an AMC in ES;
## since this municipality existed in 1980, I give it an exclusive code_amc value
df_brazil_mun$code_amc[df_brazil_mun$code_mun=="3104700"] <- as.character(max(as.numeric(df_brazil_mun$code_amc))+1)

# Municipalities with island area:
#code_mun  name_mun             state
#2605459   Fernando de Noronha  PE
#2906907   Caravelas            BA
#3205309   Vitória              ES

## (5) check code_mun values across datasets -------------------------------
## at this point, we have three dataframes with municipality-level data:
## 'df_brazil_mun' stores identifiers, geographic and administrative divisions, biome area and total area
## 'df_census_mun' stores agriculture census data for 1995, 2006 and 2017
## 'df_yearly_mun' stores yearly data on soybean farming and cattle ranching output for 1985-2024
#
## 'df_brazil_mun': 5570 unique municipalities
## 'df_census_mun': 4956 to 5563 unique municipalities depending on census year
## 'df_yearly_mun': 5570 unique municipalities every year
#
#census_mun_1995 <- df_census_mun %>% dplyr::filter(year=="1995") %>% dplyr::select(code_mun, name_mun, state) %>% unique()
#census_mun_2006 <- df_census_mun %>% dplyr::filter(year=="2006") %>% dplyr::select(code_mun, name_mun, state) %>% unique()
#census_mun_2017 <- df_census_mun %>% dplyr::filter(year=="2017") %>% dplyr::select(code_mun, name_mun, state) %>% unique()
#
#mun_all_years   <- intersect(census_mun_1995$code_mun,
#                             census_mun_2006$code_mun)
#mun_all_years   <- intersect(mun_all_years,
#                             census_mun_2017$code_mun)
#length(mun_all_years)
## 4950 municipalities are present in every census year
#incomplete_mun <- rbind(census_mun_1995[!(census_mun_1995$code_mun%in%mun_all_years),],
#                        census_mun_2006[!(census_mun_2006$code_mun%in%mun_all_years),],
#                        census_mun_2017[!(census_mun_2017$code_mun%in%mun_all_years),])
#incomplete_mun <- unique(incomplete_mun)
## 614 municipalities don't show up in at least one of the census years
#
#incomplete_mun_yearly <- df_yearly_mun %>%
#  dplyr::filter(code_mun %in% incomplete_mun$code_mun)
## many of the municipalities that are missing in one of the census year have 
## non-NA values for PAM and PPM's values, which means that they already existed
## in the 1980s and are truly missing from the Census - the other were probably created
## after 1995.
