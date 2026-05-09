# This script loads municipality-level geographic information
#
# we want to consider municipalities that were created after 1985 so the data
# is comparable throughout the period

# (1) load inputs ---------------------------------------------------------
# load table with municipality codes equivalence
brazil_mun_df <- read_csv("data_inputs/BdD/brasil_mun.csv", col_types = cols(.default = col_character())) %>%
  dplyr::select(
    code_mun   = id_municipio,              name_mun     = nome,
    state      = sigla_uf,                  code_micro   = id_microrregiao,
    name_micro = nome_microrregiao,         code_rgi     = id_regiao_imediata,
    name_rgi   = nome_regiao_imediata,      code_meso    = id_mesorregiao,
    name_meso  = nome_mesorregiao,          code_rgint   = id_regiao_intermediaria,
    name_rgint = nome_regiao_intermediaria, legal_amazon = amazonia_legal
  ) %>%
  # remove Fernando de Noronha (PE)
  dplyr::filter(code_mun != "2605459")

# load table with Áreas Minimamente Comparáveis - keep track of new municipalities
brazil_amc_df <- read_csv("data_inputs/BdD/brasil_amc.csv", col_types = cols(.default = col_character())) %>%
  # consider the period from 1980 to 2010
  dplyr::filter(ano_de=="1980" & ano_para=="2010") %>%
  # select and rename columns
  dplyr::select(
    code_mun  = id_municipio,
    code_amc  = id_amc) %>%
  # remove Fernando de Noronha (PE)
  dplyr::filter(code_mun != "2605459")

# load municipalities shapefiles from 2024
cities_sf <- sf::st_read("data_outputs/1_cell_grid/1_reproject/cities_EPSG5880.shp", quiet = TRUE) %>%
  # remove Fernando de Noronha (PE)
  dplyr::filter(code_mun != "2605459") %>%
  # add area column
  dplyr::mutate(mun_area_km2 = as.numeric(sf::st_area(geometry))/1e6)
  
# load biomes shapefiles
biomes_sf <- sf::st_read("data_outputs/1_cell_grid/1_reproject/biomes_EPSG5880.shp", quiet = TRUE)
# load protected area shapefiles
protected_sf <- sf::st_read("data_outputs/1_cell_grid/1_reproject/protected_areas_EPSG5880.shp", quiet = TRUE)
# load FAO GAEZ's raster for attainable soybean yield
fao_rast <- terra::rast("data_outputs/1_cell_grid/1_reproject/fao_brazil_soy_yield.tif")

# (2) find predominant biome per municipality -----------------------------
tic("Find predominant biome per municipality")
mun_biome_df <- cities_sf %>%
  sf::st_intersection(biomes_sf) %>%
  dplyr::mutate(intersect_area = st_area(geometry)) %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(code_mun) %>%
  dplyr::slice_max(intersect_area) %>%
  dplyr::ungroup() %>%
  dplyr::select(code_mun, name_biome)
rm(biomes_sf)
toc()

# (3) calculate municipality protected area -------------------------------
## check if protected areas overlap
#overlaps <- sf::st_overlaps(protected_sf)
#any(lengths(overlaps)>0)

tic("Calculate protected area per municipality")
# intersect municipalities' and protected areas' polygons
mun_protected_sf <- sf::st_intersection(cities_sf, protected_sf)

# compute the area (km2) of each intersection and summarise at the municipality level
mun_protected_df <- mun_protected_sf %>%
  dplyr::select(code_mun, geometry) %>%
  dplyr::mutate(protected_area_km2 = as.numeric(sf::st_area(geometry))/1e6) %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(code_mun) %>%
  dplyr::summarise(mun_protected_area_km2 = sum(protected_area_km2))
# clean up polygon data
rm(protected_sf, mun_protected_sf)
toc()

# (4) calculate municipality attainable soybean yield ---------------------
tic("Calculate municipality average attainable soybean yield")
mun_soy_yield_df <- exactextractr::exact_extract(fao_rast, cities_sf,
                                                 fun = "mean",
                                                 force_df = TRUE,
                                                 append_cols = "code_mun")

# FAO's yield is in kg/ha, I need values in tons/ha
names(mun_soy_yield_df)[2] <- "FAO_soy_yield"
mun_soy_yield_df$FAO_soy_yield <- mun_soy_yield_df$FAO_soy_yield/1000
# replace NAs with 0s
mun_soy_yield_df$FAO_soy_yield[is.na(mun_soy_yield_df$FAO_soy_yield)] <- 0
# clean up
rm(fao_rast)
toc()

# (5) join everything in a single table -----------------------------------
cities_df <- cities_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::select(code_mun, mun_area_km2)

df_brazil_mun <- brazil_amc_df %>%
  dplyr::full_join(
    brazil_mun_df,
    by = "code_mun"
  ) %>%
  dplyr::full_join(
    mun_biome_df,
    by = "code_mun"
  ) %>%
  dplyr::full_join(
    cities_df, 
    by = "code_mun"
  ) %>%
  dplyr::full_join(
    mun_protected_df,
    by = "code_mun"
  ) %>%
  dplyr::full_join(
    mun_soy_yield_df,
    by = "code_mun"
  )

# fix column 'legal_amazon': should be logical, is character
df_brazil_mun$legal_amazon <- as.logical(as.integer(df_brazil_mun$legal_amazon))
# fix column 'mun_protected_area_km2': should be 0 for municipalities without protected area, is NA
df_brazil_mun$mun_protected_area_km2[is.na(df_brazil_mun$mun_protected_area_km2)] <- 0
# fix column 'mun_protected_area_km2': should never be higher than 
mun_fix <- df_brazil_mun$mun_protected_area_km2 > df_brazil_mun$mun_area_km2
df_brazil_mun$mun_protected_area_km2[mun_fix] <- df_brazil_mun$mun_area_km2[mun_fix]
# clean up
rm(brazil_amc_df, brazil_mun_df, mun_biome_df, mun_protected_df, mun_soy_yield_df,
   cities_sf, cities_df, mun_fix)

# (5) deal with missing code_amc values -----------------------------------

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

# (6) group municipalities ------------------------------------------------
# does any AMC get redundant when the model is initialized at 1995, 2006 or 2017?
# (a) get unique values of code_mun that are available in each agriculture census year
# 1995 agriculture census: 4956 unique municipality ids
codes_1995 <- readxl::read_excel("data_inputs/IBGE/agriculture_census/1995/tabela314_EDITED.xlsx", col_types = "text") %>%
              dplyr::select(code_mun = `Cód.`)
# 2006 agriculture census: 5548 unique municipality ids
codes_2006 <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2006/tabela838_EDITED.xlsx", col_types = "text") %>%
              dplyr::select(code_mun = `Cód.`)
# 2017 agriculture census: 5563 unique municipality ids
codes_2017 <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2017/tabela6880_activity_EDITED.xlsx", col_types="text") %>% 
              tidyr::fill(`Cód.`, `Município`, .direction="down") %>%
              dplyr::select(code_mun = `Cód.`)
# identify which codes are available each year
code_mun_in_1995 <- unique(codes_1995$code_mun) # 4956 unique values
code_mun_in_2006 <- unique(codes_2006$code_mun) # 5548 unique values
code_mun_in_2017 <- unique(codes_2017$code_mun) # 5563 unique values
rm(codes_1995, codes_2006, codes_2017)

# (b) find out which code_amc are associated with municipalities that are missing from the data
df_codes <- df_brazil_mun %>% dplyr::select(code_mun, code_amc)
df_codes$in_1995 <- df_codes$code_mun %in% code_mun_in_1995
df_codes$in_2006 <- df_codes$code_mun %in% code_mun_in_2006
df_codes$in_2017 <- df_codes$code_mun %in% code_mun_in_2017
amc_has_mun_missing_in_1995 <- df_codes$code_amc[!df_codes$in_1995]
amc_has_mun_missing_in_2006 <- df_codes$code_amc[!df_codes$in_2006]
amc_has_mun_missing_in_2017 <- df_codes$code_amc[!df_codes$in_2017]

# (c) find out which municipalities are present in the data but share an AMC with an unavailable municipality
df_codes$treat_as_amc_1995 <- df_codes$code_amc %in% amc_has_mun_missing_in_1995
df_codes$treat_as_amc_2006 <- df_codes$code_amc %in% amc_has_mun_missing_in_2006
df_codes$treat_as_amc_2017 <- df_codes$code_amc %in% amc_has_mun_missing_in_2017

# (d) define appropriate code_amc according to the selected initialization year
# for 1985, just use the AMC code
df_codes$group_1985 <- df_codes$code_amc
# for 1995, use the AMC code for the municipalities that have to be merged (those that are unavailable and those that share
# an AMC with unavailable municipalities) and use the municipality code otherwise
df_codes$group_1995 <- df_codes$code_mun
df_codes$group_1995[df_codes$treat_as_amc_1995] <- df_codes$code_amc[df_codes$treat_as_amc_1995]
# for 2006, just as before
df_codes$group_2006 <- df_codes$code_mun
df_codes$group_2006[df_codes$treat_as_amc_2006] <- df_codes$code_amc[df_codes$treat_as_amc_2006]
# for 2017, just as before
df_codes$group_2017 <- df_codes$code_mun
df_codes$group_2017[df_codes$treat_as_amc_2017] <- df_codes$code_amc[df_codes$treat_as_amc_2017]

# (e) tidy and clean up
df_brazil_mun <- df_codes %>% 
  dplyr::select(code_mun, group_1985, group_1995, group_2006, group_2017) %>%
  dplyr::left_join(df_brazil_mun, by = "code_mun") %>%
  # get rid of Fernando de Noronha (code_mun=="2605459")
  dplyr::filter(code_mun != "2605459")
rm(amc_has_mun_missing_in_1995, amc_has_mun_missing_in_2006, amc_has_mun_missing_in_2017,
   code_mun_in_1995, code_mun_in_2006, code_mun_in_2017, df_codes)
