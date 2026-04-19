# This script takes data from IBGE's Census of Agriculture and generates data frames
# with state-level data on land concentration and rural workers
#
# from "0_load_census_data.R", we have three lists of dataframes:
# census1 contains dfs with columns "state" "class" "value"
# area_1985/1995/2006
# farm_1985/1995/2006
#
# census2 contains dfs with columns "code_mun" "name_mun" "class" "value"
# worker_1995
# area_activity_1995/2006/2017
# area_2017
# farm_2017
#
# census3 contains dfs with columns "code_mun" "name_mun" "activity" "class" "value"
# worker*_2006
# worker_2017

# (1) recode state names --------------------------------------------------
dict_state <- c("Acre"              ="AC", "Alagoas"             ="AL",
                "Amapá"             ="AP", "Amazonas"            ="AM",
                "Bahia"             ="BA", "Ceará"               ="CE",
                "Distrito Federal"  ="DF", "Espírito Santo"      ="ES",
                "Goiás"             ="GO", "Maranhão"            ="MA",
                "Mato Grosso"       ="MT", "Mato Grosso do Sul"  ="MS",
                "Minas Gerais"      ="MG", "Pará"                ="PA",
                "Paraíba"           ="PB", "Paraná"              ="PR",
                "Pernambuco"        ="PE", "Piauí"               ="PI",
                "Rio de Janeiro"    ="RJ", "Rio Grande do Norte" ="RN",
                "Rio Grande do Sul" ="RS", "Rondônia"            ="RO",
                "Roraima"           ="RR", "Santa Catarina"      ="SC",
                "São Paulo"         ="SP", "Sergipe"             ="SE",
                "Tocantins"         ="TO")

# auxiliary function to use "dict_state" to change values of column "state" in a given data.frame
recode_state <- function(df) {
  df <- df %>% 
    dplyr::mutate(state = recode(state,
                                 !!!dict_state,
                                 .default = state,
                                 .missing=state
                                 )
                  )
  return(df)
} # closes recode_state(df)

# apply the function to all dataframes in a list
census1 <- purrr::map(census1, recode_state)

# clean up
rm(recode_state, dict_state)

# (2) recode "class" columns ----------------------------------------------

dict_class <- c(
  # (1) area groups (1km2  =  100 ha, 25km2 = 2500 ha)
  # 1985, 1995 and 2006 censuses:
  "Total"                         ="total",
  "Menos de 100 ha"               ="under100ha",
  "Menos de 10 ha"                ="ignore",
  "10 a menos de 100 ha"          ="ignore",
  "100 a menos de 1.000 ha"       ="from100to1kha",
  "1.000 ha e mais"               ="over1kha",
  # 2017 census:
  "Mais de 0 a menos de 0,1 ha"   ="under100ha",
  "De 0,1 a menos de 0,2 ha"      ="under100ha",
  "De 0,2 a menos de 0,5 ha"      ="under100ha",
  "De 0,5 a menos de 1 ha"        ="under100ha",
  "De 1 a menos de 2 ha"          ="under100ha",
  "De 2 a menos de 3 ha"          ="under100ha",
  "De 3 a menos de 4 ha"          ="under100ha",
  "De 4 a menos de 5 ha"          ="under100ha",
  "De 5 a menos de 10 ha"         ="under100ha",
  "De 10 a menos de 20 ha"        ="under100ha",
  "De 20 a menos de 50 ha"        ="under100ha",
  "De 50 a menos de 100 ha"       ="under100ha",
  "De 100 a menos de 200 ha"      ="from100to1kha",
  "De 200 a menos de 500 ha"      ="from100to1kha",
  "De 500 a menos de 1.000 ha"    ="from100to1kha",
  "De 1.000 a menos de 2.500 ha"  ="from1kto2500ha",
  "De 2.500 a menos de 10.000 ha" ="from2500to10kha",
  "De 10.000 ha e mais"           ="over10kha",
  "Produtor sem área"             ="landless",
  # (2) activity groups:
  # 1985 and 1995 censuses:
  "Total"                                    ="total",
  "Lavoura temporária"                       ="temp_crop",
  "Lavoura permanente"                       ="perm_crop",
  "Pecuária"                                 ="cattle",
  "Produção mista (lavoura e pecuária)"      ="cattle",
  "Silvicultura e exploração florestal"      ="forestry",
  # 2006 and 2017 censuses:
  "Produção de lavouras temporárias"         ="temp_crop",
  "Produção de lavouras permanentes"         ="perm_crop",
  "Pecuária e criação de outros animais"     ="cattle",
  "Produção florestal - florestas plantadas" ="forestry",
  "Produção florestal - florestas nativas"   ="forestry",
  # (3) work days groups:
  # 2006 census:
  "Total"                     ="total",
  "Menos de 60 dias"          ="under180days",
  "De 60 a menos de 180 dias" ="under180days",
  "180 dias e mais"           ="over180days",
  # 2017 census:
  "Menos de 90 dias"          ="under180days",
  "De 90 a menos de 180 dias" ="under180days",
  "De 180 dias e mais"        ="over180days"
  )

# auxiliary function
recode_class <- function(df,                  # a dataframe
                         df_level,            # character: "mun" for municipality-level data, "state" for state level data
                         df_activity=FALSE) { # logical: TRUE for dataframes that have column `activity`
  if (df_level=="mun" & df_activity==TRUE) {
    # if the tables have municipality-level data and include an "activity" column,
    # recode the "class" and "activity" columns and summarise values at the municipality level
    df <- df %>%
      dplyr::mutate(class = recode(class, !!!dict_class, .default = class, .missing=class)) %>%
      dplyr::mutate(activity = recode(activity, !!!dict_class, .default=activity, .missing=activity)) %>%
      dplyr::group_by(code_mun, name_mun, activity, class) %>%
      dplyr::summarise(value=sum(value, na.rm=TRUE), .groups="drop")
    # return dataframe
    return(df)
  } else if (df_level=="mun" & df_activity==FALSE) {
    # if the tables have municipality-level data and don't include an "activity" column,
    # recode the "class" column and summarise values at the municipality level
    df <- df %>%
      dplyr::mutate(class = recode(class, !!!dict_class, .default = class, .missing=class)) %>%
      dplyr::group_by(code_mun, name_mun, class) %>%
      dplyr::summarise(value=sum(value, na.rm=TRUE), .groups="drop")
    return(df)
  } else if (df_level=="state") {
    # same as above, but for tables with state-level data
    df <- df %>%
      dplyr::mutate(class = recode(class, !!!dict_class, .default = class, .missing=class)) %>%
      dplyr::group_by(state, class) %>%
      dplyr::summarise(value=sum(value, na.rm=TRUE), .groups="drop") %>%
      dplyr::ungroup()
    return(df)
  } else {
    warning(paste("df_level must be either 'mun' or 'state'."))
    return()
  } # close if-else structure
} # closes recode_class(df, df_level, df_activity)

# recode 
census1 <- purrr::map(census1, recode_class, df_level="state", df_activity=FALSE)
census2 <- purrr::map(census2, recode_class, df_level="mun"  , df_activity=FALSE)
census3 <- purrr::map(census3, recode_class, df_level="mun"  , df_activity=TRUE)
rm(recode_class, dict_class)

# (3) recover the individual tables ---------------------------------------
list2env(census1, envir=.GlobalEnv)
list2env(census2, envir=.GlobalEnv)
list2env(census3, envir=.GlobalEnv)
rm(census1, census2, census3)

# (4) create state-level table --------------------------------------------
# (4.1) rename columns and add year
area_1985 <- area_1985 %>% dplyr::select(state, area_group=class, farm_area_ha=value) %>% mutate(year="1985")
area_1995 <- area_1995 %>% dplyr::select(state, area_group=class, farm_area_ha=value) %>% mutate(year="1995")
area_2006 <- area_2006 %>% dplyr::select(state, area_group=class, farm_area_ha=value) %>% mutate(year="2006")
farm_1985 <- farm_1985 %>% dplyr::select(state, area_group=class,      n_farms=value) %>% mutate(year="1985")
farm_1995 <- farm_1995 %>% dplyr::select(state, area_group=class,      n_farms=value) %>% mutate(year="1995")
farm_2006 <- farm_2006 %>% dplyr::select(state, area_group=class,      n_farms=value) %>% mutate(year="2006")

# (4.2) bind tables that have the same variable
state_area <- rbind(area_1985, area_1995, area_2006)
state_farm <- rbind(farm_1985, farm_1995, farm_2006)

# (4.3) join everything in a single table
census_state <- state_area %>% 
  dplyr::full_join(state_farm, by=join_by(state,area_group,year)) %>%
  dplyr::select(state, year, area_group, farm_area_ha, n_farms) %>%
  dplyr::filter(area_group != "ignore") # to avoid double counting farm area, these observations must be removed

# (4.4) clean up
rm(area_1985 ,area_1995, area_2006,
   farm_1985 ,farm_1995, farm_2006,
   state_area, state_farm)

# (5) create municipality-level table -------------------------------------
# (5.1) deal with data on farm area and number of farms by area groups
# (a) treat tables from census2
area_2017 <- area_2017 %>% dplyr::transmute(code_mun, name_mun, year="2017", area_group=class, farm_area_ha=value)
farm_2017 <- farm_2017 %>% dplyr::transmute(code_mun, name_mun, year="2017", area_group=class, n_farms=value)
# (b) join tables
mun_area_group <- area_2017 %>%
  dplyr::full_join(farm_2017,
                   by = join_by(code_mun, name_mun, year, area_group))
# (c) clean up
rm(area_2017,farm_2017)

# (5.2) deal with data on farm area by activity groups
# (a) treat tables from census2
area_activity_1995 <- area_activity_1995 %>% dplyr::transmute(code_mun, name_mun, year="1995", activity_group=class, activity_area=value)
area_activity_2006 <- area_activity_2006 %>% dplyr::transmute(code_mun, name_mun, year="2006", activity_group=class, activity_area=value)
area_activity_2017 <- area_activity_2017 %>% dplyr::transmute(code_mun, name_mun, year="2017", activity_group=class, activity_area=value)
# (b) join tables
mun_activity_area <- rbind(area_activity_1995, area_activity_2006, area_activity_2017)
# (c) clean up
rm(area_activity_1995, area_activity_2006, area_activity_2017)

# (5.3) deal with data on workers
# (a) treat table from census2 - add a "day_group" column since this is available on the tables from census3
worker_1995 <- worker_1995 %>% dplyr::transmute(code_mun, name_mun, year="1995", activity_group=class, day_group="total", workers=value)
# (b) treat tables from census3
# (b.1) 2006 data was divided in worker1 (relatives of the farmer) and worker2 (non-relatives of the farmer). I sum these.
worker1_2006 <- worker1_2006 %>% dplyr::transmute(code_mun, name_mun, year="2006", activity_group=activity, day_group=class, worker1=value)
worker2_2006 <- worker2_2006 %>% dplyr::transmute(code_mun, name_mun, year="2006", activity_group=activity, day_group=class, worker2=value)
worker_2006  <- worker1_2006 %>%
  dplyr::full_join(worker2_2006,
                   by = join_by(code_mun, name_mun, year, activity_group, day_group)) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(code_mun, name_mun, year, activity_group, day_group,
                   workers = worker1 + worker2)
# (b.2) 2017 data already gives us the total number of workers
worker_2017 <- worker_2017  %>% dplyr::transmute(code_mun, name_mun, year="2017", activity_group=activity, day_group=class, workers=value)
# (c) join tables
mun_workers <- rbind(worker_1995, worker_2006, worker_2017)
# (d) clean up
rm(worker_1995, worker1_2006, worker2_2006, worker_2006, worker_2017)

# (5.4) join everything in a single table
#
# note that the following municipalities are present in the 1995 census but not on the 2006 census:
# 1  3305109  São João de Meriti (RJ)
# 2  3515004      Embu das Artes (SP)
# 3  3529401                Mauá (SP)
# 4  3543303      Ribeirão Pires (SP)
# 5  3544103 Rio Grande da Serra (SP)
#
# the following municipality is present in the 1995 census but not on the 2017 census:
# 6  2919926       Madre de Deus (BA)
#
# the following municipality is present in the 2006 census but not on the 2017 census:
# 7  2919926       Madre de Deus (BA)
#
# I don't adress this issue, these will show up as NAs
#
# (a) prepare tables for joining
mun_area_group    <- mun_area_group    %>% dplyr::transmute(code_mun, name_mun, year, area_group,         activity_group="total", day_group="total", farm_area_ha, n_farms)
mun_activity_area <- mun_activity_area %>% dplyr::transmute(code_mun, name_mun, year, area_group="total", activity_group,         day_group="total", activity_area)
mun_workers       <- mun_workers       %>% dplyr::transmute(code_mun, name_mun, year, area_group="total", activity_group,         day_group,         workers)
# (b) join everything
census_mun <- mun_area_group %>%
  dplyr::full_join(mun_activity_area, by=join_by(code_mun, name_mun, year, area_group, activity_group, day_group)) %>%
  dplyr::full_join(mun_workers,       by=join_by(code_mun, name_mun, year, area_group, activity_group, day_group)) 
# (c) add a 'state' column and arrange table
census_mun <- census_mun %>%
  # extract "state" from "name_mun"
  dplyr::mutate(state=str_extract(name_mun,"(?<=\\()\\w{2}(?=\\))")) %>%
  # arrange column order
  dplyr:: select(code_mun, name_mun, state, year, area_group, activity_group, day_group,
                 farm_area_ha, n_farms, activity_area, workers, state) %>%
  # arrange row order
  dplyr::arrange(code_mun, year, area_group, activity_group, day_group)
# (d) clean up
rm(mun_area_group, mun_activity_area, mun_workers)

# (6) aggregate municipality-level data at the state level ----------------
# (6.1) sum numeric values from municipalities in the same state
mun_agg <- census_mun %>%
  # get rid of municipality identifiers
  dplyr::select(-code_mun, -name_mun) %>%
  # get rid of "landless" area group
  dplyr::filter(area_group != "landless") %>%
  # group variables
  dplyr::group_by(state, year, area_group, activity_group, day_group) %>%
  # summarise numeric values
  dplyr::summarise(farm_area_ha  = sum(farm_area_ha,  na.rm=TRUE),
                   n_farms       = sum(n_farms,       na.rm=TRUE),
                   activity_area = sum(activity_area, na.rm=TRUE),
                   workers       = sum(workers,       na.rm=TRUE),
                   .groups = "drop")

# (6.2) make sure that 'census_state' has all the same identifier columns as 'mun_agg'
census_state <- census_state %>%
  dplyr::transmute(state, year, area_group, activity_group="total", day_group="total",
                   farm_area_ha, n_farms)

# note that the data in 'mun_agg' covers 1995, 2006 and 2017;
# while the data in 'census_mun' covers 1985, 1995 and 2006
#
# also, the "area_group" classes in 2017 are different from the "area_group"
# classes in 1985, 1995 and 2006
#
# finally, both 'mun_agg' and 'census_state' have columns named
# "farm_area_ha" and "n_farms" - need to deal with this before joining

# (6.3) separate the data in 'mun_agg' in 2017 and previous to 2017
mun_agg_2017 <- mun_agg %>% dplyr::filter(year  ==  "2017")
mun_agg_prev <- mun_agg %>% dplyr::filter(year %in% c("1995","2006"))
rm(mun_agg)

# (6.4) join pre-2017 data to census_state
# we just need columns "activity_area" and "workers", since 'census_state'
# already has data on "farm_area_ha" and "n_farms" for the years before 2017
census_state <- census_state %>%
  dplyr::left_join(
    dplyr::select(mun_agg_prev, -farm_area_ha, -n_farms),
    by = join_by(state, year, area_group, activity_group, day_group)
  )
rm(mun_agg_prev)

# (6.5) join 2017 data to census_state
# we could just rbind 'mun_agg_2017' to 'census_state', but the "area group" classes
# of 2017 are more disaggregated. I don't want to lose this information but I also
# want it to be comparable with the previous year
#
# solution: use both aggregation levels for 2017
#
# (a) recode the new "area_group" classes into the previous classes
mun_agg_2017_as_prev <- mun_agg_2017
mun_agg_2017_as_prev$area_group[mun_agg_2017_as_prev$area_group=="from1kto2500ha"]  <- "over1kha"
mun_agg_2017_as_prev$area_group[mun_agg_2017_as_prev$area_group=="from2500to10kha"] <- "over1kha"
mun_agg_2017_as_prev$area_group[mun_agg_2017_as_prev$area_group=="over10kha"]       <- "over1kha"
# (b) summarise table
mun_agg_2017_as_prev <- mun_agg_2017_as_prev %>%
  dplyr::group_by(state, year, area_group, activity_group, day_group) %>%
  dplyr::summarise(farm_area_ha  = sum(farm_area_ha,  na.rm=TRUE),
                   n_farms       = sum(n_farms,       na.rm=TRUE),
                   activity_area = sum(activity_area, na.rm=TRUE),
                   workers       = sum(workers,       na.rm=TRUE),
                   .groups = "drop")
# (c) avoid double counting: take only the rows with the new "over1kha" value for area_group
mun_agg_2017_as_prev <- mun_agg_2017_as_prev %>%
  dplyr::filter(area_group=="over1kha")
# (d) bind 2017 data to 'census_state'
census_state <- rbind(census_state, mun_agg_2017, mun_agg_2017_as_prev)
# (e) make NAs explicit and arrange table
census_state <- census_state %>%
  # NAs were ignored when I aggregated municipality-level data - make them explicit
  dplyr::mutate(across(c(farm_area_ha, n_farms, activity_area, workers), ~na_if(.x,0))) %>%
  # arrange row order
  dplyr::arrange(state, year, area_group, activity_group, day_group)
# (f) clean up
rm(mun_agg_2017, mun_agg_2017_as_prev)


