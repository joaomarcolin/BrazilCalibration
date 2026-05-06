# This script treats municipality-level data from IBGE's Census of Agriculture
# for 1995, 2006 and 2017

# (1) recover the individual tables ---------------------------------------
list2env(census2, envir=.GlobalEnv) # worker_1995  area_2017    farm_2017   area_activity_1995/2006/2017
list2env(census3, envir=.GlobalEnv) # worker1_2006 worker2_2006 worker_2017
rm(census2, census3)

# (2) create municipality-level table -------------------------------------
# (2.1) deal with data on farm area and number of farms by area groups
# (a) treat tables from census2
area_2017 <- area_2017 %>% dplyr::transmute(code_mun, year="2017", area_group=class, farm_area_ha=value)
farm_2017 <- farm_2017 %>% dplyr::transmute(code_mun, year="2017", area_group=class, n_farms=value)
# (b) join tables
mun_area_group <- area_2017 %>%
  dplyr::full_join(farm_2017,
                   by = join_by(code_mun, year, area_group))
# (c) clean up
rm(area_2017,farm_2017)
# some rows have farm_area_ha==0 even when n_farms>0 - this can't be right


# (2.2) deal with data on farm area by activity groups
# (a) treat tables from census2
area_activity_1995 <- area_activity_1995 %>% dplyr::transmute(code_mun, year="1995", activity_group=class, activity_area=value)
area_activity_2006 <- area_activity_2006 %>% dplyr::transmute(code_mun, year="2006", activity_group=class, activity_area=value)
area_activity_2017 <- area_activity_2017 %>% dplyr::transmute(code_mun, year="2017", activity_group=class, activity_area=value)
# (b) join tables
mun_activity_area <- rbind(area_activity_1995, area_activity_2006, area_activity_2017)
# (c) clean up
rm(area_activity_1995, area_activity_2006, area_activity_2017)


# (2.3) deal with data on workers
# (a) treat table from census2 - add a "day_group" column since this is available on the tables from census3
worker_1995 <- worker_1995 %>% dplyr::transmute(code_mun, year="1995", activity_group=class, day_group="total", workers=value)
# (b) treat tables from census3
# (b.1) 2006 data was divided in worker1 (relatives of the farmer) and worker2 (non-relatives of the farmer). I sum these.
worker1_2006 <- worker1_2006 %>% dplyr::transmute(code_mun, year="2006", activity_group=activity, day_group=class, worker1=value)
worker2_2006 <- worker2_2006 %>% dplyr::transmute(code_mun, year="2006", activity_group=activity, day_group=class, worker2=value)
worker_2006  <- worker1_2006 %>%
  dplyr::full_join(worker2_2006,
                   by = join_by(code_mun, year, activity_group, day_group)) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(code_mun, year, activity_group, day_group,
                   workers = worker1 + worker2)
# (b.2) 2017 data already gives us the total number of workers
worker_2017 <- worker_2017  %>% dplyr::transmute(code_mun, year="2017", activity_group=activity, day_group=class, workers=value)
# (c) join tables
mun_workers <- rbind(worker_1995, worker_2006, worker_2017)
# (d) clean up
rm(worker_1995, worker1_2006, worker2_2006, worker_2006, worker_2017)


# (2.4) check NAs
#
# note that the following municipalities are present in the 1995 census but not on the 2006 census:
# 3305109  São João de Meriti (RJ)
# 3515004      Embu das Artes (SP)
# 3529401                Mauá (SP)
# 3543303      Ribeirão Pires (SP)
# 3544103 Rio Grande da Serra (SP)
#
# the following municipality is present in the 1995 census but not on the 2017 census:
# 2919926       Madre de Deus (BA)
#
# the following municipality is present in the 2006 census but not on the 2017 census:
# 2919926       Madre de Deus (BA)
#
# I don't adress this issue, these will show up as NAs
#
# (a) check that all municipalities are included in all tables
mun_area_group    <- df_mun %>% dplyr::left_join(mun_area_group,    by="code_mun") %>% dplyr::transmute(code_mun, state, year, activity_group="total", day_group="total", area_group, farm_area_ha, n_farms)
mun_activity_area <- df_mun %>% dplyr::left_join(mun_activity_area, by="code_mun") %>% dplyr::transmute(code_mun, state, year, activity_group,         day_group="total",             activity_area)
mun_workers       <- df_mun %>% dplyr::left_join(mun_workers,       by="code_mun") %>% dplyr::transmute(code_mun, state, year, activity_group,         day_group,                     workers)
# (b) fix NA rows due to some municipalities existing in df_mun but not in any of the census tables
# there are 7 municipalities with no data for land concentration in 2017 (5 in SP, 1 in RJ and 'Madre de Deus' in BA)
mun_area_group$year[is.na(mun_area_group$year)]                           <- "2017"
mun_area_group$area_group[is.na(mun_area_group$area_group)]               <- "total"
# there are 6 municipalities with no data for area by activity group (5 in SP, 1 in RJ)
mun_activity_area$year[is.na(mun_activity_area$year)]                     <- "2006"
mun_activity_area$activity_group[is.na(mun_activity_area$activity_group)] <- "total"
# there are 6 municipalities with no data on rural workers (5 in SP, 1 in RJ)
mun_workers$year[is.na(mun_workers$year)]                                 <- "2006"
mun_workers$activity_group[is.na(mun_workers$activity_group)]             <- "total"
mun_workers$day_group[is.na(mun_workers$day_group)]                       <- "total"


# (2.5) join everything in a single table
mun_census <- mun_activity_area %>%
  dplyr::full_join(mun_workers,    by=join_by(code_mun, state, year, activity_group, day_group)) %>%
  dplyr::full_join(mun_area_group, by=join_by(code_mun, state, year, activity_group, day_group)) %>%
  dplyr::select(code_mun, year, area_group, activity_group, day_group,
                farm_area_ha, n_farms, activity_area, workers)
# fix NAs in 'area_group' column
mun_census$area_group[is.na(mun_census$area_group)] <- "total"
# add grouping columns and remove Fernando de Noronha (code_mun=="2605459")
df_census_mun <- df_mun %>%
  dplyr::full_join(mun_census, by="code_mun") %>%
  dplyr::filter(code_mun != "2605459")
# clean up
rm(mun_census, df_mun)
