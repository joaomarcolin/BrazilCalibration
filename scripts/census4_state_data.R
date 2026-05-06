# This script treats state-level data from IBGE's Census of Agriculture
# for 1985, 1995, 2006 and 2017

# (1) recover the individual tables ---------------------------------------
list2env(census1, envir=.GlobalEnv)
rm(census1)

# (2) create state-level table --------------------------------------------
# (2.1) rename columns and add year
area_1985 <- area_1985 %>% dplyr::select(state, area_group=class, farm_area_ha=value) %>% mutate(year="1985")
area_1995 <- area_1995 %>% dplyr::select(state, area_group=class, farm_area_ha=value) %>% mutate(year="1995")
area_2006 <- area_2006 %>% dplyr::select(state, area_group=class, farm_area_ha=value) %>% mutate(year="2006")
farm_1985 <- farm_1985 %>% dplyr::select(state, area_group=class,      n_farms=value) %>% mutate(year="1985")
farm_1995 <- farm_1995 %>% dplyr::select(state, area_group=class,      n_farms=value) %>% mutate(year="1995")
farm_2006 <- farm_2006 %>% dplyr::select(state, area_group=class,      n_farms=value) %>% mutate(year="2006")

# (2.2) bind tables that have the same variable
state_area <- rbind(area_1985, area_1995, area_2006)
state_farm <- rbind(farm_1985, farm_1995, farm_2006)

# (2.3) join everything in a single table
df_census_state <- state_area %>% 
  dplyr::full_join(state_farm, by=join_by(state,area_group,year)) %>%
  dplyr::select(state, year, area_group, farm_area_ha, n_farms) %>%
  dplyr::filter(area_group != "ignore") # to avoid double counting farm area, these observations must be removed

# (2.4) clean up
rm(area_1985 ,area_1995, area_2006,
   farm_1985 ,farm_1995, farm_2006,
   state_area, state_farm)

# (3) aggregate municipality-level data at the state level ----------------
# area by activity group
state_activity_area <- mun_activity_area %>%
  dplyr::transmute(
    state, year,
    area_group="total", activity_group, day_group="total",
    activity_area
    ) %>%
  dplyr::group_by(state, year, area_group, activity_group, day_group) %>%
  dplyr::summarise(
    activity_area = sum(activity_area, na.rm=TRUE),
    .groups = "drop"
    )

# workers by activity group
state_workers <- mun_workers %>%
  dplyr::transmute(
    state, year,
    area_group="total", activity_group, day_group,
    workers
    ) %>%
  dplyr::group_by(state, year, area_group, activity_group, day_group) %>%
  dplyr::summarise(
    workers = sum(workers, na.rm=TRUE),
    .groups = "drop"
    )

# farms and farm area by area group
state_area_group <- mun_area_group %>%
  dplyr::transmute(
    state, year, 
    area_group, activity_group="total", day_group="total",
    farm_area_ha, n_farms
    ) %>%
  dplyr::group_by(state, year, area_group, activity_group, day_group) %>%
  dplyr::summarise(
    farm_area_ha = sum(farm_area_ha, na.rm=TRUE),
    n_farms      = sum(n_farms,      na.rm=TRUE),
    .groups = "drop"
    )

# clean up
rm(mun_activity_area, mun_workers, mun_area_group)

# (4) join in a single table ----------------------------------------------
# (4.1) add columns to df_census_state
df_census_state <- df_census_state %>%
  dplyr::transmute(
    state, year,
    area_group, activity_group="total", day_group="total",
    farm_area_ha, n_farms
  )

# (4.2) separate data by census year
# (a) df_census_state
df_census_state_1985 <- df_census_state %>% dplyr::filter(year==1985)
df_census_state_1995 <- df_census_state %>% dplyr::filter(year==1995)
df_census_state_2006 <- df_census_state %>% dplyr::filter(year==2006)
# (b) state_activity_area
state_activity_area_1995 <- state_activity_area %>% dplyr::filter(year==1995)
state_activity_area_2006 <- state_activity_area %>% dplyr::filter(year==2006)
state_activity_area_2017 <- state_activity_area %>% dplyr::filter(year==2017)
# (c) state_workers
state_workers_1995 <- state_workers %>% dplyr::filter(year==1995)
state_workers_2006 <- state_workers %>% dplyr::filter(year==2006)
state_workers_2017 <- state_workers %>% dplyr::filter(year==2017)

# (4.3) join 1995 data
df_census_state_1995 <- df_census_state_1995 %>%
  full_join(state_activity_area_1995, by=join_by(state,year,area_group,activity_group,day_group)) %>%
  full_join(state_workers_1995,       by=join_by(state,year,area_group,activity_group,day_group))

# (4.4) join 2006 data
df_census_state_2006 <- df_census_state_2006 %>%
  full_join(state_activity_area_2006, by=join_by(state,year,area_group,activity_group,day_group)) %>%
  full_join(state_workers_2006,       by=join_by(state,year,area_group,activity_group,day_group))

# (4.5) join 2017 data
# (a) deal with area_group classes - 2017 area groups are different from those in previous censuses
# recode area groups according to the previous classes and summarise
state_area_group_prev <- state_area_group
state_area_group_prev$area_group[state_area_group_prev$area_group=="from1kto2500ha"]  <- "over1kha"
state_area_group_prev$area_group[state_area_group_prev$area_group=="from2500to10kha"] <- "over1kha"
state_area_group_prev$area_group[state_area_group_prev$area_group=="over10kha"]       <- "over1kha"
state_area_group_prev <- state_area_group_prev %>%
  dplyr::group_by(state, year, area_group, activity_group, day_group) %>%
  dplyr::summarise(farm_area_ha = sum(farm_area_ha, na.rm=TRUE),
                   n_farms      = sum(n_farms,      na.rm=TRUE),
                   .groups = "drop")
# also get the new area group classes
state_area_group_2017 <- state_area_group %>%
  dplyr::filter(area_group %in% c("from1kto2500ha", "from2500to10kha", "over10kha"))
# (b) create 2017 dataframe
df_census_state_2017 <- rbind(state_area_group_prev, state_area_group_2017)
# (c) join other variables
df_census_state_2017 <- df_census_state_2017 %>%
  full_join(state_activity_area_2017, by=join_by(state,year,area_group,activity_group,day_group)) %>%
  full_join(state_workers_2017,       by=join_by(state,year,area_group,activity_group,day_group))

# (4.6) create final data.frame and clean up
# add columns that are missing in 1985
df_census_state_1985$activity_area <- NA
df_census_state_1985$workers       <- NA
# bind everything
df_census_state <- rbind(df_census_state_1985,
                         df_census_state_1995,
                         df_census_state_2006,
                         df_census_state_2017)
# clean up
rm(df_census_state_1985, df_census_state_1995, df_census_state_2006, df_census_state_2017,
   state_activity_area, state_activity_area_1995, state_activity_area_2006, state_activity_area_2017,
   state_workers, state_workers_1995, state_workers_2006, state_workers_2017,
   state_area_group, state_area_group_2017, state_area_group_prev)

## -------------------------------------------------------------------------
## (3) aggregate municipality-level data (OLD) -----------------------------
## (3.1) sum numeric values from municipalities in the same state
#mun_agg <- df_census_mun %>%
#  # get rid of municipality identifiers
#  dplyr::select(
#    state, year, area_group, activity_group, day_group,
#    farm_area_ha, n_farms, activity_area, workers
#    ) %>%
#  # group variables
#  dplyr::group_by(state, year, area_group, activity_group, day_group) %>%
#  # summarise numeric values
#  dplyr::summarise(farm_area_ha  = sum(farm_area_ha,  na.rm=TRUE),
#                   n_farms       = sum(n_farms,       na.rm=TRUE),
#                   activity_area = sum(activity_area, na.rm=TRUE),
#                   workers       = sum(workers,       na.rm=TRUE),
#                   .groups = "drop")
#
## (3.2) make sure that 'df_census_state' has all the same identifier columns as 'mun_agg'
#df_census_state <- df_census_state %>%
#  dplyr::transmute(state, year, area_group, activity_group="total", day_group="total",
#                   farm_area_ha, n_farms) %>%
#  tidyr::complete(state, year, area_group, activity_group, day_group)
#
## note that the data in 'mun_agg' covers 1995, 2006 and 2017;
## while the data in 'df_census_mun' covers 1985, 1995 and 2006
##
## also, the "area_group" classes in 2017 are different from the "area_group"
## classes in 1985, 1995 and 2006
##
## finally, both 'mun_agg' and 'df_census_state' have columns named
## "farm_area_ha" and "n_farms" - need to deal with this before joining
#
## (3.3) separate the data in 'mun_agg' in 2017 and previous to 2017
#mun_agg_2017 <- mun_agg %>% dplyr::filter(year  ==  "2017")
#mun_agg_prev <- mun_agg %>% dplyr::filter(year %in% c("1995","2006"))
#rm(mun_agg)
#
## (3.4) join pre-2017 data to df_census_state
## we just need columns "activity_area" and "workers", since 'df_census_state'
## already has data on "farm_area_ha" and "n_farms" for the years before 2017
#df_census_state <- df_census_state %>%
#  dplyr::left_join(
#    dplyr::select(mun_agg_prev, -farm_area_ha, -n_farms),
#    by = join_by(state, year, area_group, activity_group, day_group)
#  )
#rm(mun_agg_prev)
#
## (3.5) join 2017 data to df_census_state
## we could just rbind 'mun_agg_2017' to 'df_census_state', but the "area group" classes
## of 2017 are more disaggregated. I don't want to lose this information but I also
## want it to be comparable with the previous year
##
## solution: use both aggregation levels for 2017
##
## (a) recode the new "area_group" classes into the previous classes
#mun_agg_2017_as_prev <- mun_agg_2017
#mun_agg_2017_as_prev$area_group[mun_agg_2017_as_prev$area_group=="from1kto2500ha"]  <- "over1kha"
#mun_agg_2017_as_prev$area_group[mun_agg_2017_as_prev$area_group=="from2500to10kha"] <- "over1kha"
#mun_agg_2017_as_prev$area_group[mun_agg_2017_as_prev$area_group=="over10kha"]       <- "over1kha"
## (b) summarise table
#mun_agg_2017_as_prev <- mun_agg_2017_as_prev %>%
#  dplyr::group_by(state, year, area_group, activity_group, day_group) %>%
#  dplyr::summarise(farm_area_ha  = sum(farm_area_ha,  na.rm=TRUE),
#                   n_farms       = sum(n_farms,       na.rm=TRUE),
#                   activity_area = sum(activity_area, na.rm=TRUE),
#                   workers       = sum(workers,       na.rm=TRUE),
#                   .groups = "drop")
## (c) avoid double counting: take only the rows with the new "over1kha" value for area_group
#mun_agg_2017_as_prev <- mun_agg_2017_as_prev %>%
#  dplyr::filter(area_group=="over1kha")
## (d) bind 2017 data to 'df_census_state'
#df_census_state <- rbind(df_census_state, mun_agg_2017, mun_agg_2017_as_prev)
## (e) make NAs explicit and arrange table
#df_census_state <- df_census_state %>%
#  # NAs were ignored when I aggregated municipality-level data - make them explicit
#  dplyr::mutate(across(c(farm_area_ha, n_farms, activity_area, workers), ~na_if(.x,0))) %>%
#  # arrange row order
#  dplyr::arrange(state, year, area_group, activity_group, day_group)
## (f) clean up
#rm(mun_agg_2017, mun_agg_2017_as_prev)
