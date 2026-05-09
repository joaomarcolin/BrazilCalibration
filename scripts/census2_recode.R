# This script takes data from IBGE's Census of Agriculture and recodes variable names
#
# from "0_load_census_data.R", we have three lists of dataframes:
# census1 contains dfs with columns "state" "class" "value"
# area_1985/1995/2006 #    0 NA values
# farm_1985/1995/2006 #    0 NA values
#
# census2 contains dfs with columns "code_mun" "name_mun" "class" "value"
# worker_1995        #     0 NA values
# area_activity_1995 #     0 NA values
# area_activity_2006 #  3255 NA values
# area_activity_2017 #  5127 NA values
# area_2017          # 17398 NA values
# farm_2017          #     0 NA values
#
# census3 contains dfs with columns "code_mun" "name_mun" "activity" "class" "value"
# worker1_2006       #     0 NA values
# worker2_2006       #     0 NA values
# worker_2017        # 35590 NA values

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
      dplyr::summarise(value=sum(value, na.rm=TRUE), .groups="drop") %>%
      dplyr::filter(class != "landless") # get rid of landless farmers
    return(df)
  } else if (df_level=="state") {
    # same as above, but for tables with state-level data
    df <- df %>%
      dplyr::mutate(class = recode(class, !!!dict_class, .default = class, .missing=class)) %>%
      dplyr::group_by(state, class) %>%
      dplyr::summarise(value=sum(value, na.rm=TRUE), .groups="drop") %>%
      dplyr::ungroup() %>%
      dplyr::filter(class != "ignore") # avoid double counting area of small farms
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
