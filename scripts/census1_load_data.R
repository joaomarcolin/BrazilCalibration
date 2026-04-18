# This script loads data from IBGE's census of agriculture (1985, 1995, 2006, 2017)
# to get information on land concentration and number of rural workers
# at the state level

# (0) load tables ---------------------------------------------------------
# 1995
area_activity_1995   <- readxl::read_excel("data_inputs/IBGE/agriculture_census/1995/tabela314_EDITED.xlsx", col_types="text")
worker_1995          <- readxl::read_excel("data_inputs/IBGE/agriculture_census/1995/tabela321_EDITED.xlsx", col_types="text")

# 2006
area_1985            <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2006/tabela263_1985_area_EDITED.xlsx", col_types="text")
area_1995            <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2006/tabela263_1995_area_EDITED.xlsx", col_types="text")
area_2006            <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2006/tabela263_2006_area_EDITED.xlsx", col_types="text")
farm_1985            <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2006/tabela263_1985_farm_EDITED.xlsx", col_types="text")
farm_1995            <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2006/tabela263_1995_farm_EDITED.xlsx", col_types="text")
farm_2006            <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2006/tabela263_2006_farm_EDITED.xlsx", col_types="text")
worker1_2006         <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2006/tabela805_1_EDITED.xlsx", col_types="text") %>% tidyr::fill(`Cód.`, `Município`, .direction="down")
worker2_2006         <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2006/tabela805_2_EDITED.xlsx", col_types="text") %>% tidyr::fill(`Cód.`, `Município`, .direction="down")
area_activity_2006   <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2006/tabela838_EDITED.xlsx", col_types="text")

## 2017
area_2017          <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2017/tabela6880_area_EDITED.xlsx", col_types="text")     %>% tidyr::fill(`Cód.`, `Município`, .direction="down")
farm_2017          <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2017/tabela6880_farm_EDITED.xlsx", col_types="text")     %>% tidyr::fill(`Cód.`, `Município`, .direction="down")
area_activity_2017 <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2017/tabela6880_activity_EDITED.xlsx", col_types="text") %>% tidyr::fill(`Cód.`, `Município`, .direction="down")
worker_2017        <- readxl::read_excel("data_inputs/IBGE/agriculture_census/2017/tabela6888_EDITED.xlsx", col_types="text")          %>% tidyr::fill(`Cód.`, `Município`, .direction="down")

# (1) pivot and rename columns --------------------------------------------
# auxiliary function
pivot_rename <- function(df, df_level, df_activity=FALSE) {
  if (df_level=="mun") {
    # if the table has column "Grupo de de atividade econômica", don't pivot it
    keep <- if (df_activity) {c("Cód.", "Município", "Grupos de atividade econômica")} else {c("Cód.", "Município")}
    # if the table has municipality-level data, keep and rename municipality identifiers
    df <- df %>%
      tidyr::pivot_longer(cols=-all_of(keep),
                          names_to="class",
                          values_to="value") %>%
      dplyr::rename(code_mun=`Cód.`, name_mun=`Município`)
    # if the table has column "Grupo de de atividade econômica", rename it
    if (df_activity) {
      df <- df %>% dplyr::rename(activity=`Grupos de atividade econômica`)
    }
    # return data frame
    return(df)
  } else if (df_level=="state") {
    # same as above, but for state-level tables
    keep <- if (df_activity) {c("Unidade da Federação","Grupos de atividade econômica")} else {"Unidade da Federação"}
    df <- df %>%
      tidyr::pivot_longer(cols=-all_of(keep),
                          names_to="class",
                          values_to="value") %>%
      dplyr::rename(state=`Unidade da Federação`)
    if (df_activity) {
      df <- df %>% dplyr::rename(activity=`Grupos de atividade econômica`)
    }
    return(df)
  } else {
    warning("df_level must be either 'mun' or 'state'.")
    return()
  }
}

# 1985
area_activity_1995 <- pivot_rename(area_activity_1995, "mun", FALSE)
worker_1995        <- pivot_rename(worker_1995,        "mun", FALSE)

# 2006
area_1985          <- pivot_rename(area_1985,          "state", FALSE)
area_1995          <- pivot_rename(area_1995,          "state", FALSE)
area_2006          <- pivot_rename(area_2006,          "state", FALSE)
farm_1985          <- pivot_rename(farm_1985,          "state", FALSE)
farm_1995          <- pivot_rename(farm_1995,          "state", FALSE)
farm_2006          <- pivot_rename(farm_2006,          "state", FALSE)
worker1_2006       <- pivot_rename(worker1_2006,       "mun",   TRUE)
worker2_2006       <- pivot_rename(worker2_2006,       "mun",   TRUE)
area_activity_2006 <- pivot_rename(area_activity_2006, "mun",   FALSE)

# 2017
area_2017          <- dplyr::rename(area_2017, code_mun=`Cód.`, name_mun=`Município`, class=`Grupos de área total`, value=Total)
farm_2017          <- dplyr::rename(farm_2017, code_mun=`Cód.`, name_mun=`Município`, class=`Grupos de área total`, value=Total)
area_activity_2017 <- pivot_rename(area_activity_2017, "mun", FALSE)
worker_2017        <- pivot_rename(worker_2017,        "mun", TRUE)
rm(pivot_rename)

# (2) substitute weird values and group tables ----------------------------

# (2.1) auxiliary function to substitute values and turn column "value" to numeric
# "-" should be "0"
# "X" and "..." should be NA
subst_f <- function(df) {
  df <- df %>% 
    dplyr::mutate(
      value = as.numeric(
        case_when(value == "-"                 ~ "0",
                  value %in% c("X","..","...") ~ NA_character_,
                  TRUE                         ~ value)
        )
      )
}

## check all tables' column names
#
#tables_now <- data.frame(names = ls(), n_cols=NA,
#                         col1=NA, col2=NA, col3=NA,
#                         col4=NA, col5=NA)
#for (idx in 1:15) {
#  df_name <- tables_now$names[idx]
#  df <- get(df_name)
#  tables_now$n_cols[idx] <- dim(df)[2]
#  tables_now$col1[idx] <- names(df)[1]
#  tables_now$col2[idx] <- names(df)[2]
#  tables_now$col3[idx] <- names(df)[3]
#  if (dim(df)[2]==4) {
#    tables_now$col4[idx] <- names(df)[4]
#  } else if (dim(df)[2]==5) {
#    tables_now$col4[idx] <- names(df)[4]
#    tables_now$col5[idx] <- names(df)[5]
#  }
#}
#rm(tables_now, idx, df_name, df)

# (2.2) substitute values and group tables by column names
# 1st group: columns "state" "class" "value"
census1 <- list(area_1985 = subst_f(area_1985),
                area_1995 = subst_f(area_1995),
                area_2006 = subst_f(area_2006),
                farm_1985 = subst_f(farm_1985),
                farm_1995 = subst_f(farm_1995),
                farm_2006 = subst_f(farm_2006))

# 2nd group: columns "code_mun" "name_mun" "class" "value"
census2 <- list(worker_1995        = subst_f(worker_1995),
                area_activity_1995 = subst_f(area_activity_1995),
                area_activity_2006 = subst_f(area_activity_2006),
                area_activity_2017 = subst_f(area_activity_2017),
                area_2017          = subst_f(area_2017),
                farm_2017          = subst_f(farm_2017))

# 3rd group: columns "code_mun" "name_mun" "activity" "class" "value"
census3 <- list(worker1_2006 = subst_f(worker1_2006),
                worker2_2006 = subst_f(worker2_2006),
                worker_2017  = subst_f(worker_2017))

# (2.3) clean up
rm(area_1985, area_1995, area_2006, farm_1985, farm_1995, farm_2006, worker_1995,
   area_activity_1995, area_activity_2006, area_activity_2017, area_2017, farm_2017,
   worker1_2006, worker2_2006, worker_2017,
   subst_f)

