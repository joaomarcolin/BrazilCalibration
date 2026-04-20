# This script opens tables from IBGE's PAM and PPM yearly surveys at the
# municipality level for 1985-2024 and joins them in a single dataframe

# (1) load data -----------------------------------------------------------
# auxiliary function to load yearly data
# this function takes two file paths (because most data is stored in two files)
# and returns a dataframe with 4 character columns (code_mun, name_mun, year, value)
load_yearly_data <- function(path1, path2=NA) {
  # (A) for datasets that are stored in a single file:
  if (is.na(path2)) {
    # load
    table <- read_excel(path1, col_types="text")
    # rename columns
    names(table)[1:2] <- c("code_mun", "name_mun")
  # (B) for datasets that are stored in two files:
  } else {
    # load
    table1 <- read_excel(path1, col_types="text")
    table2 <- read_excel(path2, col_types="text")
    # rename columns
    names(table1)[1:2] <- c("code_mun", "name_mun")
    names(table2)[1:2] <- c("code_mun", "name_mun")
    # join
    table <- table1 %>%
      dplyr::left_join(select(table2,-name_mun), by="code_mun")
  } # closes if-else structure
  
  # pivot data to long format
  keep  <- c("code_mun", "name_mun")
  table <- table %>%
    tidyr::pivot_longer(cols=-all_of(keep), names_to="year", values_to="value")
  # returns table
  return(table)
  
} # closes load_yearly_data(path1, path2)

# PAM table 1612: soybean output (tons)
PAM_soy_output <- load_yearly_data("data_inputs/IBGE/PAM/tabela1612_output_1985_1999_EDITED.xlsx",
                                   "data_inputs/IBGE/PAM/tabela1612_output_2000_2024_EDITED.xlsx")

# (1.2) PAM table 1612: soybean area (hectares)
PAM_soy_area   <- load_yearly_data("data_inputs/IBGE/PAM/tabela1612_area_1985_1999_EDITED.xlsx",
                                   "data_inputs/IBGE/PAM/tabela1612_area_2000_2024_EDITED.xlsx")

# (1.3) PPM table 3939: herd (head of cattle)
PPM_cattle_herd <- load_yearly_data("data_inputs/IBGE/PPM/tabela3939_1985_1999_EDITED.xlsx",
                                    "data_inputs/IBGE/PPM/tabela3939_2000_2024_EDITED.xlsx")

# remove auxiliary function
rm(load_yearly_data)

# (2) deal with weird IBGE values -----------------------------------------
# There are a few weird values in the IBGE tables:
# "-"   = Zero absoluto                                   - should be "0"
# "0"   = Zero resultante de um cálculo ou arredondamento - should be "0"
# "X"   = Valor inibido para não identificar o informante - should be NA
# ".."  = Valor não se aplica                             - not present in the tables above
# "..." = Valor não disponível                            - should be NA

# auxiliary function to treat weird values
# this function takes any of the dataframes from the previous step
# and returns a similar dataframe, but with column "value" numeric and with some NAs
subst_values <- function(df) {
  df <- df %>%
    # substitute weird values
    dplyr::mutate(
      value = case_when(value == "-"            ~ "0",
                        value %in% c("X","...") ~ NA_character_,
                        TRUE                    ~ value)) %>%
    # turn 'value' column to numeric
    dplyr::mutate(value=as.numeric(value))
  # return df
  return(df)
} # closes subst_values(df)

# treat tables
PAM_soy_output  <- subst_values(PAM_soy_output)
PAM_soy_area    <- subst_values(PAM_soy_area)
PPM_cattle_herd <- subst_values(PPM_cattle_herd)

# remove auxiliary function
rm(subst_values)

# (3) join tables ---------------------------------------------------------
# rename "value" columns
PAM_soy_area    <- PAM_soy_area    %>% dplyr::select(code_mun, name_mun, year, pam_area_ha=value)
PAM_soy_output  <- PAM_soy_output  %>% dplyr::select(code_mun, year, pam_output_ton=value)
PPM_cattle_herd <- PPM_cattle_herd %>% dplyr::select(code_mun, year, ppm_herd=value)
# create final dataframe
df_yearly_mun <- PAM_soy_area %>%
  dplyr::full_join(PAM_soy_output,  by=join_by(code_mun,year)) %>%
  dplyr::full_join(PPM_cattle_herd, by=join_by(code_mun,year)) %>%
  dplyr::full_join(mapbiomas_data,  by=join_by(code_mun,year))
# clean up
rm(PAM_soy_area, PAM_soy_output, PPM_cattle_herd, mapbiomas_data)




