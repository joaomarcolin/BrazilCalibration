# This script generates df_yearly_prices, with yearly values for:
# (a) interest = cumulative interest rate (selic)
# (b) wage = minimum wage at december (reais, 2024 values)
# (c) price_C = price of one ton of cattle for slaughter (reais, 2024 values)
# (d) price_S = price of one ton of soybeans (reais, 2024 values)

# (1) load data -----------------------------------------------------------
# IPEA's IGP-DI (geral - centrado - fim período - 1994=100) price index
ipea_igp <- readxl::read_excel(
              "data_inputs/IPEA/ipeadata_IGP_DI_EDITED.xlsx",
              col_names = c("year", "index"),
              col_types = c("text", "numeric"),
              skip=1) %>%
            dplyr::filter(year %in% 1985:2024)

# IPEA's minimum wage time series
ipea_wage <- readxl::read_excel(
               "data_inputs/IPEA/ipeadata_salario_min_EDITED.xls",
               col_types=c("text","numeric")) %>%
             dplyr::filter(year %in% 1985:2024)

# IPEA's cumulative interest rate time series
ipea_interest <- readxl::read_excel(
                   "data_inputs/IPEA/ipeadata_selic_acumulada_EDITED.xls",
                   col_types=c("text","text","numeric"))

# IPEA's annual inflation rate
ipea_inflation <- readxl::read_excel(
                    "data_inputs/IPEA/ipeadata_ipc_fipe_anual_EDITED.xls",
                    col_types=c("text","numeric")) %>%
                  dplyr::rename(year=Data, ipc_rate=IPC)

# Paraná's yearly commodity prices time series (just cattle and soybean)
prices <- readxl::read_excel(
            "data_inputs/Parana/commodity_prices_EDITED.xlsx",
            col_types=c("text","numeric","numeric"))

# (2) bring prices and wages to 2024 values -------------------------------
# change the price index's base to 2024
base_index <- ipea_igp$index[ipea_igp$year=="2024"]
new_index  <- (100*ipea_igp$index)/base_index
ipea_igp$index <- new_index
# bring minimum wage to 2024 values
ipea_wage <- ipea_wage %>% dplyr::left_join(ipea_igp,by="year")
ipea_wage$wage <- 100*(ipea_wage$wage/ipea_wage$index)
ipea_wage <- ipea_wage %>% dplyr::select(-index)
# bring commodity prices to 2024 values
prices <- prices %>% dplyr::left_join(ipea_igp, by="year")
prices$price_C <- 100*(prices$price_C/prices$index)
prices$price_S <- 100*(prices$price_S/prices$index)
prices <- prices %>% dplyr::select(-index)

# (3) deal with commodity prices data -------------------------------------
# in the original data, price_C is the price of 1@ (~15kg) of cattle for slaughter,
# and price_S is for 60kg of soybeans. Since we calculate productivity as tons/hectares,
# we need the price per ton of output.
prices <- prices %>%
  dplyr::transmute(year,
                   price_C = 1000*price_C/15,
                   price_S = 1000*price_S/60)
# price data is only available for 1995-2023. I assume prices in 1985-1995 were
# at the same level as in 1995 and that prices in 2024 were the same as in 2023
prices <- prices %>%
  tidyr::complete(year=as.character(1985:2024)) %>%
  dplyr::arrange(year) %>%
  tidyr::fill(price_C, price_S, .direction="updown")

# (4) get yearly interest rate --------------------------------------------
# ipea's data gives us cumulative interest (basic rate) every month. first,
# get the monthly rates as (1+i), with i in (0,1) interval
ipea_interest$selic <- (1 + (ipea_interest$selic/100))
# calculate cumulative 12-month rate
ipea_interest$cumulative_selic <- NA
for (yr in as.character(1985:2024)) {
  # for each year, get a vector of the monthly interest rates as (1+r)
  yr_monthly_rates <- dplyr::filter(ipea_interest, year==yr)$selic
  # calculate the product of the monthly rates
  yr_interest <- prod(yr_monthly_rates) - 1
  # fill out the 'ipea_interest' dataframe
  ipea_interest$cumulative_selic[ipea_interest$year==yr] <- yr_interest
}
# keep only one row per year
ipea_interest <- ipea_interest %>%
  dplyr::filter(month=="12") %>%
  dplyr::select(year, cumulative_selic) %>%
  # get inflation rate
  dplyr::left_join(ipea_inflation, by="year") %>%
  dplyr::mutate(ipc_rate = ipc_rate/100) %>%
  # get real effective interest rate
  dplyr::transmute(
    year, 
    interest = (cumulative_selic-ipc_rate)/(1+ipc_rate)
    )


# (5) create final dataframe ----------------------------------------------

df_yearly_prices <- ipea_interest %>%
  dplyr::full_join(ipea_wage, by="year") %>%
  dplyr::full_join(prices,    by="year")

rm(ipea_igp, ipea_inflation, ipea_interest, ipea_wage, prices,
   base_index, new_index, yr, yr_interest, yr_monthly_rates)
  

