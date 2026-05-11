# This script loads Mapbiomas' data for land use and land cover variables
# at the municipality level for 1985-2024

# load table
mapbiomas_data <- read_excel("data_inputs/Mapbiomas/MAPBIOMAS_COL10_MUNICIPALITY_EDITED.xlsx",
                             col_types = c(rep("text",14),
                                           rep("numeric",40)))

# remove unnecessary columns
mapbiomas_data <- mapbiomas_data %>%
  dplyr::select(code_mun=geocode,
                class,
                `1985`:`2024`)

## simplify land cover categories
dict <- c("0"  = "mb_notobs_ha",
          "3"  = "mb_natcover_ha",
          "4"  = "mb_natcover_ha",
          "5"  = "mb_natcover_ha",
          "6"  = "mb_natcover_ha",
          "9"  = "mb_forestry_ha",
          "11" = "mb_natcover_ha",
          "12" = "mb_pasture_ha", # I previously considered this as natural cover
          "13" = "mb_natcover_ha",
          "15" = "mb_pasture_ha",
          "20" = "mb_tcrop_ha",
          "21" = "mb_pasture_ha",
          "23" = "mb_nonveg_ha",
          "24" = "mb_nonveg_ha",
          "25" = "mb_nonveg_ha",
          "29" = "mb_natcover_ha",
          "30" = "mb_nonveg_ha",
          "31" = "mb_nonveg_ha",
          "32" = "mb_natcover_ha",
          "33" = "mb_nonveg_ha",
          "35" = "mb_pcrop_ha",
          "39" = "mb_soybean_ha",
          "40" = "mb_tcrop_ha",
          "41" = "mb_tcrop_ha",
          "46" = "mb_pcrop_ha",
          "47" = "mb_pcrop_ha",
          "48" = "mb_pcrop_ha",
          "49" = "mb_natcover_ha",
          "50" = "mb_natcover_ha",
          "62" = "mb_tcrop_ha",
          "75" = "mb_notobs_ha")

## check if there are classes in mapbiomas_data that are not in dict
#unknown <- setdiff(unique(mapbiomas_data$class), names(dict))
#if (length(unknown) > 0) warning("Unmapped MapBiomas class codes: ", paste(unknown, collapse=", "))

mapbiomas_data$class <- dict[mapbiomas_data$class]
rm(dict)

# summarize
mapbiomas_data <- mapbiomas_data %>%
  dplyr::group_by(code_mun, class) %>%
  dplyr::summarise(across(where(is.numeric), \(x) sum(x,na.rm=TRUE)),
                   .groups = "drop") %>%
  dplyr::ungroup()

# mapbiomas_data has "Lagoa dos Patos" (4300002) and "Lagoa Mirim" (4300001), which are not municipalities
mapbiomas_data  <- mapbiomas_data  %>%
  dplyr::filter(!(code_mun %in% c("4300002","4300001")))

# pivot table
mapbiomas_data <- mapbiomas_data %>%
  tidyr::pivot_longer(
    cols      = `1985`:`2024`,
    names_to  = "year",
    values_to = "area"
  ) %>%
  tidyr::pivot_wider(
    names_from  = class,
    values_from = area
  ) %>%
  # some municipalities don't have some of the classes of LULC, so they show up as NA
  # but they should show up as 0 - fix it
  dplyr::mutate(
    across(everything(), ~ replace_na(.x, 0))
  )
  