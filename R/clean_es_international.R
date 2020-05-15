#' This function cleans the data from the enterprise survey
#' (country-observations).
#'
#' @param es_sheet path to .xlsx spreadsheet containing sheets with the country observations
#' in the enterprise survey on crime ("Crime"), infrastructure ("Infrastructure"), trade
#' ("Trade"), or corruption ("Corruption").
#' @param list of the cleaned input data frames.


clean_es_international <- function(es_file) {
  
  # Read data ----------------------------------------------------------  
  
  es_crime_tbl <- read_excel(es_file, sheet = "Crime") %>%
    clean_names(case = "snake")
  
  es_infra_tbl <- read_excel(es_file, sheet = "Infrastructure") %>%
    clean_names(case = "snake")
  
  es_trade_tbl <- read_excel(es_file, sheet = "Trade") %>%
    clean_names(case = "snake")
  
  es_corruption_tbl <- read_excel(es_file, sheet = "Corruption") %>%
    clean_names(case = "snake")
  
  # Prepare tibbles ----------------------------------------------------
  
  # prep crime
  es_crime_tbl <- es_crime_tbl %>%
    select(
      economy,
      year,
      share_products_stolen_during_shipment = products_shipped_to_supply_domestic_markets_that_were_lost_due_to_theft_percent_of_product_value
    ) %>%
    mutate(
      year = as.numeric(year),
      share_products_stolen_during_shipment = as.numeric(share_products_stolen_during_shipment)
    ) %>%
    filter(!is.na(share_products_stolen_during_shipment) | !is.na(year)) %>%
    mutate(
      iso3c = countrycode(.data$economy, origin = "country.name", destination = "iso3c")
    )
  
  # prep infrastructure
  es_infra_tbl <- es_infra_tbl %>%
    mutate(
      year = as.numeric(year),
      monthly_water_shortages = as.numeric(number_of_water_insufficiencies_in_a_typical_month),
      annual_share_of_sales_lost_to_outages = as.numeric(if_there_were_outages_average_losses_due_to_electrical_outages_percent_of_annual_sales),
      share_of_products_broken_in_shipment = as.numeric(proportion_of_products_lost_to_breakage_or_spoilage_during_shipping_to_domestic_markets_percent),
      iso3c = countrycode(.data$economy, origin = "country.name", destination = "iso3c")
    ) %>%
    select(
      economy,
      iso3c,
      year,
      monthly_water_shortages,
      annual_share_of_sales_lost_to_outages,
      share_of_products_broken_in_shipment
    ) %>%
    filter(!is.na(year)) %>%
    filter(!is.na(monthly_water_shortages)) %>%
    filter(!is.na(annual_share_of_sales_lost_to_outages)) %>%
    filter(!is.na(share_of_products_broken_in_shipment))
  
  # prep trade
  es_trade_tbl <- es_trade_tbl %>%
    mutate(
      year = as.numeric(year),
      days_to_clear_imports_from_customs = as.numeric(days_to_clear_imports_from_customs),
      iso3c = countrycode(.data$economy, origin = "country.name", destination = "iso3c")
    ) %>%
    select(
      economy,
      iso3c,
      year,
      days_to_clear_imports_from_customs
    ) %>%
    filter(!is.na(year) & !is.na(days_to_clear_imports_from_customs))
  
  # prep corruption
  es_corruption_tbl <- es_corruption_tbl %>%
    mutate(
      year = as.numeric(year),
      perc_firms_gifts_to_get_things_done = as.numeric(percent_of_firms_expected_to_give_gifts_to_public_officials_to_get_things_done),
      iso3c = countrycode(.data$economy, origin = "country.name", destination = "iso3c")
    ) %>%
    select(
      economy,
      iso3c,
      year,
      perc_firms_gifts_to_get_things_done
    ) %>%
    filter(!is.na(year) & !is.na(perc_firms_gifts_to_get_things_done))
  
  out_list <- list(
    "crime" = es_crime_tbl,
    "infrastructure" = es_infra_tbl,
    "trade" = es_trade_tbl,
    "corruption" = es_corruption_tbl
  )
  
  return(out_list)
}
# END
