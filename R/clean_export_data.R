#' Cleans HS96 export trade data into a tidy, balanced panel. Developed for 
#' Harvard Growth Lab and OEC format. Data is filtered based on a filter-value 
#' list.
#' Author: Søren Post
#'
#' @param export_data data frame with export data on a country-year pair (origin)k
#' @param pop_data data frame with population data. Should be in a tidy format. 
#' See the function that cleans population data.
#' @param filter_values named list of values for the filters.
#' @return The balanced, cleaned data frame of countries export values in hs96 
#' products.
#' @export

clean_export_data <- function(
  export_data,
  pop_data,
  out_path,
  ref_year = 2005,
  min_pop = 1000000,
  min_total_export = 1000000000,
  min_year = 2000,
  unreliable_countries = c("IRQ", "AFG", "TCD"),
  removed_prods = c(
      "9999", # unspecified products
      "XXXX", # unaccounted for
      "financial", # financial services
      "travel", # travel services
      "transport", # transport services
      "ict", # information technology services
      "2527", # falls to zero trade during the sample
      "1403", # same as above
      "9704" # same as above (postal stamps)
    ),
  only_services = c("BWA", "NAM", "SWZ") # relevant for Harvard Data, not BACI
  ) {
  
  # This function filters, cleans, and balances HS96 trade data from ref_year to 2017.
  # The data is from the BACI data base, but distributed by
  # the Observatory of economic complexity.
  #
  # Author: Søren Post
  
  ## The file contains four 4 parts:
  ## Part 1 reads the population data and the trade data
  ## Part 2 cleans the data according to four filters regarding pop, export, non-service products and country reliability
  ## Part 3 balances the panel
  ## Part 4 writes the cleaned and balanced file to a file.
  
  
  ##################################################################
  ##                         1: READ DATA                         ##
  ##################################################################
  
  ## I need two kinds of data: population data and exports data
  ## Population data is the world bank's development indicators, but has already been cleaned
  ## Trade data is from the Harvard Growth lab database (based on the BACI data from CEPII).
  ## Population data
  
  if (is.data.frame(export_data)) {
    hs96_raw <- export_data
  } else if (is.character(export_data)) {
    hs96_raw <- vroom(export_data)
  } else {
    stop("Error: export_data is not a character string or a data frame.")
  }
  
  if (is.data.frame(pop_data)) {
    pop_tbl <- pop_data
  } else if (is.character(pop_data)) {
    pop_tbl <- vroom(pop_data)
  } else {
    stop("Error: pop_data is not a character string or a data frame.")
  }

  filter_values <- list(
			"ref_year" = ref_year,
			"min_pop" = min_pop,
			"min_total_export" = min_total_export,
			"min_year" = min_year,
			"unreliable_countries" = unreliable_countries,
			"removed_prods" = removed_prods,
			"only_services" = only_services
			)
  
  hs96_tbl <- hs96_raw %>%
    mutate(
      country_code = str_to_upper(origin),
      export_value = as.numeric(export_val)
    ) %>%
    select(
      year,
      country_code,
      hs_product_code = hs96,
      export_value
    ) %>%
    mutate(
      export_value = if_else(is.na(export_value), 0, export_value)
    )
  
  #################################################################
  ##                        2: CLEAN DATA                        ##
  #################################################################
  
  ## I start with 4 filters.
  ## 1: population must be above 1 mio inhabitants in ref_year
  ## 2: total exports must be above 1 billion dollars in ref_year
  ## 3: products must no be service products or unspecified
  ## 4: remove afghanistan, iraq and chad due to bad reliability
  ## 5: remove botswana, swaziland, and namibia - for most of the years in the sample, they have only observations on services (which I exclude)
  
  ref_year <- filter_values$ref_year
  
  ## Filter 1: create list of country codes with population above min_pop
  pop_above <- pop_tbl %>%
    filter(year == ref_year) %>%
    filter(pop >= filter_values$min_pop)
  
  ## Filter 2: create list of country codes with total exports above 1 bil
  export_above <- hs96_tbl %>%
    filter(year == ref_year) %>%
    group_by(country_code) %>%
    summarize(
      total_export = sum(export_value)
    ) %>%
    filter(total_export >= filter_values$min_total_export)
  
  ## Filter 3: unwanted product codes and services
  
  ## Filter 4: remove the extremely unreliable trading partners (in reporting, se Bustos and Yildrim method)
  
  ## Filter 5: remove the countries that only have service reporting
  
  ## Apply filters
  hs96_clean <- hs96_tbl %>%
    filter(country_code %in% pop_above$country_code) %>%
    filter(country_code %in% export_above$country_code) %>%
    filter(!(country_code %in% filter_values$unreliable_countries)) %>%
    filter(!(hs_product_code %in% filter_values$removed_prods)) %>%
    filter(!(country_code %in% filter_values$only_services)) %>%
    filter(year >= filter_values$min_year)
  
  ##################################################################
  ##                       3: BALANCE PANEL                       ##
  ##################################################################
  
  
  ## Get list of countries present across all years
  all_year_countries <- hs96_clean %>%
    group_by(country_code) %>%
    summarize(
      number_of_years = n_distinct(year)
    ) %>%
    filter(number_of_years == max(number_of_years))
  
  ## Get list of products present across all years
  all_year_products <- hs96_clean %>%
    group_by(hs_product_code) %>%
    summarize(
      number_of_years = n_distinct(year)
    ) %>%
    filter(number_of_years == max(number_of_years))
  
  hs96_clean <- hs96_clean %>%
    filter(country_code %in% all_year_countries$country_code) %>%
    filter(hs_product_code %in% all_year_products$hs_product_code)
  
  ## Make sure that there is an observation for each country-product-year combination.
  hs96_balanced <- hs96_clean %>%
    complete(country_code, year, hs_product_code, fill = list("export_value" = 0))
  
  ## ----------------------------------------------------------------
  
  return(hs96_balanced)
}

# END
 
