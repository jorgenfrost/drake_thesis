#' Cleans data on natural resource rents as share of GDP from the WDIs.
#'
#' @param path file path to the data frame with the raw .csv data from the WDIs
#' @param min_year numeric: earliest earliest year. Used for filtering.
#' @return  
#' @export

clean_resource_rents <- function(path, min_year = 1999) {
  
  nat_rents_raw <- read_csv(path, skip = 3)
  
  nat_rents_tbl <- nat_rents_raw %>%
    select(-c(`Indicator Name`, `Indicator Code`, X65)) %>%
    gather(-c(`Country Name`, `Country Code`), key = year, value = perc_rent_of_gdp) %>%
    mutate(year = as.numeric(year)) %>%
    clean_names(case = "snake") %>%
    filter(year >= min_year)
  
  return(nat_rents_tbl)
  
}

