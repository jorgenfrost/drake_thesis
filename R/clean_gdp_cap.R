#' This function cleans GDP/cap data from WDI.
#'
#' @param gdp_cap_path path to the raw GDP/cap file from the WDIs.
#' @param min_year earliest year in output. Used to filter.
#' @return tibble with cleanead country-year observations of GDP/cap.
#' @export

clean_gdp_cap <- function(gdp_cap_path, min_year = 1999) {
  
  gdp_cap_raw <- read_csv(gdp_cap_path, skip = 4)
  
  gdp_cap_tbl <- gdp_cap_raw %>%
    select(-c(`Indicator Name`, `Indicator Code`)) %>%
    gather(-c(`Country Name`, `Country Code`), key = year, value = gdp_cap) %>%
    mutate(year = as.numeric(year)) %>%
    clean_names(case = "snake") %>%
    filter(year >= min_year)
  
  return(gdp_cap_tbl)
  
}



