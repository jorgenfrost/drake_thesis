#' Tidy up the GDP data from WDI.
#' 
#' @param gdp_path string with the path to the the data downloaded from World
#' Bank. The data is GDP, PPP constant 2011 international dollars.
#' @param min_year earliest year to include in the returned data frame.
#' @return cleaned data frame with country-year observations of GDP.

clean_gdp <- function(gdp_path, min_year) {

gdp_raw <- read_csv(here(gdp_path), skip = 3)

gdp_tbl <- gdp_raw %>%
select(-c(`Indicator Name`, `Indicator Code`)) %>%
gather(-c(`Country Name`, `Country Code`), key = year, val = gdp) %>%
clean_names(case = "snake") %>%
filter(year >= min_year)

return(gdp_tbl)
	
}
# END
