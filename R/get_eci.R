get_eci <- function() {
# COLLECT AND CLEAN ECI -----------------------------------------

# Collect
eci_raw <- read_csv(here("data/external/oec/eci_hs4_hs96_98-18.csv"))

# Clean
eci_tbl <- eci_raw %>%
	gather(-c(`Country ID`,`Country`), key = year, val = eci) %>%
	clean_names(case = "snake") 

# Fix country names
eci_tbl <- eci_tbl %>%
	mutate(
	       country_code = countrycode(
					  sourcevar = country,
					  origin = "country.name",
					  destination = "iso3c"
					  )
	       ) %>%
select(-country_id)

return(eci_tbl)
}
