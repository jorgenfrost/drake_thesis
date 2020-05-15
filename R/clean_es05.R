#' Function that cleans the ES 2005 data. Also attaches the state codes 
#' used in the ASI and shortage data.
#' 
#' @param es05_path string containing the path to the raw es05 data
#' @param state_conc_path string containing the path to the concordance table 
#' @return cleaned data frame with electricity variable I'm interested in
#' @export

clean_es05 <- function(es05_path, state_conc_path) {

es05_raw <- read_dta(here(es05_path))
state_conc_tbl <- read_csv(here(state_conc_path))

# Variables
# R6_1a1 = power outages or surgers from public grid (annual)
# R6_1b1 = avg duration in hs
# R6_1c1 = lost value (%)
# R6_2 =  do you own a generator
# R6_2a_ = if you own a generator, what percentage comes from yuor own or shared
# On a scale from 1 to 10 (1 = extremely bad, 10 = excellent), how would you rate following infrastructural facilities?
# R6_8a = quality of power
# obstacle rating for electricity:
# R11_5aB (0, 1, 2, 3, 4)
# What is THE biggest obstacle
# R115b1 

es05_tbl <- es05_raw %>%
	select(
	       idstd,
	       industry = code2,
	       es_city_code = code3,
	       power_outages_annual = r6_1a1,
	       avg_outage_duration = r6_1b1,
	       sales_lost_to_outages = r6_1c1,
	       own_generator = r6_2,
	       share_generated_electricity = r6_2a,
	       quality_of_power_supply = r6_8a,
	       obstacle_electricity = r11_5ab,
	       biggest_obstacle = r11_5b1 # electricity = B
	       ) %>%
mutate(year = 2005) %>%
left_join(state_conc_tbl, by = c("es_city_code" = "es_code"))

return(es05_tbl)

}
