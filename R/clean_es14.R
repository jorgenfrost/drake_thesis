#' This function cleans the enterprise survey microdtata on India from 2014.
#' 
#' @param es14_path string containing the path the raw es14 file.
#' @param state_conc_path string containing the path to the concordance table 
#' used to match state codes from the ASI to the state codes in the ES.
#' @return cleaned es14 table
#' @export 

clean_es14 <- function(es14_path, state_conc_path = "data/external/concord_tables/es_state_codes/esi14_to_asi_state_codes.csv") {

es14_raw <- read_dta(es14_path)

# variables:
# idstd = id
# a3a = screener region (more accurate than sampling region)
# a4b = sector id (2 digit)
# wstrict, wmedian, wweak = weights under different assumptions 
# a6b = screener size (micor, small, mediaum, large)
# c6 = betwen 2012-13 did establ experience power outs?
# c7 = if yes, typical month = how many outages?
# c8 = if yes, how long were they typically? (hours. 1 = less than one hour)
# c9a = estimate loss from power outs as perc of total annual sales OR
# c9b = as total annual losses
# c10 = did establ have or share a generator btw 12-13?
# c11 = % of electricity used comming from these generators?
# m1a = biggest obstacle faced by this establishment (8 = electricity)
# c30a = To what degree is Electricity an obstacle to current operations of establishment? 
# (0 = no obstacle, 1 = minor, 2 = moderate, 3 = major, 4 = very severe, -9 = don't know, -7 = does not apply) 

es14_tbl <- es14_raw %>%
	select(
	       idstd,
	       state = a3a,
	       sector = a4b,
	       wstrict,
	       wmedian,
	       wweak,
	       experience_power_outs = c6,
	       outages_per_month = c7,
	       avg_outage_length = c8,
	       share_sales_lost_to_outage = c9a,
	       abs_losses_to_outage = c9b,
	       own_generator = c10,
	       share_elect_from_generator = c11,
	       degree_electricity_obstacle = c30a,
	       biggest_obstacle = m1a
	       )

es14_tbl <- es14_tbl %>%
	mutate(
	       state_name = haven::as_factor(state) %>% as.character(), # get state names, not integers
	       state = as.numeric(state),
	       sector_name = haven::as_factor(sector) %>% as.character(), # get state names, not integers
	       sector = as.numeric(sector)
	       ) %>%
	filter(sector <= 37) # remove non-manufactoring firms

# ES14 states are listed by alphabet. wack. I manually construct a conc table: this is it
state_concordance_tbl <- read_csv(here(state_conc_path)) 

es14_tbl <- es14_tbl %>%
	left_join(
		  state_concordance_tbl,
		  by = c("state" = "es_code"),
		  suffix = c(".es", ".asi")
		  ) 

return(es14_tbl)
}

# END
