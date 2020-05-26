#' Function that preps the base anaylsis sample.
 
# Read for testing
# rbi_tbl <- readd("rbi_tbl")
# nss_tbl <- readd("nss_tbl")
# shortage_tbl <- readd("energy_shortages_tbl")
# plant_tbl <- readd("asi_base_sample_ls")$plant_tbl
# nic98_tbl <- readd("nic_ls")$nic98_code_tbl
# nic87_tbl <- readd("nic_ls")$nic87_code_tbl
# plant_complexity_tbl <- readd("plant_complexity_tbl")
# int_input_tbl <- readd("plant_int_input_tbl")
# labor_tbl <- readd("plant_labour_input_tbl")

prepare_analysis_sample <- function(rbi_tbl, nss_tbl, shortage_tbl, plant_tbl, nic98_tbl, nic87_tbl, plant_complexity_tbl, plant_mean_complexity_tbl, int_input_tbl, labor_tbl) {

##################################################################
##                       PLANT LEVEL DATA                       ##
##################################################################

# Get list of states -------------------------------------------------

# State kill-list:
exclude_states <- c(
  "Daman and Diu", # RBI 
  "Dadra and Nagar Haveli", # RBI 
  "Lakshadweep", # CEA 
  "Damodar Valley Corporation",  # CEA
  "Telangana", # CEA
  "A and N Islands", # too small
  "Arunachal Pradesh" # only 2 years of ASI data
  )

# Add plant level data -----------------------------------------------


plant_tbl <-
	plant_tbl %>% 
	filter(!state_name %in% exclude_states) %>%
	left_join(labor_tbl, by = c("dsl", "year")) 

# Some year-dsl pairs are mistakenly added more than once in the data. The obs have
# double values where they should only have one. These are then magnified
# throught the left_joins. I remove them here.

exclusion_plants <- plant_tbl %>%
	group_by(year, dsl) %>%
	summarize(n = n()) %>%
	filter(n != 1) %>% 
	ungroup()

plant_tbl <- plant_tbl %>%
	anti_join(exclusion_plants)



# Create deflated revenues -------------------------------------------

deflated_rev <- plant_tbl %>%
	select(year, dsl, scheme, nic5digit, revenue) %>%
	left_join(nic87_tbl, by = c("year", "dsl")) %>%
	filter(!is.na(nic87)) %>%
	select(-original_nic)

# Read deflator
output_deflator_index <- read_dta(here("data/external/deflators/Deflators/final-output-deflator.dta")) %>%
	mutate(nic87 = as.character(nic3digit)) %>%
	select(year, nic87, deflator)

# Add deflation value to observations 
deflated_rev <- deflated_rev %>%
	left_join(output_deflator_index, by = c("year", "nic87")) %>%
	mutate(
	       adjusted_revenue = revenue / (deflator / 100)
	       )

plant_tbl <- left_join(plant_tbl, deflated_rev)

# Add complexity values to plant_tbl ---------------------------------

lenient_plant_complexity_tbl <- plant_complexity_tbl %>%
	filter(match == "lenient") %>%
	select(
	       year,
	       dsl,
	       w_avg_complexity_lenient = w_avg_complexity,
	       max_complexity_lenient = max_complexity
	       )

strict_plant_complexity_tbl <- plant_complexity_tbl %>%
	filter(match == "strict") %>%
	select(
	       year,
	       dsl,
	       w_avg_complexity_strict = w_avg_complexity,
	       max_complexity_strict = max_complexity
	       )

plant_tbl <- plant_tbl %>% 
	left_join(lenient_plant_complexity_tbl, by = c("year", "dsl")) %>%
	left_join(strict_plant_complexity_tbl, by = c("year", "dsl")) 

# Add year since initial production ----------------------------------

plant_tbl <- plant_tbl %>%
	mutate(
	       time_since_entry = year - initial_production
	       )

# Add inputs ---------------------------------------------------------

plant_tbl <- plant_tbl %>%
	left_join(int_input_tbl)

# Add nic98 industry -------------------------------------------------

plant_tbl <- plant_tbl %>%
	left_join(nic98_tbl) 

##################################################################
##                       STATE LEVEL DATA                       ##
##################################################################

# SHORTAGE DATA ------------------------------------------------------

shortage_tbl <- shortage_tbl %>%
	filter(!state %in% exclude_states) %>%
	mutate(
	       state = ifelse(state == "West Bengal + Sikkim", "West Bengal", state)
	       )

# get rolling average shortage for current year + last and current year + last + two years ago
shortage_tbl <- shortage_tbl %>%
	group_by(state) %>%
	arrange(year) %>%
	mutate(
	       avg_shortage_2y = (avg_shortage + lag(x = avg_shortage, n = 1)) / 2,
	       avg_shortage_3y = (avg_shortage + lag(x = avg_shortage, n = 1) + lag(x = avg_shortage, n = 2)) / 3
	       ) %>%
	mutate(
	       peak_shortage_2y = (peak_shortage + lag(x = peak_shortage, n = 1)) / 2,
	       peak_shortage_3y = (peak_shortage + lag(x = peak_shortage, n = 1) + lag(x = peak_shortage, n = 2)) / 3
	       ) %>%
	ungroup()

# EDUCATION AND AGE DATA ---------------------------------------------

state_names_tbl <- plant_tbl %>% 
	select(state_code = new_state_code, state_name) %>%
	distinct()

nss_tbl <- nss_tbl %>%
	mutate(state_code = as.numeric(state)) %>%
	left_join(state_names_tbl, by = c("state_code")) %>%
	filter(!state_name %in% exclude_states) %>%
	select(-c(state)) %>%
	rename(state = state_name)

# POPULATION AND GDP DATA --------------------------------------------
rbi_tbl <- rbi_tbl %>%
	rename(state = asi_state) %>%
	filter(!state %in% exclude_states)

# Smooth the gdp variable --------------------------------------------

smooth_gdp_tbl <- rbi_tbl %>%
	filter(!is.na(net_gdp_cap)) %>%
	group_by(state) %>%
	arrange(year) %>%
	mutate(
	       # test method
	       previous_year = lag(year, 1),
	       next_year = lead(year, 1), 
	       # net_gdp
	       previous_net_gdp = lag(net_gdp, 1),
	       next_net_gdp = lead(net_gdp, 1),
	       smooth_net_gdp = (previous_net_gdp + net_gdp + next_net_gdp) / 3,
	       # net_gdp_cap
	       previous_net_gdp_cap = lag(net_gdp_cap, 1),
	       next_net_gdp_cap = lead(net_gdp_cap, 1),
	       smooth_net_gdp_cap = (previous_net_gdp_cap + net_gdp_cap + next_net_gdp_cap) / 3
	       ) %>%
	ungroup() %>%
	select(state, year, smooth_net_gdp, smooth_net_gdp_cap) 


growth_gdp_tbl <- rbi_tbl %>% 
	select(state, year, net_gdp_cap, net_gdp) %>%
	group_by(state) %>%
	arrange(year) %>% 
	mutate(
	       net_gdp_growth = (net_gdp - lag(net_gdp, 1)) / lag(net_gdp, 1),
	       net_gdp_cap_growth = (net_gdp_cap - lag(net_gdp_cap, 1)) / lag(net_gdp_cap, 1)
	       ) %>%
	ungroup() %>%
	select(state, year, net_gdp_growth, net_gdp_cap_growth) %>%
	mutate(
	       base_year_dmy = case_when(
					  year >= 2012 ~ "2012",
					  year %in% 2005:2011 ~ "2004",
					  year %in% 2000:2004 ~ "2000",
					  year == 1999 ~ "1994",
					  TRUE ~ NA_character_
					  )
	       )

rbi_tbl <- rbi_tbl %>%
	left_join(smooth_gdp_tbl, by = c("state", "year")) %>%
	left_join(growth_gdp_tbl, by = c("state", "year"))

# JOIN ------------------------------------------------------------

state_tbl <- shortage_tbl %>%
	left_join(nss_tbl, by = c("state", "year")) %>%
	left_join(rbi_tbl, by = c("state", "year")) 


##################################################################
##                     RETURN PREPPRED DATA                     ##
##################################################################

return_list <- list(
		    "plant_tbl" = plant_tbl,
		    "state_tbl" = state_tbl
		    )

return(return_list)

# end function
}


