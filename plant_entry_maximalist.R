##################################################################
##                         PREPARE DATA                         ##
##################################################################
# READ DATA ----------------------------------------------------------
plant_tbl <- readd("analysis_sample_ls")$plant_tbl
state_tbl <- readd("analysis_sample_ls")$state_tbl %>%
	mutate(
	       imp_urban_share = imp_urban_pop / imp_total_pop
	       ) %>%
	rename(state_name = state) 

pci_tbl <- readd("plant_pci_tbl") %>%
	rename(
	       max_pci = max_complexity,
	       avg_pci = w_avg_complexity,
	       pci_match = match
	       ) %>%
	filter(pci_match == "lenient")


# SELECT VARIABLES OF INTEREST - plant variables:
select_entry_vars <- function(df) {
	df <- df %>%
	select(
	       year,
	       dsl,
	       state_name,
	       initial_production,
	       multiplier,
	       hhi_all,
	       hhi_products,
	       input_share_all,
	       input_share_products,
	       count_products,
	       wage_share_costs,
	       wage_share_revenue,
	       wage_share_flag,
	       total_electricity_consumed_kwh,
	       electricity_qty_cost_flag,
	       adjusted_revenue,
	       self_generated_electricity_kwh
	       )
	return(df)
}

# SELECT VARIABLES OF INTEREST - plant variables:
state_tbl <- readd("analysis_sample_ls")$state_tbl %>%
	mutate(
	       imp_urban_share = imp_urban_pop / imp_total_pop
	       ) %>%
	rename(state_name = state) 

state_variables <- state_tbl %>% 
	select(
	       year,
	       state_name,
	       share_15_60,  # number of workers
	       share_finished_secondary,
	       net_gdp_cap_growth,
	       base_year_dmy,
	       net_gdp_cap,
	       smooth_net_gdp_cap,
	       smooth_net_gdp,
	       net_gdp,
	       imp_urban_share
	       )


# Entry two years ago
entry_2y_sample <- plant_tbl %>% 
	filter(time_since_entry == 2) %>%
	select_entry_vars()

# Entry three years ago
entry_3y_sample <- plant_tbl %>% 
	filter(time_since_entry == 3) %>%
	select_entry_vars()

# CREATE LAGGED SHORTAGE TBL -----------------------------------------
lagged_shortage_tbl <- state_tbl %>%
	rename(lag_shortage_year = year) %>%
	select(
	       state_name,
	       lag_shortage_year,
	       avg_shortage_2y,
	       avg_shortage_3y,
	       peak_shortage_2y,
	       peak_shortage_3y
	       )

# ASSIGN SHORTAGE VARIABLE TO PLANTS, ADD PCI, CREATE INDUSTRY-YEAR -----------
complexity_tbl <- plant_tbl %>%
	inner_join(pci_tbl) %>% 
	select(year, dsl, state_name, max_pci, avg_pci)

industry_tbl <- plant_tbl %>% 
	mutate(
	       self_gen_dummy = ifelse(self_generated_share > 0, 1, 0)
	       ) %>%
	select(year, dsl, state_name, nic98_2d, nic98_3d, nic87) 

# The plants' entry year = year of initial production
# and the "year" variable is the year of inital production.
# I attach the lagged shortage to the year of initial production,
# so that the value is the two/three years before entry
entry_2y_sample <- entry_2y_sample %>%
	inner_join(lagged_shortage_tbl, by = c("state_name", "initial_production" = "lag_shortage_year"))

entry_3y_sample <- entry_3y_sample %>%
	inner_join(lagged_shortage_tbl, by = c("state_name", "initial_production" = "lag_shortage_year"))

##################################################################
##                     PREPARE LARGER MODEL                     ##
##################################################################

# ADD VARIABLES ------------------------------------------------------
add_vars <- function(df) {

	nrow_in <- nrow(df)

	print(paste("Rows in:", nrow_in))

	# ADD LAGGED SHORTAGES

	# ADD STATE CONTROLS ----------------------------------------
	#         df <- df %>%
	#         inner_join(state_controls, by = c("year", "state_name"))
	#         
	#         rows_state <- nrow(df)
	# 
	#         print(paste("Rows after state_controls:", rows_state))

	# ADD COMPLEXITY VARS ----------------------------------------
	df <- df %>%
	inner_join(complexity_tbl, by = c("dsl", "year")) %>%
	inner_join(industry_tbl, by = c("dsl", "year")) %>%
	mutate(
	       stateyear = paste0(initial_production,"-",state_name),
	       nic87_2d = str_sub(nic87, start = 1, end = 2),
	       nic87_2year = paste0(nic87_2d, initial_production),
	       nic98_2year = paste0(nic98_2d, initial_production),
	       nic87year = paste0(nic87, initial_production)
	       ) %>%
	left_join(mean_pci_tbl, by = c("year", "state_name")) %>%
	left_join(ind_pci_tbl, by = c("year", "nic98_2d")) %>%
	left_join(state_variables, by = c("initial_production" = "year", "state_name"))



	nrow_out <- nrow(df)
	print(paste("Rows after plant vars:", nrow_out))
	df <- df %>%
		filter(!is.na(nic87))

	nrow_out2 <- nrow(df)
	print(paste("Rows after removing obs without industry:", nrow_out2))


	return(df)
}

entry_2y_final <- entry_2y_sample %>% add_vars()
entry_3y_final <- entry_3y_sample %>% add_vars()
entry_4y_final <- entry_4y_sample %>% add_vars()

##################################################################
##                MAX AND AVG PCI ON SHORTAGE DATA              ##
##################################################################

# Er sign
lm_robust(
	  max_pci ~ avg_shortage_2y + share_finished_secondary + net_gdp_cap_growth + as.factor(base_year_dmy),
	  weights = multiplier,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production),
	  clusters = stateyear,
	  se_type = "stata",
	  data = entry_2y_final
	  )
	
	  
lm_robust(
	  max_pci ~ avg_shortage_2y + share_15_60 + share_finished_secondary + net_gdp_cap_growth + net_gdp,
	  weights = multiplier,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production),
	  clusters = stateyear,
	  se_type = "stata",
	  data = entry_2y_final
	  )


