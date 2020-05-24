larger_entry_max_path = "doc/tables/plant_entry/pci_max_larger.tex"
larger_entry_mean_path = "doc/tables/plant_entry/pci_mean_larger.tex"

##################################################################
##                         PREPARE DATA                         ##
##################################################################
# READ DATA ----------------------------------------------------------
source("packages.R")
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
	left_join(state_variables, by = c("initial_production" = "year", "state_name")) %>%
	mutate(
	       asicc = ifelse(year > 2011, 1, 0)
	       )



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

##################################################################
##                MAX AND AVG PCI ON SHORTAGE DATA              ##
##################################################################

# Er sign - men ret sensitiv overfor sample size.
lm_robust(
	  avg_pci ~ avg_shortage_2y + share_finished_secondary + net_gdp_cap_growth + as.factor(base_year_dmy),
	  weights = multiplier,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production),
	  clusters = stateyear,
	  se_type = "stata",
	  data = entry_2y_final
	  )
	
# First model: 
# 
# ###### MAX PCI ################ TODO: 
# From above: 
lm1 <-lm(max_pci ~ avg_shortage_2y + share_finished_secondary + share_15_60 + net_gdp_cap_growth + as.factor(base_year_dmy) + as.factor(state_name) + as.factor(initial_production),
   weights = multiplier,
   data = entry_2y_final
   )

se1 <- coeftest(lm1, vcov = vcovCL, cluster = entry_2y_final$stateyear)

lm2 <- lm(
   max_pci ~ avg_shortage_2y + share_finished_secondary + net_gdp_cap_growth + as.factor(base_year_dmy) + as.factor(state_name) + as.factor(initial_production),
   weights = multiplier,
   data = entry_2y_final
   )
se2 <- coeftest(lm2, vcov = vcovCL, cluster = entry_2y_final$stateyear)

# Including controls on economy size, around .05 level. Changes with net_gdp_cap
lm3 <- lm(
   max_pci ~ avg_shortage_2y + share_15_60 + net_gdp + share_finished_secondary + net_gdp_cap_growth + as.factor(base_year_dmy) + as.factor(state_name) + as.factor(initial_production),
   weights = multiplier,
   data = entry_2y_final
   )
se3 <- coeftest(lm3, vcov = vcovCL, cluster = entry_2y_final$stateyear)

# Add remove trend - significance goes below
lm4 <- lm(
   max_pci ~ avg_shortage_2y + share_15_60 + net_gdp + share_finished_secondary + as.factor(base_year_dmy) + as.factor(state_name) + as.factor(initial_production),
   weights = multiplier,
   data = entry_2y_final
   )
se4 <- coeftest(lm4, vcov = vcovCL, cluster = entry_2y_final$stateyear)

# Add industry fixed
lm5 <- lm(
	  max_pci ~ avg_shortage_2y + share_15_60 + share_finished_secondary + net_gdp_cap_growth + as.factor(base_year_dmy) + as.factor(nic98_2d) + as.factor(initial_production), 
   weights = multiplier,
   data = entry_2y_final
   )
se5 <- coeftest(lm5, vcov = vcovCL, cluster = entry_2y_final$stateyear)

lm6 <- lm(
   max_pci ~ avg_shortage_2y + as.factor(initial_production) + as.factor(nic98_2d), 
   weights = multiplier,
   data = entry_2y_final
   )
se6 <- coeftest(lm6, vcov = vcovCL, cluster = entry_2y_final$stateyear)

# CREATE REGRESSION TABLE
larger_max_star <- stargazer(
		se1, se2, se3, se4, se5, se6,
		dep.var.labels = "$C^{max}_{f}$",
		dep.var.labels.include = TRUE,
		type = "latex",
		omit = c("base_year_dmy", "state_name", "initial_production", "nic98_2d"),
		omit.labels = c("GDP base dmy", "State effects", "Entry year effects", "nic98_2d"),
		float = FALSE,
		#column.sep.width = "1pt",
		font.size = "small",
	#	add.lines = list(
	#			 c("Observations:", nobs(lm1), nobs(lm2), nobs(lm3), nobs(lm4), nobs(lm5), nobs(lm6))
	#			 ),
	#	omit.table.layout = "n",
		style = "aer"
	#	notes.append = FALSE,
	#	notes.align = "l"
		)

write(larger_max_star, here(larger_entry_max_path))

########################################################
#### AVG PCI
##########################################
lm1 <-lm(
   avg_pci ~ avg_shortage_2y + share_finished_secondary + net_gdp_cap_growth + as.factor(base_year_dmy) + as.factor(state_name) + as.factor(initial_production),
   weights = multiplier,
   data = entry_2y_final
   )

se1 <- coeftest(lm1, vcov = vcovCL, cluster = entry_2y_final$stateyear)

# Add share of working age pop
lm2 <- lm(
   avg_pci ~ avg_shortage_2y + share_15_60 + share_finished_secondary + net_gdp_cap_growth + as.factor(base_year_dmy) + as.factor(state_name) + as.factor(initial_production),
   weights = multiplier,
   data = entry_2y_final
   )
se2 <- coeftest(lm2, vcov = vcovCL, cluster = entry_2y_final$stateyear)
se2.1 <- coeftest(lm2, vcov = vcovCL, cluster = entry_2y_final$state_name)

# Including controls on economy size, around .05 level. Changes with net_gdp_cap
lm3 <- lm(
   avg_pci ~ avg_shortage_2y + share_15_60 + net_gdp + share_finished_secondary + net_gdp_cap_growth + as.factor(base_year_dmy) + as.factor(state_name) + as.factor(initial_production),
   weights = multiplier,
   data = entry_2y_final
   )
se3 <- coeftest(lm3, vcov = vcovCL, cluster = entry_2y_final$stateyear)
se3.1 <- coeftest(lm3, vcov = vcovCL, cluster = entry_2y_final$state_name)

# Add remove trend - significance goes below
lm4 <- lm(
   avg_pci ~ avg_shortage_2y + share_15_60 + net_gdp + share_finished_secondary + as.factor(base_year_dmy) + as.factor(state_name) + as.factor(initial_production),
   weights = multiplier,
   data = entry_2y_final
   )
se4 <- coeftest(lm4, vcov = vcovCL, cluster = entry_2y_final$stateyear)

# Add industry fixed
lm5 <- lm(
   avg_pci ~ avg_shortage_2y + share_15_60 + net_gdp + share_finished_secondary + net_gdp_cap_growth + as.factor(base_year_dmy) + as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d), 
   weights = multiplier,
   data = entry_2y_final
   )
se5 <- coeftest(lm5, vcov = vcovCL, cluster = entry_2y_final$stateyear)

lm5.1 <- lm(
   avg_pci ~ avg_shortage_2y + share_15_60 + net_gdp + share_finished_secondary + net_gdp_cap_growth + as.factor(base_year_dmy) + as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d), 
   weights = multiplier,
   data = entry_2y_final
   )
se5.1 <- coeftest(lm5, vcov = vcovCL, cluster = entry_2y_final$stateyear)

lm6 <- lm(
   avg_pci ~ avg_shortage_2y + as.factor(initial_production) + as.factor(nic98_2d), 
   weights = multiplier,
   data = entry_2y_final
   )
se6 <- coeftest(lm6, vcov = vcovCL, cluster = entry_2y_final$stateyear)

## CREATE TABLE FOR AVG COMPLEXITY
		# covariate.labels = c("$\\bar{S_{s}}$ at initial production", "Pop share between 15-60", "Net state product", "Sec. education share", "Net state product/cap growth"),

larger_mean_star <- stargazer(
		se1, se2, se3, se4, se5, se6,
		dep.var.labels = "$C_{f}$",
		dep.var.labels.include = TRUE,
		type = "latex",
		#align = TRUE,
		omit = c("base_year_dmy", "state_name", "initial_production", "nic98_2d"),
	#	omit.labels = c("GDP base dummy", "State effects", "Entry year effects", "Industry effects"),
		float = FALSE,
		#column.sep.width = "1pt",
		font.size = "small",
		add.lines = list(
				 c("Observations:", nobs(lm1), nobs(lm2), nobs(lm3), nobs(lm4), nobs(lm5), nobs(lm6))
				 ),
		omit.table.layout = "n",
		style = "aer", 
		notes.append = FALSE,
		notes.align = "l"
		)

write(larger_mean_star, here(larger_entry_mean_path))
