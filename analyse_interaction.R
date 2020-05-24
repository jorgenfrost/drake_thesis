source("packages.R")

defl <- read_dta(here("data/external/deflators/Deflators/final-output-deflator.dta")) %>%
	mutate(nic3digit = as.character(nic3digit))

##################################################################
##                         PREPARE DATA                         ##
##################################################################
plant_tbl <- readd("analysis_sample_ls")$plant_tbl

plant_labor <- readd("plant_labour_input_tbl") %>%
	select(-total_production_cost)

state_tbl <- 
	readd("analysis_sample_ls")$state_tbl %>%
	mutate(
	       imp_urban_share = imp_urban_pop / imp_total_pop
	       ) %>%
	rename(state_name = state) %>%
	group_by(state_name) %>%
	arrange(year) %>%
	mutate(
	       lag_avg_short = lag(avg_shortage, 1),
	       lead_avg_short = lead(avg_shortage, 1)
	       )

pci_tbl <- readd("plant_pci_tbl") %>%
	rename(
	       max_pci = max_complexity,
	       avg_pci = w_avg_complexity,
	       pci_match = match
	       ) %>%
	filter(pci_match == "lenient")

input_shortage_tbl <- readd("plant_input_shortage_tbl")

# Calculate standardized production costs
tot_prod_tbl <- plant_tbl %>%
	select(year, dsl, total_production_cost) %>%
	group_by(year) %>%
	mutate(stand_production_costs = (total_production_cost - mean(total_production_cost, na.rm = TRUE)) / sd(total_production_cost, na.rm = TRUE)) %>%
	select(-total_production_cost)

# SELECT VARIABLES OF INTEREST - plant variables:
select_plant_vars <- function(df) {
	df <- df %>%
	select(
	       year,
	       dsl,
	       state_name,
	       initial_production,
	       multiplier,
	       input_share_products,
	       input_share_all,
	       total_production_cost,
	       nic87,
	       nic98_2d,
	       nic98_3d,
	       count_products,
	       avg_total_employees,
	       wage_share_costs,
	       wage_share_revenue,
	       wage_share_flag,
	       total_electricity_consumed_kwh,
	       electricity_qty_cost_flag,
	       revenue,
	       adjusted_revenue,
	       self_generated_electricity_kwh
	       )
	return(df)
}

plant_sample <- plant_tbl %>% select_plant_vars()

state_tbl <- readd("analysis_sample_ls")$state_tbl %>%
	mutate(
	       imp_urban_share = imp_urban_pop / imp_total_pop
	       ) %>%
	rename(state_name = state) 

state_sample <- state_tbl %>% 
	select(
	       year,
	       state_name,
	       avg_shortage,
	       peak_shortage,
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

##################################################################
##                JOIN TO FINAL SAMPlE DATAFRAME                ##
##################################################################

analysis_sample <- 
	plant_sample  %>% 
	inner_join(pci_tbl) %>%
	inner_join(state_sample) %>%
	left_join(plant_labor) %>%
	left_join(input_shortage_tbl) %>%
	left_join(tot_prod_tbl) %>%
	filter(!is.na(nic98_2d)) %>%
	filter(total_production_cost > 0) %>%
	mutate(
	       nic98_2year = ifelse(is.na(nic98_2d), NA, paste0(nic98_2d, year)),
	       nic87_3year = ifelse(is.na(nic87), NA, paste0(nic87, year)),
	       stateyear = paste0(state_name, year)
	       )
#################################################################
##                   RUN REVENUE REGRESSIONS                   ##
#################################################################

# Outcome:
# Adjusted revenue

# Controls to include:
# total employees, 
# wage share of revenue
# industry
# complexity
# 

	# free up memory
rm(list = ls()[ls() != "analysis_sample"])

coeftest(lm1, vcov = vcovCL, cluster = analysis_sample$stateyear)

# Adjusted revenue is only available for before 2010, so sample shrinks.
analysis_sample2 <- analysis_sample %>% filter(wage_share_flag == 0) %>% filter(avg_total_employees > 0) %>%
	mutate(self_gen_dmy = ifelse(self_generated_electricity_kwh > 0, 1, 0)) %>%
	filter(!is.na(adjusted_revenue))

# Model of base controls ---------------------------------------------
lm2.base <- lm(
	    formula = log(adjusted_revenue) ~ wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap + as.factor(state_name) + as.factor(year) + as.factor(nic98_2d),
	    weights = multiplier,
	    data = analysis_sample2
	    )

lm2.log.base.og <- lm(
	    formula = log(adjusted_revenue) ~ log(wage_share_revenue) + as.factor(self_gen_dmy) + log(avg_total_employees) + stand_production_costs + log(net_gdp_cap) + as.factor(state_name) + as.factor(year) + as.factor(nic98_2d),
	    weights = multiplier,
	    data = analysis_sample2
	    )
	
# Model: base + pci --------------------------------------------------
lm2.pci <- lm(
	    formula = log(adjusted_revenue) ~ max_pci + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap + as.factor(state_name) + as.factor(year) + as.factor(nic98_2d),
	    weights = multiplier,
	    data = analysis_sample2
	    )

lm2.log.pci <- lm(
	    formula = log(adjusted_revenue) ~ max_pci + log(wage_share_revenue) + as.factor(self_gen_dmy) + log(avg_total_employees) + stand_production_costs + log(net_gdp_cap) + as.factor(state_name) + as.factor(year) + as.factor(nic98_2d),
	    weights = multiplier,
	    data = analysis_sample2
	    )

# Model: base + pci + shortages --------------------------------------
lm2.pci.shortage <- lm(
	    formula = log(adjusted_revenue) ~ max_pci + avg_shortage + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap + as.factor(state_name) + as.factor(year) + as.factor(nic98_2d),
	    weights = multiplier,
	    data = analysis_sample2
	    )

lm2.log.pci.shortage <- lm(
	    formula = log(adjusted_revenue) ~ max_pci + avg_shortage + log(wage_share_revenue) + as.factor(self_gen_dmy) + log(avg_total_employees) + stand_production_costs + log(net_gdp_cap) + as.factor(state_name) + as.factor(year) + as.factor(nic98_2d),
	    weights = multiplier,
	    data = analysis_sample2
	    )
# Model: base + pci + supply shortages -------------------------------

# Model: base + pci + shortages --------------------------------------


coeftest(lm2.pci.shortage, vcov = vcovCL, cluster = analysis_sample2$stateyear, type = "HC1") 
coeftest(lm2.log.pci.shortage, vcov = vcovCL, cluster = analysis_sample2$stateyear, type = "HC1") 

ggplot(uncount(analysis_sample2, multiplier), aes(x = max_pci)) + 
	geom_histogram()
# 2.3 but adding self gen control
lm2.4 <- lm(
   formula = log(adjusted_revenue) ~ max_pci + avg_shortage + wage_share_revenue + self_gen_dmy + log(avg_total_employees) + stand_production_costs + net_gdp_cap + as.factor(state_name) + as.factor(year) + as.factor(nic87),
   weights = multiplier,
   data = analysis_sample2
   ) 



lm2.base %>% summary()
nobs(lm2.base)
analysis_sample2 %>%
	filter(is.na(nic87))

# TODO: Nået her til.



lm2.5 <- lm(
   formula = log(adjusted_revenue) ~ max_pci + avg_shortage + wage_share_revenue + self_gen_dmy + avg_total_employees + stand_production_costs + net_gdp_cap + as.factor(state_name) + as.factor(year) + as.factor(nic98_2d),
   weights = multiplier,
   data = analysis_sample2
   ) 


analysis_sample$avg_total_employees %>% range()
coeftest(lm2.2, vcov = vcovCL, cluster = analysis_sample$stateyear, type = "HC1")
coeftest(lm2.3, vcov = vcovCL, cluster = analysis_sample2$stateyear, type = "HC1")
coeftest(lm2.4, vcov = vcovCL, cluster = analysis_sample2$stateyear, type = "HC1")





# Tredie model: revenue by electricity og complexity interaction
lm(
   formula = adjusted_revenue ~ avg_shortage * avg_pci + total_production_cost + avg_total_employees + wage_share_revenue + net_gdp_cap + as.factor(state_name) + as.factor(year) + as.factor(nic98_3d),
   weights = multiplier,
   data = analysis_sample
   ) 



	formula = adjusted_revenue ~ avg_shortage + avg_total_employees + wage_share_revenue + net_gdp_cap,
	fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic87),
	clusters = stateyear,
	se_type = "stata",
	weights = multiplier,
	data = analysis_sample
analysis_sample$nic98_2d %>% n_distinct()
