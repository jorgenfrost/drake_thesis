source("packages.R")
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
	rename(state_name = state) 

pci_tbl <- readd("plant_pci_tbl") %>%
	rename(
	       max_pci = max_complexity,
	       avg_pci = w_avg_complexity,
	       pci_match = match
	       ) %>%
	filter(pci_match == "lenient")

input_shortage_tbl <- readd("plant_input_shortage_tbl")


# Calculate standardized production costs
tot_prod_tbl <- plant_tbl %%
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
	       self_generated_electricity_kwh,
	       total_electricity_consumed_kwh,
	       electricity_qty_cost_flag,
	       input_share_products
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

# Adjusted revenue is only available for before 2010, so sample shrinks.
analysis_sample_base <- analysis_sample %>% filter(wage_share_flag == 0) %>% filter(avg_total_employees > 0) %>%
	mutate(self_gen_dmy = ifelse(self_generated_electricity_kwh > 0, 1, 0)) 

analysis_sample2 <- analysis_sample_base %>%
	filter(!is.na(adjusted_revenue)) %>%
	select(year, dsl, adjusted_revenue, wage_share_revenue, self_gen_dmy, avg_total_employees, stand_production_costs, net_gdp_cap, state_name, nic98_2d, multiplier, max_pci, avg_pci, input_avg_shortage, avg_shortage, stateyear, base_year_dmy, total_electricity_consumed_kwh, electricity_qty_cost_flag, input_share_products) %>%
	drop_na()

# Model of base controls ---------------------------------------------
lm_robust2.base <- lm_robust(
		formula = log(adjusted_revenue) ~ wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

# Model: base + pci --------------------------------------------------

lm_robust2.pci_max <- lm_robust(
		formula = log(adjusted_revenue) ~ max_pci + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

lm_robust2.pci_avg <- lm_robust(
		formula = log(adjusted_revenue) ~ avg_pci + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )
# Model: base + pci + shortages --------------------------------------
lm_robust2.pci_max.shortage <- lm_robust(
		formula = log(adjusted_revenue) ~ max_pci + avg_shortage + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

lm_robust2.pci_avg.shortage <- lm_robust(
		formula = log(adjusted_revenue) ~ avg_pci + avg_shortage + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

# Model: base + pci + supply shortages -------------------------------
lm_robust2.pci_max.input_short <- lm_robust(
		formula = log(adjusted_revenue) ~ max_pci + input_avg_shortage + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

lm_robust2.pci_avg.input_short <- lm_robust(
		formula = log(adjusted_revenue) ~ avg_pci + input_avg_shortage + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

# Model: base + pci + shortages + pci x shortage --------------------------------------
lm_robust2.pci_max.shortage_interact <- lm_robust(
		formula = log(adjusted_revenue) ~ max_pci + avg_shortage + max_pci * avg_shortage + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

lm_robust2.pci_avg.shortage_interact <- lm_robust(
		formula = log(adjusted_revenue) ~ avg_pci + avg_shortage + avg_pci * avg_shortage + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

# Model: base + pci + supply shortages + pci x supply shortages -----------------------
lm_robust2.pci_max_input_short_interact <- lm_robust(
		formula = log(adjusted_revenue) ~ max_pci + input_avg_shortage + max_pci * input_avg_shortage + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

lm_robust2.pci_avg.input_short_interact <- lm_robust(
		formula = log(adjusted_revenue) ~ avg_pci + input_avg_shortage + avg_pci * input_avg_shortage + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

# Model: base + pci + shortage + pci x shortage + supply shortages + pci x supply shortages -----------------------
lm_robust2.pci_avg.input_short_interact_plus <- lm_robust(
		formula = log(adjusted_revenue) ~ avg_pci + input_avg_shortage + avg_pci * input_avg_shortage + avg_shortage + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

lm_robust2.pci_max.input_short_interact_plus <- lm_robust(
		formula = log(adjusted_revenue) ~ max_pci + input_avg_shortage + max_pci * input_avg_shortage + avg_shortage + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

#################################################################
##                CREATE TABLES FOR MAX PCI                    ##
#################################################################

library(texreg)
max_pci_tbl_path <- here("doc/tables/analysis_interaction_max_pci.tex")
texreg(
       l = list(lm_robust2.base, lm_robust2.pci_max, lm_robust2.pci_max.shortage, lm_robust2.pci_max.shortage_interact, lm_robust2.pci_max.input_short, lm_robust2.pci_max_input_short_interact,lm_robust2.pci_max.input_short_interact_plus),
       label = "tab:interaction_max_pci",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"),
      custom.coef.names = c("Wage/rev share", "Self-gen (1)", "Number of employees", "Production costs (z)", "State NDP/cap", "$C^{max}_{f}$", "Shortage", "$C^{max}_{f}$ $\\times$ Shortage", "Supply shortage", "$C^{max}_{f}$ $\\times$ Supply shortage"),
       include.ci = FALSE,
       file = max_pci_tbl_path,
       booktabs = TRUE,
       fontsize = "small",
       table = FALSE,
       use.packages = FALSE
       )

avg_pci_tbl_path <- here("doc/tables/analysis_interaction_avg_pci.tex")
texreg(
       l = list(lm_robust2.base, lm_robust2.pci_avg, lm_robust2.pci_avg.shortage, lm_robust2.pci_avg.shortage_interact, lm_robust2.pci_avg.input_short, lm_robust2.pci_avg.input_short_interact, lm_robust2.pci_avg.input_short_interact_plus),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"),
      custom.coef.names = c("Wage/rev share", "Self-gen (1)", "Number of employees", "Production costs (z)", "State NDP/cap", "$C_{f}$", "Shortage", "$C_{f}$ $\\times$ Shortage", "Supply shortage", "$C_{f}$ $\\times$ Supply shortage"),
       include.ci = FALSE,
       file = avg_pci_tbl_path,
       booktabs = TRUE,
       fontsize = "small",
       table = FALSE,
       use.packages = FALSE
       )

# WAGE AND INPUT SHARES ----------------------------------------------------
max_pci_wage_share_supply_interact <-
	lm_robust(
	  formula = log(adjusted_revenue) ~ max_pci + input_avg_shortage + input_avg_shortage * wage_share_revenue + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

max_pci_input_share_supply_interact <-
	lm_robust(
	  formula = log(adjusted_revenue) ~ max_pci + input_avg_shortage + input_avg_shortage * input_share_products + input_share_products + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

max_pci_wage_share_shortage_interact <-
	lm_robust(
		formula = log(adjusted_revenue) ~ max_pci + avg_shortage + avg_shortage * wage_share_revenue + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )

max_pci_input_share_shortage_interact <-
	lm_robust(
	  formula = log(adjusted_revenue) ~ max_pci + avg_shortage + avg_shortage * input_share_products + input_share_products + wage_share_revenue + as.factor(self_gen_dmy) + avg_total_employees + stand_production_costs + net_gdp_cap,
		fixed_effects = ~ as.factor(state_name) + as.factor(year) + as.factor(nic98_2d) + as.factor(base_year_dmy),
		weights = multiplier,
		data = analysis_sample2,
		clusters = stateyear,
		se_type = "stata"
	    )


      # custom.coef.names = c("Wage/rev share", "Self-gen (1)", "Number of employees", "Production costs (z)", "State NDP/cap", "$C_{f}$", "Shortage", "$C_{f}$ $\\times$ Shortage", "Supply shortage", "$C_{f}$ $\\times$ Supply shortage"),


wage_input_tbl_path <- here("doc/tables/analysis_interaction_wage_input.tex")
texreg(
       l = list(max_pci_wage_share_supply_interact, max_pci_input_share_supply_interact, max_pci_wage_share_shortage_interact,  max_pci_input_share_shortage_interact),
        custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
      custom.coef.names = c("$C^{max}_{f}$", "Supply shortage", "Wage/rev share", "Self-gen (1)", "Number of employees", "Production costs (z)", "State NDP/cap", "Supply shortage $\\times$ wage/rev share", "Int. input/rev share", "Int. input share $\\times$ Supply shortage", "Shortage", "Shortage $\\times$ wage/rev share", "Shortage $\\times$ int. input share"),
       include.ci = FALSE,
       file = wage_input_tbl_path,
       booktabs = TRUE,
       fontsize = "small",
       table = FALSE,
       use.packages = FALSE
       )

