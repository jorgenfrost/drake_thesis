#' This function is a messy piece of shit, but it saves the two regression
#' tables from the plant entry analysis.
#' 
#' @export

analyse_plant_entry <- function(
				plant_tbl = readd("analysis_sample_ls")$plant_tbl,
				pci_table_path = "doc/tables/plant_entry/pci_nonfloat.tex",
				electricity_table_path = "doc/tables/plant_entry/electricity_nonfloat.tex",
				sample_histogram_year_out = "doc/figures/analysis_entry_sample_year_histograms.pdf",
				sample_histogram_state_out = "doc/figures/analysis_entry_sample_state_histograms.pdf"
				) {
##################################################################
##                         PREPARE DATA                         ##
##################################################################
# READ DATA ----------------------------------------------------------

plant_tbl <- plant_tbl %>%
	mutate(nic98_1d = str_sub(nic98_3d, 1, 1))

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

# Create year-mean and median complexity
state_pci <- plant_tbl %>%
	inner_join(pci_tbl) %>%
	uncount(multiplier) %>%
	group_by(year, state_name) %>%
	summarize(
		  mean_state_avg_pci = mean(avg_pci),
		  mean_state_avg_pci = mean(max_pci),
		  median_state_max_pci = median(max_pci),
		  median_state_max_pci = median(avg_pci)
		  )
	
india_pci <- plant_tbl %>%
	inner_join(pci_tbl) %>%
	uncount(multiplier) %>%
	group_by(year) %>%
	summarize(
		  mean_total_avg_pci = mean(avg_pci),
		  mean_total_max_pci = mean(max_pci),
		  median_total_max_pci = median(max_pci),
		  median_total_avg_pci = median(avg_pci)
		  )

# Create industry avg pci
industry_pci <- plant_tbl %>%
	inner_join(pci_tbl) %>%
	uncount(multiplier) %>%
	group_by(nic98_2d, year) %>%
	summarize(
		  mean_ind_year_avg_pci = mean(avg_pci),
		  mean_ind_year_max_pci = mean(max_pci),
		  median_ind_year_avg_pci = median(avg_pci),
		  median_ind_year_max_pci = median(max_pci)
		  )

# Create industry avg pci
industry_all_pci <- plant_tbl %>%
	inner_join(pci_tbl) %>%
	uncount(multiplier) %>%
	group_by(nic98_2d) %>%
	summarize(
		  mean_ind_all_avg_pci = mean(avg_pci),
		  mean_ind_all_max_pci = mean(max_pci),
		  median_ind_all_avg_pci = median(avg_pci),
		  median_ind_all_max_pci = median(max_pci)
		  )

ind_pci_tbl <- industry_pci %>%
	left_join(industry_all_pci)

mean_pci_tbl <- state_pci %>%
	left_join(india_pci) 

# CREATE SAMPLE OF NEW ENTRIES ---------------------------------------
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
	       self_generated_electricity_kwh,
	       nic98_1d
	       )
	return(df)
}

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

# Select controls and outcomes for minimal model
state_controls <- state_tbl %>%
	select(
	       state_name,
	       year,

	       imp_urban_pop,
	       imp_total_pop,
	       imp_urban_share,
	       share_finished_secondary,
	       share_15_60,
	       
	       net_gdp,
	       net_gdp_growth,
	       net_gdp_cap,
	       net_gdp_cap_growth,
	       base_year_dmy,
	       
	       smooth_net_gdp,
	       smooth_net_gdp_cap
	       )

#################################################################
##                    PREPARE MINIMAL MODEL                    ##
#################################################################


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
	       nic98_1year = paste0(nic98_1d, initial_production),
	       nic87year = paste0(nic87, initial_production)
	       ) %>%
	left_join(mean_pci_tbl, by = c("year", "state_name")) %>%
	left_join(ind_pci_tbl, by = c("year", "nic98_2d"))

	nrow_out <- nrow(df)
	print(paste("Rows after plant vars:", nrow_out))
	df <- df %>%
		filter(!is.na(nic87))

	nrow_out2 <- nrow(df)
	print(paste("Rows after removing obs without industry:", nrow_out2))


	return(df)
}

entry_2y_final_minimal <- entry_2y_sample %>% add_vars()
entry_3y_final_minimal <- entry_3y_sample %>% add_vars()

##################################################################
##                MAX AND AVG PCI ON SHORTAGE DATA              ##
##################################################################
# MAX PCI
# Significant på preferred:
# SPØRGSMÅL: DET SER UD SOM OM INDUSTRIERNE STÅR FOR NÆSTEN AL MAX PCI. MEN DET ER OK?

# Preferred: industry-year effects, clustered by state-year
#max_pci_minimal <- 
	lm_robust(
	  max_pci ~ avg_shortage_2y,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2year),
	  weights = multiplier,
	  clusters = stateyear,
	  data = entry_2y_final_minimal,
	  se_type = "stata"
	  ) 
# Test w/out smallest states
#	lm_robust(
#	  avg_pci ~ avg_shortage_2y,
#	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2year),
#	  weights = multiplier,
#	  clusters = stateyear,
#	  data = entry_2y_final_minimal %>% filter(!state_name %in% c("Sikkim", "Goa", "Chandigarh(U.T)", "Nagaland")),
#	  se_type = "stata"
#	  ) 

#
#	lm_robust(
#	  avg_pci ~ avg_shortage_2y + mean_total_avg_pci,
#	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d),
#	  weights = multiplier,
#	  clusters = stateyear,
#	  data = entry_2y_final_minimal,
#	  se_type = "stata"
#	  ) 
#
## clustered by state
#	lm_robust(
#	  max_pci ~ avg_shortage_2y,
#	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2year),
#	  weights = multiplier,
#	  clusters = state_name,
#	  data = entry_2y_final_minimal,
#	  se_type = "stata"
#	  ) 
#
## with just industries, clustered by state
#	lm_robust(
#	  max_pci ~ avg_shortage_2y,
#	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d),
#	  weights = multiplier,
#	  clusters = stateyear,
#	  data = entry_2y_final_minimal,
#	  se_type = "stata"
#	  ) 
#
# AVG PCI
# Preferred: industry-year effects, clustered by state-year
# avg_pci_minimal <- lm_robust(
# 	  avg_pci ~ avg_shortage_2y,
# 	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic87_2year),
# 	  weights = multiplier,
# 	  clusters = stateyear,
# 	  data = entry_2y_final_minimal,
# 	  se_type = "stata"
# 	  ) 
# 
# # clustered by state
# 	lm_robust(
# 	  avg_pci ~ avg_shortage_2y,
# 	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic87_2year),
# 	  weights = multiplier,
# 	  clusters = state_name,
# 	  data = entry_2y_final_minimal,
# 	  se_type = "stata"
# 	  ) 
# 
# # with just industries, clustered by state
# 	lm_robust(
# 	  avg_pci ~ avg_shortage_2y,
# 	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d),
# 	  weights = multiplier,
# 	  clusters = state_name,
# 	  data = entry_2y_final_minimal,
# 	  se_type = "stata"
# 	  ) 
# 
# ##################################################################
# ##                INDUSTRY PCI ON SHORTAGE DATA                 ##
# ##################################################################
# 	lm_robust(
# 	  median_ind_year_avg_pci ~ avg_shortage_2y,
# 	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production),
# 	  weights = multiplier,
# 	  clusters = stateyear,
# 	  data = entry_2y_final_minimal,
# 	  se_type = "stata"
# 	  ) 
# 

##################################################################
##                INPUT SHARES ON SHORTAGE DATA                 ##
##################################################################

	# exclude electricity share flags

	entry_2y_electricity <-
		entry_2y_final_minimal %>% filter(electricity_qty_cost_flag != 1) %>%
		filter(total_electricity_consumed_kwh > 0) %>%
		mutate(
		       electricity_rev_share = adjusted_revenue / total_electricity_consumed_kwh,
		       sefl_gen_dmy = ifelse(self_generated_electricity_kwh > 0, 1, 0)
		       ) %>%
		filter(!is.na(electricity_rev_share)) %>%
		filter(electricity_rev_share < 63005744) # excludes one giant outlier

	entry_2y_electricity %>%
		select(electricity_rev_share) %>%
		arrange(desc(electricity_rev_share))

	# electricity share
#	lm_robust(
#	electricity_rev_share ~ avg_shortage_2y,
#	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2year),
#	  weights = multiplier,
#	  clusters = stateyear,
#	  data = entry_2y_electricity %>% filter(!state_name %in% c("Sikkim", "Goa", "Chandigarh(U.T)", "Nagaland")),
#	  se_type = "stata"
#	  ) 

	lmrob_electricity_share <-
		lm_robust(electricity_rev_share ~ avg_shortage_2y + as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d),
		  se_type = "stata",
		  clusters = stateyear,
		   weights = multiplier,
		   data = entry_2y_electricity
		   ) 

		
	entry_2y_gen_dmy <- 
		entry_2y_final_minimal %>%
		filter(electricity_qty_cost_flag != 1) %>%
		filter(total_electricity_consumed_kwh > 0) %>%
		mutate(
		       self_gen_dmy = ifelse(self_generated_electricity_kwh > 1, 1, 0)
		       )

	lmrob_self_gen <- 
		lm_robust(
			  self_gen_dmy ~ avg_shortage_2y,
			  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d),
		   weights = multiplier,
		   data = entry_2y_gen_dmy,
		   se_type = "stata",
		   clusters = stateyear
		   )

#################################################################
##                        CREATE TABLES                        ##
#################################################################

# MAX PCI MINIMAL ----------------------------------------------
max_pci_robust <- 
	lm_robust(
	  max_pci ~ avg_shortage_2y + as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d),
	  weights = multiplier,
	  clusters = stateyear,
	  data = entry_2y_final_minimal,
	  se_type = "stata"
	  )  

# AVG PCI MINIMAL ----------------------------------------------
avg_pci_robust <- 
	lm_robust(
	avg_pci ~ avg_shortage_2y + as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d),
	  weights = multiplier,
	  clusters = stateyear,
	  data = entry_2y_final_minimal,
	  se_type = "stata"
	  )  

# LAV ALLE DE REGRESSIONER DER BRUGES I OUT-TABELLEN OM TIL LM ROBUST

lmrob_max_pci <- 
	lm_robust(
	   max_pci ~ avg_shortage_2y + as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d), 
	   clusters = stateyear,
	   se_type = "stata",
	   weights = multiplier,
	   data = entry_2y_final_minimal
	)

lmrob_avg_pci <- 
	lm_robust(
	   avg_pci ~ avg_shortage_2y + as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d), 
	   clusters = stateyear,
	   se_type = "stata",
	   weights = multiplier,
	   data = entry_2y_final_minimal
	)

lmrob_median_avg_pci <- 
	lm_robust(
	   median_ind_all_avg_pci ~ avg_shortage_2y + as.factor(state_name) + as.factor(initial_production), 
	   weights = multiplier,
	   clusters = stateyear,
	   se_type = "stata",
	   data = entry_2y_final_minimal
	) 

lmrob_median_max_pci <- 
	lm_robust(
	   median_ind_all_max_pci ~ avg_shortage_2y + as.factor(state_name) + as.factor(initial_production), 
	   weights = multiplier,
	   clusters = stateyear,
	   se_type = "stata",
	   data = entry_2y_final_minimal
	) 

plant_entry_table_path <- here("doc/tables/plant_entry/entry_minimal.tex")

# book
library(texreg)
texreg(
       l = list(lmrob_max_pci, lmrob_avg_pci, lmrob_median_max_pci, lmrob_median_avg_pci, lmrob_self_gen, lmrob_electricity_share),
        custom.model.names = c("$C^{max}_{f}$", "$C_{f}$", "$C^{max}_{ind}$", "$C_{ind}$", "Self-gen (1)", "Electricity rev share"),
       custom.coef.names = c(NA, "$\\bar{S}_{s,t}$"),
       include.ci = FALSE,
       file = plant_entry_table_path,
      omit.coef = "(factor)|(ranef)",
       booktabs = TRUE,
       fontsize = "small",
       table = FALSE,
       use.packages = FALSE,
       )

# CREATE TABLE:
pci_star <- stargazer(
		      max_pci_se, avg_pci_se, industry_median_max_pci_se, industry_median_avg_pci_se,
		      title = "Association between complexity of new plants and electricity shortages",
		      dep.var.labels.include = FALSE,
		      type = "latex",
		      #align = TRUE,
		      column.labels = c("$C^{max}_{f}$", "$C_{f}$", "$C^{max}_{ind}$", "$C_{ind}$"),
		      covariate.labels = c("$\\bar{S_{s}}$ at initial production"),
		      omit = c("state_name", "initial_production", "nic98_2year"),
		      omit.labels = c("State effects", "Entry year effects", "Industry-in-year effects"),
		      float = FALSE,
		      #column.sep.width = "1pt",
		      font.size = "small",
		      add.lines = list(
				       c("Observations:", nobs(lm_max_pci), nobs(lm_avg_pci), nobs(lm_industry_all_median_max_pci), nobs(lm_industry_all_median_avg_pci))
				       ),
		      omit.table.layout = "n",
		      style = "aer", notes.append = FALSE, notes.align = "l"
		      ) 

write(pci_star, here(pci_table_path))

self_gen_star <- stargazer(
		      self_gen_se, electricity_share_se,
		      title = "Association between electricity use of new plants and electricity shortages",
		      dep.var.labels.include = FALSE,
		      type = "latex",
		      #align = TRUE,
		      column.labels = c("Self generator (1)", "Electricity revenue share"),
		      covariate.labels = c("$\\bar{S_{s}}$ at initial production"),
		      omit = c("state_name", "initial_production", "nic98_2year"),
		      omit.labels = c("State effects", "Entry year effects", "Industry-in-year effects"),
		      float = FALSE,
		      #column.sep.width = "1pt",
		      font.size = "small",
		      add.lines = list(
				       c("Observations:", nobs(lm_self_gen), nobs(lm_electricity_share))
				       ),
		      omit.table.layout = "n",
		      style = "aer", notes.append = FALSE, notes.align = "l"
		      ) 

write(self_gen_star, here(electricity_table_path))





# CREATE HISTOGRAM OF ANALYSIS SAMPLE
# Ingen af resultaterne ændrer sig ved at udelække mindste år
red_blue <- c("#a50f15", "#253494")


pci_by_year_hist <- ggplot(entry_2y_final_minimal, aes(x = avg_pci)) +
	geom_histogram(color = red_blue[1], fill = "white") +
	facet_wrap(~ year) + 
	theme_pubr() +
	xlab(TeX('$C^{max}_{f}$'))

pci_by_state_hist <- ggplot(entry_2y_final_minimal, aes(x = avg_pci)) +
	geom_histogram(color = red_blue[2], fill = "white") +
	facet_wrap(~ state_name) +
	theme_pubr() +
	xlab(TeX('$C^{max}_{f}$'))

ggsave(plot = pci_by_year_hist, here(sample_histogram_year_out))
ggsave(plot = pci_by_state_hist, here(sample_histogram_state_out))

}

