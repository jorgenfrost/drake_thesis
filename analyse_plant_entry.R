# TODO: Prepare full model


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

rca_qp_tbl <- readd("plant_complexity_rca_tbl") %>%
	rename(
	       max_rca_qp = max_complexity,
	       avg_rca_qp = w_avg_complexity,
	       rca_qp_match = match
	       ) %>%
	filter(rca_qp_match == "lenient")

mean_qp_tbl <-readd("plant_mean_complexity_tbl") %>%
	rename(
	       max_mean_qp = max_complexity,
	       avg_mean_qp = w_avg_complexity,
	       mean_qp_match = match
	       ) %>%
	filter(mean_qp_match == "lenient")
# CREATE SAMPLE OF NEW ENTRIES ---------------------------------------

# Entry two years ago
entry_2y_sample <- plant_tbl %>% 
	filter(time_since_entry == 2) %>%
	select(
	       year,
	       dsl,
	       state_name,
	       initial_production,
	       multiplier
	       )

# Entry three years ago
entry_3y_sample <- plant_tbl %>% 
	filter(time_since_entry == 3) %>%
	select(
	       year,
	       dsl,
	       state_name,
	       initial_production,
	       multiplier
	       )

# Entry 4 years ago
entry_4y_sample <- plant_tbl %>% 
	filter(time_since_entry == 4)  %>%
	select(
	       year,
	       dsl,
	       state_name,
	       initial_production,
	       multiplier
	       )

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
	inner_join(mean_qp_tbl) %>%
	inner_join(rca_qp_tbl) %>%
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

entry_4y_sample <- entry_3y_sample %>%
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
	       nic87year = paste0(nic87, initial_production)
	       )

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
entry_4y_final_minimal <- entry_4y_sample %>% add_vars()

##################################################################
##                MAX AND AVG PCI ON SHORTAGE DATA              ##
##################################################################
# MAX PCI
# Significant på preferred:
# SPØRGSMÅL: DET SER UD SOM OM INDUSTRIERNE STÅR FOR NÆSTEN AL MAX PCI. MEN DET ER OK?

# Preferred: industry-year effects, clustered by state-year
max_pci_minimal <- lm_robust(
	  max_pci ~ avg_shortage_2y,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2year),
	  weights = multiplier,
	  clusters = stateyear,
	  data = entry_2y_final_minimal,
	  se_type = "stata"
	  ) 

	lm_robust(
	  max_pci ~ avg_shortage_2y,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2year),
	  weights = multiplier,
	  clusters = stateyear,
	  data = entry_3y_final_minimal,
	  se_type = "stata"
	  ) 

	lm_robust(
	  max_pci ~ avg_shortage_2y.x,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2year),
	  weights = multiplier,
	  clusters = stateyear,
	  data = entry_4y_final_minimal,
	  se_type = "stata"
	  ) 

# clustered by state
	lm_robust(
	  max_pci ~ avg_shortage_2y,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2year),
	  weights = multiplier,
	  clusters = state_name,
	  data = entry_2y_final_minimal,
	  se_type = "stata"
	  ) 

# with just industries, clustered by state
	lm_robust(
	  max_pci ~ avg_shortage_2y,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d),
	  weights = multiplier,
	  clusters = stateyear,
	  data = entry_2y_final_minimal,
	  se_type = "stata"
	  ) 

# AVG PCI
# Preferred: industry-year effects, clustered by state-year
avg_pci_minimal <- lm_robust(
	  avg_pci ~ avg_shortage_2y,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic87_2year),
	  weights = multiplier,
	  clusters = stateyear,
	  data = entry_2y_final_minimal,
	  se_type = "stata"
	  ) 

# clustered by state
	lm_robust(
	  avg_pci ~ avg_shortage_2y,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic87_2year),
	  weights = multiplier,
	  clusters = state_name,
	  data = entry_2y_final_minimal,
	  se_type = "stata"
	  ) 

# with just industries, clustered by state
	lm_robust(
	  avg_pci ~ avg_shortage_2y,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2d),
	  weights = multiplier,
	  clusters = state_name,
	  data = entry_2y_final_minimal,
	  se_type = "stata"
	  ) 

# -------------- TODO IF TIME
# Likelyhood of being a self-generator. 
	lm_robust(
	  self_gen_dummy ~ avg_shortage_2y,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic87_2d),
	  weights = multiplier,
	  clusters = state_name,
	  data = entry_2y_final_minimal,
	  se_type = "stata"
	  ) %>% 
summary()


#################################################################
##                        CREATE TABLES                        ##
#################################################################

# MAX PCI MINIMAL
max_pci_robust <- 
	lm_robust(
	  max_pci ~ avg_shortage_2y,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2year),
	  weights = multiplier,
	  clusters = stateyear,
	  data = entry_2y_final_minimal,
	  se_type = "stata"
	  )  

lm_max_pci <- 
	lm(
	   max_pci ~ avg_shortage_2y + as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2year), 
	   weights = multiplier,
	   data = entry_2y_final_minimal
	)

nrow_max_pci <- nrow(entry_2y_final_minimal)

max_pci_se <- 
	coeftest(lm_max_pci, vcovHC, cluster = "stateyear", type = "HC1")

# avg_pci_robust
avg_pci_robust <- 
	lm_robust(
	avg_pci ~ avg_shortage_2y,
	  fixed_effects = ~ as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2year),
	  weights = multiplier,
	  clusters = stateyear,
	  data = entry_2y_final_minimal,
	  se_type = "stata"
	  )  

lm_avg_pci <- 
	lm(
	   avg_pci ~ avg_shortage_2y + as.factor(state_name) + as.factor(initial_production) + as.factor(nic98_2year), 
	   weights = multiplier,
	   data = entry_2y_final_minimal
	)

nrow_avg_pci <- nrow(entry_2y_final_minimal)

avg_pci_se <- 
	coeftest(lm_avg_pci, vcovHC, cluster = "stateyear", type = "HC1")


pci_star <- stargazer(
		      max_pci_se, avg_pci_se,
		      title = "Association between plant complexity and electricity shortages",
		      dep.var.labels.include = FALSE,
		      type = "latex",
		      #align = TRUE,
		      column.labels = c("$C^{max}$", "C"),
		      covariate.labels = c("\bar{S}"),
		      omit = c("Constant", "state_name", "initial_production", "nic98_2year"),
		      omit.labels = c("Intercept", "State effects", "Entry year effects", "Industry-in-year effects"),
		      float = FALSE,
		      #column.sep.width = "1pt",
		      font.size = "small",
		      add.lines = list(
				       c("Observations:", paste0(nrow_max_pci), paste0(nrow_max_pci))
				       ),
		      omit.table.layout = "n",
		      style = "aer", notes.append = FALSE, notes.align = "l"
		      ) 

write(pci_star, here("doc/tables/plant_entry/pci_nonfloat.tex"))
