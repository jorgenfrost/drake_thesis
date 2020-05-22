# Read data 
plant_tbl <- readd("analysis_sample_ls")$plant_tbl
state_tbl <- readd("analysis_sample_ls")$state_tbl
pci_tbl <- readd("plant_pci_tbl") %>%
	rename(
	       max_pci = max_complexity,
	       avg_pci = w_avg_complexity
	       )

# CREATE SAMPLE OF NEW ENTRIES ---------------------------------------
# Entry previous year
entry_1y_sample <- plant_tbl %>% 
	filter(time_since_entry == 1) %>%
	select(year, dsl, state_name)

# Entry two years ago
entry_2y_sample <- plant_tbl %>% 
	filter(time_since_entry == 2) %>%
	select(year, dsl, state_name)

# Entry three years ago
entry_3y_sample <- plant_tbl %>% 
	filter(time_since_entry == 3 & initial_production >= 2003) %>%
	select(year, dsl, state_name)

# CREATE LAGGED SHORTAGE TBL -----------------------------------------
lagged_shortage_tbl <- state_tbl %>%
	rename(state_name = state, avg_shortage_year = year)

# ASSIGN SHORTAGE VARIABLE TO PLANTS, CREATE INDUSTRY-YEAR -----------

plant_tbl <- plant_tbl %>%
	inner_join(lagged_shortage_tbl)


plant_tbl <- plant_tbl %>%
	mutate(
	       nic87year = paste0(nic87, year),
	       nic98_2year = paste0(nic98_2d, year)
	       )

# Select controls and outcomes for minimal model

plant_tbl %>%
	select( # TODO: outcome 1: industry-mean of electricity/revenue
	        # outcome 2: labor/rev share 
	        max_complexity_lenient, # outcome 3: max complexity (pci and qp)
	        w_avg_complexity # outcome 4: avg complexity (pci and qp)

	        # plant ID
	        # industry-year (98)
	        # industry-year (87)
	        # state 
	        # year 
	        
plant_tbl %>%
	select( # outcome 1: industry-mean of electricity/revenue
	        # outcome 2: labor/rev share 
	        # outcome 3: max complexity (pci and qp)
	        # outcome 4: avg complexity (pci and qp)

	        # plant ID
	        # industry (98)
	        # industry (87)
	        # state 
	        # year 
	        # urban populatio
	        # gdp growth
	        # gdp growth smoothed
	        # share with secondary education
	        # 

# Analysis 1: Controlling for industry

# RUN ROBUSTNESS CHECKS --------------------------------------------------------------

# Create sample of new entries

# Consideration 1: When I just take plants that are between 1:5, I will possibly get the 
# same factories 5 times in a row. I there for only take one year at a time. 



# Consideration 2: I need to have the 2- or 3-year avg shortage variable to be the year 
# of entry.

# Consideration 3: Need to be a bit careful with not getting entry years that are out of sample.



test_tbl <- entry_3y_sample %>%
	left_join(lagged_shortage_tbl, by = c("initial_production" = "avg_shortage_year", "state_name")) 


lm_robust(w_avg_complexity_lenient ~ avg_shortage_2y + as.factor(nic98_2d), data = test_tbl, weights = multiplier) %>%
	summary()

mc_plant_tbl <- readd("analysis_sample_mean_complexity_ls")$plant_tbl
mc_state_tbl <- readd("analysis_sample_mean_complexity_ls")$state_tbl

entry_1y_sample$year %>% hist()
