
##################################################################
##                        COLLECT VALUES                        ##
##################################################################

# Collect complexity values 
fit_rpca_tbl <- readd("hs96_complexity_tbl") 
fit_rca_tbl <- readd("hs96_rca_complexity_tbl") 

fit_rpca_tbl <- fit_rpca_tbl %>%
	filter(metric == "fitness" & iteration == max(iteration)) %>%
	rename(rpca = val)

fit_rca_tbl <- fit_rca_tbl %>%
	filter(metric == "fitness" & iteration == max(iteration)) %>%
	rename(rca = val)

fit_tidy_tbl <- full_join(fit_rpca_tbl, fit_rca_tbl) %>%
	gather(rca, rpca, key = metric, val = fitness) %>%
	mutate(ln_fitness = log(fitness))

fit_tbl <- full_join(fit_rpca_tbl, fit_rca_tbl) 

# Collect national values: GPD-cap and pop
pop_tbl <- readd("pop_tbl")
gdp_cap_tbl <- readd("gdp_cap_tbl")
gdp_tbl <- readd("gdp_tbl")

# Collect state-wise variables
rbi_tbl <- readd("rbi_tbl")

# Collect shortages
shortage_tbl <- readd("energy_shortages_tbl") %>%
	mutate(
	       state = ifelse(state == "West Bengal + Sikkim", "West Bengal", state)
	       )

exclude_states <- c(
  "Daman and Diu", # RBI 
  "Dadra and Nagar Haveli", # RBI 
  "Lakshadweep", # CEA 
  "Damodar Valley Corporation",  # CEA
  "Telangana", # CEA
  "A and N Islands", # too small
  "Arunachal Pradesh" # only 2 years of ASI data
  )

# Collect analysis sample
plant_tbl <- readd("analysis_sample_ls")$plant_tbl


##################################################################
##                        PREPARE VALUES                        ##
##################################################################

# Prepare shortage table ----------------------------------------
shortage_tbl <- filter(shortage_tbl, !state %in% exclude_states)

# POPULATION AND GDP DATA -----------------------------------------
rbi_tbl <- rbi_tbl %>%
	rename(state = asi_state) %>%
	filter(!state %in% exclude_states)

# Calculate the gdp growth variable ----------------------------------------
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
	       ) %>%
	mutate( # Set growth in years where base changes as NA.
	       net_gdp_growth = ifelse(year == as.numeric(base_year_dmy), NA, net_gdp_growth),
	       net_gdp_cap_growth = ifelse(year == as.numeric(base_year_dmy), NA, net_gdp_cap_growth)
	       )

rbi_tbl <- rbi_tbl %>%
	left_join(growth_gdp_tbl, by = c("state", "year"))

big_states <- rbi_tbl %>% 
	arrange(desc(imp_total_pop)) %>%
	filter(state %in% c(
				"Uttar Pradesh", 
				"Maharashtra",
				"Bihar",
				"West Bengal",
				"Andhra Pradesh",
				"Madhya Pradesh"
				)
	) 

	# Add shortage variabels 
big_states <- left_join(big_states, shortage_tbl)


# Plot BIG STATES againts -----------------------------------------

big_states <- big_states %>%
	mutate(
	       plot_dummy = paste0(state, base_year_dmy)
	       ) %>%
filter(year %in% 2000:2017)

cols <- c("#d7191c", "#fc8d59", "#fee090", "#e0f3f8", "#91bfdb", "#4575b4")

big_states_dt <- big_states %>%
	select(
	       year,
	       state,
	       avg_shortage,
	       net_gdp_growth,
	       net_gdp_cap_growth,
	       base_year_dmy
	       ) %>%
gather(avg_shortage, net_gdp_cap_growth, key = metric, val = val)

big_states_short_plot <- ggplot(
       big_states_dt,
       aes(
	   y = val,
	   x = year, 
	   color = metric
	   )
       ) +
	geom_line(size = 1.1) + 
	theme_pubr() +
	facet_wrap(~state)

# PLOT STATES COMPLEXITY  -----------------------------------------

rev_tbl <- plant_tbl %>% 
	select(year, dsl, state_name, revenue, adjusted_revenue, w_avg_complexity_strict, w_avg_complexity_lenient, multiplier, nic87) %>% 
	uncount(multiplier)

state_total_rev <- rev_tbl %>%
	group_by(year, state_name) %>%
	summarize(
		  total_state_rev = sum(revenue)
		  )

left_join(rev_tbl, state_total_rev) %>%
	mutate(
	       share_of_total_rev = revenue / 


ggplot(
       test,
       aes(
	   y = net_gdp_cap,
	   x = year, 
	   group = plot_dummy,
	   color = state
	   )
       ) +
	geom_line(size = 1.1) + 
	theme_pubr() +
	scale_color_manual(values = cols)





































# Plot
india_fit_tbl <- fit_tbl %>%
	filter(id == "IND") 
fit_tbl
ggplot(fit_tbl, aes(x = rpca, y = rca)) +
	geom_point() +
	theme_pubr()


india_fit_bbs_tbl <- fit_bbs_tbl %>%
	filter(country_code1 == "IND") %>%
	filter(year >= 1999) %>%

india_fit_plot <-
	ggplot(fit_tbl, aes(x = year, y = val, group = id)) +
	geom_line(alpha = 0.2) +
	geom_line(data = india_fit_tbl, color = "red", size = 1.3) +
	ylab("Fitness") +
	theme_pubr()

india_fit_bbs_plot <-
	ggplot(fit_bbs_tbl, aes(x = year, y = non_ln_fit, group = country_code1)) +
	geom_line(alpha = 0.2) +
	geom_line(data = india_fit_bbs_tbl, color = "red", size = 1.3) +
	ylab("Fitness") +
	theme_pubr()


fit_bbs_plot

ggplot(fit_bbs_tbl, aes(x = eci, y = non_ln_fit, group = country_code1)) +
	geom_point()
# INDIA COMPLEXITY BY STATE ------------------------------------
geom_line(data = india_fit_tbl, aes(x = year, y = val), color = "red")

mean_fit_tbl <- mean_fit_tbl %>%
	filter(id == "IND")

mean_fit_tbl
ggline(
       x = "year",
       y = "val",
       group = "id"
       )


###################################################################                    WBES BIGGEST OBSTACLE                    ##
#################################################################

