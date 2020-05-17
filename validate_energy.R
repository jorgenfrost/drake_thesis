# TODO: Functionize 
# TODO: Put in nice tables.
# TODO: lm_robust - which kinds of clusters? ("stata" has low SEs and are fast...)


# READ DATA ----------------------------------------------------------

es05_tbl <- readd("es05_tbl")
es14_tbl <- readd("es14_tbl")
shortages_tbl <- readd("energy_shortages_tbl")

es05_raw <- read_dta("/home/post/drake_project/data/external/es_india/India-2005--full data-.dta/India-2005--full-data-.dta")

ihds_in <- readd("ihds_tbl")
ihds_tbl <- ihds_in

# PREPARE DATA -------------------------------------------------------

## INITIALISE RESULT LIST
result_ls <- list()

## ESTIMATED SHORTAGES
shortages_tbl <- shortages_tbl %>%
	mutate(
	       avg_shortage_share = avg_shortage,
	       avg_shortage = avg_shortage * 100  # turn into percentage
	       )

## ENTERPRISE SURVEY 2005 
# Add shortage data
es05_tbl <- es05_tbl %>% 
	mutate(year = 2005) %>%
	left_join(shortages_tbl, by = c("asi_state_name" = "state", "year")) 

# Add biggest obstacle = electricity dummy 
es05_tbl <- es05_tbl %>%  # electricity = "B"
	mutate(biggest_obstacle_dmy = ifelse(biggest_obstacle == "B", 1, 0) %>% as_factor) 

## ENTERPRISE SURVEY 2014
# Add shortage data
es14_tbl <- es14_tbl %>%
	mutate(year = 2014) %>%
	left_join(shortages_tbl, by = c("state_name.asi" = "state", "year"))

# Add biggest obstacle = electricity dummy 
es14_tbl <- es14_tbl %>% # electricity = 8
	mutate(
	       biggest_obstacle_dmy = ifelse(biggest_obstacle == 8, 1, 0) 
	       )


## IHDS 2005 + 2012

# Create tidy tbl
ihds05_tbl <- ihds_in %>% 
	select(-c(electricity_access_12, hours_electricity_12)) %>%
	rename(
	       hours_electricity = hours_electricity_05,
	       electricity_access = electricity_access_05
	       ) %>%
	mutate(year = 2005)

ihds12_tbl <- ihds_in %>% 
	select(-c(electricity_access_05, hours_electricity_05)) %>%
	rename(
	       hours_electricity = hours_electricity_12,
	       electricity_access = electricity_access_12
	       ) %>%
	mutate(year = 2012)

ihds_tbl <- full_join(ihds05_tbl, ihds12_tbl)

# Add shortage data
ihds_joined_tbl <- left_join(ihds_tbl, shortages_tbl, by = c("state_name" = "state", "year"))

# Remove HHs with no electricity
ihds_access_tbl <- ihds_joined_tbl %>% 
	filter(electricity_access == 1 & hours_electricity > 0) 

# Create difference table
short05 <- shortages_tbl %>% 
	filter(year == 2005) %>%
	mutate(avg_shortage_2005 = avg_shortage) %>%
	select(state, avg_shortage_2005)

short12 <- shortages_tbl %>% 
	filter(year == 2012) %>%
	mutate(avg_shortage_2012 = avg_shortage) %>%
	select(state, avg_shortage_2012)

shortage_difference_tbl <- left_join(short05, short12) %>% 
	mutate(difference_shortages = avg_shortage_2012 - avg_shortage_2005)

ihds_difference_tbl <- ihds_in %>% 
	filter(electricity_access_05 == 1 & hours_electricity_05 > 0) %>%
	filter(electricity_access_12 == 1 & hours_electricity_12 > 0) %>%
	mutate(
	       difference_hours_elect = as.numeric(hours_electricity_12 - hours_electricity_05)
	       )

ihds_diff_joined_tbl <- left_join(ihds_difference_tbl, shortage_difference_tbl, by = c("state_name" = "state"))

# DEFINE HELPTER FUNCTIONS -------------------------------------------

# Function to make it easier to create the latex tables.
prepare_table <- function(tbl, caption, rows) {
	tbl[1:rows, ] %>%
	# map_dfc(function(df) if (is.numeric(df)) {round(df, digits = 2)} else df ) %>%
	kable(
	      format = "latex",
	      caption = caption,
	      booktabs = T
	      ) %>%
	add_footnote(c("Robust standard errors (White)."), notation = "symbol")
}

# Function to create the robust standard errors (p-val for coefs) 
robust_se <- function(model) {
coeftest(model, vcov = sandwich::vcovHC(model, type = "HC0"))
}

# ANALYSE ENTERPRISE SURVEYS -----------------------------------------

# ENTERPRISE SURVEY 2005 (NOTE: NO WEIGH VARIABLE IN DATA)

# Self-genration share
es05_gen_share_lm <- lm(
		       formula = share_generated_electricity ~ avg_shortage + as.factor(industry),
		       data = es05_tbl
		       )

result_ls$es05_gen_share <- robust_se(es05_gen_share_lm)

# Power quality
es05_power_quality_lm <- lm(
			    formula = quality_of_power_supply ~ avg_shortage + as.factor(industry),
			    data = es05_tbl
			    ) 

result_ls$es05_power_quality <- robust_se(es05_power_quality_lm)

# Biggest obstacle
es05_biggest_obstacle_glm <- glm(
				formula = biggest_obstacle_dmy ~ avg_shortage + as.factor(industry),
				family = "binomial",
				data = es05_tbl
				)

result_ls$es05_biggest_obstacle <- robust_se(es05_biggest_obstacle_glm)

# ENTERPISE SURVEY 2012  

# Degree electricity is an obstacle: (0-4)
es14_e_obst_lm <- lm(
		     as.numeric(degree_electricity_obstacle) ~ avg_shortage + as.factor(sector), 
		     data = es14_tbl,
		     weights = wstrict
		     ) 

result_ls$es14_e_obst <- robust_se(es14_e_obst_lm)

# Share of own electricity generated
es14_gen_share_lm <- lm(
			share_elect_from_generator ~ avg_shortage + as.factor(sector),
			data = es14_tbl,
			weights = wstrict
			)

result_ls$es14_gen_share <- robust_se(es14_gen_share_lm)

# Probability of electricity = biggest obstacle TODO: glm eller linear prob model?
es14_biggest_obstacle_glm <- glm(
				 as.factor(biggest_obstacle_dmy) ~ avg_shortage + as.factor(sector),
				 data = es14_tbl,
				 weights = wstrict,
				 family = "binomial"
				 ) 

result_ls$es14_biggest_obstacle <- robust_se(es14_biggest_obstacle_glm)

# IHDS 

# Does state-wide estimated demand shortage predict hours electricity total for individual HHs?
ihds05_hours_e_lm <- lm(
   hours_electricity ~ avg_shortage,
   data = ihds_access_tbl %>% filter(year == 2005), 
   weights = sweight
   ) 

result_ls$ihds05_hours_e_lm <- robust_se(ihds05_hours_e_lm)

ihds12_hours_e_lm <- lm(
   hours_electricity ~ avg_shortage,
   data = ihds_access_tbl %>% filter(year == 2012), 
   weights = sweight
   ) 

result_ls$ihds12_hours_e_lm <- robust_se(ihds12_hours_e_lm)

# Does difference in state-wide estimated demand shortage over time predict change in hours electricity for individual HHs?

ihds_diff_hours_e_lm <- lm(
   difference_hours_elect ~ difference_shortages, 
   data = ihds_diff_joined_tbl,
   weights = sweight
   ) 

result_ls$ihds_diff_hours_e <- robust_se(ihds_diff_hours_e_lm)

# CREATE RESULT TABLES -----------------------------------------------
folder_path <- here("doc/tables/data/validate_energy_")

result_ls$es14_biggest_obstacle %>%
	broom::tidy() %>%
	prepare_table(caption = "Electricity as biggest obstacle", rows = 2) %>%
	write(paste0(folder_path, "es14_biggest_obstacle.tex"))

