# COMPARISON SHORTAGE AND COMPLEXITY

complexity_tbl <- readd("plant_complexity_tbl")
shortage_tbl <- readd("energy_shortages_tbl")
plant_tbl <- readd("asi_base_sample_ls")$plant_tbl

id_tbl <- plant_tbl %>% select(year, dsl, multiplier, new_state_code, state_name)

lenient_tbl <- complexity_tbl %>%
	filter(match == "lenient") %>%
	left_join(id_tbl, by = c("year", "dsl")) %>%
	left_join(shortage_tbl, by = c("year", "state_name" = "state")) %>%
	mutate(
	       state_year = paste0(as.character(state_name), as.character(year))
	       ) 

strict_tbl <- complexity_tbl %>%
	filter(match == "strict") %>%
	left_join(id_tbl, by = c("year", "dsl")) %>%
	left_join(shortage_tbl, by = c("year", "state_name" = "state")) %>%
	mutate(
	       state_year = paste0(as.character(state_name), as.character(year))
	       ) 

lm_robust(
	  w_avg_complexity ~ avg_shortage,
	  data = lenient_tbl,
	  weights = multiplier,
	  clusters = state_year,
	  se_type = "stata") %>%
	summary()

lm_robust(
	  w_avg_complexity ~ avg_shortage,
	  data = strict_tbl,
	  weights = multiplier,
	  clusters = state_year,
	  se_type = "stata") %>%
	summary()

