library(tidymodels)
library(corrr)
source("_drake.R")

plant_tbl <- readd("asi_base_sample_ls")$plant_tbl
complexity_tbl <- readd("plant_complexity_tbl")
labour_input_tbl <- readd("plant_labour_input_tbl")
int_input_tbl <- readd("plant_int_input_tbl")


# Labor share and complexity  -------------------------------
id_tbl  <- plant_tbl %>%
	select(year, dsl, scheme, new_state_code, state_name, district_code, rural_urban, multiplier)

# lenient matches
lenient_tbl <- complexity_tbl %>%
	filter(match == "lenient")

lenient_tbl <- lenient_tbl %>%
	left_join(labour_input_tbl) %>%
	left_join(id_tbl) %>%
	filter(year != 1999) # 1999 year does not have total_production_cost var

uncounted_lenient_tbl <- uncount(lenient_tbl, multiplier) %>% 
	mutate(
	       ln_w_avg_comp = log(w_avg_complexity)
	       )

complexity_tbl %>%
	summarize(
		  max(w_avg_complexity),
		  max(max_complexity)
		  )
lm(ln_w_avg_comp ~ wage_share, data = uncounted_lenient_tbl) %>% tidy()
lm(max_complexity ~ wage_share, data = uncounted_lenient_tbl) %>% tidy()


filter(lenient_tbl, max_complexity == -Inf)
