# This script does two things. First, it attaches product complexity values 
# to the plant-product observations from the ASI data. Second, it computes
# the plant-complexity values based on the weighted avg method and the 
# max- or top-line complexity.
# Author: Søren Post

# TODO: Add fixed complexity vals (where value is the same for a product across years).

library(tidyverse)
library(here)


# Read product data -----------------------------------------------
output_path <- here("data/interim/asi/output_data_cleaned.rds")
output_tbl <- readRDS(output_path)

# Read complexity data --------------------------------------------
product_complexity_path <-  here("data/processed/economic_complexity/fitness_complexity_rpca_hs96.csv")
product_complexity_tbl <- read_csv(product_complexity_path) %>%
	filter(type == "product" & iteration == max(iteration)) %>%
	select(
		year,
		hs96_code = id,
		product_complexity = val
		)

# non-final iterations are for debugging.

# Define function to add complexity values to products ------------

get_plant_complexity <- function(tbl) {

if(!all(c("qty_sold", "net_sale_val", "product_complexity", "year", "dsl") %in% names(tbl))) {
	stop("Error: not all needed vars are present in tbl.")
}

# Calculate weighted avg complexity of plants ---------------------

# A) Get table of total output values.
total_output_tbl <- tbl %>%
	group_by(year, dsl) %>%
	summarize(
		total_plant_output = sum(qty_sold * net_sale_val) 
		)

# B) Add table total output values to product observations 
tbl <- left_join(tbl, total_output_tbl) 

# C) Get table of product share of total output times complexity
plant_avg_complexity_tbl <- tbl %>%
	group_by(year, dsl) %>% 
	summarize(
		w_avg_complexity = sum(((qty_sold * net_sale_val) / total_plant_output) * product_complexity, na.rm = TRUE)
		)

# Calculate top-line complexity of plants ------------------------
plant_max_complexity_tbl <- tbl %>%
	group_by(year, dsl) %>%
	summarize(
		max_complexity = max(product_complexity, na.rm = TRUE)
		)

plant_complexity_tbl <- full_join(plant_avg_complexity_tbl, plant_max_complexity_tbl)

return(plant_complexity_tbl)

}

# Add product complexity to plant-product obs -----------------------
## Since there are no non-na values where strict_hs96 != lenient_hs96, I use
## lenient as the joiner. For calculating the plant complexity, I first filter
## for NA values in lenient or strict HS96 codes
output_tbl <- left_join(output_tbl, product_complexity_tbl, by = c("year" = "year", "lenient_hs96" = "hs96_code"))

# Calculate strict and lenient plant complexity ---------------------

strict_plant_complexity_tbl <- filter(output_tbl, !is.na(strict_hs96)) %>%
	get_plant_complexity() %>%
	mutate(
		match = "strict"
		) 
lenient_plant_complexity_tbl <- filter(output_tbl, !is.na(lenient_hs96)) %>%
	get_plant_complexity() %>%
	mutate(
		match = "lenient"
		)

joined_plant_complexity_tbl <- bind_rows(lenient_plant_complexity_tbl, strict_plant_complexity_tbl)

# Write file ---------------------------------------------------------------
out_path <- here("data/processed/asi/plant_complexity.rds")
saveRDS(joined_plant_complexity_tbl, out_path)

# END
