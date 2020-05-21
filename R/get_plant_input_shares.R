#' Create labor input share of production (wages / total production cost).
#' 
#' @param plant_tbl the cleaned plant block (from the base sample list).
#' @return data frame the share of wages in total production costs
#' @export

get_labour_input_share <- function(plant_tbl) {

labour_tbl <- plant_tbl %>%
	mutate(
	       wage_share_costs = ifelse(wages == 0 | total_production_cost == 0, NA, wages / total_production_cost),
	       wage_share_revenue = ifelse(wages == 0 | revenue == 0, NA, wages / revenue)
	       ) %>%
select(dsl, year, total_production_cost, wages, wage_share_costs, wage_share_revenue)

return(labour_tbl)
}

#' This function calculates three things for each factory : A) the share of 
#' intermediate inputs in total production costs, B) the concentration (HHI) 
#' of intermediate inputs, and C) the count of inputs (products and non-
#' products + products). 
#' 
#' @param plant_tbl the cleaned plant block (from the base sample list).
#' @param input_tbl the cleaned input block (from the base sample list).
#' @return a data frame of plant-year observations of input shares, input 
#' concentration, and total number of inputs.
#' @export

get_int_input_share <- function(plant_tbl, input_tbl) {

# CALCULATE INTERMEDIATE INPUT SHARE + INPUT CONCENTRATION

# ASICC INPUT CODES (block H) ----------------------------------------
asicc_non_products <- c(
			99201, # = other basic items 
			99203, # = Non-basic Chemicals - all kinds 
			99908, # = Packing items
			99905, # = Electricity purchased
			99906, # = Petrol, Diesel, Oil, Lubricants consumed
			99907, # = Coal consumed
			99204, # = Other fuel consumed
			99220 # = Consumable store 
			)

asicc_non_inputs <- c(
			99904, # = Electricity own generated (no purchase val)
			99901, # = Total basic items (1-6)
			99920, # = Total non-basic items 
			99930  # = Total inputs
			)

# NOTE FOR EARLIER ASICC YEARS - before 2003 - ONLY 5 EXPLICIT INPUTS CAN BE NAMED IN IND. AND IMPORT ITEMS (10 IN TOTAL)

# NPCMS-11 INPUT CODES -----------------------------------------------
npcms_non_products <- c(
		  9920100, # = other basic items 
		  9920300, # = Non-basic Chemicals - all kinds 
		  9990800, # = Packing items
		  9990500, # = Electricity purchased
		  9990600, # = Gas consumed
		  9990700, # = Coal consumed
		  9920400, # = Other fuel consumed
		  9922000 # = Consumable store
		  )

npcms_non_inputs <- c( 
		  9990100, # = Total basic items (1-6)
		  9992000, # = Total non-basic items 
		  9990400, # = Electricity own generated (no purchase val)
		  9993000, # = Total inputs
		  9999999 # = Unment elecitricty demand (no purchase val)
		  )

input_tbl <- input_tbl %>%
	# remove all the "aggregated" product observations
	filter(!item_code %in% npcms_non_inputs) %>%
	filter(!item_code %in% asicc_non_inputs) %>%
	mutate(
	       non_product = case_when(
				       classification == "NPCMS11" & item_code %in% npcms_non_products ~ 1,
				       classification == "ASICC" & item_code %in% asicc_non_products ~ 1,
				       TRUE ~ 0
				       )
	       ) 

# Calculate input concentration (HHI) with and without non-products
# HHI = sum((input share of product 1)^2 + (input share of product 2)^2 + ... + (input share if product N)^2)
# Thiel_T = 1/N * Sum((x_i / mean) ln(x_i/mean))

# Find the total value of only non-products and all products for each plant
int_input_cost_tbl <- input_tbl %>% 
	group_by(year, dsl) %>%
	summarize(
		  input_val_products = sum(purchase_val[non_product == 0]),
		  input_val_all = sum(purchase_val)
		  )

# Find the input shares and calculate HHI
int_input_hhi_tbl <- input_tbl %>% 
	left_join(int_input_cost_tbl, by = c("year", "dsl")) %>% # Add total input value observation
	mutate( # Calculate each input's share of plants total input value
	       input_share_products = ifelse(non_product == 0, purchase_val / input_val_products, NA),
	       input_share_all = purchase_val / input_val_all
	       ) %>% 
	group_by(year, dsl) %>%
	summarize( # calculate HHI
		  hhi_products = sum(input_share_products^2, na.rm = TRUE),
		  hhi_all = sum(input_share_all^2)
		  ) 

# Create a count of the number of inputs to each plant
int_input_count_tbl <- input_tbl %>% 
	group_by(year, dsl) %>%
	summarize(
		  count_products = sum(non_product == 0),
		  count_all = n()
		  )

# Join all the tables and calculate the input share of total production
int_input_tbl <- plant_tbl %>%
	select(year, dsl, total_production_cost) %>%
	left_join(int_input_cost_tbl, by = c("year", "dsl"))  %>% 
	left_join(int_input_hhi_tbl, by = c("year", "dsl")) %>%
	left_join(int_input_count_tbl, by = c("year", "dsl")) %>%
	mutate(
	       input_share_products = input_val_products / total_production_cost,
	       input_share_all = input_val_all / total_production_cost
	       ) %>%
	mutate( # sanitize those observations that have no explicit products or no production cost
	       # (fx HHI != 0, there are just no obs, same goes for input_share == inf).
	       input_share_products = ifelse(input_val_products == 0 | total_production_cost == 0, NA, input_share_products),
	       hhi_products = ifelse(input_val_products == 0 | total_production_cost == 0, NA, hhi_products),
	       hhi_all = ifelse(input_val_all == 0 | total_production_cost == 0, NA, hhi_all)
	       ) 

return(int_input_tbl)

}


