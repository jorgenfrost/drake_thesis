#' This function calculates the max peak and max input/supply shortage for plants.
#'
#' @export

get_plant_input_shortage <- function() {

base_plant_tbl <- readd("asi_base_sample_ls")$plant_tbl 
base_inputs_tbl <- readd("asi_base_sample_ls")$input_tbl %>%
	filter(blk == "H") # Not interested in imported items

base_outputs_tbl <- readd("asi_base_sample_ls")$output_tbl
state_shortage <- readd("energy_shortages_tbl") %>%
	select(year, state_name = state, avg_shortage, peak_shortage)

# REMOVE PLANTS THAT ARE OBSERVED TOO MANY TIMES ---------------------
# Remove plants that appear more than once in the year-dsl pair (it is an error and will propagate through joins)
exclude_plants <- base_plant_tbl %>% 
	group_by(year, dsl) %>%
	summarize(
		  n = n()
		  ) %>%
	filter(n != 1) 

base_plant_tbl <- base_plant_tbl %>%
	anti_join(exclude_plants) %>%
	filter(revenue > 0) %>%
	filter(total_production_cost > 0) 

base_inputs_tbl <- base_inputs_tbl %>%
	anti_join(exclude_plants)

base_outputs_tbl <- base_outputs_tbl %>%
	anti_join(exclude_plants)

# SEPERATE INTO ASICC AND NPCMS YEARS --------------------------------

# ASICC
asicc_inputs_tbl <- base_inputs_tbl %>%
	filter(classification == "ASICC")

asicc_outputs_tbl <- base_outputs_tbl %>%
	filter(classification == "ASICC")

# NPCMS-2011
npcms_inputs_tbl <- base_inputs_tbl %>%
	filter(classification == "NPCMS11") 

npcms_outputs_tbl <- base_outputs_tbl %>%
	filter(classification == "NPCMS11")

# Select information that needs to be tied to the inputs
plant_rev_cost_tbl <- base_plant_tbl %>%
	select(year, dsl, scheme, nic5digit, multiplier, total_production_cost, state_name, revenue)

# Add plant information to input- and output data
add_plant_vars <- function(df) {
	
	print(paste("Rows going in:", nrow(df)))

	df <- df %>%
		inner_join(plant_rev_cost_tbl, by = c("year", "dsl"))

	print(paste("Rows going out:", nrow(df)))

	return(df)

}

asicc_inputs_tbl <- asicc_inputs_tbl %>% add_plant_vars()
npcms_inputs_tbl <- npcms_inputs_tbl %>% add_plant_vars()
asicc_outputs_tbl <- asicc_outputs_tbl %>% add_plant_vars()
npcms_outputs_tbl <- npcms_outputs_tbl %>% add_plant_vars()

# REMOVE THE ITEMS THAT ARE NON-PRODUCTS AND CANNOT BE ASSIGN TO OTHER PLANTS  
# ASICC INPUT CODES (block H) ----------------------------------------
asicc_non_products <- c(
			99201, # = other basic items 
			99203, # = Non-basic Chemicals - all kinds 
			99908, # = Packing items
			99905, # = Electricity purchased
			99906, # = Petrol, Diesel, Oil, Lubricants consumed
			99907, # = Coal consumed
			99204, # = Other fuel consumed
			99210, # = Consumable store 
			99220 # = Consumable store 
			)

asicc_non_inputs <- c(
			99904, # = Electricity own generated (no purchase val)
			99901, # = Total basic items (1-6)
			99910, # tother
			99920, # = Total non-basic items 
			99930, # = Total inputs
			99940  # = Total inputs
			)


asicc_inputs_tbl <- asicc_inputs_tbl %>%
	filter(!item_code %in% asicc_non_products) %>%
	filter(!item_code %in% asicc_non_inputs)
# NOTE FOR EARLIER ASICC YEARS - before 2003 - 
# ONLY 5 EXPLICIT INPUTS CAN BE NAMED 

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
npcms_inputs_tbl <- npcms_inputs_tbl %>%
	filter(!item_code %in% npcms_non_products) %>%
	filter(!item_code %in% npcms_non_inputs)

# ASICC OUTPUT CODES ----------------------------------------------
asicc_non_output <- c(
		      99211, # = other products
		      99950 # = total products
		      )

npcms_non_output <- c(
		      9921100, # = other products
		      9995000 # = total products
		      )

asicc_outputs_tbl <- asicc_outputs_tbl %>%
	filter(!item_code %in% asicc_non_output)

npcms_outputs_tbl <- npcms_outputs_tbl %>%
	filter(!item_code %in% npcms_non_output)


####################################################
#       INTERMISSION: FLAG UNREASONABLE VALUES     #
####################################################
asicc_flag_tbl <- left_join(asicc_inputs_tbl, plant_tbl) %>%
	select(year, dsl, revenue, item_code, purchase_val) %>%
	filter(!is.na(purchase_val)) %>%
	mutate(item_rev_share = purchase_val / revenue,
	       flag_1 = ifelse(item_rev_share > 1, 1, 0),
	       flag_2 = ifelse(item_rev_share > 2, 1, 0)
	       ) 

asicc_flag_2_tbl <- asicc_flag_tbl %>%
	filter(flag_2 == 1)

npcms_flag_tbl <- left_join(npcms_inputs_tbl, plant_tbl) %>%
	select(year, dsl, revenue, item_code, purchase_val) %>%
	filter(!is.na(purchase_val)) %>%
	mutate(item_rev_share = purchase_val / revenue,
	       flag_1 = ifelse(item_rev_share > 1, 1, 0),
	       flag_2 = ifelse(item_rev_share > 2, 1, 0)
	       ) 

npcms_flag_2_tbl <- npcms_flag_tbl %>%
	filter(flag_2 == 1)

##################################################
##  INTERMISSION END: USE FLAG_2_TBL TO REMOVE 1.200 AND 500
## OBSERVATIONS THAT PURCHASE REVS FOR TWICE TOTAL OUTPUT
##################################################


#################################################################
##               GET STATE SHARE OF EACH PRODUCT               ##
#################################################################

# I need first to get the share of each product produced in each state.

# I now group each table by state and year and get the total revenue (gross sale val for each year)
# uncounting is done instead of weighting the input
npcms_state_product_share <- npcms_outputs_tbl %>%
	anti_join(npcms_flag_2_tbl) %>% 
	uncount(multiplier) %>% 
	group_by(year, state_name, item_code) %>%
	summarize(product_output_in_state = sum(gross_sale_val, na.rm = TRUE))

asicc_state_product_share <- asicc_outputs_tbl %>%
	anti_join(asicc_flag_2_tbl) %>% 
	uncount(multiplier) %>% 
	group_by(year, state_name, item_code) %>%
	summarize(product_output_in_state = sum(gross_sale_val, na.rm = TRUE))

# I now find the total yeary output of each product, attach it to the two data frames I just made
# and find the yearly share each state contributes of each product

# NPCMS
npcms_total_product_output <- npcms_state_product_share %>% 
	group_by(year, item_code) %>%
	summarize(total_yearly_product_output = sum(product_output_in_state))

npcms_state_product_share <- left_join(npcms_state_product_share, npcms_total_product_output) %>%
	mutate(yearly_state_product_share = product_output_in_state / total_yearly_product_output)

# ASICC
asicc_total_product_output <- asicc_state_product_share %>% 
	group_by(year, item_code) %>%
	summarize(total_yearly_product_output = sum(product_output_in_state))

asicc_state_product_share <- left_join(asicc_state_product_share, asicc_total_product_output) %>%
	mutate(yearly_state_product_share = product_output_in_state / total_yearly_product_output)

# I now add the yearly state shortage to each product-year-state match and take the yearly weighted average of products'
# shortage (using yearly state share as the weight).
npcms_product_shortage <- npcms_state_product_share %>%
	left_join(state_shortage) %>%
	filter(!is.na(avg_shortage)) %>%
	group_by(year, item_code) %>%
	summarize(
	       product_avg_shortage = sum(yearly_state_product_share * avg_shortage), 
	       product_peak_shortage = sum(yearly_state_product_share * peak_shortage),
	       ) %>%
	ungroup() 

asicc_product_shortage <- asicc_state_product_share %>%
	left_join(state_shortage) %>%
	filter(!is.na(avg_shortage)) %>%
	group_by(year, item_code) %>%
	summarize(
	       product_avg_shortage = sum(yearly_state_product_share * avg_shortage), 
	       product_peak_shortage = sum(yearly_state_product_share * peak_shortage),
	       ) %>%
	ungroup() 

# I now need to add each product shortage to a table with revenue share of each of the inputs for each plant.
# First I attach revenues to the input table, then I calculated the revenue share, then I add the product shortage, and then
# I calculate the plant-input-shortage

# revenues are already attached
npcms_plant_input_shortage <- npcms_inputs_tbl %>%
	mutate(product_revenue_share = purchase_val / revenue) %>%
	left_join(npcms_product_shortage) %>%
	filter(!is.na(product_avg_shortage)) %>%
	group_by(year, dsl) %>%
	summarize(
		  max_input_peak_shortage = max(product_peak_shortage),
		  max_input_avg_shortage = max(product_avg_shortage),
		  input_avg_shortage = sum(product_revenue_share * product_avg_shortage),
		  input_peak_shortage = sum(product_revenue_share * product_peak_shortage)
		  )

asicc_plant_input_shortage <- asicc_inputs_tbl %>%
	mutate(product_revenue_share = purchase_val / revenue) %>%
	left_join(asicc_product_shortage) %>%
	filter(!is.na(product_avg_shortage)) %>%
	group_by(year, dsl) %>%
	summarize(
		  max_input_avg_shortage = max(product_avg_shortage),
		  max_input_peak_shortage = max(product_peak_shortage),
		  input_avg_shortage = sum(product_revenue_share * product_avg_shortage),
		  input_peak_shortage = sum(product_revenue_share * product_peak_shortage)
		  )

# Finally I stack the two tables and return
plant_input_shortage_tbl <- bind_rows(npcms_plant_input_shortage, asicc_plant_input_shortage)

return(plant_input_shortage_tbl)
}
