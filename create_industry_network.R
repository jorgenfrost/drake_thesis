# Read base sample
base_plant_tbl <- readd("asi_base_sample_ls")$plant_tbl 
base_inputs_tbl <- readd("asi_base_sample_ls")$input_tbl %>%
	filter(blk == "H") # Not interested in imported items

base_outputs_tbl <- readd("asi_base_sample_ls")$output_tbl

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

########## DEV #################
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
			99220 # = Consumable store 
			)

asicc_non_inputs <- c(
			99904, # = Electricity own generated (no purchase val)
			99901, # = Total basic items (1-6)
			99920, # = Total non-basic items 
			99930  # = Total inputs
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

# CALCULATE THE SHARE OF REVENUE AND TOTAL PRODUCTION COSTS EACH INPUT ACCOUNTS FOR
get_input_share <- function(df) {
	df <- df %>%
	mutate(
	       input_rev_share = purchase_val / revenue,
	       input_cost_share = purchase_val / total_production_cost
	       ) 
	return(df)
}

asicc_inputs_tbl <- asicc_inputs_tbl %>% get_input_share()
npcms_inputs_tbl <- npcms_inputs_tbl %>% get_input_share()







asicc_inputs_tbl











# How many inputs on average: 
# 1: add inputs to base plant sample. If na set 0.
# 2: group summarize mean
asicc_inputs_tbl %>%
	group_by(dsl, year, blk, multiplier) %>%
	summarize(
		  mlt = max(multiplier)
		  ) %>%
	ungroup() %>%
	summarize(
		  mean_n =
