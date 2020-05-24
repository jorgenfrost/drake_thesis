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
# CALCULATE THE SHARE OF REVENUE AND TOTAL PRODUCTION COSTS EACH INPUT ACCOUNTS FOR
# get_input_share <- function(df) {
#         df <- df %>%
#         mutate(
#                input_rev_share = purchase_val / revenue,
#                input_cost_share = purchase_val / total_production_cost
#                ) 
#         return(df)
# }
# 
# asicc_inputs_tbl <- asicc_inputs_tbl %>% get_input_share()
# npcms_inputs_tbl <- npcms_inputs_tbl %>% get_input_share()



##################################################################
##                    GET INPUT RELATIONSHIP                    ##
##################################################################

# I need to get A) the total revenue of each industry and B) the
# amount each contributes to the total input of the industry. I 
# can then divide the input from each state by the total revenue.

# A) First I get the total revenue of each industry in each year.
# This is done in the output table.

# First I add the nic98 industries to the tables. (56 and 59 different industries)
ind98_tbl <- plant_tbl %>%
	select(year, dsl, nic98_3d) %>%
	filter(!is.na(nic98_3d)) 

asicc_outputs_tbl <- asicc_outputs_tbl %>%
	inner_join(ind98_tbl)

npcms_outputs_tbl <- npcms_outputs_tbl %>%
	inner_join(ind98_tbl)

asicc_inputs_tbl <- asicc_inputs_tbl %>%
	inner_join(ind98_tbl)

npcms_inputs_tbl <- npcms_inputs_tbl %>%
	inner_join(ind98_tbl)
# 
# ggplot(npcms_outputs_tbl, aes(x = nic98_3d)) +
#         geom_bar(aes(weight = multiplier))


# I now group each table by industry and year and get the total revenue (gross sale val for each year)
# uncounting is done instead of weighting the input
npcms_ind_rev <- npcms_outputs_tbl %>%
	uncount(multiplier) %>% 
	group_by(year, nic98_3d) %>%
	summarize(industry_revenue = sum(gross_sale_val, na.rm = TRUE))

asicc_ind_rev <- asicc_outputs_tbl %>%
	uncount(multiplier) %>% 
	group_by(year, nic98_3d) %>%
	summarize(industry_revenue = sum(gross_sale_val, na.rm = TRUE))

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

asicc_ind_inputs <- asicc_inputs_tbl %>%
	anti_join(asicc_flag_2_tbl) %>% 
	uncount(multiplier) %>%
	group_by(nic98_3d, year, item_code) %>%
	summarize(industry_purchase = sum(purchase_val, na.rm = TRUE))

npcms_ind_inputs <- npcms_inputs_tbl %>%
	anti_join(npcms_flag_2_tbl) %>% 
	uncount(multiplier) %>% 
	group_by(nic98_3d, year, item_code) %>%
	summarize(industry_purchase = sum(purchase_val, na.rm = TRUE))

# I now need to do two things. First, I need to find out which industries output which
# kind of products. This is done in a similar way to the thnig above.

asicc_ind_outputs <- asicc_outputs_tbl %>%
	uncount(multiplier) %>%
	group_by(nic98_3d, year, item_code) %>%
	summarize(industry_output = sum(gross_sale_val, na.rm = TRUE))


npcms_ind_outputs <- npcms_outputs_tbl %>%
	uncount(multiplier) %>%
	group_by(nic98_3d, year, item_code) %>%
	summarize(industry_output = sum(gross_sale_val, na.rm = TRUE))


# I now have information of which industries produce how much of each product. 
# I now divide each 
asicc_product_output <- asicc_ind_outputs %>%
	group_by(year, item_code) %>%
	summarize(product_year_total_output = sum(industry_output))

left_join(asicc_ind_outputs, asicc_product_output) %>%
	mutate(year_industry_output_share = industry_output / product_year_total_output) 
# Færdigt product: jeg skal have hver

