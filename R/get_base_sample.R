#' This function filters the cleaned ASI blocks into a plant-level data set.
#' A number of variables are created along the way, with flags to mark 
#' unreliable observations. 
#'
#' @param block_paths list of strings containing the path of the cleaned ASI
#' blocks. 
#' @param state_concordance_path string containing the path of the concordance
#' table of state codes in the ASI data.
#' @param state_name_path string containing the path of a data frame with the 
#' state names of the new state codes.
#' @return return_list: list containing three data frames:
#' 1: plant_tbl = data frame containing the cleaned, filtered plant-level 
#' data with flags.
#' 2: input_tbl = data frame containing local and imported inputs, filtered to
#' the plants in plant_tbl.
#' 3: output_tbl = data frame containing output products, fitlered to 
#' the plants in plant_tbl.
#' @export

# TODO: input concentration

# state_concordance_path = here("data/external/concord_tables/asi_state_codes/state_codes_table.csv")
# state_name_path = here("data/external/concord_tables/asi_state_codes/state_names_1617.csv")

get_base_sample <- function(block_paths, state_concordance_path = here("data/external/concord_tables/asi_state_codes/state_codes_table.csv"), state_name_path = here("data/external/concord_tables/asi_state_codes/state_names_1617.csv")) {
  
  ##################################################################
  ##                         1: READ DATA                         ##
  ##################################################################
  
  block_list <- tibble(filename = block_paths) %>%
    mutate(
      data = map(filename, function(x) read_fst(x) %>% as_tibble()),
      file = str_match(
        string = filename,
        pattern = "block_[A-Z]"
      ) %>% as.character()
    )
  
  # Reading function (also removes non-unique rows)
  read_block <- function(block) {
    
    block_list %>% 
      filter(file == paste0("block_", block)) %>%
      select(data) %>%
      unnest(data) %>%
      distinct() # remove any duplicate values
  }
  
  blk_a <- read_block("A")  
  blk_b <- read_block("B")
  blk_e <- read_block("E")
  blk_h <- read_block("H")
  blk_i <- read_block("I")
  blk_j <- read_block("J")
  
  input_tbl <- bind_rows(blk_i, blk_h) %>%
    mutate(
      classification = ifelse(year >= 2011, "NPCMS11", "ASICC")
    ) 
  
  output_tbl <- blk_j %>%
    mutate(
      classification = ifelse(year >= 2011, "NPCMS11", "ASICC")
    ) 


  ##################################################################
  ##                     2: PREPARE VARIABLES                     ##
  ##################################################################
  
  # Get ID block -------------------------------------------------------
  
  id_tbl <- blk_a %>%
    select(year, dsl, scheme, state_code, district_code, nic5digit, multiplier, rural_urban, total_production_cost)
  

  # In block A (now plant_tbl), state codes are not classified to the same system
  # across all years. I use two external tables to fix this. First I use a
  # concordance table with the coresponding codes (old and new). Second I use a
  # table with the new codes paired with proper state names. 
  
  # Reading concordance table + adding dummy for join operation
  state_conc_tbl <- read_csv(state_concordance_path) %>%
    select(
      early_codes = codes_9596_9900,
      late_codes = codes_0001_0203
    ) %>%
    mutate(change_state_code = 1)
  
  # Read table with state names
  state_name_tbl <- read_csv(state_name_path)
  
  # Update state codes and names -------------------------------------
  
  # I first create a dummy, signifying which years needs to be updated (verified manually)
  id_tbl <- id_tbl %>%
    mutate(
      change_state_code = ifelse(year < 2001, 1, 0)
    )
  
  # update codes in "new_state_code"
  id_tbl <- left_join(
    x = id_tbl, 
    y = state_conc_tbl, 
    by = c("state_code" = "early_codes", "change_state_code")
  ) %>% 
    mutate(
      new_state_code = ifelse(change_state_code == 1, late_codes, state_code)
    )
  
  # add names
  id_tbl <- left_join(id_tbl, state_name_tbl, by = c("new_state_code" = "code")) %>%
    select(
      -c(change_state_code, late_codes, state_code), # remove the temporary variables
    ) %>%
    rename(state_name = name)
  
  
  # Get electricity variables ------------------------------------------
  
  # Get electricity purchased, consumed
  electricity_tbl <- input_tbl %>%
    filter((classification == "ASICC" & item_code == 99905) | (classification == "NPCMS11" & item_code == 9990500)) %>%
    mutate(
      electricity_purchased_val = purchase_val,
      electricity_consumed_kwh = qty_cons
    ) %>%
    select(year, dsl, electricity_purchased_val, electricity_consumed_kwh)
  
  # Get electricity self-generated 
  # 1: get the qty created
  self_generator_tbl <- input_tbl %>% 
    filter((classification == "ASICC" & item_code == 99904) | (classification == "NPCMS11" & item_code == 9990400)) %>% 
    mutate(
      self_generated_electricity_kwh = qty_cons,
      self_generator_dmy = ifelse(qty_cons > 0, 1, 0)
    ) %>%
    select(year, dsl, self_generated_electricity_kwh, self_generator_dmy)
  
  # 2: Get total_electricity_consumed_kwh og self_generated_share
  electricity_tbl <- left_join(electricity_tbl, self_generator_tbl, by = c("year", "dsl")) %>%
    mutate( # fix the vals from before - should be 0, not missing
      self_generated_electricity_kwh = ifelse(is.na(self_generated_electricity_kwh), 0, self_generated_electricity_kwh),
      self_generator_dmy = ifelse(is.na(self_generator_dmy), 0, self_generator_dmy)
    ) %>% 
    mutate(
      total_electricity_consumed_kwh = self_generated_electricity_kwh + electricity_consumed_kwh,
      self_generated_share = self_generated_electricity_kwh / total_electricity_consumed_kwh
    ) 
  
  # Get fuels purchased ------------------------------------------------
  
  # ASICC CODES
  # 99906 = Petrol, Diesel, Oil, Lubricants consumed
  # 99907 = Coal consumed
  # 99204 = Other fuel consumed
  
  # NPCMS CODES
  # 9990600, # = Gas consumed
  # 9990700, # = Coal consumed
  # 9920400, # = Other fuel consumed
  
  fuel_tbl <- input_tbl %>%
    filter(
      (classification == "ASICC" & item_code %in% c(99906, 99907, 99204)) | (classification == "NPCMS11" & item_code %in% c(9990600, 9990700, 992040)) 
    ) %>%
    group_by(year, dsl) %>%
    summarize(
      total_fuel_purchase_val = sum(purchase_val)
    ) %>%
    ungroup()
  
  # Total materials (inputs) purchased --------------------------------
  # BLK H: ASICC
  # 99901 = Total basic items
  # 99920 = Total non-basic items
  
  # BLK H: NPCMS 2011
  # 9990100, # = Total basic items (1-6)
  # 9992000, # = Total non-basic items 
  
  # BLK I: ASICC
  # 99940 = Total imports consumed
  
  total_materials_tbl <- input_tbl %>%
    group_by(year, dsl) %>%
    summarize(
      total_basic_items_val = sum(purchase_val[item_code == 99901 | item_code == 9990100]), # products
      total_non_basic_items_val = sum(purchase_val[item_code == 99920 | item_code == 9992000]), # energy and other
      total_inputs_val = sum(purchase_val[item_code == 99930 | item_code == 9993000 | item_code == 99940 | item_code == 9994000]), # all of it
      total_inputs_no_imports_val = sum(purchase_val[item_code == 99930 | item_code == 9993000]) # all of it
    ) %>% 
    ungroup()
  
  # Get labor information --------------------------------------------
  
  # The employment data has one observation for each employee group. I'm only
  # interested in total employees. In some years total employees have sno = 10,
  # in others sno = 9. I first create a dummy where "total employees" observation
  # has value "1", and the filter based on this. (difference seems to be based on 
  # whether child labor is included.)
  
  labor_tbl <- blk_e %>%
    mutate(
      total_employee_obs = case_when(
        year %in% 2001:2008 & sno == 10 ~ 1,
        year %in% c(1999:2000, 2009:2017) & sno == 9 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    filter(total_employee_obs == 1) %>%
    rename(
      avg_total_employees = avg_person_work
    ) %>%
    select(-c(total_employee_obs, sno))
  
  # Get total revenue --------------------------------------------------
  
  revenue_tbl <- blk_j %>%
    filter(item_code == 9995000 | item_code == 99950) %>%
    mutate(
      revenue = gross_sale_val
    ) %>%
    select(year, dsl, revenue)
  
  # Join tbls to one master tbl ----------------------------------------
  
  plant_tbl <- id_tbl %>%
    left_join(blk_b %>% select(-blk), by = c("year", "dsl")) %>%
    left_join(electricity_tbl, by = c("year", "dsl")) %>%
    left_join(fuel_tbl, by = c("year", "dsl")) %>%
    left_join(total_materials_tbl, by = c("year", "dsl")) %>%
    left_join(labor_tbl, by = c("year", "dsl")) %>%
    left_join(revenue_tbl, by = c("year", "dsl")) 
  
  id_tbl %>%
    left_join(blk_b %>% select(-blk), by = c("year", "dsl")) %>%
    left_join(electricity_tbl, by = c("year", "dsl")) 
    left_join(fuel_tbl, by = c("year", "dsl")) %>%
    left_join(total_materials_tbl, by = c("year", "dsl")) %>%
    left_join(labor_tbl, by = c("year", "dsl")) %>%
    left_join(revenue_tbl, by = c("year", "dsl")) 
  ##################################################################
  ##                      3: GET BASE SAMPLE                      ##
  ##################################################################
  
  # Get "open factories" filter ----------------------------------------
  
  open_ft <- blk_a %>% 
    filter(status_of_unit == 1) %>%
    select(year, dsl)
  
  # Unrealistically many employees -------------------------------------
  # (just 1 plant - t_man_data and wages doesn't match avg_total_emp)
  emp_ft <- labor_tbl %>% 
    filter(!avg_total_employees > 100000) %>%
    select(year, dsl)
  
  # Missing revenues - all missing revenue plants will have NA in flags 
  revenue_na_ft <- plant_tbl %>%
    filter(!is.na(revenue)) %>%
    select(year, dsl)
  
  # Labor cost input share flag ----------------------------------------
  wage_share_flag <- plant_tbl %>%
    select(year, dsl, revenue, wages) %>%
    mutate(wage_share_flag = ifelse(wages > (2 * revenue), 1, 0)) %>%
    select(year, dsl, wage_share_flag)
  
  # Materials cost input share flag ------------------------------------
  materials_share_flag <- plant_tbl %>% 
    mutate(materials_share_flag = ifelse(total_inputs_val > (2 * revenue), 1, 0)) %>%
    select(year, dsl, materials_share_flag)
  
  # Fuel- and electricity costs flag -----------------------------------
  fuel_electricity_costs_flag <- plant_tbl %>% 
    mutate(
      total_fuel_purchase_val = ifelse(is.na(total_fuel_purchase_val), 0, total_fuel_purchase_val),
      fuel_electricity_costs_flag = ifelse((electricity_purchased_val + total_fuel_purchase_val) > revenue, 1, 0)
    ) %>%
    select(year, dsl, fuel_electricity_costs_flag) 
  
  # Electricity qty flag -----------------------------------------------
  # 1) get median kWh price per unit/state
  median_electricity_price <- plant_tbl %>%
    select(year, dsl, multiplier, new_state_code, electricity_purchased_val, electricity_consumed_kwh) %>%
    mutate(
      electricity_price = electricity_purchased_val / electricity_consumed_kwh
    ) %>% 
    uncount(multiplier) %>% # uncount to make median correct
    group_by(year, new_state_code) %>%
    summarize(
      median_electricity_price = median(electricity_price, na.rm = TRUE)
    ) 
  
  # 2) add electricity price and multiply by the total consumed 
  electricity_qty_costs_flag <- plant_tbl %>% 
    select(year, dsl, new_state_code, revenue, total_electricity_consumed_kwh) %>%
    left_join(median_electricity_price, by = c("year", "new_state_code")) %>%
    mutate(synthetic_total_electricity_cost = total_electricity_consumed_kwh * median_electricity_price) %>%
    mutate(electricity_qty_cost_flag = ifelse(synthetic_total_electricity_cost > revenue, 1, 0)) %>%
    select(year, dsl, electricity_qty_cost_flag)
  
  # Join all flags to id_tbl -------------------------------------------
  flag_tbl <- id_tbl %>%
    select(year, dsl) %>%
    left_join(wage_share_flag, by = c("year", "dsl")) %>% 
    left_join(materials_share_flag, by = c("year", "dsl")) %>% 
    left_join(fuel_electricity_costs_flag, by = c("year", "dsl")) %>% 
    left_join(electricity_qty_costs_flag, by = c("year", "dsl")) 
  
  # Create flag filter ------------------------------------------------
  flag_ft <- plant_tbl %>%
    select(year, dsl) %>%
    left_join(flag_tbl, by = c("year", "dsl")) %>%
    mutate(flags = wage_share_flag + materials_share_flag + fuel_electricity_costs_flag + electricity_qty_cost_flag) %>%
    filter(flags >= 2) %>%
    select(year, dsl)
  
  # Apply filters -----------------------------------------------------
  plant_base_tbl <- plant_tbl %>%
    semi_join(open_ft, by = c("year", "dsl")) %>%
    semi_join(revenue_na_ft, by = c("year", "dsl")) %>%
    semi_join(emp_ft, by = c("year", "dsl")) %>%
    anti_join(flag_ft, by = c("year", "dsl")) %>%
    left_join(flag_tbl, by = c("year", "dsl")) 
 
  ##################################################################
  ##                      4: GET OUTPUT                           ##
  ##################################################################

# Get sale filter:
net_sale_ft <- output_tbl %>%
	mutate(
	       total_net_sale = qty_sold * net_sale_val,
	       ratio_sales = total_net_sale / gross_sale_val
	       ) %>%
filter(ratio_sales > 1.1) %>%
select(year, dsl, item_code)

# Apply filter:
output_tbl <- output_tbl %>%
	anti_join(net_sale_ft, by = c("year", "dsl", "item_code")) 

# Limit to observations from plants in base sample
output_tbl <- output_tbl %>%
	semi_join(plant_base_tbl, by = c("year", "dsl"))

  ##################################################################
  ##                      5: GET INPUT                            ##
  ##################################################################

# Limit to observations from plants in base sample
input_tbl <- input_tbl %>% 
	semi_join(plant_base_tbl, by = c("year", "dsl"))

#################################################################
##                          6: RETURN                          ##
#################################################################

return_list <- list(
		    "plant_tbl" = plant_base_tbl,
		    "output_tbl" = output_tbl,
		    "input_tbl" = input_tbl
		    )

return(return_list)

}

