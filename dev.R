source("packages.R")

input_tbl <- readd("asi_base_sample_ls")$input_tbl

# ASICC INPUT CODES (block H) ----------------------------------------
asicc_electricity_purchased <- c(99905) # = Electricity purchased)
asicc_electricity_gen <- c(99904) # = Electricity own generated kwh

asicc_total_inputs_val <- c(99930)  # = Total inputs

# NPCMS-11 INPUT CODES -----------------------------------------------
npcms_electricity_purchased_val
npcms_electricity_gen_kwh
npcms_electricity_purchased_val

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


block_paths <- readd("cleaned_blocks_ls")

  block_list <- tibble(filename = block_paths) %>%
    mutate(
      data = map(filename, function(x) read_fst(x) %>% as_tibble()),
      file = str_match(
        string = filename,
        pattern = "block_[A-Z]"
      ) %>% as.character()
    )

blk_h <- block_list %>%
	filter(file == "block_H") %>%
	select(data) %>%
	unnest(data)

blk_j <- block_list %>%
	filter(file == "block_J") %>%
	select(data) %>%
	unnest(data)

blk_e <- block_list %>%
	filter(file == "block_E") %>%
	select(data) %>%
	unnest(data)

blk_h <- blk_h %>%
    mutate(
      classification = ifelse(year >= 2011, "NPCMS11", "ASICC")
    ) 


# Get electricity self-generated -------------------------------------

self_generator_tbl <- blk_h %>% 
	filter((classification == "ASICC" & item_code == 99904) | (classification == "NPCMS11" & item_code == 9990400)) %>% 
	mutate(
	       self_generated_electricity_kwh = qty_cons,
	       self_generator_dmy = ifelse(qty_cons > 0, 1, 0)
	       ) %>%
	select(year, blk, dsl, sno, self_generated_electricity_kwh, self_generator_dmy)

# Get electricity purchased, consumed --------------------------------

electricity_tbl <- blk_h %>%
	filter((classification == "ASICC" & item_code == 99905) | (classification == "NPCMS11" & item_code == 9990500)) %>%
	mutate(
	       electricity_purchased_val = purchase_val,
	       electricity_consumed_kwh = qty_cons
	       ) %>%
	select(year, blk, dsl, sno, electricity_purchased_val, electricity_consumed_kwh)

# Get self_generator_share

# Get fuels purchased ------------------------------------------------

# ASICC CODES
# 99906 = Petrol, Diesel, Oil, Lubricants consumed
# 99907 = Coal consumed
# 99204 = Other fuel consumed
# NPCMS CODES
# 9990600, # = Gas consumed
# 9990700, # = Coal consumed
# 9920400, # = Other fuel consumed

fuel_tbl <- blk_h %>%
	filter(
	       (classification == "ASICC" & item_code %in% c(99906, 99907, 99204)) | (classification == "NPCMS11" & item_code %in% c(9990600, 9990700, 992040)) 
	       ) %>%
group_by(year, dsl) %>%
summarize(
	  total_fuel_purchase_val = sum(purchase_val)
	  )

# Total materials (inputs) purchased --------------------------------
# ASICC
# 99901 = Total basic items
# 99920 = Total non-basic items

# NPCMS 2011
# 9990100, # = Total basic items (1-6)
# 9992000, # = Total non-basic items 

total_materials_tbl <- blk_h %>%
	group_by(year, dsl) %>%
	summarize(
		  total_basic_items_val = sum(purchase_val[item_code == 99901 | item_code == 9990100]), # products
		  total_non_basic_items_val = sum(purchase_val[item_code == 99920 | item_code == 9992000]), # energy and other
		  total_inputs_val = sum(purchase_val[item_code == 99930 | item_code == 9993000]) # all of it
		  )


# Get labor information --------------------------------------------

  # The employment data has one observation for each employee group. I'm only
  # interested in total employees. In some years total employees have sno = 10,
  # in others sno = 9. I first create a dummy where "total employees" observation
  # has value "1", and the filter based on this. (difference seems to be based on 
  # whether child labor is included.)

labour_tbl <- blk_e %>%
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
    select(-total_employee_obs)

# Get total revenue -----------------------------------------------

revenue_tbl <- blk_j %>%
	filter(item_code == 9995000 | item_code == 99950) %>%
	mutate(
	       revenue = gross_sale_val
	       ) %>%
	select(year, dsl, revenue)

# Get total_electricity_consumed_kwh og self_generated_share------------------------------------------

self_generator_share_tbl <- left_join(self_generator_tbl, electricity_tbl, by = c("year", "dsl")) %>%
	mutate(
	       total_electricity_consumed_kwh = self_generated_electricity_kwh + electricity_consumed_kwh,
	       self_generated_share = self_generated_electricity_kwh / total_electricity_consumed_kwh
	       ) %>%
select(year, dsl, self_generated_share)


