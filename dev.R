source("packages.R")

# ASICC INPUT CODES (block H) ----------------------------------------
asicc_electricity_purchased <- c(99905) # = Electricity purchased)
asicc_electricity_gen <- c(99904) # = Electricity own generated kwh

asicc_total_inputs_val <- c(99930)  # = Total inputs

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




block_paths <- readd("cleaned_blocks_ls")



  
