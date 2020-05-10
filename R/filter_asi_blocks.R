#' This function takes the cleaned blocks of the ASI data, removes observations
#' from factories that are not listed as "open", fixes the state codes, and
#' joins the relevant blocks to each oher (plant-level blocks, input-blocks, 
#' output block). 
#' 
#' @param block_list list: the cleaned blocks of the ASI data.
#' @param state_concordance_path character: path to the raw concordance table of codes.
#' From the documentation on the ASI.
#' @param state_name_path character: path to a table of code-name pairs of states.
#' @return 


filter_asi_blocks <- function(
  block_paths,
  state_concordance_path = here::here("data/external/concord_tables/asi_state_codes/state_codes_table.csv"),
  state_name_path = here::here("data/external/concord_tables/asi_state_codes/state_names_1617.csv")
  ) {


#################################################################
##                    X: Create base sample                    ##
#################################################################

# Read files

block_list <- tibble(filename = block_paths) %>%
	mutate(
		data = map(filename, function(x) read_fst(x) %>% as_tibble()),
		file = str_match(
			string = filename,
			pattern = "block_[A-Z]"
			) %>% as.character()
		)

print("Creating base sample.")

## Remove non-open factories  ------------------------------------
blk_a <- block_list %>%
	filter(file == "block_A") %>%
	select(data) %>%
	unnest(data) 
	
blk_a <- blk_a %>%
	filter(status_of_unit == 1)

## Remove non-manufacturing  ------------------------------------

# Filter NIC codes that are not manufactoring (that is, not in under head 0-4).
# Headings are the first digit of the code. SInce some letters (Z's) are in the
# 1999 sample, I can't just filter numerically. I therefor extract the first 
# character and filter based on if it is lower than 5.
blk_a <- blk_a %>%
	mutate(nic_heading = str_sub(
			string = nic5digit,
			start = 1,
			end = 1
			)
		) %>%
filter(nic_heading < 5)

## Keep only open and manufactoring factories in other blocks ----

# Use the year-dsl pairs in the filtered block A to filter the rest of the data 
# frames to these factories.
open_factories <- blk_a %>%
	select(year, dsl)

# Filter each block according to the list. semi_join keeps only the rows 
# that match that has a match in open_factories (that is, only rows
# matching the dsl-year pair is kept).
block_list <- block_list %>%
	mutate(
		data = map(
			.x = data,
			.f = semi_join,
			open_factories
			)
		)

##################################################################
##                X: Fix plant-level information                ##
##################################################################
print("Fixing plant-level information.")

## Combine plant level blocks ------------------------------------

blk_a <- block_list %>%
	filter(file == "block_A") %>%
	select(data) %>%
	unnest(data) 

blk_e <- block_list %>%
	filter(file == "block_E") %>%
	select(data) %>%
	unnest(data) 

# The employment data has one observation for each employee group. I'm only
# interested in total employees. In some years total employees have sno = 10,
# in others sno = 9. I first create a dummy where "total employees" observation
# has value "1", and the filter based on this. (difference seems to be based on 
# whether child labor is included.)
blk_e <- blk_e %>%
	mutate(
	total_employee_obs = case_when(
		year %in% 2001:2008 & sno == 10 ~ 1,
		year %in% c(1999:2000, 2009:2017) & sno == 9 ~ 1,
		TRUE ~ 0
		)
	) %>%
	filter(total_employee_obs == 1)

blk_g <- block_list %>%
	filter(file == "block_G") %>%
	select(data) %>%
	unnest(data) 

plant_tbl <- blk_a %>%
	left_join(blk_g, by = c("year", "dsl")) %>%
	left_join(blk_e, by = c("year", "dsl"))

# Fix state codes ------------------------------------------------

# In block A (now plant_tbl), state codes are not classified to the same system
# across all years. I use two external tables to fix this. First I use a
# concordance table with the coresponding codes (old and new). Second I use a
# table with the new codes paired with proper state names. 

## Read external tables

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

# I first create a dummy, signifying which years needs to be updated
plant_tbl <- plant_tbl %>%
	mutate(
		change_state_code = ifelse(year < 2001, 1, 0)
		)
 
# update codes in "new_state_code"
plant_tbl <- left_join(
	x = plant_tbl, 
	y = state_conc_tbl, 
	by = c("state_code" = "early_codes", "change_state_code")
	) %>% 
	mutate(
		new_state_code = ifelse(change_state_code == 1, late_codes, state_code)
		)

# add names
plant_tbl <- left_join(plant_tbl, state_name_tbl, by = c("new_state_code" = "code")) %>%
	select(
		-c(change_state_code, late_codes, state_code), # remove the temporary variables
		) %>%
rename(state_name = name)


##################################################################
##               X: Fix product-level information               ##
##################################################################

# Product-level data comes as input products (block H and block I)
# and as output products (block J).

# Input products -----------------------------------------------

blk_h <- block_list %>%
	filter(file == "block_H") %>%
	select(data) %>%
	unnest(data) 

blk_i <- block_list %>%
	filter(file == "block_I") %>%
	select(data) %>%
	unnest(data) 

input_tbl <- rbind(blk_h, blk_i) %>%
	mutate(
		classification = ifelse(year >= 2011, "NPCMS11", "ASICC")
		)

# Output products -----------------------------------------------

blk_j <- block_list %>%
	filter(file == "block_J") %>%
	select(data) %>%
	unnest(data) 

output_tbl <- blk_j %>%
	mutate(
		classification = ifelse(year >= 2011, "NPCMS11", "ASICC"),
		item_code = as.character(item_code)
		)
		

##################################################################
##                        X: Write files                        ##
##################################################################

# Paths ----------------------------------------------------------
# path_list <-list(
#         plant_tbl_path = here("data/interim/asi/plant_data_base_sample.rds"),
#         input_tbl_path = here("data/interim/asi/input_data_base_sample.rds"),
#         output_tbl_path = here("data/interim/asi/output_data_base_sample.rds")
#         )

# Return unweighted files -----------------------------------------

return_ls <- list(
	"plant_tbl" = plant_tbl,
	"input_tbl" = input_tbl,
	"output_tbl" = output_tbl
	)

# saveRDS(
#         object = plant_tbl,
#         file = path_list$plant_tbl_path
#         )
# 
# saveRDS(
#         object = input_tbl,
#         file = path_list$input_tbl_path
#         )
# 
# saveRDS(
#         object = output_tbl,
#         file = path_list$output_tbl_path
#         )
# 
return(return_ls)

# END
}
