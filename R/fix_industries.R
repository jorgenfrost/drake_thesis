#' I first read in the plant-table. I separate it into 3 parts. The observations 
#' based on the NIC 2008 classification, the observations based on the NIC 2004 classification, 
#' and the observations based on the 1998 classifications. I first convert the 2008 into 2004.
#' I then join the 2008- and the 2004 table. Finally, I turn these into 1998. 
#' I finished by filtering for the 1998 manufacturing codes (i.e. between 15 and 37)
#'
#' @param plant_tbl
#' @return a data frame with dsl-year plant identifiers and NIC-1998 codes 
#' on 2- and 3 digit level. Also includes a "concorded_from" dummy variable
#' to see if the concordance scheme was important.
#' @export

# plant_tbl <- readd("asi_base_sample_ls")$plant_tbl

get_industries <- function(plant_tbl) {

#################################################################
##                 1: Split plant observations                 ##
#################################################################



# Use only the necessary variables, and fix the lack of leading 0s after 
# as.numeric (in an earlier cleaning process).
ind_tbl <- plant_tbl %>%
	select(year, dsl, nic5digit) %>%
	mutate(
	       from_nic = case_when(
					  year %in% 1999:2004 ~ "98",
					  year %in% 2005:2008 ~ "04",
					  year %in% 2009:2017 ~ "08",
					  TRUE ~ NA_character_
					  )
	       ) %>%
	mutate(
	       nic5digit = str_pad(nic5digit, width = 5, side = "left", pad = "0"),
	       nic4digit = str_sub(nic5digit, start = 1, end = 4)
	       )
# Separate:
ind08_og <- ind_tbl %>%
	filter(from_nic == "08")

ind04_og <- ind_tbl %>%
	filter(from_nic == "04")

ind98_og <- ind_tbl %>%
	filter(from_nic == "98")


#################################################################
##                 2: 2008 to 2004 CONCORDANCE                 ##
#################################################################

# This table is supplied by Allcott et al.
nic04_08_raw <- read_dta(here("data/external/concord_tables/nic/NIC0408Crosswalk_4digit.dta"))

# Suffers from missing leading 0s.
nic08_04_tbl <- nic04_08_raw %>%
	mutate(
	       nic08 = nic408,
	       nic04 = str_pad(nic04_4, width = 4, side = "left", pad = "0")
	       ) %>%
select(nic04, nic08) 

# There are no duplicate matches, I concord on 4-digit level.
# to see, run: 
# nic08_04_tbl %>% group_by(nic08) %>% summarize(um = n_distinct(nic04)) %>% filter(um != 1)

ind08_tbl <- ind08_og %>%
	left_join(nic08_04_tbl, by = c("nic4digit" = "nic08"))

# Join to 2004 table -------------------------------------------------

# prep 2008 table
ind08_tbl <- ind08_tbl %>%
	select(-nic4digit) %>% # just to make explicit
	select(
	       year,
	       dsl,
	       original_nic = nic5digit,
	       from_nic,
	       nic04
	       )

# prep 2004 table
ind04_tbl <- ind04_og %>%
	select(
	       year,
	       dsl,
	       original_nic = nic5digit,
	       from_nic,
	       nic04 = nic4digit
	       ) 

# join
ind0804_tbl <- bind_rows(ind04_tbl, ind08_tbl)

#################################################################
##                 3: 2004 to 1998 CONCORDANCE                 ##
#################################################################

# Create concordance table -------------------------------------------

c04_to_98_raw <- read_csv(here("data/external/concord_tables/nic/tabula-nic04_nic98.csv"))

# remove all the observations that are headers in the excel doc
c04_to_98_raw <- c04_to_98_raw %>%
	clean_names(case = "snake") %>%
	filter(!is.na(nic_98)) %>%
	select(-activity)

# fix that an asterisk is in some obs
c04_to_98_raw <- c04_to_98_raw %>% 
	mutate(
	       nic_98_clean = str_extract(nic_98, pattern = "^[0-9]{1,4}"),
	       nic_04_clean = str_extract(nic_04, pattern = "^[0-9]{1,4}"),
	       ) 

# add leading 0s to 3-digit codes (due to reading as numeric). needed to extract digit codes
c04_to_98_tbl <- c04_to_98_raw %>%
	mutate(
       nic_98_clean = str_pad(nic_98_clean, width = 4, side = "left", pad = "0"), 
       nic_04_clean = str_pad(nic_04_clean, width = 4, side = "left", pad = "0")
       ) %>%
select(
       nic04 = nic_04_clean,
       nic98 = nic_98_clean
       ) 

# create the mappings -------------------------------------------

# There are two issues in 3-digit mapping: 
# 1) code 0113 in NIC 2004 maps to 112, 113, 200 (partial mapping, growing
# berries/olives). In the 1998 sample, however, the only entry below 1000 is
# 140 (agricultural services). I therefor set the value to this.
# 2) 1513 maps to both 1513 (processing + perserving fruits and vegetables) and 
# 1549 (roasting nuts). In the 1998 data ~ 500 is 1513 and ~4900 is 1549. I set 
# them to 1549. This doesn't matter for the two-digit controls. 

c04_to_98_tbl <- c04_to_98_tbl %>%
	mutate(
	       nic98 = case_when(
				 nic04 == "0113" ~ "0140",
				 nic04 == "1513" ~ "1549",
				 TRUE ~ as.character(nic98)
				 )
	       ) %>%
mutate(
       nic98_3d = str_sub(nic98, start = 1, end = 3)
       ) %>%
select(nic04, nic98_3d) %>% 
distinct() # some uniqueness dissappeared when remove digits

# add mappings to data 
ind0804_tbl <- ind0804_tbl %>% 
	left_join(c04_to_98_tbl, by = c("nic04")) 

# Join to 1998 data --------------------------------------------------

# Prepate 1998 data
ind98_tbl <- ind98_og %>%
	mutate(
	       nic98_3d = str_sub(nic4digit, start = 1, end = 3),
	       original_nic = nic5digit
	       ) %>%
select(-c(nic4digit, nic5digit))

ind080498_tbl <- ind0804_tbl %>%
	select(-nic04) %>%
	bind_rows(ind98_tbl) %>%
	mutate(
	       nic98_2d = str_sub(nic98_3d, start = 1, end = 2)
	       )

manuf_98_tbl <- ind080498_tbl %>%
	filter(as.numeric(nic98_2d) %in% 15:37)


##################################################################
##                       1987 CONCORDANCE                       ##
##################################################################

# All of the concordance tables to 1987 are provided by Allcott et al. For each one, I
# first concord with the most prices mapping, adding missings with the less precise mappings.

# CONCORD 2004 TO 1987 -----------------------------------------------

# Read 2004 to 1987 industry concordance
nic_04_87_5d <- read_dta(here("data/external/concord_tables/nic/nic_04_87_5d.dta")) %>%
	mutate(
	       nic5digit = as.character(nic04)
	       ) %>%
select(nic87 = nic3digit, nic5digit)

nic_04_87_4d <- read_dta(here("data/external/concord_tables/nic/nic_04_87_4d.dta")) %>%
	mutate(nic04 = as.character(nic04_4)) %>%
	select(nic04, nic87 = nic3digit)
	
nic_04_87_4dcomposite <- read_dta(here("data/external/concord_tables/nic/nic_04_87_4d_composite.dta")) %>%
	mutate(nic04 = as.character(nic04_4)) %>%
	select(nic04, nic87_comp = nic387)

# Add 1987 3-digit industries to 2004 industries (set priority of 5 digit matches)
ind04_87_tbl <- ind04_og %>%
	left_join(rename(nic_04_87_5d, nic87_5d_match = nic87) , by = "nic5digit") %>%
	left_join(rename(nic_04_87_4d, nic87_4d_match = nic87), by = c("nic4digit" = "nic04")) %>%
	left_join(rename(nic_04_87_4dcomposite, nic87_4d_composite_match = nic87_comp), by = c("nic4digit" = "nic04")) %>%
	mutate(
	       nic87 = case_when(
				 is.na(nic87_5d_match) ~ nic87_4d_match,
				 is.na(nic87_5d_match) & is.na(nic87_4d_match) ~ nic87_4d_composite_match,
				 is.na(nic87_5d_match) & is.na(nic87_4d_match) & is.na(nic87_4d_composite_match) ~ 9999,
				 TRUE ~ 8888
				 )
	       ) %>%
	select(-c(nic87_5d_match, nic87_4d_match, nic87_4d_composite_match))

ind04_87_tbl <- ind04_og %>%
	mutate(nic4digit = as.numeric(nic4digit)) %>%
	mutate(nic4digit = as.character(nic4digit)) %>%
	left_join(rename(nic_04_87_5d, nic87_5d_match = nic87) , by = "nic5digit") %>%
	left_join(rename(nic_04_87_4d, nic87_4d_match = nic87), by = c("nic4digit" = "nic04")) %>%
	left_join(rename(nic_04_87_4dcomposite, nic87_4d_composite_match = nic87_comp), by = c("nic4digit" = "nic04")) %>%
	mutate(
	       nic87 = case_when(
				 is.na(nic87_5d_match) & is.na(nic87_4d_match) & is.na(nic87_4d_composite_match) ~ NA_real_,
				 is.na(nic87_5d_match) & is.na(nic87_4d_match) ~ nic87_4d_composite_match,
				 is.na(nic87_5d_match) ~ nic87_4d_match,
				 TRUE ~ nic87_5d_match
				 )
	       ) %>%
	select(-c(nic87_5d_match, nic87_4d_match, nic87_4d_composite_match))

# CONCORD 2008 TO 1987 ----------------------------------------------

# Add 1987 3 digit industries to data from 2008 that has been concorded to 2004
ind08_87_tbl <- ind08_tbl %>%
	left_join(nic_04_87_4d, by = "nic04") %>%
	left_join(nic_04_87_4dcomposite, by = "nic04") %>%
	mutate(nic87_correct = ifelse(is.na(nic87), nic87_comp, nic87)) %>%
	select(-c(nic87, nic87_comp), nic87 = nic87_correct)


# CONCORD 1998 TO 1987 ----------------------------------------------

# Add 1987 3 digit  NIC87 industries to 1998 data
nic_98_87_5d <- read_dta(here("data/external/concord_tables/nic/nic_98_87_5d.dta")) %>%
	mutate(
	       nic5digit = as.character(nic98)
	       ) %>%
select(nic87_5d = nic3digit, nic5digit)


nic_98_87_4d <-
	read_dta(here("data/external/concord_tables/nic/nic_98_87_4d.dta")) %>%
	mutate(nic4digit = as.character(nic98_4)) %>%
	select(nic4digit, nic87_4d = nic3digit)
	
ind98_87_tbl <- ind98_og %>%
	left_join(nic_98_87_5d, by = "nic5digit") %>%
	left_join(nic_98_87_4d, by = "nic4digit") %>%
	mutate(nic87 = ifelse(is.na(nic87_5d), nic87_4d, nic87_5d)) %>%
	select(-c(nic87_5d, nic87_4d, nic4digit))


# BIND ROWS TOGETHER AGAIN -----------------------------------------
# Prep
ind98_87_tbl <- ind98_87_tbl %>% rename(original_nic = nic5digit) 
ind04_87_tbl <- ind04_87_tbl %>% select(-nic4digit, original_nic = nic5digit)
ind08_87_tbl <- ind08_87_tbl %>% select(-nic04)

manuf_87_tbl <- bind_rows(
			  ind98_87_tbl,
			  ind04_87_tbl,
			  ind08_87_tbl
			  ) %>%
mutate(nic87 = as.character(nic87))


##################################################################
##                     RETURN LIST OF CODES                     ##
##################################################################

return_list <- list("nic87_code_tbl" = manuf_87_tbl, "nic98_code_tbl" = manuf_98_tbl)

}

 


