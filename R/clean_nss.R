#' Function that cleans the units fomr the NSS

clean_nss <- function() {

##################################################################
##                  X: READ AND SEPARATE FILES                  ##
##################################################################

file_paths <- list.files(
	   path = here("data/external/nss/extracted"),
	   pattern = ".dta",
	   recursive = TRUE,
	   full.names = TRUE
	   )

files <- list.files(
	   path = here("data/external/nss/extracted"),
	   pattern = ".dta",
	   recursive = TRUE,
	   full.names = FALSE
	   )


read_files <- function(path) {
	df <- read_dta(path) %>%
		clean_names(case = "snake")

	return(df)
}

blocks <- tibble(file_paths, files) %>%
	mutate(
	       data = map(file_paths, read_files)
	       ) %>%
	mutate(nss = str_match(string = files, pattern = "[0-9]{2}") %>% as.numeric()) %>%
	select(-file_paths)

# Separate the blocks and select variables

nss_ls <- list()

# NSS 55: 2000 -------------------------------------------------------
nss2000 <- blocks %>%
	filter(nss == 55) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2000) %>%
	mutate(nss = 55)

# fix state codes and select variables 
state_codes55 <- read_csv("/home/post/drake_project/data/external/concord_tables/nss_state_codes/nss_to_asi_state_codes_round55.csv")

nss_ls$nss2000 <- nss2000 %>%
	mutate(state = as.numeric(state)) %>%
	left_join(state_codes55, by = c("state" = "wrong_code55")) %>%
	select(
	       nss,
	       year,
	       state = proper_code,
	       age = b4_q5,
	       general_education = b4_q7,
	       weight = wgt_sr_comb
	       )

# NSS 56: 2001 -------------------------------------------------------
nss2001 <- blocks %>%
	filter(nss == 56) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2001) %>%
	mutate(nss = 56)

# fix state codes and select variables 
state_codes56 <- read_csv("/home/post/drake_project/data/external/concord_tables/nss_state_codes/nss_to_asi_state_codes_round56.csv")

nss_ls$nss2001 <- nss2001 %>%
	mutate(state = as.numeric(state)) %>%
	left_join(state_codes56, by = c("state" = "wrong_code56")) %>%
	select(
	       nss,
	       year,
	       state = proper_code,
	       age = b4_q5,
	       general_education = b4_q7,
	       weight = wgt_combined
	       )

# NSS 57: 2002 -------------------------------------------------------
nss2002 <- blocks %>%
	filter(nss == 57) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2002) %>%
	mutate(nss = 57)

# select variables
nss_ls$nss2002 <- nss2002 %>% 
	select(
	       nss,
	       year,
	       state,
	       age = b4_q5,
	       general_education = b4_q7,
	       weight = wgt_combined
	       )

# NSS 58: 2003 -------------------------------------------------------
nss2003 <- blocks %>%
	filter(nss == 58) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2003) %>%
	mutate(nss = 58)

# select vars 
nss_ls$nss2003 <- nss2003 %>%
	select(
	       nss,
	       year,
	       state,
	       age = b4_q5,
	       general_education = b4_q7,
	       weight = wgt_combined
	       )

# NSS 60: 2004 -------------------------------------------------------
nss2004 <- blocks %>%
	filter(nss == 60) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2004) %>%
	mutate(nss = 60)

# select vars
nss_ls$nss2004 <- nss2004 %>%
	select(
	       nss,
	       year,
	       state,
	       age = b4_c5,
	       general_education = b4_c7,
	       weight = wgt_combined
	       )


# NSS 61: 2005 -------------------------------------------------------
nss2005 <- blocks %>%
	filter(nss == 61) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2005) %>%
	mutate(nss = 61)

nss_ls$nss2005 <- nss2005 %>%
	select(
	       nss,
	       year,
	       state = state_code,
	       age,
	       general_education,
	       weight = weight_combined
	       )

# NSS 62: 2006 -------------------------------------------------------
nss2006 <- blocks %>%
	filter(nss == 62) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2006) %>%
	mutate(nss = 62)

# select vars
nss_ls$nss2006 <- nss2006 %>%
	select(
	       nss,
	       year,
	       state,
	       age = b4_q5,
	       general_education = b4_q7,
	       weight = wgt_comb
	       )

# NSS 63: 2007 -------------------------------------------------------
nss2007 <- blocks %>%
	filter(nss == 63) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2007) %>%
	mutate(nss = 63)

# select vars
nss_ls$nss2007 <- nss2007 %>%
	select(
	       nss,
	       year,
	       state,
	       age = b4_q5,
	       general_education = b4_q7,
	       weight = wgt_combined
	       )

# NSS 64: 2008 -------------------------------------------------------
nss2008 <- blocks %>%
	filter(nss == 64) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2008) %>%
	mutate(nss = 64)

nss_ls$nss2008 <- nss2008 %>%
	select(
	       nss,
	       year,
	       state,
	       age = b4_c5,
	       general_education = b4_c7,
	       weight = wgt_combined
	       )

# NSS 66: 2010 -------------------------------------------------------
nss2010 <- blocks %>%
	filter(nss == 66) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2010) %>%
	mutate(nss = 66)

# Select variables
nss_ls$nss2010 <- nss2010 %>% 
	select(
	       nss,
	       year,
	       state,
	       age,
	       general_education,
	       weight
	       )

# NSS 68: 2012 -------------------------------------------------------
nss2012 <- blocks %>%
	filter(nss == 68) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2012) %>%
	mutate(nss = 68)

nss_ls$nss2012 <- nss2012 %>%
	select(
	       nss,
	       year,
	       state,
	       age,
	       general_education,
	       weight = multiplier_comb
	       )


# NSS 70: 2013 -------------------------------------------------------
nss2013 <- blocks %>%
	filter(nss == 70) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2013) %>%
	mutate(nss = 70)

nss_ls$nss2013 <- nss2013 %>%
	select(
	       nss,
	       year,
	       state,
	       age = b4q5,
	       general_education = b4q6,
	       weight = weight_sc
	       )
# NSS 71: 2014 -------------------------------------------------------
nss2014 <- blocks %>%
	filter(nss == 71) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2014) %>%
	mutate(nss = 71)

nss_ls$nss2014 <- nss2014 %>%
	select(
	       nss,
	       year,
	       state = state_cd,
	       age,
	       general_education = gen_edu,
	       weight = wgt_combined
	       )

# NSS 72: 2015 -------------------------------------------------------
nss2015 <- blocks %>%
	filter(nss == 72) %>%
	select(data) %>%
	unnest(data) %>%
	mutate(year = 2015) %>%
	mutate(nss = 72)

nss_ls$nss2015 <- nss2015 %>%
	select(
	       nss,
	       year,
	       state,
	       age = b4q5,
	       general_education = b4q7,
	       weight = weight_sc
	       )

##################################################################
##                      X: JOIN BLOCKS AND FIX EDUCATION        ##
##################################################################

join_fun <- function(df) {
	df %>%
		mutate(
		       nss = as.numeric(nss),
		       state = as.numeric(state),
		       year = as.numeric(year),
		       general_education = as.numeric(general_education),
		       age = as.numeric(age)
		       ) %>%
	mutate(
	       state = str_pad(state, width = 2, side = "left", pad = "0")
	       )
}

rm(blocks)

nss_tbl <- map_dfr(nss_ls, join_fun)

rm(nss_ls)

# Create "finished secondary dummy"
nss_tbl <- nss_tbl %>%
	mutate(
	       finished_secondary = case_when(
					     nss == 72 & general_education >=10 ~ 1,
					     nss == 71 & general_education >=10 ~ 1,
					     nss == 70 & general_education >=8 ~ 1,
					     nss == 68 & general_education >=8 ~ 1,
					     nss == 66 & general_education >= 8 ~ 1,
					     nss == 64 & general_education >= 10 ~ 1,
					     nss == 63 & general_education >= 6 ~ 1,
					     nss == 62 & general_education >= 8 ~ 1,
					     nss == 61 & general_education >= 8 ~ 1,
					     nss == 60 & general_education >= 6 ~ 1,
					     nss == 58 & general_education >= 6 ~ 1,
					     nss == 57 & general_education >= 6 ~ 1,
					     nss == 56 & general_education >= 6 ~ 1,
					     nss == 55 & general_education >= 8 ~ 1,
					     TRUE ~ 0
					     )
	       )



#################################################################
##              X: CALCULATE STATE-YEAR VARIABLES              ##
#################################################################

# get share that finished secondary
edu_tbl <- nss_tbl %>%
	group_by(year, state) %>%
	summarize(
		  share_finished_secondary = weighted.mean(finished_secondary, weight, na.rm = TRUE)
		  )

# share between 15 and 60
age_tbl <- nss_tbl %>%
	mutate(
	       between_15_60_dmy = ifelse(age %in% 15:60, 1, 0)
	       ) %>%
group_by(year, state) %>%
summarize(
	  share_15_60 = weighted.mean(between_15_60_dmy, weight, na.rm = TRUE)
	  )

##################################################################
##                    X: RETURN JOINED TABLE                    ##
##################################################################

out_tbl <- full_join(edu_tbl, age_tbl) 

return(out_tbl)
}
