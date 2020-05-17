#' This veeeery long and tedious function cleans the data blocks from the 
#' Annual Survey of Industries (ASI) into a format where variables are called
#' the same in different years. There is a lot of variable renaming, type-fixing, 
#' and recoding going on. To get around an issue with the 8GB RAM present on my
#' current machine being overclocked, the cleaned blocks are saved. The function 
#' instead just returns their location as a list of strings.
#'
#' @param asi_files list: filenames of the blocks of all the ASI files.
#' @return list of the files names written out.
#' @export

# Author: Søren Post

# Structure:
# First I define the functions and the objects used globally (like the lookup
# table). The functions are to be applied in this order:
# clean names to snake case
# fix strange stuff in the data (like double variables, etc)
# change names according to the lookup table
# extract the name of the block and put in the list
# change variable types

# TODO: Inkludér Gross sale val

clean_asi_blocks <- function(asi_files) {
  
  ##################################################################
  ##             0: Define function and lookup tables             ##
  ##################################################################
  
  print("Initiating functions and lookup tables.")
  
  ## ---------------------------------------------------------------
  ##                    0.1: Define functions                     -
  ## ---------------------------------------------------------------
  
  # fix_special_cases fixes some of the one-off issues there are
  # with some of the data blocks. For instance, for some reason,
  # the data from 1998-99 has two variables containing state codes.
  # Both match state_code "bad_var"-names. One therefor has to be
  # remove to avoid a "duplicate variable" issue later in the process.
  
  # A larger problem is that (incredibly!) several years have variables
  # that are called the same, but contains different information. This
  # needs to be fixed on a per-year basis.
  
  fix_special_cases <- function(x) {
    
    # This function has the structure:
    # 1. Fix year variable
    # 2. Make sure that there isn't any blocks with more than one unique
    # value in their "block" or "year" columns. This is used to select the
    # blocks with special cases.
    # 3. Fix the years with the "[A-Z]_itm[1-9]{2}" format of variable names
    # (bc they sometimes have the same names for different variables). Years
    # involved are 1998-99 -> 2004-05 and
    
    ## FIX YEAR VARIABLE ########################################
    
    # First: I sort using the year variable. Therefor I first fix this variable.
    # some year variables are listed in 2 digits (fx 98 or 01).
    # to fix, I add block for years in the 90s and 2000 to years in the oughts.
    if ("yr" %in% names(x)) {
      x <- x %>%
        mutate(
          year = as.numeric(yr)
        ) %>%
        select(-yr)
    } else if ("year" %in% names(x)) {
      x <- x %>%
        mutate(year = as.numeric(year))
    } else {
      stop("Error: There isn't a 'year' or 'yr' column in the block.")
    }
    
    x <- x %>%
      mutate(
        year = case_when(
          year > 90 & year < 100 ~ year + 1900,
          year < 20 ~ year + 2000,
          TRUE ~ as.numeric(year)
        )
      )
    
    # For the data from 2014-15, there is a single year observation that has the value 0.
    # This is obviously a mistake. I therefor code all observaitons from the 2015 data as coming
    # from 2015.
    
    if (2015 %in% unique(x$year)) {
      x$year <- 2015
    }
    
    ## GET BLOCK AND YEAR ########################################
    
    # get current year and block of df
    # used to select dfs with special cases
    # that need fixing.
    
    
    current_year <- unique(x$year)
    
    if ("blk" %in% names(x)) {
      current_block <- unique(x$blk)
    } else if ("block" %in% names(x)) {
      current_block <- unique(x$block)
    }
    
    # some debugging
    print(
      str_glue("Now fixing special cases: block {current_block} in year {current_year}.")
    )
    
    # some error handling
    if (length(current_year) != 1) {
      print(current_year)
      stop("Error: more or less than one unique year in data frame")
    }
    
    if (length(current_block) != 1) {
      print(current_year)
      stop("Error: more or less than one unique block in data frame")
    }
    
    current_year <- as.numeric(current_year)
    current_block <- as.character(current_block)
    
    
    ## FIX BLOCKS W. SPECIAL NAMING SCHEME ######################
    
    # This is a very hacky solution, but since the variables do not fit each
    # other, what can you do?
    
    # fix 1998-99
    if (current_year == 1999) {
      if (current_block == "A") {
        x <- x %>%
          select(-a_itm7) %>%
          rename(
            nic5digit = ind_cd,
            district_code = a_itm8,
            rural_urban = a_itm10,
            no_of_units = a_itm11,
            unit_status = a_itm12
          ) %>%
          mutate(
            total_production_cost = NA # 1999 sample does not contain this var
          )
        # end block A
      } else if (current_block == "B") {
	      x <- x %>% 
		      rename(
			     initial_production = b_itm5
			     )
	# end block B
      } else if (current_block == "E") {
        x <- x %>%
          rename(
            sno = e_itm1,
            t_man_day = e_itm4,
            avg_person_work = e_itm7
          ) %>%
          mutate(
            wages = NA # 1999 sample does not contain this var
          )
        
        # end block E
      } else if (current_block == "G") {
        x <- x %>%
          rename(
            var_st_semi_fin = g_itm2,
            val_elec_gen_sold = g_itm3,
          )
        # end block G
      } else if (current_block == "H") {
        x <- x %>%
          rename(
            sno = h_itm1,
            item_code = h_itm3,
	    qty_cons = h_itm5,
            purchase_val = h_itm6
          ) %>%
      mutate(
	     unit_code = NA # 1999 sample does not contain this var
	     )
        # end block H
      } else if (current_block == "I") {
        x <- x %>%
          rename(
            sno = i_itm1,
            item_code = i_itm3,
	    qty_cons = i_itm5,
            purchase_val = i_itm6
          ) %>%
      mutate(
	     unit_code = NA # 1999 sample does not contain this var
	     )
        # end block I
      } else if (current_block == "J") {
        x <- x %>%
          rename(
            sno = j_itm1,
            item_code = j_itm2,
            qty_sold = j_itm4,
	    gross_sale_val = j_itm5,
            net_sale_val = j_itm10
          ) %>%
      mutate(
	     unit_code = NA # 1999 sample does not contain this var
	     )
        # end block J
      }
    }
    
    # fix 1999-00
    if (current_year == 2000) {
      if (current_block == "A") {
        x <- x %>%
          rename(
            scheme = a_itm3,
            nic5digit = a_itm5,
            state_code = a_itm7,
            district_code = a_itm8,
            rural_urban = a_itm9,
            no_of_units = a_itm11,
            unit_status = a_itm12,
            total_production_cost = e_itm11
          )
        # end block A
      } else if (current_block == "B") {
	      x <- x %>% 
		      rename(
			     initial_production = b_itm5
			     )
	# end block B
      } else if (current_block == "E") {
        x <- x %>%
          rename(
            sno = e_itm1,
            t_man_day = e_itm3,
            avg_person_work = e_itm6,
            wages = e_itm7
          )
        
        # end block E
      } else if (current_block == "G") {
        x <- x %>%
          rename(
            var_st_semi_fin = g_itm2,
            val_elec_gen_sold = g_itm3
          )
        # end block G
      } else if (current_block == "H") {
        x <- x %>%
          rename(
            sno = h_itm1,
            item_code = h_itm3,
	    unit_code = h_itm4,
	    qty_cons = h_itm5,
            purchase_val = h_itm6
          )
        # end block H
      } else if (current_block == "I") {
        x <- x %>%
          rename(
            sno = i_itm1,
            item_code = i_itm3,
	    unit_code = i_itm4,
	    qty_cons = i_itm5,
            purchase_val = i_itm6
          )
        # end block I
      } else if (current_block == "J") {
        x <- x %>%
          rename(
            sno = j_itm1,
            item_code = j_itm3,
	    unit_code = j_itm4,
            qty_sold = j_itm6,
	    gross_sale_val = j_itm7,
            net_sale_val = j_itm12
          )
        # end block J
      }
    }
    
    # fix 2000-01
    if (current_year == 2001) {
      if (current_block == "A") {
        x <- x %>%
          rename(
            scheme = a_itm_3,
            nic5digit = a_itm_5,
            state_code = a_itm_7,
            district_code = a_itm_8,
            rural_urban = a_itm_9,
            no_of_units = a_itm_11,
            unit_status = a_itm_12,
            total_production_cost = e_itm_12
          )
        # end block A
      } else if (current_block == "B") {
	      x <- x %>% 
		      rename(
			     initial_production = b_itm_6
			     )
	# end block B
      } else if (current_block == "E") {
        x <- x %>%
          rename(
            sno = e_itm1,
            t_man_day = e_itm3,
            avg_person_work = e_itm6,
            wages = e_itm8
          )
        
        # end block E
      } else if (current_block == "G") {
        x <- x %>%
          rename(
            var_st_semi_fin = g_itm2,
            val_elec_gen_sold = g_itm3
          )
        # end block G
      } else if (current_block == "H") {
        x <- x %>%
          rename(
            sno = h_itm1,
            item_code = h_itm3,
	    unit_code = h_itm4,
	    qty_cons = h_itm5,
            purchase_val = h_itm6
          )
        # end block H
      } else if (current_block == "I") {
        x <- x %>%
          rename(
            sno = i_itm1,
            item_code = i_itm3,
	    unit_code = i_itm4,
	    qty_cons = i_itm5,
            purchase_val = i_itm6
          )
        # end block I
      } else if (current_block == "J") {
        x <- x %>%
          rename(
            sno = j_itm1,
            item_code = j_itm3,
	    unit_code = j_itm4,
            qty_sold = j_itm6,
	    gross_sale_val = j_itm7,
            net_sale_val = j_itm12
          )
        # end block J
      }
    }
    
    # fix 2001-02
    if (current_year == 2002) {
      if (current_block == "A") {
        x <- x %>%
          rename(
            scheme = a_itm3,
            nic5digit = a_itm5,
            state_code = a_itm7,
            district_code = a_itm8,
            rural_urban = a_itm9,
            no_of_units = a_itm11,
            unit_status = a_itm12,
            total_production_cost = e_itm12
          )
        # end block A
      } else if (current_block == "B") {
	      x <- x %>% 
		      rename(
			     initial_production = b_itm6
			     )
	# end block B
      } else if (current_block == "E") {
        x <- x %>%
          rename(
            sno = e_itm1,
            t_man_day = e_itm3,
            avg_person_work = e_itm6,
            wages = e_itm7
          )
        
        # end block E
      } else if (current_block == "G") {
        x <- x %>%
          rename(
            var_st_semi_fin = g_itm2,
            val_elec_gen_sold = g_itm3
          )
        # end block G
      } else if (current_block == "H") {
        x <- x %>%
          rename(
            sno = h_itm1,
            item_code = h_itm3,
	    unit_code = h_itm4,
	    qty_cons = h_itm5,
            purchase_val = h_itm6
          )
        # end block H
      } else if (current_block == "I") {
        x <- x %>%
          rename(
            sno = i_itm1,
            item_code = i_itm3,
	    unit_code = i_itm4,
	    qty_cons = i_itm5,
            purchase_val = i_itm6
          )
        # end block I
      } else if (current_block == "J") {
        x <- x %>%
          rename(
            sno = j_itm1,
            item_code = j_itm3,
	    unit_code = j_itm4,
            qty_sold = j_itm6,
	    gross_sale_val = j_itm7,
            net_sale_val = j_itm12
          )
        # end block J
      }
    }
    
    # fix 2002-03
    if (current_year == 2003) {
      if (current_block == "A") {
        x <- x %>%
          rename(
            scheme = a_itm3,
            nic5digit = a_itm5,
            state_code = a_itm7,
            district_code = a_itm8,
            rural_urban = a_itm9,
            no_of_units = a_itm11,
            unit_status = a_itm12,
            total_production_cost = e_itm12
          )
        # end block A
      } else if (current_block == "B") {
	      x <- x %>% 
		      rename(
			     initial_production = b_itm6
			     )
	# end block B
      } else if (current_block == "E") {
        x <- x %>%
          rename(
            sno = e_itm1,
            t_man_day = e_itm3,
            avg_person_work = e_itm6,
            wages = e_itm8
          )
        
        # end block E
      } else if (current_block == "G") {
        x <- x %>%
          rename(
            var_st_semi_fin = g_itm2,
            val_elec_gen_sold = g_itm3
          )
        # end block G
      } else if (current_block == "H") {
        x <- x %>%
          rename(
            sno = h_itm1,
            item_code = h_itm3,
	    unit_code = h_itm4, 
	    qty_cons = h_itm5,
            purchase_val = h_itm6
          )
        # end block H
      } else if (current_block == "I") {
        x <- x %>%
          rename(
            sno = i_itm1,
            item_code = i_itm3,
	    unit_code = i_itm4, 
	    qty_cons = i_itm5,
            purchase_val = i_itm6
          )
        # end block I
      } else if (current_block == "J") {
        x <- x %>%
          rename(
            sno = j_itm1,
            item_code = j_itm3,
	    unit_code = j_itm4,
            qty_sold = j_itm6,
	    gross_sale_val = j_itm7,
            net_sale_val = j_itm12
          )
        # end block J
      }
    }
    
    # fix 2003-04
    if (current_year == 2004) {
      if (current_block == "A") {
        x <- x %>%
          rename(
            scheme = a_itm3,
            nic5digit = a_itm5,
            state_code = a_itm7,
            district_code = a_itm8,
            rural_urban = a_itm9,
            no_of_units = a_itm11,
            unit_status = a_itm12,
            total_production_cost = e_itm_12
          )
        # end block A
      } else if (current_block == "B") {
	      x <- x %>% 
		      rename(
			     initial_production = b_itm6
			     )
	# end block B
      } else if (current_block == "E") {
        x <- x %>%
          rename(
            sno = e_itm1,
            t_man_day = e_itm3,
            avg_person_work = e_itm6,
            wages = e_itm8
          )
        # end block E
      } else if (current_block == "G") {
        x <- x %>%
          rename(
            var_st_semi_fin = g_itm2,
            val_elec_gen_sold = g_itm3
          )
        # end block G
      } else if (current_block == "H") {
        x <- x %>%
          rename(
            sno = h_itm1,
            item_code = h_itm3,
	    unit_code = h_itm4,
	    qty_cons = h_itm5,
            purchase_val = h_itm6
          )
        # end block H
      } else if (current_block == "I") {
        x <- x %>%
          rename(
            sno = i_itm1,
            item_code = i_itm3,
	    unit_code = i_itm4,
	    qty_cons = i_itm5,
            purchase_val = i_itm6
          )
        # end block I
      } else if (current_block == "J") {
        x <- x %>%
          rename(
            sno = j_itm1,
            item_code = j_itm3,
	    unit_code = j_itm4,
            qty_sold = j_itm6,
	    gross_sale_val = j_itm7,
            net_sale_val = j_itm12
          )
        # end block J
      }
    }
    
    # fix 2004-05
    if (current_year == 2005) {
      if (current_block == "A") {
        x <- x %>%
          rename(
            scheme = a_itm3,
            nic5digit = a_itm5,
            state_code = a_itm7,
            district_code = a_itm8,
            rural_urban = a_itm9,
            no_of_units = a_itm11,
            unit_status = a_itm12,
            total_production_cost = e_itm12
          )
        # end block A
      } else if (current_block == "B") {
	      x <- x %>% 
		      rename(
			     initial_production = b_itm6
			     )
	# end block B
      } else if (current_block == "E") {
        x <- x %>%
          rename(
            sno = e_itm1,
            t_man_day = e_itm3,
            avg_person_work = e_itm6,
            wages = e_itm8
          )
        
        # end block E
      } else if (current_block == "G") {
        x <- x %>%
          rename(
            var_st_semi_fin = g_itm2,
            val_elec_gen_sold = g_itm3
          )
        # end block G
      } else if (current_block == "H") {
        x <- x %>%
          rename(
            sno = h_itm1,
            item_code = h_itm3,
	    unit_code = h_itm4,
	    qty_cons = h_itm5,
            purchase_val = h_itm6
          )
        # end block H
      } else if (current_block == "I") {
        x <- x %>%
          rename(
            sno = i_itm1,
            item_code = i_itm3,
	    unit_code = i_itm4,
	    qty_cons = i_itm5,
            purchase_val = i_itm6
          )
        # end block I
      } else if (current_block == "J") {
        x <- x %>%
          rename(
            sno = j_itm1,
            item_code = j_itm3,
	    unit_code = j_itm4,
            qty_sold = j_itm6,
	    gross_sale_val = j_itm7,
            net_sale_val = j_itm12
          )
        # end block J
      }
    }
    
    # fix 2008-09 and 2009-10 (same var names)
    if (current_year %in% c(2009, 2010)) {
      if (current_block == "A") {
        x <- x %>%
          rename(
            scheme = a_itm3,
            nic5digit = a_itm5,
            state_code = a_itm7,
            district_code = a_itm8,
            rural_urban = a_itm9,
            no_of_units = a_itm11,
            unit_status = a_itm12,
            total_production_cost = e_itm14
          )
        # end block A
      } else if (current_block == "B") {
	      x <- x %>% 
		      rename(
			     initial_production = b_itm7
			     )
	# end block B
      } else if (current_block == "E") {
        x <- x %>%
          rename(
            sno = e_itm1,
            t_man_day = e_itm3,
            avg_person_work = e_itm6,
            wages = e_itm8
          )
        
        # end block E
      } else if (current_block == "G") {
        x <- x %>%
          rename(
            var_st_semi_fin = g_itm2,
            val_elec_gen_sold = g_itm3
          )
        # end block G
      } else if (current_block == "H") {
        x <- x %>%
          rename(
            sno = h_itm1,
            item_code = h_itm3,
	    unit_code = h_itm4,
	    qty_cons = h_itm5,
            purchase_val = h_itm6
          )
        # end block H
      } else if (current_block == "I") {
        x <- x %>%
          rename(
            sno = i_itm1,
            item_code = i_itm3,
	    unit_code = i_itm4,
	    qty_cons = i_itm5,
            purchase_val = i_itm6
          )
        # end block I
      } else if (current_block == "J") {
        x <- x %>%
          rename(
            sno = j_itm1,
            item_code = j_itm3,
	    unit_code = j_itm4,
            qty_sold = j_itm6,
	    gross_sale_val = j_itm7,
            net_sale_val = j_itm12
          )
        # end block J
      }
    }
    
    
    return(x)
    # end function
  }
  
  ## ----------------------------------------------------------------
  
  # fix_variable_names changes the names of the variable names
  # I need in the final data frame to names common for all of the dfs.
  # I use a lookup table (variable_lookup_tbl) to correct the names.
  # I save the table in the processed data folder for easy inspection.
  
  fix_variable_names <- function(x, var_lookup) {
    # x is a data frame that needs correct names
    # var_lookup is a data frame with a column of bad names (bad_var)
    # and a column of matched correct names (correct_var).
    
    if ("blk" %in% names(x)) {
      current_block <- unique(x$blk)
    } else if ("block" %in% names(x)) {
      current_block <- unique(x$block)
    }
    
    print(str_glue("Fixing variable names: block {current_block} in year {x$year[1]}"))
    
    for (i in seq_along(names(x))) {
      
      # check if var needs to be changed
      if (names(x)[i] %in% var_lookup$bad_var) {
        
        # create index to pull the correct var
        index <- var_lookup$bad_var %in% names(x)[i]
        
        # change the name to the correct one
        names(x)[i] <- var_lookup$correct_var[index]
        
        # end loop
      }
      
      # end if
    }
    
    return(x)
    
    # end function
  }
  
  ## ----------------------------------------------------------------
  
  # In order to separate and merge the proper parts of the data frames,
  # I need to know which block of the questionnaire they come from.
  # This block system is not the same accross all years. This function
  # converts the block names of the years not following the main pattern,
  # and extracts all the correct block names to a "super" variable on the
  # main list of data frames.
  
  get_block <- function(x) {
    
    # throws an error if there is more than one block
    return(unique(x$blk))
    
    # end function
  }
  
  ## ----------------------------------------------------------------
  
  # Some of the original data is saved as the wrong variable type.
  # fix_variable_types coerces the types of variables to the correct
  # type.
  
  fix_variable_types <- function(df) {
    print(str_glue("Fixing variable types: block {df$blk[1]} in year {df$year[1]}"))
    
    correct_types_fun <- function(x) {
      
      # TODO: THE item_code VARIABLE SHOULD BE KEPT AS CHARACTER
      # If not group under header 01 is converting incorrectly to CPC-2
      
      # any_letters checks if a vector contains a letter
      any_letters <- function(x) {
        any(grepl("[A-Za-z]", x, perl = T))
      }
      # check only 10 first observations. Error is thrown if we try
      # to convert chr to numeric anyway, so I only need the basic info.
      has_letters <- any_letters(x = x[1:10])
      
      # if the vector contains a letter, the type is set to as.character
      # if not, it is set to numeric.
      if (has_letters == TRUE) {
        x <- as.character(x)
      } else {
        x <- as.numeric(x)
      }
      
      return(x)
      # end correct_type function
    }
    
    # By mapping unto the x-tibble, the function is applied columnwise.
    df <- df %>% map_dfc(correct_types_fun)
    
    # Finally, since the nic5digit variable contains some letters in year 1998-99
    # but only numbers in the other years, the above method does not work. I
    # therefor coerce all nic5digit variables to character type.
    
    if ("nic5digit" %in% names(df)) {
      df$nic5digit <- as.character(df$nic5digit)
    }
    
    return(df)
  }
  
  
  ## ----------------------------------------------------------------
  # select_block_vars basically just checks the block of the current
  # data frame and selects the relevant variables based on the
  # lookup list defined below.
  
  select_block_vars <- function(df, var_list) {
    
    # for each data frame the blk is the same across all obs.
    # If not, an error is thrown.
    current_block <- unique(df$blk)
    current_year <- unique(df$year)
    
    # early return for blocks that are not relevant
    if (!(current_block %in% names(var_list))) {
      return(df)
    }
    
    print(str_glue("Now selecting the relevant variables for {current_block} in year {current_year}."))
    print(names(df))
    
    relevant_variables <- block_variable_list[[current_block]]
    
    df <- df %>%
      select(all_of(relevant_variables))
    
    return(df)
    # end function
  }
  
  ## ---------------------------------------------------------------
  ##                  0.2: Define lookup tables                   -
  ## ---------------------------------------------------------------
  
  # I now define the lookup-table of variable names. This is implemented
  # with a little bit of double work. It is mostly done to make sure I
  # get it right. "bad_var" is the lookup-variable. If a variable
  # name matches here, it needs to change to the corresponding "correct_var"
  # value.
  
  variable_lookup_tbl <- tibble(
    bad_var = c(
      "yr", # year
      "block", # blk
      "state_cd", # state_code
      "district_cd", # district_code
      "district", # district_code
      "rural_urban_cd", # rural urban
      "mult", # multilplier
      "multilplier", # multiplier
      "noof_units", # no_of_unit
      "unit", # number of units
      "ind_cd_return", # nic5digit
      "ind_cd", # nic5digit
      "inc5digit", # nic5digit
      "state", # state_code
      "unit_status", # status_of_unit
      "status_unit",
      "statusofunit",
      "mandays_worked_total", # t_man_day
      "avg_noof_persons_worked", # avg_person_work
      "ave_number_personwork", # avg_person_work
      "s_no", # sno
      "value_in_elect_gen_and_sold", # val_elec_gen_sold
      "value_elec_generat_sold", # val_elec_gen_sold
      "var_stok_sem_fin_goods", # var_st_sem_fin
      "var_instock_ofsemi_finished_goods", # var_st_sem_fin #
      "pur_val", # purchase_val
      "purchase_value", # purchase_val
      "qty_consumed", # qty_cons
      "purvaldel", # purchase_val
      "purch_value", # purchase_val
      "rateperunit", # rate_per_unit
      "rate_perunit", # rate_per_unit
      "net_saleval", # net_sale_val
      "netsaleval", # net_sale_val
      "per_unit_netsale_value", # net_sale_val
      "per_unit_net_sale_val", # net_sale_val
      "itemcode", # item_code
      "rsl", # dsl (before 1998)
      "ind_5digit", # nic5digit
      "wgt",
      "factories",
      "sector",
      "status",
      "no_of_factories",
      "man_days_mfd",
      "avg_pers_workd",
      "val_elect_gen", # val_elec_gen_sold
      "var_semi_fin", # var_st_semi_fin
      "item",
      "per_unit_net_sal",
      "manday_manu",
      "avg_pers_work",
      "si_no",
      "ex_fact_val_output", # exfact_val_output
      "exfactval_output", # exfact_val_output
      "ex_factval_output", # exfact_val_output
      "ex_fact_val_of_qty_manuf", # exfact_val_output
      "qtysold", # qty_sold,
      "costofprod", # total_production_cost
      "cost_prod", # see above
      "costop", # see above
      "costof_prod", # see above
      "prod_cost", # see above
      "wages_sal", # wages
      "wagessalaries_rs", # wages
      "wages_salary", # wages
      "uom", #unit_code
      "unitcode", # unit_code
      "qtycons", # qty_cons
      "unit_qty", # unit_code
      "gross_sal_val",
      "grosssalval",
      "gross_salevalue",
      "init_prod", # initial production
      "inti_prod", # initial production
      "yearof_in_prod",
      "yr_initial_production",
      "unit_quantity_code"
    )
  ) %>%
    mutate(
      correct_var = case_when(
        bad_var == "yr" ~ "year", # all blocks
        bad_var == "block" ~ "blk", # all blocks
        bad_var == "state_cd" ~ "state_code", # block A start
        bad_var == "state" ~ "state_code",
        bad_var == "district_cd" ~ "district_code",
        bad_var == "district" ~ "district_code",
        bad_var == "rural_urban_cd" ~ "rural_urban",
        bad_var == "mult" ~ "multiplier",
        bad_var == "multilplier" ~ "multiplier",
        bad_var == "noof_units" ~ "no_of_units",
        bad_var == "unit" ~ "no_of_units",
        bad_var == "ind_cd_return" ~ "nic5digit",
        bad_var == "ind_cd" ~ "nic5digit",
        bad_var == "inc5digit" ~ "nic5digit",
        bad_var == "unit_status" ~ "status_of_unit",
        bad_var == "status_unit" ~ "status_of_unit",
        bad_var == "statusofunit" ~ "status_of_unit", # block A finish
        bad_var == "mandays_worked_total" ~ "t_man_day", # block E start
        bad_var == "avg_noof_persons_worked" ~ "avg_person_work",
        bad_var == "ave_number_personwork" ~ "avg_person_work",
        bad_var == "s_no" ~ "sno", # block E finish
        bad_var == "value_in_elect_gen_and_sold" ~ "val_elec_gen_sold", # block G start
        bad_var == "value_elec_generat_sold" ~ "val_elec_gen_sold",
        bad_var == "var_stok_sem_fin_goods" ~ "var_st_semi_fin",
        bad_var == "var_instock_ofsemi_finished_goods" ~ "var_st_semi_fin", # block G end
        bad_var == "rateper_unit" ~ "rate_per_unit", # block H start
        bad_var == "unitcode" ~ "unit_code",
        bad_var == "unit_of_qty" ~ "unit_code",
        bad_var == "unit_quantity_code" ~ "unit_code",
        bad_var == "pur_val" ~ "purchase_val",
        bad_var == "purchase_value" ~ "purchase_val",
        bad_var == "qty_consumed" ~ "qty_cons", # block H end
        bad_var == "purvaldel" ~ "purchase_val", # block I start
        bad_var == "purch_value" ~ "purchase_val",
        bad_var == "rate_perunit" ~ "rate_per_unit",
        bad_var == "rateperunit" ~ "rate_per_unit", # blokc I finsih
        bad_var == "net_saleval" ~ "net_sale_val", # block J start
        bad_var == "netsaleval" ~ "net_sale_val",
        bad_var == "per_unit_netsale_value" ~ "net_sale_val",
        bad_var == "per_unit_net_sale_val" ~ "net_sale_val",
        bad_var == "itemcode" ~ "item_code", # block J finish
        bad_var == "rsl" ~ "dsl", # assic start
        bad_var == "ind_5digit" ~ "nic5digit",
        bad_var == "wgt" ~ "multiplier",
        bad_var == "sector" ~ "rural_urban",
        bad_var == "factories" ~ "no_of_units",
        bad_var == "status" ~ "status_of_unit",
        bad_var == "no_of_factories" ~ "no_of_units",
        bad_var == "man_days_mfd" ~ "t_man_day",
        bad_var == "avg_pers_workd" ~ "avg_person_work",
        bad_var == "val_elect_gen" ~ "val_elec_gen_sold",
        bad_var == "var_semi_fin" ~ "var_st_semi_fin",
        bad_var == "item" ~ "item_code",
        bad_var == "uom" ~ "unit_code",
        bad_var == "per_unit_net_sal" ~ "net_sale_val",
        bad_var == "manday_manu" ~ "t_man_day",
        bad_var == "avg_pers_work" ~ "avg_person_work",
        bad_var == "si_no" ~ "sno",
        bad_var == "ex_fact_val_output" ~ "exfact_val_output",
        bad_var == "exfactval_output" ~ "exfact_val_output",
        bad_var == "ex_factval_output" ~ "exfact_val_output",
        bad_var == "ex_fact_val_of_qty_manuf" ~ "exfact_val_output",
        bad_var == "j_itm10" ~ "net_sale_val",
        bad_var == "qtysold" ~ "qty_sold",
        bad_var == "costofprod" ~ "total_production_cost",
        bad_var == "cost_prod" ~ "total_production_cost",
        bad_var == "costop" ~ "total_production_cost",
        bad_var == "costof_prod" ~ "total_production_cost",
        bad_var == "prod_cost" ~ "total_production_cost",
        bad_var == "wages_sal" ~ "wages",
        bad_var == "wagessalaries_rs" ~ "wages",
        bad_var == "wages_salary" ~ "wages",
	bad_var == "uom" ~ "unit_code",
	bad_var == "unitcode" ~ "unit_code",
	bad_var == "qtycons" ~ "qty_cons",
	bad_var == "unit_qty" ~ "unit_code",
	bad_var == "gross_sal_val" ~ "gross_sale_val",
	bad_var == "grosssalval" ~ "gross_sale_val",
	bad_var == "gross_salevalue" ~ "gross_sale_val",
	bad_var == "init_prod" ~ "initial_production",
	bad_var == "inti_prod" ~ "initial_production",
	bad_var == "yearof_in_prod" ~ "initial_production",
	bad_var == "yr_initial_production" ~ "initial_production",
	bad_var == "unit_quantity_code" ~ "unit_code",
        TRUE ~ NA_character_
      )
    )
  
  
  ## ---------------------------------------------------------------
  
  # The following is the list of variables I want to use from each block.
  block_variable_list <- list(
    A = c(
      "year",
      "blk",
      "dsl", # sample ID
      "scheme",
      "state_code",
      "district_code",
      "nic5digit", # industry code
      "rural_urban", # dummy
      "status_of_unit", # closed, open, non-response, etc
      "no_of_units", # joint returns?
      "total_production_cost", # total cost of production
      "multiplier" # sample weight
    ),
    B = c(
	  "year",
	  "blk",
	  "dsl",
	  "initial_production"
	  ),
    E = c(
      "year",
      "blk",
      "dsl", # sample ID
      "sno", # employee group described
      "t_man_day", # mandays worked manufacturing total
      "avg_person_work", # constructed: avg number of persons working
      "wages"
    ),
    G = c(
      "year",
      "blk",
      "dsl",
      "var_st_semi_fin", # variation in stock of semi finished goods
      "val_elec_gen_sold" # value of electricity generated
    ),
    H = c(
      "year",
      "blk",
      "dsl",
      "sno", # serial no
      "item_code", # NPCMS-11 code / ASICC code
      "unit_code", # unit 
      "qty_cons", # qty consumed
      "purchase_val" # purchase value
    ),
    I = c(
      "year",
      "blk",
      "dsl",
      "sno", # serial no
      "item_code", # NPCMS-11 code / ASICC code
      "unit_code", # unit 
      "qty_cons", # qty consumed
      "purchase_val" # purchase value at delivery
    ),
    J = c(
      "year",
      "blk",
      "dsl",
      "item_code",
      "unit_code",
      "qty_sold",
      "net_sale_val",
      "gross_sale_val"
    )
  )
  
  #################################################################
  ##                        1: Read files                        ##
  #################################################################
  
  print("Reading files.")
  
  # Read all the ASI files into a list of data frames ("data")
  # (takes a little time: 264 data blocks, 2GB)
  # For now, I limit myself to the samples after 1997
  # (those before require a substantially greater amount of cleaning).
  
  asi_nest <- tibble(filename = asi_files) %>%
    mutate(
      data = map(filename, read_dta), # read files
      data = map(data, clean_names, case = "snake"), # improve colnames
      survey_start = str_match( # create column with year
        string = filename,
        pattern = "[0-9]{4}"
      ) %>% as.numeric(),
    ) %>%
    filter(survey_start > 1998)
  
  ## ---------------------------------------------------------------
  
  ##################################################################
  ##                       2: Data cleaning                       ##
  ##################################################################
  
  ## ----------------------------------------------------------------
  ##                2.1: Fix variable names, types                 -
  ## ----------------------------------------------------------------
  
  # I now apply the functions to conduct the first round of cleaning the
  # data: varible names, block, and types.
  
  asi_nest <- asi_nest %>%
    mutate(
      data = map(
        .x = data,
        .f = fix_special_cases
      ),
      data = map(
        .x = data,
        .f = fix_variable_names,
        var_lookup = variable_lookup_tbl
      ),
      block = map_chr(
        .x = data,
        .f = get_block
      ),
      data = map(
        .x = data,
        .f = fix_variable_types
      )
    )
  
  ## ---------------------------------------------------------------
  ##                  2.2: Select block variables                 -
  ## ---------------------------------------------------------------
  print("Selecting block variables.")
  
  # I now select the proper variables for each block.
  # This is done by applying the select_block_vars function that
  # knows which variables to select. The function should be mapped
  # unto the list-column.
  
  # apply the function to select the relevant variables.
  asi_nest <- asi_nest %>%
    filter(block %in% names(block_variable_list)) %>%
    mutate(
      data = map(
        data,
        select_block_vars,
        var_list = block_variable_list
      )
    )
  
  ##################################################################
  ##                        3: Write files                        ##
  ##################################################################
  
  print("Writing files to disk.")
  
  # Bind all years of the respective blocks together and write
  # to file.
  
  for (i in seq_along(unique(asi_nest$block))) {
    current_block <- unique(asi_nest$block)[i]
    
    block_tbl <- asi_nest %>%
      filter(block == current_block) %>%
      select(data) %>%
      unnest(data)
    
    write_fst(
      x = block_tbl,
      path = here(paste0("data/interim/asi/asi_block_", current_block, "_cleaned.fst"))
    )
    
    rm(block_tbl)
  }
  
  cleaned_files_ls <- list.files(
    path = here("data/interim/asi"),
    pattern = "cleaned",
    full.names = TRUE
  )
  
  return(cleaned_files_ls)
  
} 

