#' This function is fixes the product classifcations in the output
#' block of the ASI data. The input is the cleaned data block 
#' which needs to have its products converted. It then separates the block
#' into the years that are classified in ASICC and in NPCMS-2011. Next, it
#' convert both parts into CPC-2 and join them. Finally it convert
#' the re-joined output and input parts into HS07 and evaluate the loss
#' of output and obsevations under the "strict" and the "lenient" approach.
#'
#' @param output_data data frame: the cleaned product output block (J) 
#' from the ASI data.
#' @param return character: a string to choose what the function returns. 
#' Possible values are "main", "verbose", and "summary". 
#' - "main" return the output table with the HS96 product classification (both 
#' strict and lenient). 
#' - "verbose" is the more messy output block with all the intermediate variables 
#' (useful for debugging)
#' - "summary" is a list of data frames that evaluate the loss of output- and observations 
#' coverting between the product-classifications.
#' @ plant_data data frame: the cleaned plant block from the base sample list. Used
#' for evaluating observations losses by states and assigning weights.
#' @param cpc2_path charatcter: path to table with codes and names
#' of CPC-2 classification. From UNSD.
#' 
# Note: the strict match includes only codes that were perfectly mapped
# between classifications. The lenient approach just chooses the firt
# of possible matches. Since the classification are pretty granular (and
# partial matches therefor very close to each other, this shouldn't be very
# problematic, albeit not very rigorous).

# TODO: Hvor i svinget er det at der ryger nogle observationer?

fix_asi_products <- function(output_data, plant_data = NULL, return = "main",
  cpc2_path = here("data/external/concord_tables/product_codes/CPC_Ver_2_english_structure.txt")
) {
  
  # check inputs
  if (!is.data.frame(output_data)) {
    stop("Error: `output_data` is not a data frame.")
  } 
  
  return <- stringr::str_to_lower(return)
  
  if (!return %in% c("main", "verbose", "summary")) {
    stop("Error: `return` is not one of the acceptable values. Options are 'main', 'verbose', or 'summary'.")
  }
  
  if (!is.character(cpc2_path)) {
    stop("Error: `cpc2_path` is not a string.")
  } 
  
  # get concordance tables
  asicc_to_cpc2_tbl <- get_asicc_cpc2_concordance()
  cpc2_to_hs07_tbl <- get_cpc2_hs07_concordance()
  hs07_to_hs96_tbl <- get_hs07_hs96_concordance()

  
  # Separate observation by classification ------------------------
  
  asicc_output_tbl <- output_data %>%
    filter(classification == "ASICC")
  
  npcms_output_tbl <- output_data %>%
    filter(classification == "NPCMS11")
  
  rm("output_data")
  
  #################################################################
  ##                  1: Convert products to CPC-2               ##
  #################################################################
  
  cpc2_tbl <- read_csv(cpc2_path) %>%
    clean_names() %>%
    rename(cpc2_desc = description)
  
  # Add CPC-2 concordance to ASICC years ----------------------------
  
  asicc_output_tbl <- left_join(asicc_output_tbl, asicc_to_cpc2_tbl, by = c("item_code" = "asicc_code"))
  
  # Add CPC-2 concordance to NPCMS11 years  -------------------------
  
  concord_npcms <- function(df) {
    
    # I remove all the observations that are "Other products/by products" = 
    # 9921100; "Total:" = 9995000.
    df <- df %>%
      filter(!item_code %in% c(9921100, 9995000))
    
    # This is way more hacky then it needs to be, but the way I implemented
    # the fix_types didn't account for leading zeroes in the item codes.
    # CPC-2 codes are just NPCMS-11 codes with the last two digits removed.
    # When converting from character to numeric (in fixing variable types)
    # leading zeroes were removed. Fixing is a hassle.
    # I first rectify this (padding with a 0) and them remove the last two digits.
    
    df <- df %>%
      mutate(
        item_code = str_pad( # fix lost leading 0s
          string = item_code,
          width = 7,
          side = "left",
          pad = "0"
        )
      ) %>%
      mutate(
        cpc2_code = str_sub( # remove last two digits
          string = item_code,
          start = 1,
          end = 5
        )
      ) %>%
      left_join(cpc2_tbl, by = c("cpc2_code" = "code"))
    
    return(df) 
  }
  
  # Output
  npcms_output_tbl <- concord_npcms(npcms_output_tbl)
  
  # Clean up variables and rejoin tables -----------------------------------------------------
  
  # ASICC data
  prepare_asicc <- function(df) {
    
    # I remove all the observations that are "Other products/by products" = 
    # 9921100; "Total:" = 9995000.
    df <- df %>%
      filter(!item_code %in% c("99211", "99950"))
    
    df <- df %>%
      mutate(
        item_code = as.character(item_code)
      ) %>%
      select(
        -c(
          npcms_2011,
          strict_npcms_2011,
          lenient_npcms_2011,
          partial
        )
      )
    
    return(df)
  }
  
  # NPCMS-11 data
  # Since NPCMS-11 maps perfectly to CPC-2, no difference between strict and lenient matches.
  # However, since the codes were converted using the digit-approach, I still need to weed out
  # the "asi-specific" codes (like "total" and "by-products"). I do this by using the list of 
  # CPC-2 codes from UNSTATS
  
  prepare_npcms <- function(df) {
    
    df <- df %>%
      mutate(
        strict_cpc2 = ifelse(cpc2_code %in% cpc2_tbl$code, cpc2_code, NA),
        lenient_cpc2 = strict_cpc2 # just copy, no need to run twice
      ) %>%
      select(-c(cpc2_code))
    
    return(df)
  }
  
  # Output
  asicc_output_tbl <- prepare_asicc(asicc_output_tbl)
  npcms_output_tbl <- prepare_npcms(npcms_output_tbl)
  
  # Bind together
  output_tbl <- bind_rows(asicc_output_tbl, npcms_output_tbl)
  
  ##################################################################
  ##                     2: Convert CPC-2 codes to HS07           ##
  ##################################################################
  
  # create strict matches
  strict_cpc2_to_hs07_tbl <- cpc2_to_hs07_tbl %>% 
    select(cpc2_code, strict_hs07, hs07_desc) %>%
    distinct() # bc some of the uniqueness is contained in lenient matches and this means that left_join expands rows.
  print("done")
  output_tbl <- left_join(
    x = output_tbl,
    y = strict_cpc2_to_hs07_tbl,
    by = c("strict_cpc2" = "cpc2_code")
  ) 
  
  # add lenient matches 
  lenient_cpc2_to_hs07_tbl <- cpc2_to_hs07_tbl %>%
    select(cpc2_code, lenient_hs07, hs07_desc) %>%
    distinct() # bc some of the uniqueness is contained in strict matches and this means that left_join expands rows.
  
  output_tbl <- left_join(
    x = output_tbl,
    y = lenient_cpc2_to_hs07_tbl,
    by = c("lenient_cpc2" = "cpc2_code")
  ) 
  
  #################################################################
  ##                3: Convert HS07 codes to HS96                ##
  #################################################################
  
  # create strict matches
  strict_hs07_to_hs96_tbl <- hs07_to_hs96_tbl %>% 
    select(
      hs07_code,
      strict_hs96,
      hs96_desc
    ) %>%
    distinct() # bc some of the uniqueness is contained in lenient matches and this means that left_join expands rows.
  
  output_tbl <- left_join(
    x = output_tbl,
    y = strict_hs07_to_hs96_tbl,
    by = c("strict_hs07" = "hs07_code")
  ) 
  
  # add lenient matches 
  lenient_hs07_to_hs96_tbl <- hs07_to_hs96_tbl %>%
    select(hs07_code, lenient_hs96, hs96_desc) %>%
    distinct() # bc some of the uniqueness is contained in strict matches and this means that left_join expands rows.
  
  output_tbl <- left_join(
    x = output_tbl,
    y = lenient_hs07_to_hs96_tbl,
    by = c("lenient_hs07" = "hs07_code")
  ) 
  
  if (return == "summary") {
	  	
    ##################################################################
    ##           4: Evaluate loss of output, observations           ##
    ##################################################################
    
    # How many observations are lost when converting products? --------
    
   # Add state name and id to output tbl
	  plant_tbl <- plant_data %>% 
		  select(year, dsl, new_state_code, state_name, multiplier)

	  output_tbl <- output_tbl %>% 
		  left_join(plant_tbl, by = c("year", "dsl"))
    # Across years
    year_obs_sum_tbl <- output_tbl %>%
      group_by(year) %>%
      summarize(
        total_obs = n(),
        strict_cpc2_obs = sum(!is.na(strict_cpc2)),
        lenient_cpc2_obs = sum(!is.na(lenient_cpc2)),
        strict_hs07_obs = sum(!is.na(strict_hs07)),
        lenient_hs07_obs = sum(!is.na(lenient_hs07)),
        strict_hs96_obs = sum(!is.na(strict_hs96)),
        lenient_hs96_obs = sum(!is.na(lenient_hs96))
      ) %>%
      mutate(
        strict_cpc2_obs_change = (strict_cpc2_obs - total_obs) / total_obs,
        lenient_cpc2_obs_change = (lenient_cpc2_obs - total_obs) / total_obs,
        strict_hs07_obs_change = (strict_hs07_obs - total_obs) / total_obs,
        lenient_hs07_obs_change = (lenient_hs07_obs - total_obs) / total_obs,
        strict_hs96_obs_change = (strict_hs96_obs - total_obs) / total_obs,
        lenient_hs96_obs_change = (lenient_hs96_obs - total_obs) / total_obs
      )
    
    # Across states
    state_obs_sum_tbl <- output_tbl %>%
      group_by(state_name) %>%
      summarize(
        total_obs = n(),
        strict_cpc2_obs = sum(!is.na(strict_cpc2)),
        lenient_cpc2_obs = sum(!is.na(lenient_cpc2)),
        strict_hs07_obs = sum(!is.na(strict_hs07)),
        lenient_hs07_obs = sum(!is.na(lenient_hs07)),
        strict_hs96_obs = sum(!is.na(strict_hs96)),
        lenient_hs96_obs = sum(!is.na(lenient_hs96))
      ) %>%
      mutate(
        strict_cpc2_change = (strict_cpc2_obs - total_obs) / total_obs,
        lenient_cpc2_change = (lenient_cpc2_obs - total_obs) / total_obs,
        strict_hs07_change = (strict_hs07_obs - total_obs) / total_obs,
        lenient_hs07_change = (lenient_hs07_obs - total_obs) / total_obs,
        strict_hs96_obs_change = (strict_hs96_obs - total_obs) / total_obs,
        lenient_hs96_obs_change = (lenient_hs96_obs - total_obs) / total_obs
      )
    
    
    # How much output is lost when converting products? ----------------
    
    # define output fun
    get_output <- function(tbl, classification, group) {
      # some eval magic; https://dplyr.tidyverse.org/articles/programming.html
      group_var <- enquo(group)
      class_var <- enquo(classification)
      
      output_name <- paste0(quo_name(class_var), "_output")
      change_name <- paste0(quo_name(class_var), "_change")
      
      output_name2 <- as.name(output_name)
      
      output_tbl <- tbl %>%
        filter(!is.na(!! class_var)) %>%
        group_by(!! group_var) %>%
        summarize(
          !! output_name := sum(qty_sold * net_sale_val * multiplier) 
        )
      
      total_tbl <- tbl %>%
        group_by(!! group_var) %>%
        summarize(
          total_output = sum(qty_sold * net_sale_val * multiplier) 
        )
      
      # Remember: !! tells function that I do the qouting for a given input, 
      # sym() turns the string to a symbol (not-a-string) so it can read it as the variable.
      return_tbl <- left_join(total_tbl, output_tbl) %>%
        mutate(
          !! change_name := (!! rlang::sym(output_name) - !! rlang::sym("total_output")) / !! rlang::sym("total_output")
        )
      
      return(return_tbl)
    }
    
    # BY YEAR: 
    
    # strict CPCP-2
    year_output_strict_cpc2 <- output_tbl %>%
      get_output(
        group = year,
        classification = strict_cpc2
      )
    
    # lenient CPCP-2
    year_output_lenient_cpc2 <- output_tbl %>%
      get_output(
        group = year,
        classification = lenient_cpc2
      )
    
    # strict HS07
    year_output_strict_hs07 <- output_tbl %>%
      get_output(
        group = year,
        classification = strict_hs07
      )
    
    # lenient HS07
    year_output_lenient_hs07 <- output_tbl %>%
      get_output(
        group = year,
        classification = lenient_hs07
      )
    
    # strict HS96
    year_output_strict_hs96 <- output_tbl %>%
      get_output(
        group = year,
        classification = strict_hs96
      )
    
    # lenient HS96
    year_output_lenient_hs96 <- output_tbl %>%
      get_output(
        group = year,
        classification = lenient_hs96
      )

    # Join to summary table
    year_output_sum_tbl <- year_output_strict_cpc2 %>%
      left_join(year_output_lenient_cpc2) %>%
      left_join(year_output_strict_hs07) %>%
      left_join(year_output_lenient_hs07) %>%
      left_join(year_output_strict_hs96) %>%
      left_join(year_output_lenient_hs96)
    
    # BY STATE: 
    
    # strict CPCP-2
    state_output_strict_cpc2 <- output_tbl %>%
      get_output(
        group = state_name,
        classification = strict_cpc2
      )
    
    # lenient CPCP-2
    state_output_lenient_cpc2 <- output_tbl %>%
      get_output(
        group = state_name,
        classification = lenient_cpc2
      )
    
    # strict HS07
    state_output_strict_hs07 <- output_tbl %>%
      get_output(
        group = state_name,
        classification = strict_hs07
      )
    
    # lenient HS07
    state_output_lenient_hs07 <- output_tbl %>%
      get_output(
        group = state_name,
        classification = lenient_hs07
      )
    
    # strict HS96
    state_output_strict_hs96 <- output_tbl %>%
      get_output(
        group = state_name,
        classification = strict_hs96
      )
    
    # lenient HS96
    state_output_lenient_hs96 <- output_tbl %>%
      get_output(
        group = state_name,
        classification = lenient_hs96
      )
    
    # Join to summary table
    state_output_sum_tbl <- state_output_strict_cpc2 %>%
      left_join(state_output_lenient_cpc2) %>%
      left_join(state_output_strict_hs07) %>%
      left_join(state_output_lenient_hs07) %>%
      left_join(state_output_strict_hs96) %>%
      left_join(state_output_lenient_hs96)
    
    # Return summary list 
    
    summary_list <- list(
      "year_obs_sum_tbl" = year_obs_sum_tbl,
      "state_obs_sum_tbl" = state_obs_sum_tbl,
      "year_output_sum_tbl" = year_output_sum_tbl,
      "state_output_sum_tbl" = state_output_sum_tbl
    )
    
    return(summary_list)
    
  } else if (return == "verbose") {
    
    # I create two versions of the output_tbl. output_tbl_verbose keeps all of the
    # intermediate variables for easier bug finding. output_tbl_cleaned only keeps the hs96 columns.
    output_tbl_verbose <- output_tbl
    
    return(output_tbl_verbose) 
    
  } else if (return == "main") {
    
    output_tbl_cleaned <- output_tbl %>%
      select(
        all_of(
          c(
            names(output_tbl)[!str_detect(string = names(output_tbl), pattern = "^(lenient|strict)|_desc|blk")],
            "lenient_hs96", "strict_hs96", "hs96_desc.y"
          )
        )
      ) %>%
      rename(hs96_desc = hs96_desc.y)
    
    return(output_tbl_cleaned)
    
  }
  # END 
}
