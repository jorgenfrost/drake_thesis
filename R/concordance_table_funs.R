#' This function creates a concordance table between ASICC and CPC-2 product
#' classifications. Thanks to Tejaswi Velayudhan for providing the raw
#' pdf-to-excel version of the ASICC-NPCMS concordance pdf from MOSPI. The 
#' finished table has both strict and lenient matches. Strict matches only
#' includes matches the are non-partial. Lenient matches are just the first 
#' of partial matches.
#' 
#' @param asicc_to_cpc2_raw_path character: path to data frame with the raw conversion from the
#' pdf table on MOSPI's homepage.
#' @param cpc2_table_path character: path to the table with all CPC-2 codes and names. From UNSD.
#' @return ASICC to CPC-2 concordance table with both strict and lenient matches.
#' 
#' @export

get_asicc_cpc2_concordance <- function(
  asicc_to_cpc2_raw_path = here::here("data/external/concord_tables/product_codes/asicc_to_npcms11_raw.csv"),
  cpc2_table_path = here::here("data/external/concord_tables/product_codes/CPC_Ver_2_english_structure.txt")
) {
  
  # The following creates a concordance table between ASICC and CPC-2
  # product codes. All sourced tables with codes and names are from UNSTAT.
  # Final table has all the CPC-2 codes with corresponding HS07 codes. If 
  # a CPC-2 code has more than one HS07 code matching it, a "partial" dummy 
  # takes the value 1. I also add CPC and HS names.
  
  # Read tables -------------------------------------------------------
  
  # Raw concordance table (converted from PDF, supplied by Teju)
  asicc_to_cpc2_raw <-read_csv(
    file = asicc_to_cpc2_raw_path
  ) %>%
    clean_names(case = "snake")
  
  # CPC-2 names (from UNSTATS/UNSD)
  cpc2_tbl <- read_csv(cpc2_table_path) %>%
    rename(
      cpc2code = Code, 
      cpc2_desc = Description
    )
  
  # Clean table, add partial, and names  ------------------------------
  
  # Remove all the rows that are "left over" headers from the
  # pdf to csv conversion + NPCMS-2011 that are "invalid"
  asicc_to_cpc2_tbl <- asicc_to_cpc2_raw %>%
    filter(asicc_code != "ASICC CODE") %>%
    filter(npcms_2011 != "Invalid")
  
  # Create partial match dummy column: 
  # partial matches on the side of two asicc codes to NPCMS-11 doesn't matter.
  # partial matches of one asicc code to two npcms_2011 does matter.
  # This means that a pattern like "0219900(p)" is OK, but "0219900(p) + 0219901(p)" is not.
  # All observations with more than one NPCMS-2011 match has more than the 10 characters
  # in "0219900(p)". I exploit this.
  
  asicc_to_cpc2_tbl<- asicc_to_cpc2_tbl %>%
    mutate(
      partial = ifelse(
        test = str_length(npcms_2011) > 10, 
        yes = 1,
        no = 0
      )
    ) 
  
  # Create strict and lenient matches ---------------------------------
  
  #  Create "strict match" - only npcms-11 code for matches, where.
  #  Create "lenient match" column - first match, also for partial.
  asicc_to_cpc2_tbl <- asicc_to_cpc2_tbl  %>%
    mutate(
      strict_npcms_2011 = ifelse(
        test = partial == 0, 
        yes = str_sub(
          string = npcms_2011,
          start = 1,
          end = 7
        ),
        no = NA
      ),
      lenient_npcms_2011 = str_sub(
        string = npcms_2011,
        start = 1,
        end = 7
      )
    )
  
  
  # Create CPC-2 codes from the NPCMS-matches by remove last two digits
  asicc_to_cpc2_tbl <- asicc_to_cpc2_tbl %>%
    mutate(
      strict_cpc2 = str_sub(
        string = strict_npcms_2011,
        start = 1,
        end = 5
      ),
      lenient_cpc2 = str_sub(
        string = lenient_npcms_2011,
        start = 1,
        end = 5
      )
    )
  
  
  # Add names from CPC-2 table
  asicc_to_cpc2_tbl <- left_join(asicc_to_cpc2_tbl, cpc2_tbl, by = c("lenient_cpc2" = "cpc2code")) %>%
    rename(
      asicc_desc = item_description
    )
  
  # Return files -------------------------------------------------------
  
  return(asicc_to_cpc2_tbl)
  
}

#' This function creates a concordance table between CPC-2 and HS07 product
#' classifications. The finished table has both strict and lenient matches.
#' Strict matches only includes matches the are non-partial. Lenient matches
#' are just the first of partial matches.
#' 
#' @param cpc2_to_hs07_path character: path to data frame with the CPC-2 to HS07 concordance
#' table provided by UNSD.
#' @param cpc2_table_path character: path to the table with all CPC-2 codes and names. From UNSD.
#' @param hs07_table_path character: path to the table with all HS07 codes and names. From UNSD.
#' @return CPC-2 to HS07 concordance table with both strict and lenient matches.
#' 
#' @export

get_cpc2_hs07_concordance <- function(
  cpc2_to_hs07_path = here::here("data/external/concord_tables/product_codes/CPCv2_HS07.txt"),
  cpc2_table_path = here::here("data/external/concord_tables/product_codes/CPC_Ver_2_english_structure.txt"),
  hs07_table_path = here::here("data/external/concord_tables/product_codes/HS07CodeandDescription.csv")
) {
  
  # The following creates a concordance table between CPC-2 and HS07
  # product codes. All sourced tables with codes and names are from UNSTAT.
  # Final table has all the CPC-2 codes with corresponding HS07 codes. If 
  # a CPC-2 code has more than one HS07 code matching it, a "partial" dummy 
  # takes the value 1. I also add CPC and HS names.
  
  # Read tables -------------------------------------------------------
  
  # Read concordance table: CPC-2 to HS07 (6 digit)
  cpc2_to_hs07_tbl <- read_csv(
    file = cpc2_to_hs07_path
  ) %>%
    clean_names(case = "snake")
  
  # Read CPC-2 names 
  cpc2_tbl <- read_csv(
    file = cpc2_table_path
  ) %>%
    rename(
      cpc2code = Code, 
      cpc2desc = Description
    )
  
  # Read HS07 names
  hs07_tbl <- read_csv(
    file = hs07_table_path
  ) %>%
    clean_names(case = "snake") %>%
    filter(level == "4") %>%
    select(
      hs07_code = code,
      hs07_desc = description
    )
  
  # Reduce HS 6-digit to 4-digit and check matches ------------------
  
  # create 4 digit version of hs07
  cpc2_to_hs07_tbl <- cpc2_to_hs07_tbl %>%
    mutate(
      hs07code_4 = str_sub(
        string = hs07code,
        start = 1,
        end = 4
      )
    ) %>%
    select(
      cpc2code, 
      hs07code = hs07code_4
    ) 
  
  # check for partial matching to hs07 four digit
  # how many hs07codes for each cpc2code
  partial_tbl <- cpc2_to_hs07_tbl %>%
    group_by(cpc2code) %>%
    summarise(
      hs4_matches = n_distinct(hs07code)
    ) %>%
    mutate(
      partial = ifelse(hs4_matches != 1, 1, 0)
    )
  
  # Add partial column to concordance table
  cpc2_to_hs07_tbl <- left_join(cpc2_to_hs07_tbl, partial_tbl) 
  
  # remove duplicate rows (those that are no longer partially matched
  # after converting from 6- to 4-digit HS codes).
  cpc2_to_hs07_tbl <- cpc2_to_hs07_tbl %>%
    distinct()
  
  # Add strict and lenient matches ----------------------------------
  
  # Create strict match: only codes that a non-partial mappings.
  cpc2_to_hs07_tbl <- cpc2_to_hs07_tbl %>%
    mutate(
      strict_hs07 = ifelse(hs4_matches == 1, hs07code, NA)
    ) 
  
  # Create lenient matches (just first match of possible partial mappings).
  # distinct w. .keep_all keeps first row of non unique values.
  cpc2hs07_lenient <- cpc2_to_hs07_tbl %>%
    distinct(cpc2code, .keep_all = TRUE) %>%
    select(cpc2code, lenient_hs07 = hs07code)
  
  # Add lenient matches column to concordance table
  cpc2_to_hs07_tbl <- left_join(cpc2_to_hs07_tbl, cpc2hs07_lenient)
  
  # Add names -------------------------------------------------------
  
  # Add CPC-2 names
  cpc2_to_hs07_tbl <- left_join(cpc2_to_hs07_tbl, cpc2_tbl)
  
  # Add HS07 names
  cpc2_to_hs07_tbl <- left_join(cpc2_to_hs07_tbl, hs07_tbl, by = c("lenient_hs07" = "hs07_code"))
  
  # Clean up selection of cols and write file -----------------------
  
  cpc2_to_hs07_tbl <- cpc2_to_hs07_tbl %>%
    select(
      cpc2_code = cpc2code,
      cpc2_desc = cpc2desc,
      partial,
      strict_hs07,
      lenient_hs07,
      hs07_desc
    )
  
  # Return concordance -----------------------------------------------
  return(cpc2_to_hs07_tbl) 
  
}

#' This function creates a concordance table between HS07 and HS96 product
#' classifications. The finished table has both strict and lenient matches.
#' Strict matches only includes matches the are non-partial. Lenient matches
#' are just the first of partial matches.
#' 
#' @param hs07_to_hs96_path character: path to data frame with the HS07 to HS96 concordance
#' table provided by UNSD.
#' @param hs07_table_path character: path to the table with all HS07 codes and names. From UNSD.
#' @param hs96_table_path character: path to the table with all HS96 codes and names. From UNSD.
#' @return HS07 to HS96 concordance table with both strict and lenient matches.
#' 
#' @export

get_hs07_hs96_concordance <- function(
  hs07_to_hs96_path = here::here("data/external/concord_tables/product_codes/HS 2007 to HS 1996 Correlation and conversion tables.csv"),
  hs07_table_path = here::here("data/external/concord_tables/product_codes/HS07CodeandDescription.csv"),
  hs96_table_path = here:: here("data/external/concord_tables/product_codes/HS96CodeandDescription.csv")
) {
  
  # Read tables ----------------------------------------------------
  
  # Read original concordance table
  hs07_to_hs96_raw <- read_csv(
    file = hs07_to_hs96_path,
    skip = 1
  ) %>%
    clean_names(case = "snake")
  
  # HS07 names
  hs07_tbl <- read_csv(
    file = 	hs07_table_path
  ) %>%
    clean_names(case = "snake") %>%
    filter(level == 4) %>%
    select(
      hs07_code = code,
      hs07_desc = description
    )
  
  # HS96 names
  hs96_tbl <- read_csv(
    file = hs96_table_path
  ) %>%
    clean_names(case = "snake") %>%
    filter(level == 4) %>%
    select(
      hs96_code = code,
      hs96_desc = description
    )
  
  # Make codes 4 digits
  hs07_to_hs96_tbl <- hs07_to_hs96_raw %>%
    mutate(
      hs07_code = str_sub(
        string = hs_2007,
        start = 1,
        end = 4
      ),
      hs96_code = str_sub(
        string = hs_1996,
        start = 1,
        end = 4
      )
    ) %>%
    select(
      hs07_code,
      hs96_code
    ) %>%
    distinct()
  
  # Create strict mappings
  partial_tbl <- hs07_to_hs96_tbl %>%
    group_by(hs07_code) %>%
    summarize(
      hs96_matches = n_distinct(hs96_code)
    ) 
  
  
  hs07_to_hs96_tbl <- left_join(hs07_to_hs96_tbl, partial_tbl) %>%
    mutate(
      strict_hs96 = ifelse(hs96_matches == 1, hs96_code, NA)
    )
  
  # Create lenient matches (just first match of possible partial mappings).
  # distinct w. .keep_all keeps first row of non unique values.
  lenient_hs07_to_hs96 <- hs07_to_hs96_tbl %>%
    distinct(hs07_code, .keep_all = TRUE) %>%
    select(hs07_code, lenient_hs96 = hs96_code)
  
  # Join and add names
  hs07_to_hs96_tbl <- left_join(hs07_to_hs96_tbl, lenient_hs07_to_hs96) %>%
    left_join(hs07_tbl) %>%
    left_join(hs96_tbl)
  
  
  
  # Write file ------------------------------------------------------
  
  return(hs07_to_hs96_tbl)
  
  # END
}

