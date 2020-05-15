#' This functions cleans the IHDS 2005, 2012 HH samples (basically just selects
#' variables) and joins them based on a linking table. 
#'
#' @param ihds05hh_path the filename to the HH block of the ihds 2005 survey.
#' @param ihds12hh_path the filename to the HH block of the ihds 2012 survey.
#'
#' @return A tibble with panel data on the electricity access of the 
#'  households that were surveyed in both 2005 and 2012.
#' @export

clean_ihds = function(ihds05hh_path, ihds12hh_path) {
  
  # Prepare HH-part of 2005 data ----------------------------------------
  
  ihds05hh_raw <- read_dta(ihds05hh_path)
  
  ihds05hh_tbl <- ihds05hh_raw %>%
    clean_names(case = "snake") %>%
    select(
      stateid,
      distid,
      psuid,
      hhid,
      #                idhh,
      hhsplitid,
      distname,
      dist01, 
      sweight_05 = sweight,
      electricity_access_05 = fu1,
      hours_electricity_05 = fu1a
    ) %>%
    mutate(
      state_name = haven::as_factor(stateid) %>% as.character(),
      stateid = as.numeric(stateid),
      stateid = as.numeric(stateid),
      distid = as.numeric(distid),
      psuid = as.numeric(psuid),
      hhid = as.numeric(hhid),
      dummy2005 = 1
    )  %>%
    mutate(
      state_name = case_when(
        state_name == "Uttaranchal" ~ "Uttarakhand", # sp changed in 2011
        state_name == "Chhatishgarh" ~ "Chhattisgarh", # sp?
        TRUE ~ as.character(state_name)
      )
    )
  
  
  # Prepare HH-part of 2012 data ----------------------------------------
  
  ihds12hh_raw <- read_dta(ihds12hh_path)
  
  # Variables:
  # STATEID = state
  # DISTID = district
  # PSUID = village code
  # IDHH = unique HH id
  # wT = sample wieght
  # DISTRICT = 2001 census ID: state + district
  # FU1 = electricity access to HH
  # FU1A = electricity access number of hours per day
  
  ihds12hh_tbl <- ihds12hh_raw %>%
    clean_names(case = "snake") %>%
    select(
      stateid,
      distid,
      psuid,
      hhid,
      hhsplitid,
      #                idhh,
      wt_12 = wt,
      district,
      electricity_access_12 = fu1,
      hours_electricity_12 = fu1a
    ) %>%
    mutate(
      state_name = haven::as_factor(stateid) %>% as.character() %>%
        str_replace_all(
          pattern = "[0-9]", # replace numbers w. nothing
          replace = ""
        ) %>%
        str_trim(), # remove trailing space
      stateid = as.numeric(stateid),
      distid = as.numeric(distid),
      psuid = as.numeric(psuid),
      hhid = as.numeric(hhid),
      hhsplitid = as.numeric(hhsplitid),
      dummy2012 = 1
    )
  
  # Link surveys together ----------------------------------------------
  
  link_file <- read_csv("/home/post/drake_project/data/external/ihds/linkhh.txt") %>%
    clean_names(case = "snake")
  
  # Linking is done per https://www.ihds.umd.edu/guide-merging-files
  round2hh_plus <- left_join(
    x = ihds12hh_tbl, 
    y = link_file,
    by = c("stateid", "distid", "psuid", "hhid", "hhsplitid")
  )
  
  ihds_joined_tbl <- ihds05hh_tbl %>%
    rename(
      hhid2005 = hhid,
      hhsplitid2005 = hhsplitid
    ) %>%
    full_join(round2hh_plus) %>%
    filter(dummy2005 == 1 & dummy2012 == 1)
  # now has the 40018 households (as in the helper doc) that were surveyed both rounds.
  
  # Fix state names to be the same as in the ASI data ------------------
  
  ihds_joined_tbl <- ihds_joined_tbl %>%
    mutate(
      state_name = case_when(
        state_name == "Jammu & Kashmir" ~ "Jammu and Kashmir",
        state_name == "Chandigarh" ~ "Chandigarh(U.T.)",
        state_name == "Uttarakhand" ~ "Uttrakhand",
        state_name == "Orissa" ~ "Odisha",
        state_name == "Daman & Diu" ~ "Daman and Diu",
        state_name == "Dadra+Nagar Haveli" ~ "Dadra and Nagar Haveli",
        state_name == "Pondicherry" ~ "Puducherry",
        TRUE ~ state_name
      )
    )
  
  # Clean survey variables up ------------------------------------------
  
  # Which weighting variable? From "https://ihds.umd.edu/faq-page#n227":
  # Q: Which weights should be used with which data?
  # A:
  # 
  # Weights are a complex issue. Our recommendation:
  # If doing individual cross sectional analyses, then use the appropriate individual survey weight (WT for 2012 and SWEIGHT for 2005).
  # If doing a panel analysis, best approximation is to use the weights for 2005 rather than 2012.
  
  # Use the SWEIGHT variable from the 2005 sample: from https://www.icpsr.umich.edu/web/pages/DSDR/idhs-data-guide.html#weight:
  # The IHDS sample is a complex combination of rural and urban samples (see the Sample section). To calculate population estimates for India or for individual states, SWEIGHT is needed as a design weight in all analyses. If doing individual cross sectional analyses, then use the appropriate individual survey weight, SWEIGHT.
  ihds_joined_tbl <- ihds_joined_tbl %>%
    select(
      stateid,
      state_name,
      distid,
      psuid,
      hhid = hhid2005,
      dist01, # district code based on 2001 census
      sweight = sweight_05,
      electricity_access_05,
      hours_electricity_05,
      electricity_access_12,
      hours_electricity_12,
    )
  
  
  return(ihds_joined_tbl)
  
  # Citations: from: https://ihds.umd.edu/faq-page#n180
  # IHDS I Citation:
  # 
  # Desai, Sonalde, Reeve Vanneman, and National Council of Applied Economic Research, New Delhi. India Human Development Survey (IHDS), 2005. ICPSR22626-v8. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2010-06-29. http://doi.org/10.3886/ICPSR22626.v8
  # 
  # IHDS II Citation:
  # 
  # Desai, Sonalde, and Reeve Vanneman and National Council of Applied Economic Research, New Delhi. India Human Development Survey-II (IHDS-II), 2011-12. ICPSR36151-v2. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2015-07-31. http://doi.org/10.3886/ICPSR36151.v2
  
}
