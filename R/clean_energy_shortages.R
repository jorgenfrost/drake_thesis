#' This functions does three things. First, a
#' 
#' 
# This script combines the energy data (estimated demand, estimated peak demand)
# from Alcott et al (1993 - 2010) and the analogous energy data I've collected
# from the CEA website. I also do some cleaning.

# get list of all the .csv files
clean_energy_shortages <- function(
  new_cea_files = list.files(
    here("data/external/power_supply_india/csv/"),
    pattern = "*.csv",
    full.names = TRUE
  ),
  min_year = 1999,
  old_avg_raw = read_csv(here("./data/external/allcott_energy_data_india/india_energy_data/allcott_EnergyRequirement.csv")),
  old_peak_raw = read_csv(here("./data/external/allcott_energy_data_india/india_energy_data/allcott_PeakDemand.csv")),
  avg_2003_data = read_csv(here("./data/external/allcott_energy_data_india/india_energy_data/energy_requirement_2003_data.csv")),
  peak_2003_data = read_csv(here("./data/external/allcott_energy_data_india/india_energy_data/peak_demand_2003_data.csv"))
  
) {
  # old cea data
  old_avg_raw <- bind_rows(old_avg_raw, avg_2003_data)
  old_peak_raw <- bind_rows(old_peak_raw, peak_2003_data)
  
  old_avg_tbl <- old_avg_raw %>%
    clean_names(case = "snake") %>%
    select(year, state, requirement_mu, availability_mu) # exclude any empty cols
  
  old_peak_tbl <- old_peak_raw %>% 
    clean_names(case = "snake") %>%
    select(year, state, peak_demand_mw, peak_met_mw) # exclude any empty cols
  
  # newer cea data
  # separate
  new_avg_files <- new_cea_files[!str_detect(new_cea_files, "peak")]
  new_peak_files <- new_cea_files[str_detect(new_cea_files, "peak")]
  
  # read into df with all years
  new_avg_tbl <- map_dfr(new_avg_files, read_csv)
  new_peak_tbl <- map_dfr(new_peak_files, read_csv)
  
  # Remove aggregated regions ----------------------------------
  
  aggr_region <- c("All India", "Northern Region", "North-Eastern Region", "Eastern Region", "Southern Region", "Western Region")
  
  new_avg_tbl <- new_avg_tbl %>% 
    filter(!state %in% aggr_region)
  new_peak_tbl <- new_peak_tbl %>% 
    filter(!state %in% aggr_region)
  
  # Fix state names --------------------------------------------
  
  fix_states <- function(df, source) {
    if (source == "old") {
      df <- df %>%
        mutate(
          state = case_when(
            state == "Andaman- Nicobar" ~ "A and N Islands", # sp
            state == "AndhraPradesh" ~ "Andhra Pradesh", # sp
            state == "Arunachal PR." ~ "Arunachal Pradesh", # s
            state == "Chandigarh" ~ "Chandigarh(U.T.)",
            state == "Jammu & Kashmir" ~ "Jammu and Kashmir",
            state == "Daman & Diu" ~ "Daman and Diu",
            state == "Dadar Nagar Haveli" ~ "Dadra and Nagar Haveli", # sp
            state == "DVC" ~ "Damodar Valley Corporation", # sp
            state == "W.Bengal + Sikkim" ~ "West Bengal + Sikkim", # sp
            state == "Lakshadweep#" ~ "Lakshadweep", # sp
            state == "Pondicheny" ~ "Puducherry", # should have been Pondicherry, "Puducherry" bc name was changed in 2006.
            state == "Pondicherry" ~ "Puducherry", # name was change ind 2006
            state == "Chattisgarh" ~ "Chhattisgarh", # sp
            state == "Uttaranchal" ~ "Uttrakhand", # name was changed in 2006
            state == "Uttarakhand" ~ "Uttrakhand",
            state == "Orissa" ~ "Odisha", # changed name in 2011
            TRUE ~ as.character(state)
          )
        ) 
    } else if (source == "new") {
      
      df <- df %>%
        mutate(
          state = case_when(
            state == "Chandigarh" ~ "Chandigarh(U.T.)",
            state == "Orissa" ~ "Odisha",
            state == "Jammu & Kashmir" ~ "Jammu and Kashmir",
            state == "Uttarakhand" ~ "Uttrakhand",
            state == "Andaman & Nicobar" ~ "A and N Islands",
            state == "Daman & Diu" ~ "Daman and Diu",
            state == "Dadra & Nagar Haveli" ~ "Dadra and Nagar Haveli",
            TRUE ~ state
          )
        )
    } 
    
    return(df)
  }
  
  new_avg_tbl <- fix_states(df = new_avg_tbl, source = "new")
  new_peak_tbl <- fix_states(df = new_peak_tbl, source = "new")
  old_avg_tbl <- fix_states(df = old_avg_tbl, source = "old")
  old_peak_tbl <- fix_states(df = old_peak_tbl, source = "old")
  
  # Join tables from same source -------------------------------
  
  new_tbl <- full_join(new_avg_tbl, new_peak_tbl) %>%
    mutate(source = "cea")
  
  old_tbl <- full_join(old_avg_tbl, old_peak_tbl) %>%
    mutate(source = "allcott")
  
  # Join tables from across source -----------------------------
  
  # First: remove the years from old data (whichs is typed) that are 
  # also present in new data (whichs is handled digitally).
  
  old_tbl <- old_tbl %>%
    filter(year < min(new_tbl$year)) 
  
  # Join and remove the data before the years I'm interested in. 
  full_tbl <- full_join(new_tbl, old_tbl) %>%
    filter(year >= min_year)
  
  full_tbl <- full_tbl %>%
    mutate(
      avg_shortage = (requirement_mu - availability_mu) / requirement_mu,
      peak_shortage = (peak_demand_mw - peak_met_mw) / peak_demand_mw
    )

  # Add lagged shortage values ---------------------------------


  
  return(full_tbl)
}
