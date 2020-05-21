#' This functions does two things. First, it attaches product complexity values 
#' to the plant-product observations from the ASI data. Second, it computes
#' the plant-complexity values based on the weighted avg method and the 
#' max (or top-line) complexity.
#'
#' @param product_complexity_data data frame: contains the fitness-complexity values. 
#' needs a variable listing if an entry contains a product or a country. 
#' @param plant_output_data data frame: contains the data on output products of the 
#' plants in the ASI. This data frame needs to first have its product fixed. 
#' @return data frame with plant-year observations of the average complexity and 
#' maximum complexity.

get_plant_complexity <- function(product_complexity_data, plant_output_data, mean_vals = FALSE) {
  
  # check inputs
  if (!is.data.frame(product_complexity_data)) {
    stop("Error: `product_complexity_data` is not a data frame.")
  } 
  
  if (!is.data.frame(plant_output_data)) {
    stop ("Error: `plant_output_data` is not a data frame.")
  }
  
  # Prep data data --------------------------------------------
 if(mean_vals == FALSE) { 
  product_complexity_tbl <- product_complexity_data %>%
    filter(type == "product" & iteration == max(iteration)) %>%
    select(
      year,
      hs96_code = id,
      product_complexity = val
    )
 } else if (mean_vals == TRUE) {
  product_complexity_tbl <- product_complexity_data %>%
    filter(type == "product" & iteration == max(iteration)) %>%
    select(
      hs96_code = id,
      product_complexity = val
    )

 }
  # non-final iterations are for debugging.
  
  output_tbl <- plant_output_data
  
  # Define function to add complexity values to products ------------
  
  calculate_plant_complexity <- function(tbl) {
    
    if(!all(c("qty_sold", "net_sale_val", "product_complexity", "year", "dsl") %in% names(tbl))) {
      stop("Error: not all needed vars are present in tbl.")
    }
    
    # Calculate weighted avg complexity of plants ---------------------
    
    # A) Get table of total output values.
    total_output_tbl <- tbl %>%
      group_by(year, dsl) %>%
      summarize(
        total_plant_output = sum(qty_sold * net_sale_val) 
      ) %>%
      ungroup()
    
    # B) Add table total output values to product observations 
    tbl <- left_join(tbl, total_output_tbl) 
    
    # C) Get table of product share of total output times complexity
    plant_avg_complexity_tbl <- tbl %>%
      group_by(year, dsl) %>% 
      summarize(
        w_avg_complexity = sum(((qty_sold * net_sale_val) / total_plant_output) * product_complexity, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Calculate top-line complexity of plants ------------------------
    plant_max_complexity_tbl <- tbl %>%
      group_by(year, dsl) %>%
      summarize(
        max_complexity = max(product_complexity, na.rm = TRUE)
      ) %>%
      ungroup()
    
    plant_complexity_tbl <- full_join(plant_avg_complexity_tbl, plant_max_complexity_tbl)
    
    return(plant_complexity_tbl)
    
  }
  
  # Add product complexity to plant-product obs -----------------------
  ## Since there are no non-na values where strict_hs96 != lenient_hs96, I use
  ## lenient as the joiner. For calculating the plant complexity, I first filter
  ## for NA values in lenient or strict HS96 codes
  if (mean_vals == FALSE) {
  output_tbl <- left_join(output_tbl, product_complexity_tbl, by = c("year" = "year", "lenient_hs96" = "hs96_code"))
  } else if (mean_vals == TRUE) {
  output_tbl <- left_join(output_tbl, product_complexity_tbl, by = c("lenient_hs96" = "hs96_code"))
  }
  # Calculate strict and lenient plant complexity ---------------------
  
  strict_plant_complexity_tbl <- filter(output_tbl, !is.na(strict_hs96)) %>%
    calculate_plant_complexity() %>%
    mutate(
      match = "strict"
    ) 
  lenient_plant_complexity_tbl <- filter(output_tbl, !is.na(lenient_hs96)) %>%
    calculate_plant_complexity() %>%
    mutate(
      match = "lenient"
    )
  
  joined_plant_complexity_tbl <- bind_rows(lenient_plant_complexity_tbl, strict_plant_complexity_tbl)
  
  # Return file ---------------------------------------------------------------
  return(joined_plant_complexity_tbl)
  
}
# END
