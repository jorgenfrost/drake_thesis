#' This function applies the fitness-complexity algorithm (2012) to trade 
#' data that has been normalized with RCA or RpcA. The result of running
#' the script is a joined data frame of country-fitness and product 
#' complexity values.
#' 
#' @param df data frame: country-product-year RCA/RpcA data, binary 
#' for normal version, actual value for weighted.
#' @param its integer: the number of iterarations to run the algorithm over.
#' @param mean_value logical: whether the input data frame sans over several 
#' years (and contains are "year" column) or is the average values for the 
#' period. Used in robustness checks.
#' @return A tibble with both fitness and complexity values for either a
#' product-year or a country-year pair. Values are provided for each iteration
#' for analysis purposes.
#' @export

get_product_complexity <- function(df, its = 50, mean_value = FALSE) {
  
  # Handle inputs
  if(!is.data.frame(df)) {
    stop("Error: `df` is not a data frame.")
  }
  
  if (!its %% 1 == 0) {
    stop("Error: `its` is not a whole number. It is not possible to have 
         fractions of an iteration.")
  }
  
  if (!is.logical(mean_value)) {
    stop("Error: `mean_value` is not a logical (TRUE or FALSE).")
  }
  
  # Because of laziness, the functions below require that the col is named rca.
  if ("rca_cap" %in% names(df)) {
    df <- df %>%
      rename(
        rca = rca_cap
      )
  }
  
  
  # Define helper function ------------------------------------------
  # The `apply_algorithm` function takes the nested dataframe and turns
  # it into an RCA matrix and applies the fitness algorithm.
  
  
  rca_tbl <- df 
  
  apply_algorithm <- function(input_tbl, iterations) {
    
    rca_mat <- input_tbl %>% 
      spread(key = hs_product_code, value = rca) %>%
      as.data.frame() %>% # to allow rownames (not allowed in tibble)
      column_to_rownames(var = "country_code") %>%
      as.matrix() 
    
    fitness_complexity_tbl <- fitness2012(RCA = rca_mat, N = iterations)
    
    return(fitness_complexity_tbl)
    
  }
  
  # Apply algorithm -------------------------------------------------
  
  if (mean_value == FALSE) {
    # Make rca binary and nest
    rca_nest <- rca_tbl %>%
      select(country_code, year, hs_product_code, rca) %>%
      mutate(
        rca = ifelse(rca >= 1, 1, 0)
      ) %>%
      group_by(year) %>%
      nest()
    
    # Apply algorithm
    rca_fitness_tbl <- rca_nest %>%
      mutate(
        fitness_complexity = map(data, apply_algorithm, iterations = its)
      ) %>%
      select(year, fitness_complexity) %>%
      unnest(cols = fitness_complexity)
  } else if (mean_value == TRUE) {
    ## apply algo to rca (mean)
    rca_fitness_tbl <- rca_tbl %>%
      select(country_code, hs_product_code, rca) %>%
      mutate(
        rca = ifelse(rca >= 1, 1, 0)
      ) %>%
      apply_algorithm(iterations = its)
  }
  
  return(rca_fitness_tbl)
  
  # END
}



