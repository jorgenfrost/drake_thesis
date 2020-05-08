#' This function takes a country by product matrix in and calculates country
#' fitness and product complexity based on the Fitness Complexity (FC)
#' algorithm in the 2012 version.
#'
#' @param RCA A country-by-product matrix where elements are RCA values 
#' (preferably binary).
#' @param N the amount of iterations to run the algorithm over.
#' 
#' @export

fitness2012 <- function(RCA, N) {
  # Input:
  # RCA = country by product RCA matrix
  M <- RCA
  # N = number of iterations to run the algorithm
  
  # OUTPUT: tibble containing both complexity and fitness values
  
  #############
  ## PREPARE 
  #############
  
  # Number of countries C
  C <- nrow(M)
  countries <- rownames(M)
  
  # Number of products P
  P <- ncol(M)
  products <- colnames(M)
  
  # First I define the initial conditions
  F0 <- rep(1, C)
  Q0 <- rep(1, P)
  
  F_prev <- F0
  Q_prev <- Q0
  
  # I also create a collection matrix (one for each metric to capture how iterations change the value. 
  # Each row is an iteration, each column is the countrty (for fitness) or the product (for complexity),
  # and elements are fitness or complexity at the given iteration
  collect_F <- matrix(NA, nrow = N, ncol = C, dimnames = list(1:N, countries))
  collect_Q <- matrix(NA, nrow = N, ncol = P, dimnames = list(1:N, products)) 
  
  #############
  ## ALGORITHM
  #############
  
  # Start loop.
  for (n in 1:N) {
    
    collect_F[n, ] <- F_prev
    collect_Q[n, ] <- Q_prev
    
    # First I weight each product in M by its complexity from the previous iteration. 
    # R multiplies vectors into matrices by columns, repeating them end-to-end when the dimensions do not fit. 
    # I get around this issue by first making Q_prev into a matrix, where each row is the Q_prev vector.
    # I then just elementwise multiply the matrices.
    
    # finally I take rowSums, so that FN is the lenght of C and has the sum of the weighted products exported by each country
    Q_prev_mat <- matrix(Q_prev, nrow = C, ncol = P, byrow = TRUE) # byrow makes it so the matrix is filled one row at a time.
    Ftilde <- rowSums(M * Q_prev_mat)
    
    # Same idea, but here the weightings need to be based on countries, so the matrix F_prev_matrix needs to be filled colwise.
    F_prev_mat <- matrix(F_prev, nrow = C, ncol = P, byrow = FALSE)
    Qtilde <- 1 / (colSums(M * (1 / F_prev_mat)))
    
    # I now apply the normalisation by average values
    FN <- Ftilde / mean(Ftilde)
    QN <- Qtilde / mean(Qtilde)
    
    F_prev <- FN
    Q_prev <- QN
    
  }
  
  ## Make collection matrices long
  fitness_long <- collect_F %>%
    as.data.frame() %>%
    rownames_to_column(var = "iteration") %>% 
    gather(-iteration, key = id, value = val) %>%
    as_tibble() %>%
    mutate(
      type = "country",
      metric = "fitness"
    )
  
  complexity_long <- collect_Q %>%
    as.data.frame() %>% 
    rownames_to_column(var = "iteration") %>%
    gather(-iteration, key = id, value = val) %>%
    as_tibble() %>%
    mutate(
      type = "product", 
      metric = "complexity"
    )
  
  ## join
  return_tbl <- rbind(fitness_long, complexity_long) %>%
    select(iteration, id, type, metric, val) %>%
    mutate(
      iteration = as.numeric(iteration) - 1 # because iteration 0 (1 for all vars) is called iteration 1 in the function
    )
  
  ## return it
  return(return_tbl)
  
}
