merge_blocks <- function(nested_df) {
  
  # Bind all years of the respective blocks together and write
  # to file.
  asi_nest <- nested_df
  
  return_list <- list()
  
  for (i in seq_along(unique(asi_nest$block))) {
    current_block <- unique(asi_nest$block)[i]
    
    block_tbl <- asi_nest %>%
      filter(block == current_block) %>%
      select(data) %>%
      unnest(data)
    
    
    return_list[[paste0("asi_block_", current_block, "_cleaned")]] <- block_tbl
  }
  
  return(return_list)
}