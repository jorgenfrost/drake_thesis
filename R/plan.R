the_plan <-
  drake::drake_plan(

    ## Plan targets in here.

    ## Complexity: preferred
    pop_tbl = clean_pop_data(min_year = 1999),
    hs96_export_raw_tbl = vroom::vroom(
      file = here::here("data/external/international_trade/year_origin_hs96_4.tsv"),
      delim = "\t"
    ),
    hs96_export_cleaned_tbl = clean_export_data(
      export_data = hs96_export_raw_tbl,
      pop_data = pop_tbl
    ),
    hs96_rpca_tbl = get_comparative_advantage(
      trade_data = hs96_export_cleaned_tbl,
      pop_data = pop_tbl,
      metric = "RpcA",
      mean_val = FALSE
    ),
    hs96_product_complexity = get_product_complexity(
	    df = hs96_rpca_tbl,
	    its = 50,
	    mean_value = FALSE
	    ),
    asi_nest = clean_asi_blocks(),
    asi_block_ls = merge_blocks(nested_df = asi_nest)
    
    #     asi_base_sample = placeholder_fun(),
    #     asi_plant_complexity = placeholder_fun(),
    #     india_energy_data = placeholder_fun()

    ## TODO: Complexity: robustness (mean values instead of yearly, RCA instead of RpcA)
    #     hs96_mean_comparative_advantage_tbl = get_comparative_advantage(
    #       trade_data = hs96_export_cleaned_tbl,
    #       pop_data = pop_tbl,
    #       mean_val = TRUE
    #     ),
    #     hs96_product_complexity = get_product_complexity(
    #             dat = hs96_comparat
    #             ),
    #     hs96_mean_product_complexity = get_product_complexity(
    #             trade_data = hs96_mean_comparative_advantage_tbl,
    #             pop_data = pop_tbl,
    #             mean_val = TRUE
    #             ),
    # 
    # 
  )
