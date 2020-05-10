the_plan <-
  drake::drake_plan(
    
    ## Plan targets in here.
    
    ## Complexity: preferred specification
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
    hs96_product_complexity_tbl = get_product_complexity(
      df = hs96_rpca_tbl,
      its = 50,
      mean_value = FALSE
    ),
    
    ## ASI data: preferred specification
    cleaned_blocks_ls = clean_asi_blocks(
      asi_files = list.files(
        here("data/external/asi/extracted"),
        recursive = TRUE,
        pattern = ".dta",
        full.names = TRUE
      )
    ),
    asi_base_sample_ls = filter_asi_blocks(
      block_paths = cleaned_blocks_ls
    ),
    output_hs96_tbl = fix_asi_products(
      output_data = asi_base_sample_ls$output_tbl,
      return = "main"
    ),
    plant_complexity_tbl = get_plant_complexity(
      product_complexity_data = hs96_product_complexity_tbl,
      plant_output_data = output_hs96_tbl
    ),
    plant_input_concentration = placeholder_fun(),
    plant_input_share = placeholder_fun(),
    
    # Energy data: preferred
    energy_shortages_tbl = clean_energy_shortages(
      new_cea_files <- list.files(
        here("data/external/power_supply_india/csv/"),
        pattern = "*.csv",
        full.names = TRUE
	),
	min_year = 1999,
	old_avg_raw = read_csv(
		file_in("./data/external/allcott_energy_data_india/india_energy_data/allcott_EnergyRequirement.csv")
		),
	old_peak_raw = read_csv(
		file_in("./data/external/allcott_energy_data_india/india_energy_data/allcott_PeakDemand.csv")
		),
	avg_2003_data = read_csv(
		file_in("./data/external/allcott_energy_data_india/india_energy_data/energy_requirement_2003_data.csv")
		),
	peak_2003_data = read_csv(
		file_in("./data/external/allcott_energy_data_india/india_energy_data/peak_demand_2003_data.csv")
		)
	),

    # Clean IHDS data
    ihds_tbl = placeholder_fun(),

    # Clean enterprise surveys 
    es14_tbl = placeholder_fun(),
    es??_tbl = placeholder_fun(),

    # Energy reliability
    ihds_energy_reliability = placeholder_fun(),
    es14_energy_reliability = placeholder_fun()
    

    #     asi_base_sample = placeholder_fun(),
    #     asi_plant_complexity = placeholder_fun(),
    #     india_energy_data = placeholder_fun()
    # TODO: Complexity: robustness (mean values instead of yearly, RCA instead of RpcA)
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
