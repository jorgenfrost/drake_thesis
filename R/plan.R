the_plan <-
  drake::drake_plan(
    
    ## Plan targets in here.
    
    ## Complexity: preferred specification
    pop_tbl = clean_pop_data(
      min_year = 2000
    ),
    hs96_export_raw_tbl = vroom(
      file = file_in("./data/external/international_trade/year_origin_hs96_4.tsv"),
      delim = "\t"
    ),
    hs96_export_cleaned_tbl = clean_export_data(
      export_data = hs96_export_raw_tbl,
      pop_data = pop_tbl,
      min_year = 2000
    ),
    hs96_rpca_tbl = get_comparative_advantage(
      trade_data = hs96_export_cleaned_tbl,
      pop_data = pop_tbl,
      metric = "RpcA",
      mean_val = FALSE
    ),
    hs96_rca_tbl = get_comparative_advantage(
      trade_data = hs96_export_cleaned_tbl,
      pop_data = pop_tbl,
      metric = "RCA",
      mean_val = FALSE
    ),
    hs96_complexity_tbl = get_product_complexity(
      df = hs96_rpca_tbl,
      its = 50,
      mean_value = FALSE
    ),
    hs96_rca_complexity_tbl = get_product_complexity(
      df = hs96_rca_tbl,
      its = 50,
      mean_value = FALSE
    ),
   pci_tbl = get_pci(),
   eci_tbl = get_eci(),
    
    ## ASI data: preferred specification
    cleaned_blocks_ls = clean_asi_blocks(
      asi_files = list.files(
        here("data/external/asi/extracted"),
        recursive = TRUE,
        pattern = ".dta",
        full.names = TRUE
      )
    ),
    asi_base_sample_ls = get_base_sample(
      block_paths = cleaned_blocks_ls
    ),
    output_hs96_tbl = fix_asi_products(
      output_data = asi_base_sample_ls$output_tbl,
      return = "main"
    ),
    plant_complexity_tbl = get_plant_complexity(
      product_complexity_data = hs96_complexity_tbl,
      plant_output_data = output_hs96_tbl,
      mean_vals = FALSE,
      pci = FALSE
    ),
    plant_complexity_rca_tbl = get_plant_complexity(
      product_complexity_data = hs96_rca_complexity_tbl,
      plant_output_data = output_hs96_tbl,
      mean_vals = FALSE,
      pci = FALSE
    ),
    plant_pci_tbl = get_plant_complexity(
      product_complexity_data = pci_tbl,
      plant_output_data = output_hs96_tbl,
      mean_vals = FALSE,
      pci = TRUE
    ),
    plant_int_input_tbl = get_int_input_share(
      plant_tbl = asi_base_sample_ls$plant_tbl,
      input_tbl = asi_base_sample_ls$input_tbl
    ),
    plant_labour_input_tbl = get_labour_input_share(
      plant_tbl = asi_base_sample_ls$plant_tbl
    ),
    nic_ls = get_industries(
      asi_base_sample_ls$plant_tbl
    ),
    
    # Energy data: preferred
    energy_shortages_tbl = clean_energy_shortages(
      new_cea_files <- list.files(
        here("data/external/power_supply_india/csv/"),
        pattern = "*.csv",
        full.names = TRUE
      ),
      min_year = 1998,
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
    # Clean NSS data
    nss_tbl = clean_nss(),
    
    # Clean RBI data 
    rbi_tbl = clean_rbi(
      pop_density = file_in("./data/external/rbi/changed_manually/density_of_population.xlsx"),
      net_domestic_product = file_in("./data/external/rbi/changed_manually/net_state_domestic_product_constant_prices.xlsx"),
      per_cap_net_domestic_product = file_in("./data/external/rbi/changed_manually/per_capita_net_domestic_product_constant_prices.xlsx"),
      rural_pop = file_in("./data/external/rbi/changed_manually/population_in_rural_area.xlsx"),
      urban_pop = file_in("./data/external/rbi/changed_manually/population_in_urban_area.xlsx"),
      total_pop = file_in("./data/external/rbi/changed_manually/total_population.xlsx"),
      state_codes = file_in("./data/external/concord_tables/rbi_state_codes/rbi_to_asi_state_codes.csv"),
      imp_pop_density = file_in("./data/external/rbi/changed_manually/density_of_population_imputed.xlsx"),
      imp_rural_pop = file_in("./data/external/rbi/changed_manually/population_in_rural_area_imputed.xlsx"),
      imp_urban_pop = file_in("./data/external/rbi/changed_manually/population_in_urban_area_imputed.xlsx"),
      imp_total_pop = file_in("./data/external/rbi/changed_manually/total_population_imputed.xlsx")
    ),
    
    # Clean IHDS data
    ihds_tbl = clean_ihds(
      ihds05hh_path = file_in("./data/external/ihds/ihds_2005/ICPSR_22626/DS0002/22626-0002-Data.dta"),
      ihds12hh_path = file_in("./data/external/ihds/ihds_2012/ICPSR_36151/DS0002/36151-0002-Data.dta")
    ),
    
    # Clean enterprise surveys 
    es14_tbl = clean_es14(
      es14_path = file_in("./data/external/es_india/India-2014-full data-.dta/India-2014-full-data-.dta"),
      state_conc_path = file_in("./data/external/concord_tables/es_state_codes/esi14_to_asi_state_codes.csv")
    ),
    es05_tbl = clean_es05(
      es05_path <- file_in("./data/external/es_india/India-2005--full data-.dta/India-2005--full-data-.dta"),
      state_conc_path <- file_in("./data/external/concord_tables/es_state_codes/es05_to_asi_state_codes.csv")
    ),
    es_int_ls = clean_es_international(
      es_file = file_in("./data/external/es_international/enterprise_surveys_int.xlsx")
    ),
    # Clean analysis sample
    analysis_sample_ls = prepare_analysis_sample(
      rbi_tbl = rbi_tbl,
      nss_tbl = nss_tbl,
      shortage_tbl = energy_shortages_tbl,
      plant_tbl = asi_base_sample_ls$plant_tbl,
      nic98_tbl = nic_ls$nic98_code_tbl,
      nic87_tbl = nic_ls$nic87_code_tbl,
      plant_complexity_tbl = plant_complexity_tbl,
      int_input_tbl = plant_int_input_tbl,
      labor_tbl = plant_labour_input_tbl
    ),
# analysis_sample_mean_complexity_ls = prepare_analysis_sample(
		    #       rbi_tbl = rbi_tbl,
		    #       nss_tbl = nss_tbl,
		    #       shortage_tbl = energy_shortages_tbl,
		    #       plant_tbl = asi_base_sample_ls$plant_tbl,
		    #       nic98_tbl = plant_nic98_tbl %>% distinct(),
		    #       plant_complexity_tbl = plant_mean_complexity_tbl,
		    #       int_input_tbl = plant_int_input_tbl,
		    #       labor_tbl = plant_labour_input_tbl
		    #     
    
    
    # Energy reliability
    #     ihds_energy_reliability = placeholder_fun(), # TODO:
    #     es14_energy_reliability = placeholder_fun(), # TODO:
    
    # GDP data
    gdp_cap_tbl = clean_gdp_cap(
      gdp_cap_path = file_in("./data/external/gpd_cap_international/gdp_cap_ppp_constant_2011_int_dollars.csv"),
      min_year = 2000
    ),
    resource_rents_tbl = clean_resource_rents(
      path = file_in("./data/external/resource_rents/natural_resource_rents.csv"),
      min_year = 2000
    ),
    gdp_tbl = clean_gdp(
      gdp_path = file_in("./data/external/gdp_international/gdp_ppp_constant_2011_int_dollars.csv"),
      min_year = 2000
    ),
    
    
    # Plots
		    match_density_plot = plot_complexity_density_after_match(
									     plant_qp_tbl = plant_complexity_tbl,
									     plant_tbl = analysis_sample_ls$plant_tbl,
									     write_to = file_in("./doc/figures/appendix/appendix_density_product_match.pdf")
									     ),
		    wbes_bar_plot = plot_wbes_bars(
						   es05_in = es05_tbl,
						   es14_in = es14_tbl,
						   write_to = file_out("./doc/figures/background_wbes.pdf")),
    disruption_plot = plot_disruptions(
      es_ls = es_int_ls,
      fit = hs96_complexity_tbl,
      gdp_cap = gdp_cap_tbl,
      write_to = file_out("./doc/figures/introduction_disruption_plot.pdf")
    ),
    gdp_cap_fitness_plot = plot_gdp_cap_fitness(
      fit = hs96_complexity_tbl,
      gdp_cap = gdp_cap_tbl,
      ref_year = 2010,
      resource_rents = resource_rents_tbl,
      write_to = file_out("./doc/figures/introduction_gdp_cap_fitness_plot.pdf")
    ),
    richest_poorest_exporters_plot = plot_richest_poorest_exporters(
      complexity = hs96_complexity_tbl,
      gdp_cap = gdp_cap_tbl,
      rpca = hs96_rpca_tbl,
      ref_year = 2010,
      write_to = file_out("./doc/figures/framework_richest_poorest_exporters.pdf")
    ),
    fitness_convergence_plot = plot_fitness_conv(
      fit_tbl = hs96_complexity_tbl,
      ref_year = 2010,
      its = 40,
      write_to = file_out("./doc/figures/appendix/appendix_fitness_convergence_plot.pdf")
    ),
    complexity_convergence_plot = plot_complexity_conv(
      fit_tbl = hs96_complexity_tbl,
      ref_year = 2010,
      its = 40,
      write_to = file_out("./doc/figures/appendix/appendix_complexity_convergence_plot.pdf")
    ),

		    # ANALYSE
		    plant_entry_analysis = analyse_plant_entry(
					plant_tbl = analysis_sample_ls$plant_tbl,
					pci_table_path = file_out("doc/tables/plant_entry/pci_nonfloat.tex"),
					electricity_table_path = file_out("doc/tables/plant_entry/electricity_nonfloat.tex"),
					sample_histogram_year_out = file_in("doc/figures/analysis_entry_sample_year_histograms.pdf"),
					sample_histogram_state_out = file_in("doc/figures/analysis_entry_sample_state_histograms.pdf")
					),
    
    # Evaluation functions
    export_filter_eval = evaluate_export_filter(
      tbl_clean = hs96_export_cleaned_tbl,
      tbl_raw = hs96_export_raw_tbl,
      pop_tbl = pop_tbl,
      gdp_tbl = gdp_tbl,
      ref_year = 2017
    ),
    #     asi_base_sample = placeholder_fun(),
    #     asi_plant_complexity = placeholder_fun(),
    #     india_energy_data = placeholder_fun()
   ## Complexity: robustness (mean values instead of yearly, RCA instead of RpcA)
    hs96_mean_rpca_tbl = get_comparative_advantage(
      trade_data = hs96_export_cleaned_tbl,
      pop_data = pop_tbl,
      metric = "RpcA",
      mean_val = TRUE
    ),
    hs96_mean_rca_tbl = get_comparative_advantage(
      trade_data = hs96_export_cleaned_tbl,
      pop_data = pop_tbl,
      metric = "RCA",
      mean_val = TRUE
    ),
hs96_mean_complexity_tbl = get_product_complexity(
							 df = hs96_mean_rpca_tbl,
							 its = 50,
							 mean_value = TRUE
							 ),
hs96_mean_rca_complexity_tbl = get_product_complexity(
							 df = hs96_mean_rca_tbl,
							 its = 50,
							 mean_value = TRUE
							 ),
    plant_mean_complexity_tbl = get_plant_complexity(
						     product_complexity_data = hs96_mean_complexity_tbl,
						     plant_output_data = output_hs96_tbl,
						     mean_vals = TRUE
    )

    # 
    #                     product_concordance_eval = evaluate_product_concordance( # has to be non-concorded output tbl
    #                                                                             output_tbl = asi_base_sample_ls$output_tbl,
    #                                                                             plant_tbl = asi_base_sample_ls$plant_tbl
    #                                                                             )
    
  )
