#' Creates plots of different disruptions to production agains GDP/cap and 
#' country fitness.
#'
#' @param es_ls list of cleaned enterprise survey blocks
#' @param fit_tbl data frame with country-year observations of fitness values
#' @param gdp_cap_tbl data frame with country-year observations of GDP/cap
#' @param write_to a string that gives the location the finished plot should
#' be saved to. The plot is saved as .pdf.
#' @export

plot_disruptions <- function(es_ls, fit, gdp_cap, write_to) {
  
  # Fitness data (already cleaned)
  fit_tbl <- fit %>%
    filter(type == "country" & iteration == max(iteration)) %>%
    rename(iso3c = id) %>%
    select(
      year, 
      iso3c,
      fitness = val
    ) %>%
    mutate(ln_fitness = log(fitness))
  
  
  # GDP per capita data (already cleaned)
  gdp_cap_tbl <- gdp_cap %>%
    select(
      iso3c = country_code,
      year,
      gdp_cap
    ) %>%
    mutate(ln_gdp_cap = log(gdp_cap))
  
  # Add GDP/cap and fitness to each ES block ------------------
  # Version with NA values.
  add_vars <- function(df, fit = fit_tbl, gdp_cap = gdp_cap_tbl) {
    df <- df %>% 
      drop_na() %>% # drop NA values
      group_by(iso3c) %>% 
      filter(year < max(fit$year)) %>%
      filter(year == max(year)) %>%
      left_join(fit, by = c("iso3c", "year")) %>%
      left_join(gdp_cap, by = c("iso3c", "year")) %>%
      ungroup() %>%
      mutate(fit_na = ifelse(is.na(ln_fitness), 1, 0) %>% as.character())
    
    return(df)
  }
  
  # version w/o NAs
  # add_vars_no_na <- function(df, fit = fit_tbl, gdp_cap = gdp_cap_tbl) {
  #         df <- df %>% 
  #                 left_join(fit, by = c("iso3c", "year")) %>%
  #                 left_join(gdp_cap, by = c("iso3c", "year")) %>%
  #                 drop_na() %>% # drop NA values
  #                 group_by(iso3c) %>% 
  #                         filter(year == max(year)) %>%
  #                         ungroup()
  # 
  #                 return(df)
  # }
  
  # Apply function from above
  es_ls <- map(es_ls, add_vars)
  # es_ls_no_na <- map(es_ls, add_vars_no_na)
  
  # Construct plots --------------------------------------------
  
  # Create plot template
  make_plot <- function(
    input_df,
    x.var = "ln_gdp_cap",
    y.var,
    x.text = "GDP/cap, ln", 
    y.text = " ",
    point.size = 2,
    color.var = "ln_fitness",
    title.text,
    xlimits = c(6.5, 10),
    gradient_cols = c("#a50f15", "#ffffbf", "#253494"),
    legend.text = "Fitness, ln"
  ) {
    
    
    if (!is.character(y.var)) {
      stop("Error: no `y.var` supplied.")
    }
    
    out_plot <- ggscatter(
      data =input_df,
      x = x.var,
      y = y.var,
      xlab = x.text,
      ylab = y.text,
      size = point.size,
      color = color.var,
      add = "reg.line",
      shape = "fit_na",
      add.params = list(color = "black"),
    ) %>% 
      ggpar(
        legend.title = legend.text,
        font.tickslab = 10,
        font.x = 10,
        font.y = 8,
        font.title = 10,
        title = title.text
      ) +
      # gradient_color(gradient_cols) +
      scale_color_gradientn(colours = gradient_cols, na.value = "gray") +
      scale_shape_manual(values = c(20, 4)) +
      guides(shape = FALSE, color = guide_colorbar(label = FALSE, ticks = FALSE)) +
      scale_x_continuous(limits = xlimits)
    
    return(out_plot)
    
  }
  
  shipment_plot <- make_plot(
    input_df = es_ls$infrastructure,
    y.var = "share_of_products_broken_in_shipment",
    title.text = "% products spoiled, \n broken in shipment"
  )
  
  electricity_plot <- make_plot(
    input_df = es_ls$infrastructure,
    y.var = "annual_share_of_sales_lost_to_outages",
    title.text = "% yearly sales lost to \n electricity shortages"
  )
  
  water_plot <- make_plot(
    input_df = es_ls$infrastructure,
    y.var = "monthly_water_shortages",
    title.text = "# of monthly water \n shortages"
  )
  
  theft_plot <- make_plot(
    input_df = es_ls$crime,
    y.var = "share_products_stolen_during_shipment",
    title.text = "% product value stolen \n during shipment"
  )
  
  customs_plot <- make_plot(
    input_df = es_ls$trade,
    y.var = "days_to_clear_imports_from_customs",
    title.text = "# of days to clear imports \n from customs"
  )
  
  gifts_plot <- make_plot(
    input_df = es_ls$corruption,
    y.var = "perc_firms_gifts_to_get_things_done",
    title.text = "% firms expected to bribe \n officials to 'get things done'"
  )
  
  return_plot <- ggarrange(
    shipment_plot,
    electricity_plot,
    water_plot,
    theft_plot,
    customs_plot,
    gifts_plot,
    ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom"
  ) 
  
  ggsave(plot = return_plot, filename = here(write_to))
  
  return(return_plot)
  
}

#' Creates a plot of country fitness (ln) vs GDP/cap (ln)
#' 
#' @param fit data frame containing the country-year fitness observations.
#' @param gdp_cap data frame containing the county-year GDP/cap observations.
#' @param ref_year numeric: the year to plot data from
#' @param resource_rents data frame of country-year observations of income from 
#' resource rents as share of GDP.
#' @param write_to a string that gives the location the finished plot should
#' @return a scatter plot of fitness vs GDP/cap value
#' @export

plot_gdp_cap_fitness <- function(fit, gdp_cap, resource_rents, ref_year, write_to) {
  
  fit_tbl <- fit %>% 
    filter(iteration == max(iteration) & metric == "fitness") %>%
    rename(fitness = val, country_code = id)
  
  res_rent_tbl <- resource_rents %>%
    select(-country_name)
  
  gdp_cap_tbl <- gdp_cap %>%
    select(-country_name)
  
  # Join tables ------------------------------------------------
  joined_tbl <- fit_tbl %>%
    left_join(gdp_cap_tbl, by = c("country_code", "year")) %>%
    left_join(res_rent_tbl, by = c("country_code", "year")) %>%
    filter(year == ref_year) %>%
    mutate(
      res_10_dummy = ifelse(perc_rent_of_gdp >= 10, "above 10%", "below 10%"),
      ln_gdp_cap = log(gdp_cap),
      stand_fitness = (fitness - mean(fitness)) / sd(fitness),
      ln_fitness = log(fitness)
    )
  
  
  # Create plot ------------------------------------------------
  
  
  colors <- c("#a50f15", "#253494")
  
  return_plot <- ggscatter(
    joined_tbl %>% filter(!is.na(gdp_cap)),
    x = "ln_fitness",
    y = "ln_gdp_cap",
    xlab = "fitness, ln",
    ylab = "GDP/cap, ln",
    add = "reg.line",
    color = "res_10_dummy",
    shape = "res_10_dummy",
    legend.title = "Resource rents as % of GDP",
    font.legend = 17,
    font.tickslab = 12
  ) + 
    scale_color_manual(values = colors) +
    scale_shape_manual(values = c(20, 4)) +
    stat_regline_equation(
      aes(
        color = res_10_dummy,
        label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")
      ), 
      label.x = c(-4, -1),
      label.y = c(10, 7.5),
      size = 5
    )
  
  ggsave(plot = return_plot, filename = here(write_to))
  
  return(return_plot)
  
}

#' Creates plot of the mean ln(GDP/cap) of the five richest and poorest
#' countries that export a product with RpcA >= 1 against the products'
#' ln(complexity).
#'
#' @param complexity_tbl data frame with product-year observations of product
#' @param ref_year numeric: year to plot
#' complexity.
#' @param gdp_cap_tbl data frame with country-year observations of GDP/cap
#' @param comp_adv_tbl data frame containing country-year-product observations of 
#' RpcA.
#' @param write_to a string that gives the location the finished plot should
#' be saved to. The plot is saved as .pdf.
#' @return the plot that is also saved
#' @export

plot_richest_poorest_exporters <- function(complexity, ref_year, gdp_cap, rpca, write_to) {
  
  # Prepare data ----------------------------------------------
  
  rpca_tbl <- rpca %>% 
    filter(year == ref_year) %>%
    mutate(rpca_bin = ifelse(rca_cap >= 1, 1, 0)) %>%
    select(country_code, year, hs_product_code, export_value, rpca_bin)
  
  complexity_tbl <- complexity %>%
    filter(metric == "complexity") %>%
    filter(year == ref_year & iteration == max(iteration)) %>%
    rename(hs_product_code = id, complexity = val) %>%
    select(-c(type, metric, iteration))
  
  gdp_cap_tbl <- gdp_cap %>%
    filter(year == ref_year)
  
  joined_tbl <- rpca_tbl %>% 
    left_join(complexity_tbl, by = c("year", "hs_product_code")) %>%
    left_join(gdp_cap_tbl, by = c("year", "country_code")) %>%
    mutate(
      ln_complexity = log(complexity),
      exporter_gdp_cap = ifelse(rpca_bin == 1, gdp_cap, NA),
      ln_exporter_gdp_cap = log(exporter_gdp_cap)
    )
  
  # Remove products with less then 10 exporters
  too_few_exporters_list <- joined_tbl %>%
    group_by(hs_product_code) %>%
    summarize(
      number_exporters = sum(rpca_bin)
    ) %>%
    filter(
      number_exporters < 10
    )
  
  joined_tbl <- joined_tbl %>%
    filter(!(hs_product_code %in% too_few_exporters_list$hs_product_code))
  
  # Find the five poorest and richest richest exporters ----------------
  
  low_rank_five_and_below <- joined_tbl %>%
    group_by(hs_product_code) %>%
    mutate(
      rank = row_number(exporter_gdp_cap) # lower is lower in rank, 1 = lowest gdp
    ) %>%
    filter(rank < 6)
  
  high_rank_five_and_below <- joined_tbl %>%
    group_by(hs_product_code) %>%
    mutate(
      rank = row_number(desc(exporter_gdp_cap)) # lower is lower in rank, 1 = lowest gdp
    ) %>%
    filter(
      rank < 6
    )
  
  # Find the mean ln(GDP/cap) of the two groups (per product) ----------
  mean_five_lowest <- low_rank_five_and_below %>%
    group_by(hs_product_code) %>%
    summarise(
      ln_complexity = mean(ln_complexity), # doesn't matter, all values for a product are the same 
      mean_ln_gdp_lowest = mean(ln_exporter_gdp_cap) 
    )
  
  mean_five_highest <- high_rank_five_and_below %>%
    group_by(hs_product_code) %>%
    summarise(
      ln_complexity = mean(ln_complexity), # doesn't matter, all values for a product are the same 
      mean_ln_gdp_highest = mean(ln_exporter_gdp_cap) 
    )
  
  # Join the mean-value groups to plotting data set --------------------
  
  mean_tbl <- left_join(mean_five_highest, mean_five_lowest, by = c("hs_product_code", "ln_complexity")) %>%
    rename(
      mean_5_high = mean_ln_gdp_highest, 
      mean_5_low = mean_ln_gdp_lowest
    ) %>%
    gather(
      -c(hs_product_code, ln_complexity),
      key = group,
      value = mean_ln_gdp) %>%
    mutate(
      group = case_when(
        group == "mean_5_high" ~ "richest five exporters", 
        group == "mean_5_low" ~ "poorest five exporters",
        TRUE ~ NA_character_
      )
    )
  
  # Create plot -------------------------------------------------------
  
  out_plot <- ggscatter(
    mean_tbl,
    x = "ln_complexity",
    y = "mean_ln_gdp",
    xlab = "Product complexity, ln",
    ylab = "GDP/cap, ln",
    color = "group",
    shape = "group",
    add = "reg.line",
    add.params = list(size = 0.5),
    font.legend = 17,
    font.tickslab = 12,
    legend.title = "Average ln(GPD/cap) of:",
    font.x = 12,
    font.y = 12
  ) + 
    scale_shape_manual(values = c(4, 20)) +
    scale_color_manual(values = c("#253494", "#bd0026")) +
    guides(linetype = FALSE)
  
  ggsave(
    plot = out_plot,
    filename = here(write_to)
  )
  # END 
  return(out_plot)
}

#' Function that creates and saves the convergence plots for the fitness
#' algorithm.
#' 
#' @param fit_tbl the data frame containing fitness and complexity values
#' @param year year to plot
#' @param its number of iterations to plot
#' @param write_to string that gives the destination of final
#' @param gradient_cols Vector of length 3 with colors to make gradient (hex code)
#' @export

# TODO: Make nicer.

plot_fitness_conv <- function(fit_tbl, ref_year = 2010, its = 40, write_to, gradient_cols = c("#a50f15", "#ffffbf", "#253494")) {

fit_tbl <- fit_tbl %>% 
	filter(metric == "fitness") %>%
	filter(year == ref_year) %>%
	filter(iteration <=its) %>%
	mutate(ln_fit = log(val))

fit_plot <- ggplot(fit_tbl, aes(x = iteration, y = ln_fit, group = id, color = val)) +
geom_line() +
theme_pubr() +
scale_color_gradientn(colours = gradient_cols, na.value = "gray") +
labs(colour = "Fitness", y = "Fitness, ln", x = "iteration") +
guides(color = FALSE)

ggsave(plot = fit_plot, filename = here(write_to))
return(fit_plot)

}

#' Function that creates and saves the convergence plots for the complexity
#' algorithm.
#' 
#' @param fit_tbl the data frame containing fitness and complexity values
#' @param year year to plot
#' @param its number of iterations to plot
#' @param write_to string that gives the destination of final
#' @param gradient_cols Vector of length 3 with colors to make gradient (hex code)
#' @export

plot_complexity_conv <- function(fit_tbl, ref_year = 2010, its = 40, write_to, gradient_cols = c("#a50f15", "#ffffbf", "#253494")) {

comp_tbl <- fit_tbl %>% 
	filter(metric == "complexity") %>%
	filter(year == ref_year) %>%
	filter(iteration <=its) %>%
	mutate(ln_comp = log(val))

comp_plot <- ggplot(comp_tbl, aes(x = iteration, y = ln_comp, group = id, color = val)) +
geom_line() +
theme_pubr() +
scale_color_gradientn(colours = gradient_cols, na.value = "gray") +
labs(colour = "Complexity", y = "Complexity, ln", x = "iteration") +
guides(color = FALSE)

ggsave(plot = comp_plot, filename = here(write_to))

return(comp_plot)

}
