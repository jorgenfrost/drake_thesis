library(janitor)
library(haven)
library(drake)
library(tidyverse)


library(countrycode)
pci_tbl <- readd("pci_tbl")

rca <- vroom("/home/post/drake_project/data/external/international_trade/year_origin_hs96_4.tsv", delim = "\t")

# PREPARE
rca <- rca %>% 
	mutate(
export_rca = as.numeric(export_rca),
export_rca_binary = case_when(
is.na(export_rca) ~ 0,
export_rca >= 1 ~ 1,
TRUE ~ 0
)
)

rca <- rca %>% 
mutate(iso3c = str_to_upper(origin)) %>%
filter(year == 2010)

rca_tbl <- rca %>% select(
year, 
country_code = iso3c,
hs_product_code = hs96,
rpca_bin = export_rca_binary) 

pci_tbl <- pci_tbl %>%
	select(year, complexity = pci, hs_product_code = hs96_code) %>%
	filter(year == 2010)

gdp_cap_tbl <- readd("gdp_cap_tbl") %>%
filter(year == 2010)

  joined_tbl <- rca_tbl %>% 
    left_join(pci_tbl, by = c("year", "hs_product_code")) %>%
    left_join(gdp_cap_tbl, by = c("year", "country_code")) %>%
    mutate(
      exporter_gdp_cap = ifelse(rpca_bin == 1, gdp_cap, NA),
      ln_exporter_gdp_cap = log(exporter_gdp_cap)
    ) %>% 
    filter(year == 2010)
 
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
      complexity = mean(complexity), # doesn't matter, all values for a product are the same 
      mean_ln_gdp_lowest = mean(ln_exporter_gdp_cap) 
    )
  
  mean_five_highest <- high_rank_five_and_below %>%
    group_by(hs_product_code) %>%
    summarise(
      complexity = mean(complexity), # doesn't matter, all values for a product are the same 
      mean_ln_gdp_highest = mean(ln_exporter_gdp_cap) 
    )
  
  # Join the mean-value groups to plotting data set --------------------
  
  mean_tbl <- left_join(mean_five_highest, mean_five_lowest, by = c("hs_product_code", "complexity")) %>%
    rename(
      mean_5_high = mean_ln_gdp_highest, 
      mean_5_low = mean_ln_gdp_lowest
    ) %>%
    gather(
      -c(hs_product_code, complexity),
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
    x = "complexity",
    y = "mean_ln_gdp",
    xlab = "Product complexity (PCI)",
    ylab = "GDP/cap, ln",
    color = "group",
    shape = "group",
    add = "reg.line",
    add.params = list(size = 0.5),
    font.legend = 12,
    font.tickslab = 12,
    legend.title = "Average ln(GPD/cap) of:",
    font.x = 12,
    font.y = 12
  ) + 
    scale_shape_manual(values = c(4, 20)) +
    scale_color_manual(values = c("#253494", "#bd0026")) +
    guides(linetype = FALSE)
 
 out_plot
  ggsave(
    plot = out_plot,
    filename = here("doc/figures/richest_poorest_pci.pdf"),
    height = 12,
    width = 20,
    units = "cm")

################

# GPD / ECI plot

  
eci_tbl <- read_csv("/home/post/drake_project/data/external/oec/eci_hs4_hs96_98-18.csv")

library(countrycode)
fit_tbl <- 
    filter(iteration == max(iteration) & metric == "fitness") %>%
    rename(fitness = val, country_code = id)

fit_tbl <- eci_tbl %>% 
	gather(-c(`Country ID`, `Country`), key = year, val = eci) %>%
	clean_names(case = "snake") %>%
	mutate(country_code = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
	rename(fitness = eci) %>%
	mutate(year = as.numeric(year))

eci_tbl
resource_rents <- readd("resource_rents_tbl")
  res_rent_tbl <- resource_rents %>%
    select(-country_name)
 gdp_cap_tbl 
  gdp_cap_tbl <- gdp_cap_tbl %>%
    select(-country_name)
  
  # Join tables ------------------------------------------------
  joined_tbl <- fit_tbl %>%
    left_join(gdp_cap_tbl, by = c("country_code", "year")) %>%
    left_join(res_rent_tbl, by = c("country_code", "year")) %>%
    filter(year == 2010) %>%
    mutate(
      res_10_dummy = ifelse(perc_rent_of_gdp >= 10, "above 10%", "below 10%"),
      ln_gdp_cap = log(gdp_cap),
    )
  
  
  # Create plot ------------------------------------------------
  
  
  colors <- c("#a50f15", "#253494")
  return_plot <- ggscatter(
    joined_tbl %>% filter(!is.na(gdp_cap)),
    x = "fitness",
    y = "ln_gdp_cap",
    xlab = "Economic complexity (ECI)",
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
      size = 7
    )
 return_plot 
  ggsave(plot = return_plot, filename = here(write_to))
  

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
  joined_tbl
  return_plot <- ggscatter(
    joined_tbl %>% filter(!is.na(gdp_cap) & !is.na(fitness)),
    x = "fitness",
    y = "ln_gdp_cap",
    xlab = "Economic complexity (ECI)",
    ylab = "GDP/cap, ln",
    add = "reg.line",
    color = "res_10_dummy",
    shape = "res_10_dummy",
    legend.title = "Resource rents as % of GDP",
    font.legend = 12,
    font.tickslab = 12
  ) + 
    scale_color_manual(values = colors) +
    scale_shape_manual(values = c(20, 4)) +
    stat_regline_equation(
      aes(
        color = res_10_dummy,
        label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")
      )
    )
  
#  ggsave(plot = return_plot, filename = here("doc/figures/eci_cap.pdf"),
	 height = 10,
	 units = "cm"
	 )
  

