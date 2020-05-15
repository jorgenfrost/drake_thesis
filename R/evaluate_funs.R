#' Evalute the result of cleaning the export data. 
#' 
#' @param cleaned_tbl the resulting data frame after cleaning
#' @param raw_tbl the data before cleaning
#' @param pop data frame with country year observations of popualtion
#' @param gdp_tbl data frame with country year observations of total GDP
#' @param ref_year numeric: year to evaluate on
#' @return a data frame with the change in population, exports, and GDP.
#' @export

evaluate_export_filter <- function(tbl_clean, tbl_raw, pop_tbl, gdp_tbl, ref_year) {

# Total export remaning
raw_export <- tbl_raw[tbl_raw$year == ref_year, ]$export_val %>% as.numeric() %>% sum(na.rm = TRUE)

clean_export <- tbl_clean[tbl_clean$year == ref_year, ]$export_value %>% as.numeric() %>% sum(na.rm = TRUE)

export_change <- (clean_export - raw_export) / raw_export

# Population remaining

raw_pop <- pop_tbl %>%
	filter(year == ref_year) %>%
	filter(country_code %in% str_to_upper(tbl_raw$origin)) %>%
	pull(pop) %>%
	sum()

clean_pop <- pop_tbl %>%
	filter(year == ref_year) %>%
	filter(country_code %in% str_to_upper(tbl_clean$country_code)) %>%
	pull(pop) %>%
	sum()

pop_change <- (clean_pop - raw_pop) / raw_pop

# GDP remaining
raw_gdp <- gdp_tbl %>%
	filter(year == ref_year) %>%
	filter(country_code %in% str_to_upper(tbl_raw$origin)) %>%
	pull(gdp) %>%
	sum(na.rm = TRUE)

clean_gdp <- gdp_tbl %>%
	filter(year == ref_year) %>%
	filter(country_code %in% str_to_upper(tbl_clean$country_code)) %>%
	pull(gdp) %>%
	sum(na.rm = TRUE)

gdp_change <- (clean_gdp - raw_gdp) / raw_gdp

return(tibble(export_change, pop_change, gdp_change))

}

#' Evaluate the loss of observations and output when doing product concordance.
#' 
#' @param output_tbl the pre-product fixing output_tbl
#' @param plant_tbl the plant tbl from the base sample list
#' @param hs96_tbl the output tbl with hs96 products
#'

hs96_tbl <- readd("output_hs96_tbl")
plant_tbl <- readd("asi_base_sample_ls")$plant_tbl

evaluate_product_concordance <- function(output_tbl, plant_tbl, hs96_tbl) {

# Get summary tables -------------------------------------------------

summary_ls <- fix_asi_products(
		output_data = output_tbl,
		plant_data = plant_tbl,
		return = "summary"
		)

# Define table function 

prepare_table <- function(tbl, caption) {
	tbl %>%
	map_dfc(function(df) if (is.numeric(df)) {round(df, digits = 2)} else df ) %>%
	kable(
	      format = "latex",
	      caption = caption,
	      booktabs = T
	      ) %>%
column_spec(5:6, bold = T) 
}

# OBSERVATION LOSS ---------------------------------------------------

year_obs_tbl <- summary_ls$year_obs_sum_tbl %>%
	select(year, total_obs, lenient_hs96_obs, strict_hs96_obs, lenient_hs96_obs_change, strict_hs96_obs_change)

state_obs_tbl <- summary_ls$state_obs_sum_tbl %>%
	select(state_name, total_obs, lenient_hs96_obs, strict_hs96_obs, lenient_hs96_obs_change, strict_hs96_obs_change)

# Create yearly observation tbl
year_obs_kable <- year_obs_tbl %>% 
	rename(
	       `unmatched` = total_obs,
	       `lenient` = lenient_hs96_obs,
	       `strict` = strict_hs96_obs,
	       `lenient change` = lenient_hs96_obs_change,
	       `strict change` = strict_hs96_obs_change
	       ) %>%
prepare_table(caption = "'Lenient' vs 'strict' matching to HS96: observations by year")


# Create state observation tbl

state_obs_kable <- state_obs_tbl %>%
	rename(
	       `state` = state_name,
	       `unmatched` = total_obs,
	       `lenient` = lenient_hs96_obs,
	       `strict` = strict_hs96_obs,
	       `lenient change` = lenient_hs96_obs_change,
	       `strict change` = strict_hs96_obs_change
	       ) %>% 
prepare_table(caption = "'Lenient' vs 'strict' matching to HS96: observations by state")

# OUTPUT LOSS ---------------------------------------------------
year_output_tbl <- summary_ls$year_output_sum_tbl %>%
	select(year, total_output, lenient_hs96_output, strict_hs96_output, lenient_hs96_change, strict_hs96_change)

state_output_tbl <- summary_ls$state_output_sum_tbl %>%
	select(state_name, total_output, lenient_hs96_output, strict_hs96_output, lenient_hs96_change, strict_hs96_change)

# By year
year_output_kable <- year_output_tbl %>% 
	rename(
	       `unmatched` = total_output,
	       `lenient` = lenient_hs96_output,
	       `strict` = strict_hs96_output,
	       `lenient change` = lenient_hs96_change,
	       `strict change` = strict_hs96_change
	       ) %>%
prepare_table(caption = "'Lenient' vs 'strict' matching to HS96: output by year (current R)")

state_output_kable <- state_output_tbl %>% 
	rename(
	       `state` = state_name,
	       `unmatched` = total_output,
	       `lenient` = lenient_hs96_output,
	       `strict` = strict_hs96_output,
	       `lenient change` = lenient_hs96_change,
	       `strict change` = strict_hs96_change
	       ) %>%
prepare_table(caption = "'Lenient' vs 'strict' matching to HS96: output by state (current R)") %>%
kable_styling(
      latex_options = c("scale_down")
	      ) 

# WRITE TABLES -------------------------------------------------------
folder_path <- here("doc/tables/appendix_data_cleaning_")

write(year_obs_kable, paste0(folder_path, "year_obs.tex"))
write(state_obs_kable, paste0(folder_path, "state_obs.tex"))
write(year_output_kable, paste0(folder_path, "year_output.tex"))
write(state_output_kable, paste0(folder_path, "state_output.tex"))

# Plot observations ----------------------------------------

strict_plot_tbl <- hs96_tbl %>% 
	left_join(
		  select(plant_tbl, year, dsl, new_state_code, state_name, multiplier),
		  by = c("year", "dsl")
		  ) %>%
group_by(year, state_name) %>%
summarize(
	  obs = sum(!is.na(strict_hs96))
	  )

lenient_plot_tbl <- hs96_tbl %>%
	left_join(
		  select(plant_tbl, year, dsl, new_state_code, state_name, multiplier),
		  by = c("year", "dsl")
		  ) %>%
group_by(year, state_name) %>%
summarize(
	  obs = sum(!is.na(lenient_hs96))
	  )

ggplot(strict_plot_tbl, aes(y = state_name, x = year, fill = obs)) +
	geom_raster()

ggplot(lenient_plot_tbl, aes(y = state_name, x = year, fill = obs)) +
	geom_raster()
# TODO

}






























