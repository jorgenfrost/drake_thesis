tbl_raw <- readd("hs96_export_raw_tbl")
tbl_clean <- readd("hs96_export_cleaned_tbl")
pop_tbl <- readd("pop_tbl")
gdp_tbl <- readd("gdp_tbl")
ref_year <- 2017

# Summary table

# Export remaining 
evaluate_filter_loss <- function(cleaned, raw, pop, gdp, ref_year) {

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

pop_change <- (clean_pop - raw_export) / raw_export

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

return(tibble(export_change, pop_change gdp_change))

}
