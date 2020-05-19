#' Function that cleans and joins the state-level variables from RBI.

clean_rbi <- function(pop_density, net_domestic_product, per_cap_net_domestic_product, rural_pop, urban_pop, total_pop, state_codes) {

#################################################################
##                        X: Clean data                        ##
#################################################################

# Net domestic product, constant prices ------------------------------
net_domestic_product_raw <- read_excel(here(net_domestic_product))

# I follow the ASI data in setting years by last value (eg. 2001-2002 = 2002)
net_domestic_product_tbl <- net_domestic_product_raw %>%
	gather(-`States/Union Territories`, key = year, val = net_gdp) %>%
	clean_names(case = "snake") %>% 
	mutate(
	       year = str_sub(string = year, start = 1, end = 4) %>% 
		       as.numeric() + 1
	       ) %>%
	mutate(
	       net_gdp = as.numeric(net_gdp)
	       )

# Per capita net domestic product, constant prices -------------------
per_cap_net_domestic_product_raw <- read_excel(here(per_cap_net_domestic_product))

# I follow the ASI data in setting years by last value (eg. 2001-2002 = 2002)
per_cap_net_domestic_product_tbl <- per_cap_net_domestic_product_raw %>%
	gather(-`States/ Union Territories`, key = year, val = net_gdp_cap) %>%
	clean_names(case = "snake") %>% 
	mutate(
	       year = str_sub(string = year, start = 1, end = 4) %>% 
		       as.numeric() + 1
	       ) %>%
	mutate(net_gdp_cap = as.numeric(net_gdp_cap))

# Population density -------------------------------------------------
pop_density_raw <- read_excel(here(pop_density))

# clean tabel
pop_density_tbl <- pop_density_raw %>%
	gather(-`States/Union Territories`, key = year, val = pop_sqkm) %>%
	clean_names(case = "snake") %>%
	mutate(
	       year = as.numeric(year),
	       pop_sqkm = as.numeric(pop_sqkm)
	       ) 

# Rural population ---------------------------------------------------
rural_pop_raw <- read_excel(here(rural_pop))

rural_pop_tbl <- rural_pop_raw %>% 
	gather(-`States/Union Territories`, key = year, val = rural_pop) %>%
	clean_names(case = "snake")  %>%
	mutate(
	       year = as.numeric(year),
	       rural_pop = as.numeric(rural_pop)
	       )

# Urban population ---------------------------------------------------
urban_pop_raw <- read_excel(here(urban_pop))

urban_pop_tbl <- urban_pop_raw %>% 
	gather(-`States/Union Territories`, key = year, val = urban_pop) %>%
	clean_names(case = "snake")  %>%
	mutate(
	       year = as.numeric(year),
	       urban_pop = as.numeric(urban_pop)
	       )

# Total population ---------------------------------------------------
total_pop_raw <- read_excel(here(total_pop))

total_pop_tbl <- total_pop_raw %>%
	gather(-`States/Union Territories`, key = year, val = total_pop) %>%
	clean_names(case = "snake")  %>%
	mutate(
	       year = as.numeric(year),
	       total_pop = as.numeric(total_pop)
	       )

#################################################################
##                 X: Join and fix state codes                 ##
#################################################################
gdp_tbl <- per_cap_net_domestic_product_tbl %>%
	full_join(net_domestic_product_tbl) %>%
	rename(state = states_union_territories)

pop_tbl <- urban_pop_tbl %>%
	full_join(rural_pop_tbl) %>%
	full_join(total_pop_tbl) %>%
	rename(state = states_union_territories)

state_tbl <- full_join(gdp_tbl, pop_tbl)

state_tbl <- left_join(state_tbl, read_csv(state_codes), by = c("state" = "rbi_state")) %>%
	rename(rbi_state = state)

return(state_tbl)

}

