# READ DATA ----------------------------------------------------------

es05_tbl <- readd("es05_tbl")
es14_tbl <- readd("es14_tbl")
shortages_tbl <- readd("energy_shortages_tbl")

es05_raw <- read_dta("/home/post/drake_project/data/external/es_india/India-2005--full data-.dta/India-2005--full-data-.dta")

result_ls <- list()

## ENTERPRISE SURVEY 2005 
# Add shortage data
es05_tbl <- es05_tbl %>% 
	mutate(year = 2005) %>%
	left_join(shortages_tbl, by = c("asi_state_name" = "state", "year")) 

# Add biggest obstacle = electricity dummy 
es05_tbl <- es05_tbl %>%  # electricity = "B"
	mutate(biggest_obstacle_dmy = ifelse(biggest_obstacle == "B", 1, 0) %>% as_factor) 

## ENTERPRISE SURVEY 2014
# Add shortage data
es14_tbl <- es14_tbl %>%
	mutate(year = 2014) %>%
	left_join(shortages_tbl, by = c("state_name.asi" = "state", "year"))

# Add biggest obstacle = electricity dummy 
es14_tbl <- es14_tbl %>% # electricity = 8
	mutate(
	       biggest_obstacle_dmy = ifelse(biggest_obstacle == 8, 1, 0) 
	       )

# DEFINE HELPTER FUNCTIONS -------------------------------------------

# ANALYSE ENTERPRISE SURVEYS -----------------------------------------

# Self generation share: WBES 2005 ---------------------------------------------
es05_selfgen <- es05_tbl %>%
  select(idstd, industry, self_gen = share_generated_electricity, year, asi_state_name, avg_shortage, own_generator) %>%
  filter(own_generator == 1 & !is.na(self_gen))

# Create linear model
selfgen05_model <- lm(
  formula = self_gen ~ avg_shortage + factor(industry),
  data = es05_selfgen
)
selfgen05_rob <- 
	lm_robust(
		  formula = self_gen ~ avg_shortage,
		  fixed_effects = factor(industry),
		  data = es05_selfgen,
		  clusters = asi_state_name,
		  se_type = "stata"
		  )

# Get robust, cluster standard errors
selfgen05_robust <- coeftest(selfgen05_model, vcov = vcovCL, cluster = es05_selfgen$asi_state_name)

# Power quality: WBES 2005 ---------------------------------------------
es05_quality <- es05_tbl %>%
  select(idstd, industry, quality = quality_of_power_supply, asi_state_name, avg_shortage) %>%
  filter(!is.na(quality))

# Create line model
quality05_model <- lm(
  formula = quality ~ avg_shortage + factor(industry),
  data = es05_quality
)

# Get robust, clustered standard errors
quality05_robust <- coeftest(quality05_model, vcov = vcovCL, cluster = es05_quality$asi_state_name, type = "HC1")

# Electricity is an obstacle: WBES 2005 ---------------------------------------------
es05_obstacle <- es05_tbl %>%
  select(idstd, industry, obstacle = obstacle_electricity, year, avg_shortage, asi_state_name) %>%
  filter(!is.na(obstacle))

# Create linear model
obstacle05_model <- lm(
  formula = as.numeric(obstacle) ~ avg_shortage + factor(industry),
  data = es05_obstacle
)

# Get robust, cluster standard errors
obstacle05_robust <- coeftest(obstacle05_model, .vcov = vcovCL, cluster = es05_obstacle$asi_state_name)

# Self generation share: WBES 2014 ---------------------------------------------
es14_selfgen <- es14_tbl %>%
  select(idstd, industry = sector, asi_state_name = state_name.asi, self_gen = share_elect_from_generator, year, avg_shortage, own_generator, wstrict) %>%
  mutate(asi_state_name = as.factor(asi_state_name)) %>%
  filter(own_generator == 1 & !is.na(self_gen))

# Create linear model
selfgen14_model <- lm(
  formula = self_gen ~ avg_shortage + factor(industry),
  data = es14_selfgen,
  weights = wstrict
)

# Get robust, cluster standard errors
selfgen14_robust <- coeftest(selfgen14_model, vcov = vcovCL, cluster = es14_selfgen$asi_state_name)
coeftest(selfgen14_model, vcov = vcovCL, cluster = es14_selfgen$asi_state_name)

# Self generation share: WBES 2014 ---------------------------------------------
es14_obstacle <- es14_tbl %>%
  select(idstd, industry = sector, wstrict, obstacle = degree_electricity_obstacle, avg_shortage, asi_state_name = state_name.asi) %>%
  mutate(asi_state_name = as.factor(asi_state_name))

# Create linear model
obstacle14_model <- lm(
  formula = obstacle ~ avg_shortage + factor(industry),
  data = es14_obstacle,
  weights = wstrict
)

# Get robust, clustered standard errors
obstacle14_robust <- coeftest(obstacle14_model, vcov. = vcovCL, cluster = es14_obstacle$asi_state_name)

# CREATE TABLE -----------

wbes_star <- stargazer(
  selfgen05_robust, obstacle05_robust, quality05_robust, selfgen14_robust, obstacle14_robust,
 title = "World Bank Enterprise Surveys and the Shortage variable",
 dep.var.caption = "",
 dep.var.labels.include = FALSE,
  type = "latex",
  #align = TRUE,
 star.cutoffs = c(0.05, 0.01, 0.001),
  column.labels = c("Self-gen share", "Obstacle", "Power quality", "Self-gen share", "Obstacle"),
  covariate.labels = c("Shortage"),
  omit = c("industry"),
  omit.labels = c("Industry FE"),
  label = "tab:wbes_validity",
  float = FALSE,
 #column.sep.width = "1pt",
 font.size = "small",
  add.lines = list(
    c("Obervations:", paste0(nrow(es05_selfgen)), paste0(nrow(es05_obstacle)), paste0(nrow(es05_quality)), paste0(nrow(es14_selfgen)), paste0(nrow(es14_obstacle))),
    c("WBES:", "2005", "2005", "2005", "2014", "2014")
  ),
		       omit.table.layout = "n",
          style = "aer", notes.append = FALSE, notes.align = "l"
	  )

  write(wbes_star, here("doc/tables/energy_validity/wbes_nonfloat.tex"))

