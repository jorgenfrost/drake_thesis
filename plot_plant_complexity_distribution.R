plant_qp_tbl <- readd("plant_complexity_tbl")
plant_pci_tbl <- readd("plant_pci_tbl")
plant_qp_mean_tbl <- readd("plant_mean_complexity_tbl")

plant_tbl <- readd("analysis_sample_ls")$plant_tbl
id_tbl <- plant_tbl %>%
	select(year, dsl, multiplier, state_name)

# ADD SAMPLE WEIGHTS (MULTIPLIER) ------------------------------------
plant_qp_tbl <- plant_qp_tbl %>%
	inner_join(id_tbl)

plant_pci_tbl <- plant_pci_tbl %>%
	inner_join(id_tbl)

plant_qp_mean_tbl <- plant_qp_mean_tbl %>%
	inner_join(id_tbl)

# CREATE STANDARDIZED VALUES
plant_qp_tbl <- plant_qp_tbl %>%
	group_by(match) %>%
	mutate(
	       z_avg_comp = (w_avg_complexity - mean(w_avg_complexity)) / sd(w_avg_complexity)
	       ) 

plant_qp_mean_tbl <- plant_qp_mean_tbl %>%
	group_by(match) %>%
	mutate(
	       z_avg_comp = (w_avg_complexity - mean(w_avg_complexity)) / sd(w_avg_complexity)
	       ) 

# CREATE STRICT DISTRIBUTION -------------------------------------------
strict_qp_tbl <- plant_qp_tbl %>%
	filter(match == "strict") 

strict_pci_tbl <- plant_pci_tbl %>%
	filter(match == "strict")

# Histograms: weighted average 
ggplot(strict_qp_tbl, aes(x = log(w_avg_complexity))) +
	geom_histogram(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

ggplot(strict_pci_tbl, aes(x = w_avg_complexity)) +
	geom_histogram(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

# Histograms: max 
ggplot(strict_qp_tbl, aes(x = log(max_complexity))) +
	geom_histogram(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

ggplot(strict_pci_tbl, aes(x = max_complexity)) +
	geom_histogram(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

# Density: max
ggplot(strict_qp_tbl, aes(x = log(max_complexity))) +
	geom_density(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

ggplot(strict_pci_tbl, aes(x = max_complexity)) +
	geom_density(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

# CREATE LENIENT PLOTS -------------------------------------------
lenient_qp_tbl <- plant_qp_tbl %>%
	filter(match == "lenient") 

lenient_pci_tbl <- plant_pci_tbl %>%
	filter(match == "lenient")

# Histograms: weighted average 
ggplot(lenient_qp_tbl, aes(x = log(w_avg_complexity))) +
	geom_histogram(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

ggplot(lenient_pci_tbl, aes(x = w_avg_complexity)) +
	geom_histogram(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

# Histograms: max 
ggplot(lenient_qp_tbl, aes(x = log(max_complexity))) +
	geom_histogram(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

ggplot(lenient_pci_tbl, aes(x = max_complexity)) +
	geom_histogram(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

# Density: max
ggplot(lenient_qp_tbl, aes(x = log(max_complexity))) +
	geom_density(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

ggplot(lenient_pci_tbl, aes(x = max_complexity)) +
	geom_density(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

# PLOTS THAT COMPARE DISTRIBUTIOS ------------------------------------
max_complexity_match_dp <- ggplot(plant_qp_tbl, aes(x = log(max_complexity), color = match)) +
	geom_density(aes(weight = multiplier)) +
	facet_wrap(~ state_name) + 
	theme_pubr() +
	xlab("Max complexity, ln")

avg_complexity_match_dp <- ggplot(plant_qp_tbl, aes(x = log(x = w_avg_complexity), color = match)) +
	geom_density(aes(weight = multiplier)) +
	facet_wrap(~ state_name) + 
	theme_pubr() +
	xlab("Plant complexity, ln")


max_pci_match_dp <- ggplot(plant_pci_tbl, aes(x = max_complexity, color = match)) +
	geom_density(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

avg_pci_match_dp <- ggplot(plant_pci_tbl, aes(x = w_avg_complexity, color = match)) +
	geom_density(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

avg_pci_match_year_dp <- ggplot(plant_pci_tbl, aes(x = w_avg_complexity, color = match)) +
	geom_density(aes(weight = multiplier)) +
	facet_wrap(~ year)

# Check max vs avg
pci_gather_tbl <- plant_pci_tbl %>% 
	filter(match == "lenient") %>%
	gather(w_avg_complexity, max_complexity, key = metric, val = complexity)

qp_gather_tbl <- plant_qp_tbl %>% 
	filter(match == "lenient") %>%
	gather(w_avg_complexity, max_complexity, key = metric, val = complexity)

pci_metric_state_dp <-
	ggplot(pci_gather_tbl, aes(x = complexity, color = metric)) +
	geom_density(aes(weight = multiplier)) +
	facet_wrap(~ state)

qp_metric_state_dp <-
	ggplot(qp_gather_tbl, aes(x = log(complexity), color = metric)) +
	geom_density(aes(weight = multiplier)) +
	facet_wrap(~ state_name) 

plant_qp_tbl %>% arrange(desc(w_avg_complexity))
# MEAN VS NON MEAN ---------------------------------------------
joined_lenient <- plant_qp_mean_tbl %>% 
	mutate(val = "meaned") %>%
	bind_rows(plant_qp_tbl %>% mutate(val = "yearly")) %>%
	filter(match == "lenient")

ggplot(joined, aes(x = log(max_complexity), fill = val)) +
	geom_histogram(aes(weight = multiplier)) +
	facet_wrap(~ state_name)

# ------------ DEVELOP ----------------
