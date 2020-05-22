# Collect complexity values
plant_pci_tbl <- readd("plant_pci_tbl")

plant_complexity_tbl <- readd("plant_complexity_tbl")

# Collect plant_tbl
plant_tbl <- readd("analysis_sample_ls")$plant_tbl

# Get pci lenient
plant_tbl %>% 
	select(year, dsl, multiplier, state_name, revenue) %>%
	left_join(plant_pci_tbl )
