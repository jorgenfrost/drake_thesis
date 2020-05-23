library(janitor)
library(haven)
library(drake)
library(tidyverse)
io_tbl <- read_dta("/home/post/drake_project/data/external/replication_archive_AER_september_2015/01.-Data/IOtables/12Oct_IO8990.dta") %>% select(iottsector, commodity) %>% as.data.frame()
read_dta("/home/post/drake_project/data/external/replication_archive_AER_september_2015/01.-Data/IOtables/16Oct_IO8990_Forward.dta")
io_conc <- read_dta("/home/post/drake_project/data/external/replication_archive_AER_september_2015/01.-Data/IOtables/14Sept_IOConc_updt.dta") %>%
	rename(nic87 = nic3digit) %>%
	mutate(nic87 = as.character(nic87))
plant_tbl <- readd("analysis_sample_ls")$plant_tbl %>%
	select(dsl, year, nic87, multiplier) %>%
	filter(!is.na(nic87)) 

plant_iot <- plant_tbl %>%
	inner_join(io_conc) 

ggplot(plant_iot, aes(x = as.factor(iottsector))) +
	geom_bar(aes(weight = multiplier))
