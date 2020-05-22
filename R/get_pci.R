get_pci <- function() {
# COLLECT AND CLEAN PCI -------------------------------------------
# I get 4 digit HS96 PCI values from OEC PRO. They are listed with 5-6 digits (first 1 og 2 are section headings).
# I remove these headings first, leaving me with PCI values at the HS96 digit level (Se email from Alex).
pci_raw <- read_csv(here("data/external/oec/pci_hs4_hs96_98-18.csv"))

pci_tbl <- pci_raw %>%
	gather(-c(`HS4`,`HS4 ID`), key = year, val = pci) %>%
	clean_names(case = "snake") %>%
	mutate(
	       hs96_code = case_when(
				str_length(hs4_id) == 5 ~ str_sub(hs4_id, start = 2, end = 5),
				str_length(hs4_id) == 6 ~ str_sub(hs4_id, start = 3, end = 6),
				TRUE ~ NA_character_
				)
	       ) %>%
	select(-hs4_id, hs96_name = hs4) %>%
	mutate(year = as.numeric(year))

return(pci_tbl)

}
