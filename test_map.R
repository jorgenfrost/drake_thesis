library(tidyverse)
library(maps)
library(sf)
library(ozmaps)
library(here)
library(ggmap)

India_states <- st_read(here("data/external/gis/gadm36_IND_shp/gadm36_IND_1.shp"))
India_states

ggplot(India_states) +
	geom_sf(aes(fill = NAME_1)) +
	coord_sf() +
	guides(fill = FALSE) +
	geom_sf_label(aes(label = NAME_1)) 


