## library() calls go here
library(conflicted) # explicitly control overlapping function names
# library(dotenv) # not sure, something with the env params used in drake
library(drake) # better, make-like project management
library(latex2exp)
library(tidyverse) # 
library(fst) # very fast read/write ops
library(magrittr) # le pibe
library(here) # relative project paths
library(janitor) # cleaning names 
library(haven) # reading .dta
library(readxl) # reading xls/x
library(ggthemes) # plots
library(ggpubr) # easy plots
library(vroom) # faaaaaast reading
library(countrycode) # converting iso3
library(knitr) # tables 
library(kableExtra) # better tables
library(sandwich) # robust standard errors
library(lmtest) # more adv regression stuff
library(igraph) # working with networks and network graphs
library(estimatr)
library(stargazer)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("gather", "tidyr")
conflicted::conflict_prefer("lag", "dplyr")

