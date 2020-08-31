library(ggplot2)
library(ggcorrplot)
library(zeallot)
library(boot)

# Reading Processed data
setwd("~/Research/Pioneer/Project/Predicting-Poverty/scripts/")
nl <- read.csv("../processed/nl_wealth_data.csv")
dhs <- read.csv("../data/dhs/rwanda_dhs_cluster.csv")
df <- merge(nl, dhs, by.x="id", by.y="DHSCLUST")
df <- df[, !(names(df) %in% c(
    "X", "DHSID", 
    "Rainfall_2015", "Malaria_2015", "ITN_Coverage_2015", 
    "Enhanced_Vegetation_Index_2015", "BUILT_Population_2014", 
    "All_Population_Density_2015", "All_Population_Density_2015"
    ))]

# Environmental Variables


