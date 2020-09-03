library(ggplot2)
library(Hmisc)
library(corrplot)
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
    "All_Population_Density_2015", "All_Population_Density_2015",
    "All_Population_Count_2015"
    ))]

cat(str(df))

# Climatic and Environmental Variables
env <- df[c("Aridity", "Enhanced_Vegetation_Index_2010", 
     "Potential_Evapotranspiration", "Global_Human_Footprint",
     "Rainfall_2010"
     )]

colnames(env)[colnames(env) == 'Enhanced_Vegetation_Index_2010'] <- 'EVI'
colnames(env)[colnames(env) == 'Potential_Evapotranspiration'] <- 'Evapotranspiration'
colnames(env)[colnames(env) == 'Global_Human_Footprint'] <- 'Human Footprint'
colnames(env)[colnames(env) == 'Rainfall_2010'] <- 'Rainfall'

env_corr <- rcorr(as.matrix(env))
print("ENVIRONMENTAL:")
print(env_corr)
corrplot(env_corr$r, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45, p.mat = env_corr$P, sig.level = 0.01, insig = "blank")

# OBSERVATION: Aridity, Rainfall, Evapotranspiration are highly correlated, choose best variable.

# TODO: Plotting Aridity, Rainfall, Evapotranspiration 

# Geographic/Positional Variables

geo <- df[c("Proximity_to_National_Borders", "Proximity_to_Protected_Areas", "Proximity_to_Water", "Travel_Times")]

colnames(geo)[colnames(geo) == 'Proximity_to_National_Borders'] <- 'National Borders'
colnames(geo)[colnames(geo) == 'Proximity_to_Protected_Areas'] <- 'Protected Areas'
colnames(geo)[colnames(geo) == 'Proximity_to_Water'] <- 'Water Bodies'
colnames(geo)[colnames(geo) == 'Travel_Times'] <- 'Nearest City'

geo_corr <- rcorr(as.matrix(geo))
print("GEOGRAPHIC:")
print(geo_corr)
corrplot(geo_corr$r, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45, p.mat = geo_corr$P, sig.level = 0.01, insig = "blank")

# OBSERVATION: Negative correlation with Nearest City and National Borders, Protected Areas as such regions would be much further away from cities.

# TODO: Plotting Nearest City and National Borders, Protected Areas

# Socioeconomic Variables

de <- df[c("All_Population_Count_2010", "All_Population_Density_2010")]

colnames(de)[colnames(de) == 'All_Population_Count_2010'] <- 'Population'
colnames(de)[colnames(de) == 'All_Population_Density_2010'] <- 'Density'

de_corr <- rcorr(as.matrix(de))
print("DEMOGRAPHIC:")
print(de_corr)
corrplot(de_corr$r, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45, p.mat = de_corr$P, sig.level = 0.01, insig = "blank")

# TODO: Clustering based on Density?

# Imaging Variables

night <- df[c("id", "mean")]
day <- read.csv("../processed/DHS_daytime.csv")
im <- merge(day, night, by.x="idx", by.y="id")

im = im[, !(names(df) %in% c("X"))]

colnames(im)[colnames(im) == 'mean'] <- 'MeanLuminosity'

im_corrs = data.frame( coeff=integer(ncol(day)-3), corr=numeric(ncol(day)-3) )

for( i in 0:(ncol(day)-4)) {
    cat(i)
    im_corrs$coeff[i] = i
    im_corrs$corr[i] = cor( im[[ paste("X",i, sep="") ]], im$MeanLuminosity)
}

corr = im_corrs$corr
coeff = im_corrs$coeff

plot(coeff, corr)
     