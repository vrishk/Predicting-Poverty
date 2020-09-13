library(ggplot2)
library(Hmisc)
library(corrplot)
library(zeallot)
library(boot)
library(autothresholdr)

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


# --------------------------


# Climatic and Environmental Variables
env <- df[c("id", "Aridity", "Enhanced_Vegetation_Index_2010", 
            "Potential_Evapotranspiration", "Global_Human_Footprint",
            "Rainfall_2010"
)]

colnames(env)[colnames(env) == 'Enhanced_Vegetation_Index_2010'] <- 'EVI'
colnames(env)[colnames(env) == 'Potential_Evapotranspiration'] <- 'Evapotranspiration'
colnames(env)[colnames(env) == 'Global_Human_Footprint'] <- 'Human Footprint'
colnames(env)[colnames(env) == 'Rainfall_2010'] <- 'Rainfall'

env_corr <- rcorr(as.matrix(env[, !(names(env) %in% c("id"))]))
print("ENVIRONMENTAL:")
print(env_corr)
corrplot(env_corr$r, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45, p.mat = env_corr$P, 
         sig.level = 0.01, insig = "blank")

# OBSERVATION: Aridity, Rainfall, Evapotranspiration are highly correlated

# PCA of Aridity, Rainfall, Evapotranspiration -> Humidity Score

env_pca <- prcomp(env[c("Aridity", "Rainfall", "Evapotranspiration")], center = TRUE, scale = TRUE)

summary(env_pca) 
screeplot(env_pca, type = "l", npcs = 3) # First component contain ~95% of all variation

env["Humidity Score"] = env_pca$x[,1]


# --------------------------


# Geographic/Positional Variables

geo <- df[c("id", "Proximity_to_National_Borders", "Proximity_to_Protected_Areas", 
            "Proximity_to_Water", "Travel_Times")]

colnames(geo)[colnames(geo) == 'Proximity_to_National_Borders'] <- 'National Borders'
colnames(geo)[colnames(geo) == 'Proximity_to_Protected_Areas'] <- 'Protected Areas'
colnames(geo)[colnames(geo) == 'Proximity_to_Water'] <- 'Water Bodies'
colnames(geo)[colnames(geo) == 'Travel_Times'] <- 'Nearest City'

geo_corr <- rcorr(as.matrix(geo[, !(names(geo) %in% c("id"))]))
print("GEOGRAPHIC:")
print(geo_corr)
corrplot(geo_corr$r, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45, p.mat = geo_corr$P, sig.level = 0.01, insig = "blank")

# OBSERVATION: Negative correlation with Nearest City and National Borders, 
# Protected Areas as such regions would be much further away from cities.

# PCA of Nearest City and National Borders, Protected Areas -> Location Score

geo_pca <- prcomp(geo[c("Nearest City", "Protected Areas", "National Borders")], center = TRUE, scale = TRUE)

summary(geo_pca) 
screeplot(geo_pca, type = "l", npcs = 3) # First two components contain ~83% of all variation

geo["Location Score 1"] = geo_pca$x[, 1]
geo["Location Score 2"] = geo_pca$x[, 2]


# --------------------------


# Demographic Variables

de <- df[c("id", "All_Population_Count_2010", "All_Population_Density_2010")]

colnames(de)[colnames(de) == 'All_Population_Count_2010'] <- 'Population'
colnames(de)[colnames(de) == 'All_Population_Density_2010'] <- 'Density'

de_corr <- rcorr(as.matrix(de[, !(names(de) %in% c("id"))]))
print("DEMOGRAPHIC:")
print(de_corr)
corrplot(de_corr$r, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45, p.mat = de_corr$P, 
         sig.level = 0.01, insig = "blank")

# Clustering based on Density -> Otsu's Binarization

thresh <- auto_thresh(trunc(de$Density), method = "Otsu")
print("THRESHOLD: ")
print(thresh)
hist(de$Density,  breaks=30, xlab = "Density", ylab = "Frequency", main = "Otsu's Threshold for Density")
abline(v=thresh,col="red")

de["Pop Score"] = as.numeric(de$Density > thresh)


# --------------------------


# Imaging Variables

night <- df[c("id", "mean")]
day <- read.csv("../processed/DHS_daytime.csv")
exp_var <- read.csv("../processed/daytime_explained_variance.csv")
im <- merge(night, day, by.x="id", by.y="idx")

im = im[, !(names(im) %in% c("X"))]

colnames(im)[colnames(im) == 'mean'] <- 'Mean Luminosity'

im_corrs = data.frame( coeff=integer(ncol(day)-3), corr=numeric(ncol(day)-3) )

for( i in 0:(ncol(day)-4)) {
    cat(i, "\n")
    im_corrs$coeff[i] = i
    im_corrs$corr[i] = cor( im[[ paste("X",i, sep="") ]], im["Mean Luminosity"])
}

corr = im_corrs$corr
coeff = im_corrs$coeff

plot(coeff, corr, xlab = "Coefficient", ylab = "Correlation")
plot(PCA~X, data=exp_var, xlab = "Coefficient", ylab = "Variance")

v <- cumsum(exp_var$PCA)
print(v/max(v))
plot(v, xlab = "Coefficient", ylab = "Cumulative Variance")

# First 10 for PCA

im_relevant <- im[, c(
    "id", "X0", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "Mean Luminosity", "wealth"
)]

# --------------------------

# Write Processed data to csv

final <- Reduce(function(x, y) merge(x, y), list(env, geo, de, im_relevant))

final <- final[, !(names(final) %in% c(
    "Aridity", "Evapotranspiration", "Rainfall", "National Borders", "Protected Areas", "Nearest City"
))]

write.csv(final, "../processed/all_features.csv")

