# Importing Libraries
library(rgdal)
library(raster)
library(ggplot2)
library(rasterVis)
library(zeallot)

# Loading NightLight data
setwd("~/Research/Pioneer/Project/Poverty-Prediction-by-Satellite-Imagery/scripts/")
shape <- raster("../data/nighttime_image/F182010.v4d_web.stable_lights.avg_vis.tif")

# Get Cluster Section from Coordinates
get_NL_section <- function(lat, long) {
    c(r, c) %<-% rowColFromCell(shape, cellFromXY(shape, cbind(long,lat)))
    section <- crop(shape, extent(shape, r-5, r+4, c-5, c+4))
    return(section)
}

# Clusters from Processed DHS data
clusters <- read.csv("../processed/rwanda_cluster_avg_asset_2010.csv")

# Create DF of NL features with wealth
df <- data.frame(id = integer(nrow(clusters)), 
                 max = numeric(nrow(clusters)), 
                 min = numeric(nrow(clusters)), 
                 median = numeric(nrow(clusters)),
                 std = numeric(nrow(clusters)),
                 wealth = numeric(nrow(clusters)))

for (i in 1:nrow(clusters)) {
    df$id[i] <- clusters[i, "cluster"]
    df$wealth[i] <- clusters[i, "wlthindf"]
    lat <- clusters[i, "latitude"]
    long <- clusters[i, "longitude"]
    section <- get_NL_section(lat, long)
    
    # NL Features Used
    df$max[i] <- cellStats(section, 'max')
    df$min[i] <- cellStats(section, 'min') 
    df$mean[i] <-cellStats(section, 'mean')
    df$median[i] <-cellStats(section, median)
    df$std[i] <-cellStats(section, 'sd')
    
    print(i)
}

# Plot mean vs wealth
plot(wealth~mean, df)

# Write Data
write.csv(df, "../processed/nl_wealth_data.csv")
