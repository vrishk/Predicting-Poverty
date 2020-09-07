library(ggplot2)
library(Hmisc)
library(corrplot)
library(zeallot)
library(boot)
library(autothresholdr)

# Reading Processed data
setwd("~/Research/Pioneer/Project/Predicting-Poverty/scripts/")
df <- read.csv("../processed/all_features.csv")

# Features
str(df)

# Interfeature Correlation

corr <- rcorr(as.matrix(df[, !(names(df) %in% c("id", "X"))]))
corrplot(corr$r, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45, p.mat = corr$P, 
         sig.level = 0.01, insig = "blank")

# Linear Model (without Nightlight)

m <- lm(wealth~EVI + Human.Footprint + Humidity.Score +
            Water.Bodies + Location.Score.1 + Location.Score.2 + Population + Density + 
            Pop.Score + X0 + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9,
        data=df)
summary(m)

# Removing Insignificant Variables (without Nightlight)

m <- lm(wealth~Human.Footprint + Location.Score.1 + Density + X0 + X2 + X4 + X5 + X6 + X9,
        data=df)
summary(m)

# Linear Model

m <- lm(wealth~Mean.Luminosity + EVI + Human.Footprint + Humidity.Score +
            Water.Bodies + Location.Score.1 + Location.Score.2 + Population + Density + 
            Pop.Score + X0 + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9,
        data=df)
summary(m)

# Removing Insignificant Variables

m <- lm(wealth~Mean.Luminosity + Water.Bodies + Pop.Score + X0 + X4 + X5 + X6,
        data=df)
summary(m)

# OBSERVATION: Not too much of a jump in variance explained: the problem is nightlight is already a very good predictor

# Case of low Nightlight Variation (<10)

hist(df$wealth)
hist(df$Mean.Luminosity)
thresh <- 



