library(ggplot2)
library(Hmisc)
library(corrplot)
library(zeallot)
library(boot)
library(autothresholdr)
library(randomForest)
library(caTools)

# Reading Processed data
setwd("~/Research/Pioneer/Project/Predicting-Poverty/scripts/")
df <- read.csv("../processed/all_features.csv")



# Interfeature Correlation

corr <- rcorr(as.matrix(df[, !(names(df) %in% c("id", "X"))]))
corrplot(corr$r, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45, p.mat = corr$P, 
         sig.level = 0.01, insig = "blank")



# --------------------------
# SIMPLE MODEL
# --------------------------

# ALL DATA

# Features
str(df)

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



# OBSERVATION: Not too much of a jump in variance explained: the problem is nightlight is already a very good predictor

# --------------------------

# LOW LUMINOSITY

# Case of low Nightlight Variation (<10)

hist(df$wealth)
hist(df$Mean.Luminosity)
thresh <- auto_thresh(trunc(df$Mean.Luminosity), method = "Otsu")
print("THRESHOLD: ")
print(thresh)
hist(df$Mean.Luminosity,  breaks=30, xlab = "Mean Luminosity", ylab = "Frequency", main = "Otsu's Threshold for ")
abline(v=thresh,col="red")

ll <- df[df$Mean.Luminosity<thresh, ]


m <- lm(wealth~Human.Footprint + Location.Score.1 + Density + X0 + X2 + X4 + X5 + X6 + X9,
        data=ll)
summary(m)

# --------------------------


# --------------------------
# COMPLEX MODELS
# --------------------------


sample = sample.split(df$wealth, SplitRatio = .75)

train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)

# Without Nightlight

rf <- randomForest(
    wealth ~ EVI + Human.Footprint + Humidity.Score +
        Water.Bodies + Location.Score.1 + Location.Score.2 + Population + Density + 
        Pop.Score + X0 + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9,
    data=train,  
)
print(rf)
prediction <- predict(rf, test)
names(prediction) <- NULL
print("RF Without Nightlight: ")
cor(prediction, test$wealth)

# All Variables

rf_n <- randomForest(
    wealth ~ Mean.Luminosity + EVI + Human.Footprint + Humidity.Score +
        Water.Bodies + Location.Score.1 + Location.Score.2 + Population + Density + 
        Pop.Score + X0 + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9,
    data=train
)
print(rf_n)
prediction <- predict(rf_n, test)
names(prediction) <- NULL
print("RF with All Variables: ")
cor(prediction, test$wealth)

# Low Luminosity

sample = sample.split(ll$wealth, SplitRatio = .75)

train = subset(ll, sample == TRUE)
test  = subset(ll, sample == FALSE)

rf_ll <- randomForest(
    wealth~Mean.Luminosity + EVI + Human.Footprint + Humidity.Score +
        Water.Bodies + Location.Score.1 + Location.Score.2 + Population + Density + 
        Pop.Score + X0 + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9,
    data=ll
)
print(rf_ll)
prediction <- predict(rf_ll, test)
names(prediction) <- NULL
print("RF Low Luminosity: ")
cor(prediction, test$wealth)
