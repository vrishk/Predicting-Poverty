library(ggplot2)
library(zeallot)
library(boot)

# Reading Processed data
setwd("~/Research/Pioneer/Project/Predicting-Poverty/scripts/")
df <- read.csv("../processed/nl_wealth_data.csv")

# Simple Linear Model of all variables
lm <- lm(wealth~mean + median + std + max + min, data=df)
plot(lm)
summary(lm)

# Simple Linear Model of only mean, median, min
lm <- lm(wealth~mean + median + min, data=df)
plot(lm)
summary(lm)

# Simple Linear Model of only mean -> Highest adj-R^2
lm <- lm(wealth~mean, data=df)
plot(lm)
summary(lm)

# Plot with mean
plot(wealth~mean, data=df, xlab = "Mean Nightlight Luminosity", ylab = "Wealth")
abline(lm)

# Bootstrapped Linear Model only mean
set.seed(42)
N <- 5000
boot_func <- function(d, i) {
    return(summary(lm(wealth~mean, data=d[i,]))$coeff[2])
}
boot_res <- boot(df, boot_func, R = N)
hist(boot_res$t, xlab="Slopes")

boot.ci(boot_res)
