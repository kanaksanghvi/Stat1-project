# Load required libraries
library(tidyverse)
library(car)    # For Levene’s test and VIF
library(ggpubr) # For normality check
library(MASS)   # For transformations

# Load dataset
data <- read.csv("Data-1.csv")

# Hypothesis 1: Private schools charge more tuition than public schools (t-test)
# Assumption check: Normality & variance equality
# QQ Plot for Tuition Fee
qqnorm(data$TUITION_FEE, main = "QQ Plot of Tuition Fee")
qqline(data$TUITION_FEE, col = "red", lwd = 2) # Adds reference line

#we see normality doesnt hold

shapiro.test(data$TUITION_FEE)  # Normality check
leveneTest(TUITION_FEE ~ SCHOOL_TYPE, data = data)  # Equal variances check
# we see both dont hold

unique(data$SCHOOL_TYPE)
wilcox.test(TUITION_FEE ~ SCHOOL_TYPE, data = data)

