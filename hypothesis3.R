# Load required libraries
library(tidyverse)
library(car)    # For Levene’s test and VIF
library(ggpubr) # For normality check
library(MASS)   # For transformations

# Load dataset
data <- read.csv("Data-1.csv")

# Check data structure
str(data)
summary(data)


# Hypothesis 3: Career choices affect tuition fees (ANOVA)
# Assumption checks: Normality of residuals and equal variances
qqnorm(data$TUITION_FEE, main = "QQ Plot of Tuition Fee")
qqline(data$TUITION_FEE, col = "red", lwd = 2) # Adds reference line
shapiro.test(data$TUITION_FEE)  # Normality
leveneTest(TUITION_FEE ~ CAREER_NAME, data = data)  # Homogeneity of variances
kruskal.test(TUITION_FEE ~ CAREER_NAME, data = data)
library(FSA)
dunnTest(TUITION_FEE ~ CAREER_NAME, data = data, method="bonferroni")

library(ggplot2)
ggplot(data, aes(x = CAREER_NAME, y = TUITION_FEE)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
