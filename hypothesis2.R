# Load required libraries
library(tidyverse)
library(car)    # For Levene’s test and VIF
library(ggpubr) # For normality check
library(MASS)   # For transformations

# Load dataset
data <- read.csv("Data-1.csv")

# Hypothesis 2: Lower SES students are more likely to receive scholarships (Chi-Square test)
table_ses_scholarship <- table(data$SOCIOECONOMIC_STATUS, data$SCHOLARSHIP)
chisq.test(table_ses_scholarship) # Chi-square test

# Check if expected values are adequate (should be ≥5 in at least 80% of cells)
expected_vals <- chisq.test(table_ses_scholarship)$expected
min(expected_vals)

#chi test not valid
fisher.test(table_ses_scholarship, simulate.p.value = TRUE, B = 10000)
