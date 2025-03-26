# Load necessary libraries
library(MASS)
library(car)
library(ggplot2)
library(caret)
library(glmnet)
library(lmtest)  # For heteroskedasticity test

# Reload the dataset
data <- read.csv("Data-1.csv")

# Remove missing values
data <- na.omit(data)

# Function to evaluate polynomial degrees using AIC and Adjusted R-squared
evaluate_poly <- function(degree) {
  formula <- as.formula(paste("TUITION_FEE ~",
                              paste(paste0("poly(", c("SOCIOECONOMIC_STATUS", "FATHER_INCOME", "MOTHER_INCOME", "MONTHLY_SCHOOL_FEE"), ", ", degree, ")"), collapse = " + "),
                              "+ PBM"))
  model <- lm(formula, data = data)
  return(c(AIC = AIC(model), Adj_R2 = summary(model)$adj.r.squared))
}

# Try different polynomial degrees and find the best one
degrees <- 1:5
results <- sapply(degrees, evaluate_poly)
results_df <- as.data.frame(t(results))
colnames(results_df) <- c("AIC", "Adjusted_R2")

# Find the best degree based on the lowest AIC
best_degree <- degrees[which.min(results_df$AIC)]
cat("Best polynomial degree based on AIC:", best_degree, "\n")
print(results_df)

# Fit the final model using the best degree
final_formula <- as.formula(paste("TUITION_FEE ~",
                                  paste(paste0("poly(", c("SOCIOECONOMIC_STATUS", "FATHER_INCOME", "MOTHER_INCOME", "MONTHLY_SCHOOL_FEE"), ", ", best_degree, ")"), collapse = " + "),
                                  "+ PBM"))
final_model <- lm(final_formula, data = data)

# Summary of the final model
summary(final_model)


