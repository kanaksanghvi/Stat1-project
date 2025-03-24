# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)
library(corrplot)

# Load the dataset
data <- read.csv("Data-1.csv")
data <- na.omit(data)

# Fit the best polynomial model
final_model <- lm(TUITION_FEE ~ poly(SOCIOECONOMIC_STATUS, 4) + 
                    poly(FATHER_INCOME, 1) + 
                    poly(MOTHER_INCOME, 2) + 
                    poly(MONTHLY_SCHOOL_FEE, 4) + PBM, data = data)

# Display summary
summary(final_model)

# Set up a 2x2 plotting layout for residual analysis
par(mfrow = c(2, 2))

# Residuals vs Fitted (Homoscedasticity Check)
plot(final_model$fitted.values, final_model$residuals, 
     main = "Residuals vs Fitted", 
     xlab = "Fitted Values", ylab = "Residuals", 
     pch = 16, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# Q-Q Plot (Normality Check)
qqnorm(final_model$residuals, main = "Normal Q-Q Plot")
qqline(final_model$residuals, col = "red", lwd = 2)

# Scale-Location Plot (Variance Homogeneity)
plot(final_model$fitted.values, sqrt(abs(final_model$residuals)), 
     main = "Scale-Location Plot", 
     xlab = "Fitted Values", ylab = "√|Residuals|", 
     pch = 16, col = "purple")
abline(h = 0, col = "red", lwd = 2)

# Cook's Distance (Outlier Detection)
plot(cooks.distance(final_model), type = "h", 
     main = "Cook's Distance", 
     xlab = "Observation", ylab = "Cook's Distance", 
     col = "darkgreen")
abline(h = 4/(nrow(data)), col = "red", lwd = 2, lty = 2)

# Reset plot layout
par(mfrow = c(1, 1))

