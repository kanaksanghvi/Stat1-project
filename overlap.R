# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)
library(corrplot)
library(gridExtra)

# Load the dataset
data <- read.csv("Data-1.csv")
data <- na.omit(data)

# Fit the polynomial regression model
poly_model <- lm(TUITION_FEE ~ poly(SOCIOECONOMIC_STATUS, 4) + 
                   poly(FATHER_INCOME, 1) + 
                   poly(MOTHER_INCOME, 2) + 
                   poly(MONTHLY_SCHOOL_FEE, 4) + PBM, data = data)

# Fit the linear regression model
linear_model <- lm(TUITION_FEE ~ SOCIOECONOMIC_STATUS + FATHER_INCOME + MOTHER_INCOME + 
                     MONTHLY_SCHOOL_FEE + PBM, data = data)

# Set up a 2x2 plotting layout
par(mfrow = c(2, 2))

# 1. Residuals vs Fitted (Homoscedasticity Check)
plot(linear_model$fitted.values, linear_model$residuals, 
     main = "Residuals vs Fitted", 
     xlab = "Fitted Values", ylab = "Residuals", 
     pch = 16, col = "red", ylim = range(c(linear_model$residuals, poly_model$residuals)))
points(poly_model$fitted.values, poly_model$residuals, pch = 16, col = "blue")
abline(h = 0, col = "black", lwd = 2)
legend("topright", legend = c("Linear", "Polynomial"), col = c("red", "blue"), pch = 16)

# 2. Q-Q Plot (Normality Check)
qqnorm(linear_model$residuals, main = "Normal Q-Q Plot", col = "red", pch = 16)
qqline(linear_model$residuals, col = "red", lwd = 2)
points(qqnorm(poly_model$residuals, plot.it = FALSE), col = "blue", pch = 16)
qqline(poly_model$residuals, col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Linear", "Polynomial"), col = c("red", "blue"), pch = 16)

# 3. Scale-Location Plot (Variance Homogeneity)
plot(linear_model$fitted.values, sqrt(abs(linear_model$residuals)), 
     main = "Scale-Location Plot", 
     xlab = "Fitted Values", ylab = "√|Residuals|", 
     pch = 16, col = "red", ylim = range(c(sqrt(abs(linear_model$residuals)), sqrt(abs(poly_model$residuals)))))
points(poly_model$fitted.values, sqrt(abs(poly_model$residuals)), pch = 16, col = "blue")
abline(h = 0, col = "black", lwd = 2)
legend("topright", legend = c("Linear", "Polynomial"), col = c("red", "blue"), pch = 16)

# 4. Cook's Distance (Outlier Detection)
plot(cooks.distance(linear_model), type = "h", 
     main = "Cook's Distance", 
     xlab = "Observation", ylab = "Cook's Distance", 
     col = "red", ylim = range(c(cooks.distance(linear_model), cooks.distance(poly_model))))
lines(cooks.distance(poly_model), type = "h", col = "blue")
abline(h = 4/(nrow(data)), col = "black", lwd = 2, lty = 2)
legend("topright", legend = c("Linear", "Polynomial"), col = c("red", "blue"), lty = 1)

# Reset plot layout
par(mfrow = c(1, 1))
