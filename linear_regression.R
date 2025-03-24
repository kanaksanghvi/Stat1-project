# Load necessary libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(car)
library(lmtest)  # For additional assumption tests

# Load the dataset
data <- read.csv("Data-1.csv")
data <- na.omit(data)

# Select relevant numeric columns for correlation analysis
numeric_vars <- data %>% select(SOCIOECONOMIC_STATUS, FATHER_INCOME, MOTHER_INCOME, SIBLING_COUNT, TUITION_FEE)

# Set up a 2x2 plotting layout for scatter plots
par(mfrow = c(2, 2))

# Scatter plots to visualize linearity
plot1 <- ggplot(data, aes(x = SOCIOECONOMIC_STATUS, y = TUITION_FEE)) +
  geom_point() + geom_smooth(method = "lm", col = "red") +
  ggtitle("SOCIOECONOMIC_STATUS vs TUITION_FEE")

plot2 <- ggplot(data, aes(x = FATHER_INCOME, y = TUITION_FEE)) +
  geom_point() + geom_smooth(method = "lm", col = "blue") +
  ggtitle("FATHER_INCOME vs TUITION_FEE")

plot3 <- ggplot(data, aes(x = MOTHER_INCOME, y = TUITION_FEE)) +
  geom_point() + geom_smooth(method = "lm", col = "green") +
  ggtitle("MOTHER_INCOME vs TUITION_FEE")

library(gridExtra)
grid.arrange(plot1, plot2, plot3, nrow = 2)

# Reset plotting layout
par(mfrow = c(1, 1))

# Fit an initial model
model <- lm(TUITION_FEE ~ SOCIOECONOMIC_STATUS + FATHER_INCOME + MOTHER_INCOME + 
              MONTHLY_SCHOOL_FEE + FACTOR + PBM, data = data)

# Calculate VIF
vif(model)

# Final model based on selected variables
final_model <- lm(TUITION_FEE ~ SOCIOECONOMIC_STATUS + FATHER_INCOME + MOTHER_INCOME + 
                    MONTHLY_SCHOOL_FEE + PBM, data = data)

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
