# Load necessary libraries
library(ggplot2)
library(caret)  # For cross-validation

# Load dataset (replace with actual file)
data <- read.csv("Data-1.csv")
data <- na.omit(data)
# Fit the initial polynomial regression model
final_model <- lm(TUITION_FEE ~ SOCIOECONOMIC_STATUS + 
                    FATHER_INCOME + MOTHER_INCOME +
                    MONTHLY_SCHOOL_FEE + PBM, data = data)

# Compute Cook's Distance
cooksD <- cooks.distance(final_model)

# Define threshold (4/n rule)
threshold <- 4 / nrow(data)

# Identify high-influence points
outliers <- which(cooksD > threshold)

num_outliers_removed <- length(outliers)

# Print the result
cat("Number of outliers removed:", num_outliers_removed, "\n")

outlier_data <- data[outliers, c("TUITION_FEE", "SOCIOECONOMIC_STATUS", "FATHER_INCOME", 
                                 "MOTHER_INCOME", "MONTHLY_SCHOOL_FEE", "PBM")]

# Display the extracted values
print(outlier_data)

# Remove outliers
cleaned_data <- data[-outliers, ]

# Refit the model with cleaned data
final_model_cleaned <- lm(TUITION_FEE ~ SOCIOECONOMIC_STATUS + 
                            FATHER_INCOME + MOTHER_INCOME +
                            MONTHLY_SCHOOL_FEE + PBM, data = cleaned_data)

# Display summary of cleaned model
summary(final_model_cleaned)

# overlap with non cleaned data
par(mfrow = c(2, 2))

# Residuals vs Fitted (Homoscedasticity Check)
plot(final_model$fitted.values, final_model$residuals, 
     main = "Residuals vs Fitted", 
     xlab = "Fitted Values", ylab = "Residuals", 
     pch = 16, col = "blue")
points(final_model_cleaned$fitted.values, final_model_cleaned$residuals, 
       pch = 16, col = "red")
abline(h = 0, col = "black", lwd = 2)

# Q-Q Plot (Normality Check)
qqnorm(final_model$residuals, main = "Normal Q-Q Plot", col = "blue", pch = 16)
qqline(final_model$residuals, col = "blue", lwd = 2)
points(qqnorm(final_model_cleaned$residuals, plot.it = FALSE), 
       col = "red", pch = 16)
qqline(final_model_cleaned$residuals, col = "red", lwd = 2)

# Scale-Location Plot (Variance Homogeneity)
plot(final_model$fitted.values, sqrt(abs(final_model$residuals)), 
     main = "Scale-Location Plot", 
     xlab = "Fitted Values", ylab = "√|Residuals|", 
     pch = 16, col = "blue")
points(final_model_cleaned$fitted.values, sqrt(abs(final_model_cleaned$residuals)), 
       pch = 16, col = "red")
abline(h = 0, col = "black", lwd = 2)

# Cook's Distance (Outlier Detection)
plot(cooks.distance(final_model), type = "h", 
     main = "Cook's Distance", 
     xlab = "Observation", ylab = "Cook's Distance", 
     col = "blue")
points(cooks.distance(final_model_cleaned), type = "h", col = "red")
abline(h = 4/(nrow(data)), col = "blue", lwd = 2, lty = 2)
abline(h = 4/(nrow(cleaned_data)), col = "red", lwd = 2, lty = 2)

# Reset plot layout
par(mfrow = c(1, 1))


