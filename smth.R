library(stats)
library(ggplot2)

#linear
linear_model <- lm(TUITION_FEE ~ SOCIOECONOMIC_STATUS + 
                     FATHER_INCOME + MOTHER_INCOME +
                     MONTHLY_SCHOOL_FEE + PBM, 
                   data = data)

cooksD_linear <- cooks.distance(linear_model)
outliers_linear <- which(cooksD_linear > 4/nrow(data))
cleaned_data_linear <- data[-outliers_linear, ]

final_linear <- lm(TUITION_FEE ~ SOCIOECONOMIC_STATUS + 
                     FATHER_INCOME + MOTHER_INCOME +
                     MONTHLY_SCHOOL_FEE + PBM, 
                   data = cleaned_data_linear)

#poly
poly_model <- lm(TUITION_FEE ~ poly(SOCIOECONOMIC_STATUS, 4) + 
                   poly(FATHER_INCOME, 1) + 
                   poly(MOTHER_INCOME, 2) + 
                   poly(MONTHLY_SCHOOL_FEE, 4) + PBM, 
                 data = data)

cooksD_poly <- cooks.distance(poly_model)
outliers_poly <- which(cooksD_poly > 4/nrow(data))
cleaned_data_poly <- data[-outliers_poly, ]

final_poly <- lm(TUITION_FEE ~ poly(SOCIOECONOMIC_STATUS, 4) + 
                   poly(FATHER_INCOME, 1) + 
                   poly(MOTHER_INCOME, 2) + 
                   poly(MONTHLY_SCHOOL_FEE, 4) + PBM, 
                 data = cleaned_data_poly)

#comparison function
compare_models <- function(linear_model, poly_model) {
  metrics <- data.frame(
    Metric = c("AIC", "R-squared", "Adjusted R-squared"),
    Linear_Model = c(
      AIC(linear_model),
      summary(linear_model)$r.squared,
      summary(linear_model)$adj.r.squared
    ),
    Polynomial_Model = c(
      AIC(poly_model),
      summary(poly_model)$r.squared,
      summary(poly_model)$adj.r.squared
    )
  )
  return(metrics)
}

#comparison
model_comparison <- compare_models(final_linear, final_poly)
print(model_comparison)
