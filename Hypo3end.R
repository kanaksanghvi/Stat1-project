library(ggplot2)

# Create a dataframe with the SES group-wise percentages
df <- data.frame(
  SES_Group = rep(0:7, each=5),
  Category = rep(c("Very Low", "Low", "Medium", "High", "Very High"), times=8),
  Percentage = c(27.27, 18.18, 18.18, 36.36, 0.00,
                 23.65, 29.79, 16.17, 16.32, 14.07,
                 24.78, 23.60, 17.78, 17.23, 16.60,
                 18.14, 19.92, 18.61, 20.58, 22.74,
                 7.55, 14.34, 18.87, 23.77, 35.47,
                 1.64, 16.39, 16.39, 26.23, 39.34,
                 8.33, 33.33, 0.00, 25.00, 33.33,
                 0.00, 33.33, 33.33, 33.33, 0.00)
)

# Ensure the Category order from Very Low to Very High
df$Category <- factor(df$Category, levels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Create a stacked bar plot with a blue gradient
ggplot(df, aes(x = factor(SES_Group), y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#d0ebff", "#74c0fc", "#339af0", "#1c7ed6", "#0b5394")) +
  labs(title = "SES Group-wise Distribution of Students by Category", 
       x = "SES Group", 
       y = "Percentage of Students", 
       fill = "Category") +
  theme_minimal()