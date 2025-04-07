library(tidyverse)
library(ggplot2)
library(corrplot)
library(dplyr)


library(tidyverse)

# Load CSV
data <- read.csv("C:/Users/ANJAN MAJHI/Downloads/Stat 1 Project/Data-1.csv")

# Check structure
str(data)

# Handle missing values
data_clean <- na.omit(data)

str(data)   # Check the structure of the dataset
head(data)  # Show first few rows
summary(data)  # Summary statistics
dim(data)   # Number of rows and columns


# View the first few rows
head(data)

# Check column names
colnames(data)

# Check data types
str(data)

# Summary statistics
summary(data)



#### Bar Chart: Distribution of Socioeconomic Status (1)

library(ggplot2)

ggplot(data, aes(x = SOCIOECONOMIC_STATUS)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Socioeconomic Status",
       x = "Socioeconomic Status",
       y = "Count") +
  theme_minimal()

#### Distribution of school type (2)

library(ggplot2)

ggplot(data, aes(x = SCHOOL_TYPE, fill = SCHOOL_TYPE)) +
  geom_bar() +
  scale_fill_manual(values = c("steelblue", "darkorange")) +  # Custom colors
  labs(title = "Distribution of School Type Attended by Students",
       x = "School Type",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend (optional)



#### Parent Residence pie (3)

library(ggplot2)
library(dplyr)

# Count the number of students per residence type
residence_data <- data %>%
  count(PARENT_RESIDENCE_LOCATION)

# Create the pie chart
ggplot(residence_data, aes(x = "", y = n, fill = factor(PARENT_RESIDENCE_LOCATION))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Proportion of Parent Residence Location", fill = "Residence Type") +
  theme_void()


#### Scholarship Pie (4)


# Count the number of students with/without scholarships
scholarship_data <- data %>%
  count(SCHOLARSHIP)

# Create the pie chart
ggplot(scholarship_data, aes(x = "", y = n, fill = factor(SCHOLARSHIP))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Proportion of Students Receiving Scholarships", fill = "Scholarship") +
  theme_void()


#### Socioeconomic Status by Career Stacked bar (5)

library(ggplot2)
library(dplyr)

# Convert SOCIOECONOMIC_STATUS to numeric (ensures correct color mapping)
data$SOCIOECONOMIC_STATUS <- as.numeric(as.character(data$SOCIOECONOMIC_STATUS))

# Summarize: Count students by Career and Socioeconomic Status





for(i in data$CAREER_NAME){

}


career_counts <- data %>%
  group_by(CAREER_NAME, SOCIOECONOMIC_STATUS) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(Proportion = n / sum(n))  # Convert counts to proportions

career_counts
# Plot horizontal stacked bar chart with Dark Blue → Light Blue → White gradient
ggplot(career_counts, aes(y = reorder(CAREER_NAME, -Proportion), x = Proportion, fill = SOCIOECONOMIC_STATUS)) +
  geom_col(position = "stack", width = 0.6, color = "black") +  # Adjust width for spacing
  scale_fill_gradientn(colors = c("#00008B", "#1E90FF", "#ADD8E6", "#FAF9F6")) +  # Dark Blue → Light Blue → White
  labs(title = "Distribution of Socioeconomic Status by Career",
       y = "Career",
       x = "Proportion",
       fill = "Socioeconomic Status") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),  
        legend.position = "bottom",
        panel.grid.major.y = element_blank())  # Remove horizontal grid lines

#newcopy1
ggplot(career_counts, aes(y = reorder(CAREER_NAME, -n), x = Proportion, fill = SOCIOECONOMIC_STATUS)) +
  geom_col(position = "stack", width = 0.6, color = "black") +  # Adjust width for spacing
  scale_fill_gradientn(colors = c("#00008B", "#1E90FF", "#ADD8E6", "#FAF9F6")) +  # Dark Blue → Light Blue → White
  labs(title = "Distribution of Socioeconomic Status by Career",
       y = "Career",
       x = "Proportion",
       fill = "Socioeconomic Status") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),  
        legend.position = "bottom",
        panel.grid.major.y = element_blank())  # Remove horizontal grid lines

#### (6) Tuition Fees vs Socioeconomic status by campus Scatter plot

library(ggplot2)
library(dplyr)
library(scales)  # For formatting tuition fees

# Ensure variables are in the correct format
data$SOCIOECONOMIC_STATUS <- as.numeric(as.character(data$SOCIOECONOMIC_STATUS))
data$PROGRAM_LOCATION_CODE <- as.factor(data$PROGRAM_LOCATION_CODE)  # Campus as a categorical variable

# Scatter plot of Tuition Fees vs. Campus, colored by Socioeconomic Status
ggplot(data, aes(x = PROGRAM_LOCATION_CODE, y = TUITION_FEE, color = SOCIOECONOMIC_STATUS)) +
  geom_jitter(alpha = 0.7, size = 1.2, width = 0.2) +  # **Smaller dots (size = 1.2)**
  scale_color_gradient(low = "red", high = "green") +  # Dark-to-light blue gradient for status
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +  # Convert tuition fees to millions
  labs(title = "Distribution of Tuition Fees by Campus and Socioeconomic Level",
       x = "Campus Code",
       y = "Tuition Fee (Millions COP)",
       color = "Socioeconomic Status") +
  theme_minimal() +
  theme(legend.position = "right")

#### Correlation matrix with numerical attributes (7)

library(ggplot2)
library(dplyr)
library(corrplot)

# Ensure numeric columns
data <- data %>%
  mutate(across(c(TUITION_FEE, SOCIOECONOMIC_STATUS, FATHER_INCOME, MOTHER_INCOME, SIBLING_COUNT, FACULTY_CODE), as.numeric))

# Select only numeric columns including FACULTY_CODE
numeric_data <- data %>%
  select(TUITION_FEE, SOCIOECONOMIC_STATUS, FATHER_INCOME, MOTHER_INCOME, SIBLING_COUNT, FACULTY_CODE) %>%
  na.omit()  # Remove missing values

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Open a fresh plotting window (fixes text margin issue)
plot.new()
par(mar = c(2, 2, 2, 2))  # Adjust margins to prevent text overlap

# Plot full correlation matrix with Deep Blue → Lime Green → Yellow gradient
corrplot(cor_matrix, method = "color", type = "full",
         col = colorRampPalette(c("darkblue", "blue", "lightblue", "limegreen", "yellow"))(200),  # **Gradient Updated**
         tl.col = "black", tl.srt = 45, tl.cex = 0.7,  # Adjusted text size
         number.cex = 0.8, addCoef.col = "black")  # Show correlation values

#### Correlation matrix with non-numerical attributes (8)

library(ggplot2)
library(dplyr)
library(corrplot)

# Convert categorical variables to numeric
data_transformed <- data %>%
  mutate(
    CAREER_CODE = as.numeric(as.factor(CAREER_CODE)),
    FACULTY_CODE = as.numeric(as.factor(FACULTY_CODE)),
    PROGRAM_LOCATION_CODE = as.numeric(as.factor(PROGRAM_LOCATION_CODE)),
    SCHOOL_TYPE = as.numeric(as.factor(SCHOOL_TYPE)),
    PARENT_RESIDENCE_LOCATION = as.numeric(as.factor(PARENT_RESIDENCE_LOCATION)),
    RESIDENCE_TYPE = as.numeric(as.factor(RESIDENCE_TYPE))
  )  # Convert categorical variables to numeric

# Select numeric + transformed categorical columns
full_numeric_data <- data_transformed %>%
  select(CAREER_CODE, TUITION_FEE, SOCIOECONOMIC_STATUS, FATHER_INCOME, MOTHER_INCOME, 
         PARENT_RESIDENCE_LOCATION, RESIDENCE_TYPE, SCHOOL_TYPE) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%  # **Force numeric conversion**
  na.omit()  # Remove missing values

# Check structure after conversion
str(full_numeric_data)

# Compute correlation matrix
cor_matrix_full <- cor(full_numeric_data, use = "complete.obs")

# Open a fresh plotting window (fixes text margin issue)
plot.new()
par(mar = c(2, 2, 2, 2))  # Adjust margins to prevent text overlap

# Plot full correlation matrix with Deep Blue → Lime Green → Yellow gradient
corrplot(cor_matrix_full, method = "color", type = "full",
         col = colorRampPalette(c("darkblue", "blue", "lightblue", "limegreen", "yellow"))(200),
         tl.col = "black", tl.srt = 45, tl.cex = 0.7,
         number.cex = 0.8, addCoef.col = "black")  # Show correlation values
