# Oluwatomisin Jegede, ALY 6000, 05/04/2024

diabetes <- read.csv("diabetes.csv", TRUE)

diabetes <- clean_names(diabetes)

glimpse(diabetes)

# removing empty rows
diabetes <- na.omit(diabetes)

# removing unwanted columns
diabetes <- diabetes %>%
  select(-skin_thickness, -diabetes_pedigree_function)

summary(diabetes)

#Visualizations for data questions
# Histogram for Glucose levels by Outcome

ggplot(diabetes, aes(x = glucose, fill = factor(outcome))) + 
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  labs(title = "Distribution of Glucose Levels by Diabetes Outcome",
       x = "Glucose",
       y = "Count",
       fill = "outcome") +
  scale_fill_manual(values = c("lightblue", "salmon"))


# Boxplot for BMI by Outcome

ggplot(diabetes, aes(x = factor(outcome), y = bmi, fill = factor(outcome))) +
  geom_boxplot() +
  labs(title = "BMI Distribution by Diabetes Outcome",
       x = "Outcome",
       y = "BMI",
       fill = "Outcome") +
  scale_fill_manual(values = c("lightblue", "salmon"))  


# Scatter plot for Glucose vs. Age colored by Outcome

ggplot(diabetes, aes(x = glucose, y = age, color = factor(outcome))) + 
  geom_point() +
  labs(title = "Glucose vs. Age by Diabetes Outcome",
       x = "Glucose",
       y = "Age",
       color = "outcome")
  
# Create new variables

diabetes$bmi_category <- cut(diabetes$bmi, 
                             breaks = c(0, 18.5, 24.9, 29.9, Inf), 
                             labels = c("Underweight", "Normal Weight", 
                                        "Overweight", "Obese"))

diabetes$age_group <- cut(diabetes$age, 
                          breaks = c(20, 30, 50, Inf), 
                          labels = c("21-30", "31-50", "51+"))

diabetes$diabetes_risk_score <- diabetes$glucose + 
  diabetes$bmi + diabetes$age 

# Grouping and summarizing data

# Rank patients based on their diabetes risk score

diabetes$risk_rank <- rank(-diabetes$diabetes_risk_score)  

# Create a new column to categorize risk levels (e.g., High Risk, Medium Risk, Low Risk)

diabetes$risk_level <- ifelse(diabetes$risk_rank <= 20, "High Risk",
                        ifelse(diabetes$risk_rank <= 50, "Medium Risk", "Low Risk"))


# Display top 10 high-risk individuals

high_risk_individuals <- diabetes[diabetes$risk_level == "High Risk", ]

head(high_risk_individuals, 10)  # 


# Visualization of BMI categories by Diabetes Outcome

ggplot(diabetes, aes(x = bmi_category, fill = factor(outcome))) + 
  geom_bar(position = "dodge") +
  labs(title = "BMI Categories by Diabetes Outcome",
       x = "BMI Category",
       y = "Count",
       fill = "Outcome") +
  scale_fill_manual(values = c("lightblue", "salmon")) 


# Heat map of correlation matrix

correlation_matrix <- cor(diabetes[, c("glucose", "bmi", "age", "outcome")])

heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(50),
        main = "Correlation Matrix")



# Line chart of Glucose levels by Age Group and Diabetes Outcome

ggplot(diabetes, aes(x = age, y = glucose, color = factor(outcome), group = age_group)) + 
  geom_line() +
  labs(title = "Trend of Glucose Levels by Age Group and Diabetes Outcome",
       x = "Age",
       y = "Glucose",
       color = "Outcome",
       group = "Age Group")


