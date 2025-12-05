# CAP6768FINAL
r code for final project



R code:

# Load required libraries
library(tidyverse)

# Import dataset
retail <- read_csv("data_analytics_retail.csv")

# Inspect data
glimpse(retail)

# Handle missing values (temperature has some NAs)
retail_clean <- retail %>%
  filter(!is.na(temperature))

# 1. Correlation Analysis
# -------------------------------

# Correlation between daily customers and temperature
correlation <- cor(retail_clean$daily_customers, retail_clean$temperature)
print(paste("Correlation between customers and temperature:", round(correlation, 3)))

# 2. Visualization: Customers vs Temperature
# -------------------------------

ggplot(retail_clean, aes(x = temperature, y = daily_customers)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(title = "Daily Customers vs Temperature",
       x = "Temperature (°F)",
       y = "Number of Customers") +
  theme_minimal()

# 3. Linear Regression: Customers ~ Temperature
# -------------------------------

lm_customers <- lm(daily_customers ~ temperature, data = retail_clean)
summary(lm_customers)

# 4. Multiple Regression: Revenue ~ Temperature + Customers
# -------------------------------

lm_revenue <- lm(daily_revenue ~ temperature + daily_customers, data = retail_clean)
summary(lm_revenue)

# 5. Visualization: Revenue vs Temperature & Customers
# -------------------------------

# Scatterplot of revenue vs temperature
ggplot(retail_clean, aes(x = temperature, y = daily_revenue)) +
  geom_point(color = "forestgreen", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange") +
  labs(title = "Daily Revenue vs Temperature",
       x = "Temperature (°F)",
       y = "Revenue ($)") +
  theme_minimal()

# Scatterplot of revenue vs customers
ggplot(retail_clean, aes(x = daily_customers, y = daily_revenue)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Daily Revenue vs Customers",
       x = "Number of Customers",
       y = "Revenue ($)") +
  theme_minimal()

# 6. Extra: Facet by Day of Week
# -------------------------------

ggplot(retail_clean, aes(x = temperature, y = daily_customers, color = day_of_week)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Customer Trends by Day of Week",
       x = "Temperature (°F)",
       y = "Number of Customers") +
  theme_minimal()

Bonus (output regression results table in R console):

install.packages("stargazer")

library(stargazer)

# Side-by-side regression tables
stargazer(lm_customers, lm_revenue,
          type = "text",
          title = "Regression Results: Customers and Revenue",
          dep.var.labels = c("Daily Customers", "Daily Revenue"),
          covariate.labels = c("Temperature", "Daily Customers"),
          digits = 3,
          out = "regression_results.txt")  # optional export


install.packages("caret")

library(caret)

# Define cross-validation method (10-fold CV)
train_control <- trainControl(method = "cv", number = 10)

# Model 1: Customers ~ Temperature
cv_customers <- train(daily_customers ~ temperature,
                      data = retail_clean,
                      method = "lm",
                      trControl = train_control)

print(cv_customers)

# Model 2: Revenue ~ Temperature + Customers
cv_revenue <- train(daily_revenue ~ temperature + daily_customers,
                    data = retail_clean,
                    method = "lm",
                    trControl = train_control)

print(cv_revenue)
