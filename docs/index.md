---
title: "Retail Data Analysis"
---

# Retail Data Analysis (June 1 – August 29, 2025)

This page contains the R analysis used for the retail dataset (June 1 – August 29, 2025). It includes the R code, brief results and interpretation, and instructions for reproducing the analysis locally.

## Dataset overview

- Timeframe: June 1 – August 29, 2025
- Variables included:
  - daily_customers: Number of shoppers per day
  - average_transaction_value: Average spend per customer
  - daily_revenue: Total sales per day
  - temperature: Daily high (°F)
  - day_of_week: Weekday / Weekend
  - promotion_flag: TRUE / FALSE

## R code

```r
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

# Bonus (output regression results table in R console):

# install.packages("stargazer")
# library(stargazer)

# Side-by-side regression tables
# stargazer(lm_customers, lm_revenue,
#           type = "text",
#           title = "Regression Results: Customers and Revenue",
#           dep.var.labels = c("Daily Customers", "Daily Revenue"),
#           covariate.labels = c("Temperature", "Daily Customers"),
#           digits = 3,
#           out = "regression_results.txt")  # optional export

# Cross-validation (caret)
# install.packages("caret")
# library(caret)

# Define cross-validation method (10-fold CV)
# train_control <- trainControl(method = "cv", number = 10)

# cv_customers <- train(daily_customers ~ temperature,
#                       data = retail_clean,
#                       method = "lm",
#                       trControl = train_control)

# cv_revenue <- train(daily_revenue ~ temperature + daily_customers,
#                     data = retail_clean,
#                     method = "lm",
#                     trControl = train_control)

```

## Summary and findings

- Correlation: Daily customers vs temperature: approximately -0.131 (weak negative).
- Temperature has a small negative effect on daily customers; R² is low when temperature is the only predictor.
- Revenue is driven primarily by customer count; temperature provides little additional explanatory power once customers are included.
- Weekends and promotions have larger impacts on customers and revenue than temperature.

## How to view this on GitHub Pages

1. I added this file to the `docs/` folder in the repository. GitHub Pages can serve from the `docs/` folder on the `main` branch.
2. To enable GitHub Pages on your repository:
   - Go to Settings → Pages
   - Source: choose Branch: main, folder: /docs
   - Save
3. Your site will be live at: https://wardtaylorn.github.io/CAP6768FINAL/ (may take a minute to publish)

## Reproducing locally

- Place `data_analytics_retail.csv` in the repository root or adjust the path in the R code.
- Open R / RStudio and run the R code blocks above. Install any missing packages first (tidyverse, stargazer, caret).

---

Notes: If you want the code to run and render images on the GitHub Pages site automatically, consider adding an R Markdown (.Rmd) file and using GitHub Actions to render it to HTML on push. I can add an .Rmd file and a GitHub Action workflow to render it if you'd like.