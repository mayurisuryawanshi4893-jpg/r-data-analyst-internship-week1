# Week 2 - Data Visualization and Insight Communication using R
library(dplyr)
library(ggplot2)

url <- "https://raw.githubusercontent.com/IBM/telco-customer-churn-on-icp4d/master/data/Telco-Customer-Churn.csv"
df <- read.csv(url, stringsAsFactors=FALSE)

df$TotalCharges <- trimws(df$TotalCharges)
df$TotalCharges[df$TotalCharges==""] <- NA
df$TotalCharges <- as.numeric(df$TotalCharges)
df <- df %>% filter(!is.na(TotalCharges))

# 1. Bar chart
ggplot(df, aes(x=Churn)) + geom_bar() +
  labs(title="Customer Churn Distribution", x="Churn Status", y="Number of Customers") +
  theme_minimal()

# 2. Histogram
ggplot(df, aes(x=tenure)) + geom_histogram(bins=20) +
  labs(title="Distribution of Customer Tenure", x="Tenure (Months)", y="Number of Customers") +
  theme_minimal()

# 3. Scatter plot
ggplot(df, aes(x=tenure, y=TotalCharges)) +
  geom_point(alpha=0.35) + geom_smooth(method="lm", se=FALSE) +
  labs(title="Tenure vs Total Charges", x="Tenure (Months)", y="Total Charges") +
  theme_minimal()

# 4. Churn rate by contract
contract_summary <- df %>% group_by(Contract) %>% summarise(ChurnRate=mean(Churn=="Yes")*100)
ggplot(contract_summary, aes(x=Contract, y=ChurnRate)) + geom_col() +
  labs(title="Churn Rate by Contract Type", x="Contract Type", y="Churn Rate (%)") +
  theme_minimal()

# 5. Line chart
monthly_summary <- df %>% group_by(tenure) %>% summarise(AvgMonthlyCharges=mean(MonthlyCharges))
ggplot(monthly_summary, aes(x=tenure, y=AvgMonthlyCharges)) +
  geom_line() + geom_point() +
  labs(title="Average Monthly Charges by Tenure", x="Tenure (Months)", y="Average Monthly Charges") +
  theme_minimal()

# 6. Box plot
ggplot(df, aes(x=Churn, y=MonthlyCharges)) + geom_boxplot() +
  labs(title="Monthly Charges by Churn Status", x="Churn", y="Monthly Charges") +
  theme_minimal()
