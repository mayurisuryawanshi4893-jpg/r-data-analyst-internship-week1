# Week 1 - Data Cleaning and Preliminary Analysis with R
# Dataset: IBM Telco Customer Churn
# Source: https://raw.githubusercontent.com/IBM/telco-customer-churn-on-icp4d/master/data/Telco-Customer-Churn.csv

# 1. Load packages
library(dplyr)
library(ggplot2)

# 2. Load dataset
url <- "https://raw.githubusercontent.com/IBM/telco-customer-churn-on-icp4d/master/data/Telco-Customer-Churn.csv"
df <- read.csv(url, stringsAsFactors = FALSE)

# 3. Initial inspection
dim(df)
str(df)
summary(df)

# 4. Check duplicates
sum(duplicated(df$customerID))

# 5. Handle blank values in TotalCharges and convert to numeric
df$TotalCharges <- trimws(df$TotalCharges)
df$TotalCharges[df$TotalCharges == ""] <- NA
df$TotalCharges <- as.numeric(df$TotalCharges)

# Check missing values
colSums(is.na(df))

# 6. Remove rows with missing TotalCharges
df_clean <- df %>% filter(!is.na(TotalCharges))

# 7. Convert categorical variables to factors
factor_cols <- c("gender","Partner","Dependents","PhoneService",
                 "MultipleLines","InternetService","OnlineSecurity",
                 "OnlineBackup","DeviceProtection","TechSupport",
                 "StreamingTV","StreamingMovies","Contract",
                 "PaperlessBilling","PaymentMethod","Churn")
df_clean[factor_cols] <- lapply(df_clean[factor_cols], factor)

# Treat SeniorCitizen as categorical
df_clean$SeniorCitizen <- factor(df_clean$SeniorCitizen,
                                 levels = c(0,1),
                                 labels = c("No","Yes"))

# 8. Outlier detection using IQR
outlier_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm=TRUE)
  q3 <- quantile(x, 0.75, na.rm=TRUE)
  iqr <- q3-q1
  lower <- q1 - 1.5*iqr
  upper <- q3 + 1.5*iqr
  sum(x < lower | x > upper, na.rm=TRUE)
}

outlier_iqr(df_clean$tenure)
outlier_iqr(df_clean$MonthlyCharges)
outlier_iqr(df_clean$TotalCharges)

# 9. Min-max normalization for numerical variables
minmax <- function(x) (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
df_clean$tenure_norm <- minmax(df_clean$tenure)
df_clean$MonthlyCharges_norm <- minmax(df_clean$MonthlyCharges)
df_clean$TotalCharges_norm <- minmax(df_clean$TotalCharges)

# 10. Binary encoding example
df_clean$Churn_binary <- ifelse(df_clean$Churn == "Yes", 1, 0)

# 11. Summary statistics
summary(df_clean[, c("tenure","MonthlyCharges","TotalCharges")])

# 12. Churn distribution
table(df_clean$Churn)
prop.table(table(df_clean$Churn))*100

# 13. Churn by contract
contract_churn <- prop.table(table(df_clean$Contract, df_clean$Churn), margin=1)*100
round(contract_churn, 2)

# 14. Grouped statistics
df_clean %>%
  group_by(Churn) %>%
  summarise(
    mean_tenure = mean(tenure),
    median_tenure = median(tenure),
    mean_monthly = mean(MonthlyCharges),
    median_monthly = median(MonthlyCharges)
  )

# 15. Correlation
cor(df_clean[, c("tenure","MonthlyCharges","TotalCharges")])

# 16. Visualizations
ggplot(df_clean, aes(x=Churn)) +
  geom_bar() +
  labs(title="Customer Churn Distribution", x="Churn", y="Number of Customers")

ggplot(df_clean, aes(x=Contract, fill=Churn)) +
  geom_bar(position="fill") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Churn Rate by Contract Type", y="Percentage")

ggplot(df_clean, aes(x=Churn, y=MonthlyCharges)) +
  geom_boxplot() +
  labs(title="Monthly Charges by Churn Status")

ggplot(df_clean, aes(x=tenure, fill=Churn)) +
  geom_histogram(bins=18, alpha=0.55, position="identity") +
  labs(title="Tenure Distribution by Churn Status")
