# Week 3 - Statistical Analysis and Predictive Modeling using R
library(dplyr)
library(caret)
library(pROC)
library(ggplot2)

# 1. Load data
url <- "https://raw.githubusercontent.com/IBM/telco-customer-churn-on-icp4d/master/data/Telco-Customer-Churn.csv"
df <- read.csv(url, stringsAsFactors=FALSE)

# 2. Clean TotalCharges
df$TotalCharges <- trimws(df$TotalCharges)
df$TotalCharges[df$TotalCharges==""] <- NA
df$TotalCharges <- as.numeric(df$TotalCharges)
df <- df %>% filter(!is.na(TotalCharges))

# 3. Hypothesis test: monthly charges and churn
t_test <- t.test(MonthlyCharges ~ Churn, data=df)
print(t_test)

# H0: mean MonthlyCharges is equal for churned and non-churned customers.
# H1: mean MonthlyCharges differs between the groups.

# 4. Chi-square test: contract and churn
chi_test <- chisq.test(table(df$Contract, df$Churn))
print(chi_test)

# H0: Contract and Churn are independent.
# H1: Contract and Churn are associated.

# 5. Correlation and normality check
cor.test(df$tenure, df$TotalCharges, method="pearson")
shapiro.test(sample(df$MonthlyCharges, min(5000,nrow(df))))

# 6. Create modeling data
df$Churn <- factor(df$Churn, levels=c("No","Yes"))
df$SeniorCitizen <- factor(df$SeniorCitizen)
df$PaperlessBilling <- factor(df$PaperlessBilling)
df$Partner <- factor(df$Partner)
df$Dependents <- factor(df$Dependents)

model_df <- df %>% select(Churn, tenure, MonthlyCharges, TotalCharges,
                           Contract, InternetService, PaymentMethod,
                           PaperlessBilling, SeniorCitizen, Partner, Dependents)

# 7. Stratified 80/20 split
set.seed(42)
idx <- createDataPartition(model_df$Churn, p=.80, list=FALSE)
train <- model_df[idx,]
test <- model_df[-idx,]

# 8. Logistic regression
fit <- glm(Churn ~ tenure + MonthlyCharges + TotalCharges +
           Contract + InternetService + PaymentMethod +
           PaperlessBilling + SeniorCitizen + Partner + Dependents,
           data=train, family=binomial)

summary(fit)

# 9. Predictions and confusion matrix
prob <- predict(fit, newdata=test, type="response")
pred <- factor(ifelse(prob >= .50, "Yes", "No"), levels=c("No","Yes"))
cm <- confusionMatrix(pred, test$Churn, positive="Yes")
print(cm)

# 10. ROC-AUC
roc_obj <- roc(test$Churn, prob, levels=c("No","Yes"), direction="<")
print(auc(roc_obj))
plot(roc_obj, main="ROC Curve - Logistic Regression")

# 11. 5-fold cross-validation
ctrl <- trainControl(method="cv", number=5, classProbs=TRUE,
                     summaryFunction=twoClassSummary)
cv_fit <- train(Churn ~ tenure + MonthlyCharges + TotalCharges +
                Contract + InternetService + PaymentMethod +
                PaperlessBilling + SeniorCitizen + Partner + Dependents,
                data=model_df, method="glm", family=binomial,
                metric="ROC", trControl=ctrl)
print(cv_fit)

# 12. Diagnostics
par(mfrow=c(2,2))
plot(fit)

# 13. Improvement ideas:
# - Try regularized logistic regression (glmnet)
# - Compare Random Forest / Gradient Boosting
# - Tune probability threshold for business costs
# - Handle class imbalance with resampling or class weights
