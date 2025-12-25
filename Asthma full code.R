# FINAL PROJECT – ASTHMA MANAGEMENT
# Data-Driven Insights into Asthma Risk Factors & Outcomes

# 1. SETUP 

library(dplyr)
library(ggplot2)
library(glmnet)
library(pROC)
library(randtests)
library(MASS)      # For ordinal logistic regression
library(nnet)      # For multinomial logistic regression
set.seed(70836)


# 2. LOAD DATA

asthma <- read.csv(
  "~/Documents/ALY6015/final project/synthetic_asthma_dataset.csv",
  stringsAsFactors = TRUE
)

str(asthma)
summary(asthma)


# 3. EDA 

num_vars <- c(
  "Age", "BMI", "Medication_Adherence",
  "Number_of_ER_Visits", "Peak_Expiratory_Flow",
  "FeNO_Level", "Family_History", "Has_Asthma"
)

summary(asthma[, num_vars])

table(asthma$Gender)
table(asthma$Smoking_Status)
table(asthma$Air_Pollution_Level)
table(asthma$Comorbidities)
table(asthma$Has_Asthma)
table(asthma$Asthma_Control_Level)


# 4. VISUALIZATIONS 

# 4.1 Age distribution
ggplot(asthma, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "#915833", color = "white") +
  scale_x_continuous(breaks = seq(0, 90, 10)) +
  labs(
    title = "Distribution of Age",
    subtitle = "Ages are approximately uniformly distributed across the cohort",
    x = "Age (years)",
    y = "Number of Patients"
  ) +
  theme_classic(base_size = 14)

# 4.2 BMI distribution 
ggplot(asthma, aes(x = BMI)) +
  geom_histogram(bins = 30, fill = "#009688", color = "white") +
  geom_vline(aes(xintercept = median(BMI, na.rm = TRUE)),
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribution of BMI",
    subtitle = "Red dashed line shows the median BMI",
    x = "BMI",
    y = "Number of Patients"
  ) +
  theme_classic(base_size = 14)

# 4.3 ER visits distribution
ggplot(asthma, aes(x = Number_of_ER_Visits)) +
  geom_histogram(binwidth = 1, fill = "#FF9800", color = "white",
                 boundary = 0, closed = "left") +
  scale_x_continuous(breaks = 0:6) +
  labs(
    title = "Distribution of ER Visits",
    subtitle = "Most patients have 0–2 emergency visits per year",
    x = "Number of ER Visits",
    y = "Number of Patients"
  ) +
  theme_classic(base_size = 14)

# 4.4 ER visits by asthma control level 
ggplot(asthma, aes(x = Asthma_Control_Level,
                   y = Number_of_ER_Visits,
                   fill = Asthma_Control_Level)) +
  geom_boxplot() +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(..y.., 1)),
    vjust = -0.5,
    color = "black",
    size = 3
  ) +
  scale_fill_manual(values = c(
    "N/A"              = "#BDBDBD",
    "Not Controlled"   = "#EF5350",
    "Poorly Controlled"= "#FFA726",
    "Well Controlled"  = "#66BB6A"
  )) +
  labs(
    title = "ER Visits by Asthma Control Level",
    subtitle = "Numeric labels indicate median ER visits in each group",
    x = "Asthma Control Level",
    y = "Number of ER Visits"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

# 4.5 Severity by Air Pollution (NEW)
asthma$Asthma_Control_Level <- factor(
  asthma$Asthma_Control_Level,
  levels = c("Well Controlled", "Poorly Controlled", "Not Controlled"),
  ordered = TRUE
)

asthma_only <- subset(asthma, Has_Asthma == 1)
asthma_only$Control_Ordered <- asthma_only$Asthma_Control_Level

ggplot(asthma_only, aes(x = Air_Pollution_Level, fill = Control_Ordered)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c(
    "Well Controlled" = "#66BB6A",
    "Poorly Controlled" = "#FFA726",
    "Not Controlled" = "#EF5350"
  )) +
  labs(
    title = "Asthma Severity by Air Pollution Level",
    subtitle = "Proportion of patients in each control category",
    x = "Air Pollution Level",
    y = "Proportion",
    fill = "Control Level"
  ) +
  theme_classic(base_size = 14)

# 4.6 Medication Adherence by Severity 
ggplot(asthma_only, aes(x = Control_Ordered, y = Medication_Adherence, 
                        fill = Control_Ordered)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Well Controlled" = "#66BB6A",
    "Poorly Controlled" = "#FFA726",
    "Not Controlled" = "#EF5350"
  )) +
  labs(
    title = "Medication Adherence by Asthma Severity",
    x = "Asthma Control Level",
    y = "Medication Adherence"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")


# 5. CORRELATION MATRIX

numeric_df <- asthma[, num_vars]
cor_matrix <- cor(numeric_df, use = "complete.obs")
cor_matrix


# REGRESSION MODELS 

# 6. MULTIPLE LINEAR REGRESSION 

lm_fit <- lm(Number_of_ER_Visits ~ Age + BMI +
               Family_History + Medication_Adherence +
               FeNO_Level + Has_Asthma,
             data = asthma)

summary(lm_fit)


# 7. MULTIPLE LINEAR REGRESSION WITH AIR QUALITY
# Research Question: Does air pollution affect ER visits?

lm_fit_air <- lm(Number_of_ER_Visits ~ Age + BMI +
                   Family_History + Medication_Adherence +
                   FeNO_Level + Has_Asthma + Air_Pollution_Level,
                 data = asthma)

summary(lm_fit_air)


# 8. INTERACTION MODEL: MEDICATION × AIR QUALITY
# Research Question 3: How do medication adherence and air quality 
# JOINTLY affect patient outcomes?

interaction_model <- lm(
  Number_of_ER_Visits ~ Medication_Adherence * Air_Pollution_Level + 
    Age + BMI + Family_History + Has_Asthma + Smoking_Status,
  data = asthma
)

summary(interaction_model)

# Compare with main effects only model
main_effects_model <- lm(
  Number_of_ER_Visits ~ Medication_Adherence + Air_Pollution_Level + 
    Age + BMI + Family_History + Has_Asthma + Smoking_Status,
  data = asthma
)

# Test if interaction is significant
anova(main_effects_model, interaction_model)

# Compare AIC
AIC(main_effects_model, interaction_model)

# Visualize interaction
ggplot(asthma, aes(x = Medication_Adherence, 
                   y = Number_of_ER_Visits,
                   color = Air_Pollution_Level)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", se = TRUE, size = 1.5) +
  scale_color_manual(values = c(
    "Low" = "#66BB6A",
    "Moderate" = "#FFA726",
    "High" = "#EF5350"
  )) +
  labs(
    title = "Interaction: Medication Adherence × Air Quality",
    subtitle = "Effect on Emergency Room Visits",
    x = "Medication Adherence",
    y = "Number of ER Visits",
    color = "Air Pollution\nLevel"
  ) +
  theme_classic(base_size = 14)


# 9. POISSON REGRESSION 

pois_fit <- glm(Number_of_ER_Visits ~ Age + BMI +
                  Family_History + Medication_Adherence +
                  FeNO_Level + Has_Asthma,
                data = asthma, family = poisson)

summary(pois_fit)

# Compare AIC: linear vs Poisson
AIC(lm_fit, pois_fit)


# 9A. PREDICTING REPEATED ER UTILIZATION (PROXY FOR READMISSION)
# Research Question 2: Can we predict the likelihood of hospital readmission?
# Note: Dataset lacks readmission data, so we use multiple ER visits (2+) 
# as a proxy for high healthcare utilization

# Create binary outcome: Multiple ER visits (2+) vs. few (0-1)
asthma$Multiple_ER <- ifelse(asthma$Number_of_ER_Visits >= 2, 1, 0)

cat("\n--- Distribution of Multiple ER Visits ---\n")
table(asthma$Multiple_ER)
prop.table(table(asthma$Multiple_ER))

# Logistic regression predicting repeated ER utilization
readmission_proxy <- glm(
  Multiple_ER ~ Age + BMI + Family_History + Medication_Adherence +
    FeNO_Level + Has_Asthma + Smoking_Status + Air_Pollution_Level +
    Physical_Activity_Level + Comorbidities,
  data = asthma,
  family = binomial
)

summary(readmission_proxy)

# Odds ratios
cat("\n--- Odds Ratios for Repeated ER Utilization ---\n")
exp(coef(readmission_proxy))

# Confidence intervals
cat("\n--- 95% Confidence Intervals ---\n")
exp(confint(readmission_proxy))

# Among asthma patients only - create Multiple_ER for asthma_only subset
asthma_only$Multiple_ER <- ifelse(asthma_only$Number_of_ER_Visits >= 2, 1, 0)

readmission_asthma <- glm(
  Multiple_ER ~ Age + BMI + Medication_Adherence + FeNO_Level +
    Smoking_Status + Air_Pollution_Level + Physical_Activity_Level +
    Asthma_Control_Level + Comorbidities,
  data = asthma_only,
  family = binomial
)

summary(readmission_asthma)

cat("\n--- Odds Ratios (Asthma Patients Only) ---\n")
exp(coef(readmission_asthma))


# 10. POISSON REGRESSION WITH INTERACTION 

pois_interaction <- glm(
  Number_of_ER_Visits ~ Medication_Adherence * Air_Pollution_Level + 
    Age + BMI + Family_History + Has_Asthma,
  data = asthma, 
  family = poisson
)

summary(pois_interaction)

# Compare models
AIC(pois_fit, pois_interaction)


# PREDICTIVE MODELING 

# 11. TRAIN/TEST SPLIT 

model_df <- asthma %>%
  dplyr::select(Age, BMI, Family_History,
                Medication_Adherence, FeNO_Level,
                Number_of_ER_Visits, Has_Asthma) %>%
  na.omit()

n <- nrow(model_df)
train_idx <- sample(1:n, size = floor(0.75 * n))

train_data <- model_df[train_idx, ]
test_data  <- model_df[-train_idx, ]


# 12. LOGISTIC REGRESSION

logit_fit <- glm(Has_Asthma ~ Age + BMI + Family_History,
                 data = train_data, family = binomial)

summary(logit_fit)

# Predictions on test set
test_prob <- predict(logit_fit, newdata = test_data, type = "response")
test_pred <- ifelse(test_prob > 0.5, 1, 0)

# Confusion matrix
table(Predicted = test_pred, Actual = test_data$Has_Asthma)

# ROC curve and AUC
roc_logit <- roc(test_data$Has_Asthma, test_prob)
auc_logit <- auc(roc_logit)
auc_logit

plot(
  roc_logit,
  main = paste("ROC Curve – Logistic Regression (AUC =", round(auc_logit, 3), ")")
)


# 13. LASSO LOGISTIC REGRESSION 

x_train <- model.matrix(Has_Asthma ~ Age + BMI + Family_History,
                        data = train_data)[, -1]
y_train <- train_data$Has_Asthma

cv_lasso <- cv.glmnet(
  x_train, y_train,
  alpha = 1,
  family = "binomial"
)

lambda_min_lasso <- cv_lasso$lambda.min
lambda_1se_lasso <- cv_lasso$lambda.1se

lambda_min_lasso
lambda_1se_lasso

plot(cv_lasso)
title("LASSO Logistic Regression – CV Curve", line = 2.5)

lasso_fit <- glmnet(
  x_train, y_train,
  alpha = 1,
  lambda = lambda_min_lasso,
  family = "binomial"
)

coef(lasso_fit)

# Predictions with LASSO
x_test <- model.matrix(Has_Asthma ~ Age + BMI + Family_History,
                       data = test_data)[, -1]
test_prob_lasso <- predict(lasso_fit, newx = x_test, type = "response")
test_pred_lasso <- ifelse(test_prob_lasso > 0.5, 1, 0)

table(Predicted = test_pred_lasso, Actual = test_data$Has_Asthma)

roc_lasso <- roc(test_data$Has_Asthma, as.numeric(test_prob_lasso))
auc_lasso <- auc(roc_lasso)
auc_lasso

plot(
  roc_lasso,
  main = paste("ROC Curve – LASSO Logistic Regression (AUC =", round(auc_lasso, 3), ")")
)


# MODELING ASTHMA SEVERITY 
# Research Question 1: What demographic or environmental factors most strongly 
# influence asthma SEVERITY?

# 14. ORDINAL LOGISTIC REGRESSION FOR SEVERITY 

# Prepare data: only asthma patients with valid control levels
asthma_severity <- asthma_only %>%
  filter(!is.na(Control_Ordered)) %>%
  na.omit()

# Ordinal logistic regression model
severity_model <- polr(
  Control_Ordered ~ Age + Gender + BMI + Smoking_Status + 
    Air_Pollution_Level + Physical_Activity_Level + 
    Medication_Adherence + Comorbidities,
  data = asthma_severity,
  Hess = TRUE
)

summary(severity_model)

# Calculate odds ratios
cat("\n--- Odds Ratios for Severity Model ---\n")
exp(coef(severity_model))

# 95% Confidence intervals for odds ratios
cat("\n--- 95% Confidence Intervals ---\n")
exp(confint(severity_model))

# Calculate p-values
ctable <- coef(summary(severity_model))
p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
cat("\n--- P-values ---\n")
print(p_values)


# 15. ALTERNATIVE: MULTINOMIAL LOGISTIC REGRESSION 

severity_multinom <- multinom(
  Control_Ordered ~ Age + Gender + BMI + Smoking_Status + 
    Air_Pollution_Level + Physical_Activity_Level + 
    Medication_Adherence + Comorbidities,
  data = asthma_severity
)

summary(severity_multinom)

# Odds ratios
cat("\n--- Multinomial Model Odds Ratios ---\n")
exp(coef(severity_multinom))


# NONPARAMETRIC METHODS 

# 16. KRUSKAL–WALLIS TEST 

kw_test <- kruskal.test(Number_of_ER_Visits ~ Asthma_Control_Level,
                        data = asthma_only)
kw_test

pairwise.wilcox.test(asthma_only$Number_of_ER_Visits,
                     asthma_only$Asthma_Control_Level,
                     p.adjust.method = "bonferroni")


# 17. WILCOXON RANK-SUM TEST 

wilcox_bmi <- wilcox.test(BMI ~ Has_Asthma, data = asthma)
wilcox_bmi


# 18. SPEARMAN RANK CORRELATION 

spearman_test <- cor.test(asthma$FeNO_Level,
                          asthma$Number_of_ER_Visits,
                          method = "spearman")
spearman_test


# 19. RUNS TEST 

runs_test <- runs.test(asthma$Number_of_ER_Visits)
runs_test


# ADDITIONAL INTERACTION ANALYSES 

# 20. INTERACTION AMONG ASTHMA PATIENTS ONLY 

interaction_asthma <- lm(
  Number_of_ER_Visits ~ Medication_Adherence * Air_Pollution_Level + 
    Age + BMI + Smoking_Status,
  data = asthma_only
)

summary(interaction_asthma)


# 21. MEDICATION × SEVERITY INTERACTION 

interaction_severity <- lm(
  Number_of_ER_Visits ~ Medication_Adherence * Asthma_Control_Level + 
    Age + BMI + Air_Pollution_Level,
  data = asthma_only
)

summary(interaction_severity)

# Visualize
ggplot(asthma_only, aes(x = Medication_Adherence, 
                        y = Number_of_ER_Visits,
                        color = Asthma_Control_Level)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c(
    "Well Controlled" = "#66BB6A",
    "Poorly Controlled" = "#FFA726",
    "Not Controlled" = "#EF5350"
  )) +
  labs(
    title = "Medication Adherence Effect by Asthma Severity",
    x = "Medication Adherence",
    y = "Number of ER Visits",
    color = "Control Level"
  ) +
  theme_classic(base_size = 14)


# SUMMARY STATISTICS 

# 22. SUMMARY BY ADHERENCE AND AIR QUALITY 

asthma %>%
  mutate(
    Adherence_Level = cut(Medication_Adherence, 
                          breaks = c(0, 0.33, 0.67, 1),
                          labels = c("Low", "Medium", "High"))
  ) %>%
  group_by(Adherence_Level, Air_Pollution_Level) %>%
  summarise(
    Mean_ER_Visits = mean(Number_of_ER_Visits),
    SD_ER_Visits = sd(Number_of_ER_Visits),
    N = n(),
    .groups = "drop"
  ) %>%
  print()


# END OF ANALYSIS 
cat("\n\nANALYSIS COMPLETE\n")
cat("All research questions addressed:\n")
cat("1. Severity predictors: Sections 14-15 (Ordinal/Multinomial Logistic)\n")
cat("2. Readmission likelihood: Sections 6-10, 9A (ER visits as proxy + Multiple ER model)\n")
cat("3. Joint effects (Medication × Air Quality): Sections 8, 10, 20-21 (Interaction models)\n")
cat("\nNote: Question 2 uses ER visits as proxy for readmission (not available in dataset)\n")