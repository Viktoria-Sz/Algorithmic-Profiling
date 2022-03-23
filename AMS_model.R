# AMS MODEL REBUILD ############################################################

# Libraries --------------------------------------------------------------------
library(tidyverse)
library(caret) # Confusion Matrix
library(InformationValue) # Optimal cutoff threshold
# library(ISLR)
options(na.action = na.warn)
options(scipen=100) # prevent scientific notion (e)

# Load data --------------------------------------------------------------------
data <- readRDS("data/JuSAW_prepared.rds")

# Import other scripts
source("variable_sets.R", encoding="utf-8") # for predefined feature sets

# Split in Training and Test data ----------------------------------------------

# Train Original AMS logistic model with train-data ----------------------------
model_ams <- glm(EMPLOYMENTDAYS ~ GENDER_female + STATEGROUP
                 + EDUCATION + CHILDCARE 
                 + IMPAIRMENT_strong + OCCUPATIONGROUP + EMPLOYMENT
                 + BUSINESSCASEDUR + BUSINESSCASEFREQ + SUPPORTMEASURE, 
                 family = "binomial", data = data)
summary(model_ams)
# What about missing values? -> Look at discriptives
# Beschäftigungsverlauf -> einzelne Jahre

# Predictions 
data$prob_ams <- predict(model_ams, data, type="response")

# Confusion Matrix -------------------------------------------------------------
data$class_pred <-ifelse(data$prob_ams > 0.66, 1, 0)
table(data$class_pred)
data$truth <- ifelse(data$EMPLOYMENTDAYS == ">=90 Tage", 1, 0)
data$truth <- ifelse(is.na(data$class_pred), NA, data$truth)
table(data$truth)

#find optimal cutoff probability to use to maximize accuracy
#optimal <- optimalCutoff(data$truth, data$prob_ams, optimiseFor = "misclasserror")[1]

# Confusion matrix
confusionMatrix(data$class_pred, data$truth)

#calculate sensitivity, specificity, total misclassification error rate
sensitivity(data$truth, data$class_pred)
specificity(data$truth, data$class_pred)
misClassError(data$truth, data$class_pred, threshold=0.66)

# Checking ROC
library(Epi)
ROC(form = EMPLOYMENTDAYS ~ GENDER_female + AGEGROUP + STATEGROUP 
    + EDUCATION + CHILDCARE + RGS
    + IMPAIRMENT + OCCUPATIONGROUP + EMPLOYMENT
    + BUSINESSCASEDUR + BUSINESSCASEFREQ + SUPPORTMEASURE, data=data,plot="ROC")

# Looking at variable effects
library(effects)
plot(allEffects(model1))

# Checking model assumptions
# 1. Normality of errors (qqplot)
plot(model1, which=2)
# 2. Constant variance (residual plots, tests)
plot(model1, which=3)
# 3. Independence of errors (sequence plot, DW test)
plot(rstudent(model1), type='l')
# 4. Outliers (leverage, Cooks distance)

# Train alternativ models ------------------------------------------------------
# Logistic regression model with "better" AMS variables
model_exp <- glm(EMPLOYMENTDAYS ~ GENDER_female + AGEGROUP + STATEGROUP 
                 + EDUCATION + CHILDCARE_both + RGS
                 + IMPAIRMENT + OCCUPATIONGROUP_all + EMPLOYMENT
                 + BUSINESSCASEDUR + BUSINESSCASEFREQ + SUPPORTMEASURE, 
                 family = "binomial", data = data)
summary(model_exp)


# Predictions
predicted_prob_alt <- predict(model_exp, data, type="response")

# Odds Ratios from AMS documentation ###########################################
# Kurzfristiges Kriterium: Partiell valide schätzbare jugendliche Population
model_ams_OR_youth <- glm(EMPLOYMENTDAYS ~ GENDER_female + AGEGROUP 
                 + EDUCATION + CHILDCARE + RGS
                 + IMPAIRMENT + OCCUPATIONGROUP 
                 + BUSINESSCASEFREQ + SUPPORTMEASURE, 
                 family = "binomial", data = data)
summary(model_ams_OR_youth)
OR_coefs_youth <- c(-0.13, 0.09, 0.13, 0.0, 0.48, 0.40, -0.46, -0.10, -0.17, -0.15, 
              -0.36, -0.03, 0.01, -0.02, -0.06, -0.21, -0.14)
model_ams_OR_youth$coefficients <- OR_coefs_youth

# Predictions 
data$prob_ams_OR_youth <- predict(model_ams_OR_youth, data, type="response")

data$class_pred_OR_youth <-ifelse(data$prob_ams_OR_youth > 0.66, 1, 0)
table(data$class_pred_OR_youth, data$truth[is.na(data$class_pred_OR_youth)])


# Kurzfristiges Kriterium: Voll valide shcätzbare Population
model_ams_OR <- glm(EMPLOYMENTDAYS ~ GENDER_female + AGEGROUP + STATEGROUP
                    + EDUCATION + CHILDCARE + RGS + IMPAIRMENT 
                    + OCCUPATIONGROUP + EMPLOYMENT
                    + BUSINESSCASEFREQ + BUSINESSCASEDUR + SUPPORTMEASURE, 
                    family = "binomial", data = data)
summary(model_ams_OR)
OR_coefs <- c(0.48, -0.05, 0.0, 0.0, -0.01, 0.04, 0.10, 0.03, -0.08, -0.10, 
              -0.16, -0.10, -0.33, 0.07, -0.31, 0.20, 0.36, 0.63, -0.21, -0.19, -0.03)
model_ams_OR$coefficients <- OR_coefs

# Predictions 
data$prob_ams_OR <- predict(model_ams_OR, data, type="response")

data$class_pred_OR <-ifelse(data$prob_ams_OR > 0.66, 1, 0)
table(data$class_pred_OR)
