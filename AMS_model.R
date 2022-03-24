# AMS MODEL REBUILD ####################################################################################################

# Libraries ------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(caret) # Confusion Matrix
library(InformationValue) # Optimal cutoff threshold
library(jtools) # for plotting coefficients
# library(ISLR)
#options(na.action = na.warn)
options(scipen=100) # prevent scientific notion (e)
set.seed(42)

# Load data ------------------------------------------------------------------------------------------------------------
data <- readRDS("data/JuSAW_prepared.rds")

# Import other scripts
source("variable_sets.R", encoding="utf-8") # for predefined feature sets

# Split in Training and Test data --------------------------------------------------------------------------------------
train_test_ratio = .8
data_train = dplyr::sample_frac(tbl = data, size = train_test_ratio)
data_test = dplyr::anti_join(x = data, y = data_train, by = "case")


# AMS full -------------------------------------------------------------------------------------------------------------
f_ams <- as.formula(paste("EMPLOYMENTDAYS", paste(ams[-1], collapse = "+"), sep = "~"))
model_ams <- glm(f_ams, family = "binomial", data = data_train)
summary(model_ams)
#ams_log_coefs <- load("models/ams_log_coefs.rds")

# Plotting coefficients
plot_summs(model_ams) +
  ggtitle("Koeffizienten AMS full")

# Predictions 
data$prob_ams <- predict(model_ams, data, type="response")
data$class_pred_ams <-ifelse(data$prob_ams > 0.66, 1, 0)
table(data$class_pred_ams, data$r_besch, useNA = "always")

#find optimal cutoff probability to use to maximize accuracy
#optimal <- optimalCutoff(data$truth, data$prob_ams, optimiseFor = "misclasserror")[1]

# Confusion matrix
confusionMatrix(data$class_pred, data$r_besch)

#calculate sensitivity, specificity, total misclassification error rate
sensitivity(data$truth, data$class_pred)
specificity(data$truth, data$class_pred)
misClassError(data$truth, data$class_pred, threshold=0.66)

# Kurzfristiges Kriterium: Voll valide shcätzbare Population -----------------------------------------------------------
model_ams_OR <- glm(f_ams, family = "binomial", data = data)
summary(model_ams_OR)
OR_coefs <- c(0.48, -0.05, 0.0, 0.0, -0.01, 0.04, 0.10, 0.03, -0.08, # RGS-Typ 2: -0.10, 
              -0.16, -0.10, -0.33, 0.07, -0.31, 0.20, 0.36, 0.63, -0.21, -0.19, -0.03)
names(OR_coefs) <- names(model_ams_OR$coefficients)
model_ams_OR$coefficients <- OR_coefs


plot_summs(model_ams_OR) +
  ggtitle("Koeffizienten AMS full documentation")
plot_summs(model_ams, model_ams_OR, ci_level = 0,
           model.names = c("AMS full", "AMS full documentation")) +
  ggtitle("Koeffizienten AMS full + documentation")

# Predictions 
data$prob_ams_OR <- predict(model_ams_OR, data, type="response")

data$class_pred_OR <-ifelse(data$prob_ams_OR > 0.66, 1, 0)
table(data$class_pred_OR)

# AMS youth ------------------------------------------------------------------------------------------------------------
f_ams_youth <- as.formula(paste("EMPLOYMENTDAYS", paste(ams_youth[-1], collapse = "+"), sep = "~"))
model_ams_youth <- glm(f_ams_youth, family = "binomial", data = data_train)
summary(model_ams_youth)
#ams_youth_log_coefs <- load("models/ams_youth_logcoefs.rds")

# Plotting coefficients
plot_summs(model_ams) +
  ggtitle("Koeffizienten AMS Youth")


# OR Coefs youth -------------------------------------------------------------------------------------------------------
model_ams_OR_youth <- glm(f_ams_youth, family = "binomial", data = data)
OR_coefs_youth <- c(-0.13, 0.09, 0.13, 0.0, 0.48, 0.40, -0.46, -0.17, -0.15, 
                    -0.36, -0.03, 0.01, -0.02, -0.06, -0.21, -0.14)
names(OR_coefs_youth) <- names(model_ams_OR_youth$coefficients)
model_ams_OR_youth$coefficients <- OR_coefs_youth


plot_summs(model_ams_OR_youth) +
  ggtitle("Koeffizienten AMS Youth documentation")
plot_summs(model_ams_youth, model_ams_OR_youth, ci_level = 0,
           model.names = c("AMS Youth", "AMS Youth documentation")) +
  ggtitle("Koeffizienten AMS Youth + documentation")


# Predictions 
data$prob_ams_OR_youth <- predict(model_ams_OR_youth, data, type="response")

data$class_pred_OR_youth <-ifelse(data$prob_ams_OR_youth > 0.66, 1, 0)
table(data$class_pred_OR_youth, data$truth[is.na(data$class_pred_OR_youth)])



########################################################################################################################
# RESTE ################################################################################################################
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

