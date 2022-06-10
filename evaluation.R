# EVALUATION ###########################################################################################################
# Preparation ==========================================================================================================
# Libraries ------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(fairness) # fairness
library(yardstick) # performance
#library(pROC)


# Load data ------------------------------------------------------------------------------------------------------------
load("data/df1.Rda")
load("data/df2.Rda")
load("data/df3.Rda")

# make estimate column with 0.66 and 0.5
df1$estimate_0.66 = as.factor(ifelse(df1$probabilities >= 0.66, 1, 0))
df1$estimate_0.5 = as.factor(ifelse(df1$probabilities >= 0.5, 1, 0))

# Load other scripts ---------------------------------------------------------------------------------------------------
source("functions.R", encoding="utf-8") # for predefined feature sets

# Performance ==========================================================================================================
# Confucion Matrix -----------------------------------------------------------------------------------------------------
conf_mats = subset(df1, subset = (task == "AMS youth")) %>%
  group_by(model) %>%
  conf_mat(truth_01, estimate = estimate_0.66)

names(conf_mats$conf_mat) = conf_mats$model
conf_mats$conf_mat

conf_mats_0.5 = subset(df1, subset = (task == "AMS youth")) %>%
  group_by(model) %>%
  conf_mat(truth_01, estimate = estimate_0.5)

names(conf_mats_0.5$conf_mat) = conf_mats_0.5$model
conf_mats_0.5$conf_mat

# ROC and PRC Curves ---------------------------------------------------------------------------------------------------
for(i in unique(df1$task)){
  subset(df1, subset = (task == i)) %>%
    filter(!is.na(probabilities)) %>%
    #filter(model == "OR" | model == "Logistic Regression" | model == "encode.colapply.classif.glmnet") %>%
    group_by(model) %>%
    roc_curve(truth_01, probabilities, event_level = 'second') %>%
    autoplot() +
    ggtitle(i)
}

for(i in unique(df1$task)){
  sub = subset(df1, subset = (task == i)) %>%
    filter(!is.na(probabilities)) %>%
    #filter(model == "OR" | model == "Logistic Regression" | model == "encode.colapply.classif.glmnet") %>%
    group_by(model)
  
   autoplot(roc_curve(sub, truth_01, probabilities, event_level = 'second'))  +
    ggtitle(i)
}

subset(df1, subset = (task == "AMS youth")) %>%
  #filter(model == "OR" | model == "Logistic Regression" | model == "encode.colapply.classif.glmnet") %>%
  group_by(model) %>%
  roc_curve(truth_01, probabilities, event_level = 'second') %>%
  autoplot() +
  ggtitle("AMS youth")


# PRC curve
subset(df1, subset = (task == "AMS youth")) %>%
  #filter(model == "OR" | model == "Logistic Regression" | model == "encode.colapply.classif.glmnet") %>%
  group_by(model) %>%
  pr_curve(truth_01, probabilities, event_level = 'second') %>%
  autoplot() +
  ggtitle("AMS youth")


# Performance Measures and Heatmap -------------------------------------------------------------------------------------
# Measures
performance_measure_set = metric_set(accuracy, 
                                     sens, # sensitivity, recall, TPR
                                     spec, # specificity, TNR
                                     precision, # Precision, Positive predictive value PPV
                                     f_meas # fbeta, with beta = 1
                                     )


performance_measures = df1 %>%
  filter(task == "AMS youth") %>%
  group_by(model) %>%
  performance_measure_set(truth_01, estimate = estimate_0.66, event_level = 'second')

measures_female = df1 %>%
  filter(task == "AMS youth" & GENDER == "female") %>%
  group_by(model) %>%
  performance_measure_set(truth_01, estimate = estimate_0.66, event_level = 'second') %>%
  rename(female = .estimate)

measures_male = df1 %>%
  filter(task == "AMS youth" & GENDER == "male") %>%
  group_by(model) %>%
  performance_measure_set(truth_01, estimate = estimate_0.66, event_level = 'second') %>%
  rename(male = .estimate)

measures = full_join(performance_measures, measures_female, by = c("model", ".metric", ".estimator"))
measures = full_join(measures, measures_male, by = c("model", ".metric", ".estimator"))

# AUC
auc = df1 %>%
  filter(task == "AMS youth" & GENDER == "female") %>%
  group_by(model) %>%
  roc_auc(truth_01, probabilities, event_level = 'second')

auc_female = df1 %>%
  filter(task == "AMS youth" & GENDER == "female") %>%
  group_by(model) %>%
  roc_auc(truth_01, probabilities, event_level = 'second') %>%
  rename(female = .estimate)

auc_male = df1 %>%
  filter(task == "AMS youth" & GENDER == "male") %>%
  group_by(model) %>%
  roc_auc(truth_01, probabilities, event_level = 'second') %>%
  rename(male = .estimate)

auc = full_join(auc, auc_female, by = c("model", ".metric", ".estimator"))
auc = full_join(auc, auc_male, by = c("model", ".metric", ".estimator"))

measures = bind_rows(measures, auc)


# Heatmap
heatmap(measures, model, .metric, .estimate)
heatmap(measures, model, .metric, male)
heatmap(measures, model, .metric, female)


# RESTE ################################################################################################################
df1[1:10, c("task", "model", "probabilities")]

subset = subset(df1, subset = (task == "AMS youth"))
subset(subset, subset = ( model == "OR" ), c(probabilities,truth_01, test_ids, task))
subset(df1, subset = (task == "AMS youth" & model == "OR" ), probabilities)
subset(df2, subset = (task == "AMS youth"), OR)
subset(df3, subset = (task == "AMS youth" & model != "OR" ))


autoplot(roc_curve(subset(df1, subset = (task == "AMS youth" & model == "Logistic Regression")), 
                   truth_01, probabilities, event_level = "second"))
