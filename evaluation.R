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
# 0.66 Threshold
confmat_list_0.66 = list()
for(i in unique(df1$task)){
  conf_mats_0.66 = subset(df1, subset = (task == i)) %>%
    group_by(model) %>%
    conf_mat(truth_01, estimate = estimate_0.66)
  
  names(conf_mats_0.66$conf_mat) = conf_mats_0.66$model
  
  confmat_list_0.66 = append(confmat_list_0.66, list(conf_mats_0.66))
  #conf_mats_0.66$conf_mat
}
names(confmat_list_0.66) = unique(df1$task)

# 0.5 Threshold
confmat_list_0.5 = list()
for(i in unique(df1$task)){
  conf_mats_0.5 = subset(df1, subset = (task == i)) %>%
    group_by(model) %>%
    conf_mat(truth_01, estimate = estimate_0.5)
  
  names(conf_mats_0.5$conf_mat) = conf_mats_0.5$model
  
  confmat_list_0.5 = append(confmat_list_0.5, list(conf_mats_0.5))
  #conf_mats_0.5$conf_mat
}
names(confmat_list_0.5) = unique(df1$task)


# ROC and PRC Curves ---------------------------------------------------------------------------------------------------
roc_list = list()
for(i in unique(df1$task)){
  sub = subset(df1, subset = (task == i)) %>%
    filter(!is.na(probabilities)) %>% # since not all tasks have OR results
    #filter(model == "OR" | model == "Logistic Regression" | model == "encode.colapply.classif.glmnet") %>%
    group_by(model)
  
   roc_plot = autoplot(roc_curve(sub, truth_01, probabilities, event_level = 'second'))  +
    ggtitle(i)
   
   roc_list = append(roc_list, list(roc_plot))
}

# PRC curve
prc_list = list()
for(i in unique(df1$task)){
  sub = subset(df1, subset = (task == i)) %>%
    filter(!is.na(probabilities)) %>%
    #filter(model == "OR" | model == "Logistic Regression" | model == "encode.colapply.classif.glmnet") %>%
    group_by(model)
  
  prc_plot = autoplot(pr_curve(sub, truth_01, probabilities, event_level = 'second')) +
    ggtitle(i)
  
  prc_list = append(prc_list, list(prc_plot))
  
}


# Measures -------------------------------------------------------------------------------------------------------------
performance_measure_set = metric_set(accuracy, 
                                     sens, # sensitivity, recall, TPR
                                     spec, # specificity, TNR
                                     precision, # Precision, Positive predictive value PPV
                                     f_meas # fbeta, with beta = 1
                                     )

measure_list = list()
for(i in unique(df1$task)){
  performance_measures = df1 %>%
    filter(task == i) %>%
    filter(!is.na(estimate_0.66)) %>%
    group_by(model) %>%
    performance_measure_set(truth_01, estimate = estimate_0.66, event_level = 'second')
  
  measures_female = df1 %>%
    filter(task == i & GENDER == "female") %>%
    filter(!is.na(estimate_0.66)) %>%
    group_by(model) %>%
    performance_measure_set(truth_01, estimate = estimate_0.66, event_level = 'second') %>%
    rename(female = .estimate)
  
  measures_male = df1 %>%
    filter(task == i & GENDER == "male") %>%
    filter(!is.na(estimate_0.66)) %>%
    group_by(model) %>%
    performance_measure_set(truth_01, estimate = estimate_0.66, event_level = 'second') %>%
    rename(male = .estimate)
  
  measures = full_join(performance_measures, measures_female, by = c("model", ".metric", ".estimator"))
  measures = full_join(measures, measures_male, by = c("model", ".metric", ".estimator"))
  
  # AUC
  auc = df1 %>%
    filter(task == i & GENDER == "female") %>%
    filter(!is.na(probabilities)) %>%
    group_by(model) %>%
    roc_auc(truth_01, probabilities, event_level = 'second')
  
  auc_female = df1 %>%
    filter(task == i & GENDER == "female") %>%
    filter(!is.na(probabilities)) %>%
    group_by(model) %>%
    roc_auc(truth_01, probabilities, event_level = 'second') %>%
    rename(female = .estimate)
  
  auc_male = df1 %>%
    filter(task == i & GENDER == "male") %>%
    filter(!is.na(probabilities)) %>%
    group_by(model) %>%
    roc_auc(truth_01, probabilities, event_level = 'second') %>%
    rename(male = .estimate)
  
  auc = full_join(auc, auc_female, by = c("model", ".metric", ".estimator"))
  auc = full_join(auc, auc_male, by = c("model", ".metric", ".estimator"))
  
  measures = bind_rows(measures, auc)
  
  measures = mutate(measures, gender_diff = male - female)
  
  # order variables for nicer plotting
  metrics_order <- c("accuracy", 
                     "sens", # sensitivity, recall, TPR
                     "spec", # specificity, TNR
                     "roc_auc",
                     "precision", # Precision, Positive predictive value PPV
                     "f_meas" # fbeta, with beta = 1
  )
  measures$.metric = factor(measures$.metric, level = metrics_order)
  
  model_order <- c("Featureless", "OR", "Logistic Regression", "encode.colapply.classif.glmnet", "Random Forest",
                   "Decision Tree", "encode.colapply.classif.xgboost", "encode.colapply.classif.svm", "KKNN"
  )
  measures$model = factor(measures$model, level = rev(model_order))
  
  # append to list over tasks
  measure_list = append(measure_list, list(measures))
}
names(measure_list) = unique(df1$task)


# Heatmap --------------------------------------------------------------------------------------------------------------
heatmap_list = list()
for(i in unique(df1$task)){
  p = heatmap(measure_list[[i]], model, .metric, .estimate) +
    ggtitle(i)
  heatmap_list = append(heatmap_list,  list(p))
}
heatmap_male_list = list()
for(i in unique(df1$task)){
  p = heatmap(measure_list[[i]], model, .metric, male) +
    ggtitle(i)
  heatmap_male_list = append(heatmap_male_list,  list(p))
}
heatmap_female_list = list()
for(i in unique(df1$task)){
  p = heatmap(measure_list[[i]], model, .metric, female) +
    ggtitle(i)
  heatmap_female_list = append(heatmap_female_list,  list(p))
}
heatmap_genderdiff_list = list()
for(i in unique(df1$task)){
  p = heatmap_diff(measure_list[[i]], model, .metric, gender_diff) +
    ggtitle(i)
  heatmap_genderdiff_list = append(heatmap_genderdiff_list,  list(p))
}


# RESTE ################################################################################################################
df1[1:10, c("task", "model", "probabilities")]

subset = subset(df1, subset = (task == "AMS youth"))
subset(subset, subset = ( model == "OR" ), c(probabilities,truth_01, test_ids, task))
subset(df1, subset = (task == "AMS youth" & model == "OR" ), probabilities)
subset(df2, subset = (task == "AMS youth"), OR)
subset(df3, subset = (task == "AMS youth" & model != "OR" ))


autoplot(roc_curve(subset(df1, subset = (task == "AMS youth" & model == "Logistic Regression")), 
                   truth_01, probabilities, event_level = "second"))

# ROC Curve
subset(df1, subset = (task == "AMS youth")) %>%
  #filter(model == "OR" | model == "Logistic Regression" | model == "encode.colapply.classif.glmnet") %>%
  group_by(model) %>%
  roc_curve(truth_01, probabilities, event_level = 'second') %>%
  autoplot() +
  ggtitle("AMS youth")


for(i in unique(df1$task)){
  subset(df1, subset = (task == i)) %>%
    filter(!is.na(probabilities)) %>%
    #filter(model == "OR" | model == "Logistic Regression" | model == "encode.colapply.classif.glmnet") %>%
    group_by(model) %>%
    roc_curve(truth_01, probabilities, event_level = 'second') %>%
    autoplot() +
    ggtitle(i)
}

