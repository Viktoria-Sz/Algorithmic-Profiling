# MODELLING ####################################################################
# Preparation ==================================================================
# Libraries --------------------------------------------------------------------
library(tidyverse)
#library(modelr)
library(mlr3verse) # machine learning set-up for modeling
options(na.action = na.warn)
options(scipen=100) # prevent scientific notion (e)

# Load data --------------------------------------------------------------------
# load preperad dataset
data <- readRDS("data/JuSAW_prepared.rds")

# Load other scripts -----------------------------------------------------------

# Task set-up ------------------------------------------------------------------
# Validation and Train-/Test Split

# Set up different tasks with different variable sets
task_AMS = as_task_classif(data, target = "EMPLOYMENTDAYS", positive = ">=90 Tage", id = "AMS")
print(task_AMS)
task_AMS$set_col_roles("case", roles = "name")

# Evaluation measures (use msrs() to get a list of all measures):
measures = msrs(c("classif.acc", "classif.auc", "classif.tpr", "classif.fpr", "classif.ce"))

design = benchmark_grid(
  tasks = task_AMS, # more tasks with different variable sets
  learners = lrns(c("classif.ranger", "classif.rpart", "classif.featureless"),
                  predict_type = "prob", predict_sets = c("train", "test")),
  resamplings = rsmps("cv", folds = 3))
bmr = benchmark(design)

measures = list(
  msr("classif.auc", predict_sets = "train", id = "auc_train"),
  msr("classif.auc", id = "auc_test"))
tab = bmr$aggregate(measures)
print(tab)

# library("data.table")
# group by levels of task_id, return columns:
# - learner_id
# - rank of col '-auc_train' (per level of learner_id)
# - rank of col '-auc_test' (per level of learner_id)
ranks = tab[, .(learner_id, rank_train = rank(-auc_train), rank_test = rank(-auc_test)), by = task_id]
print(ranks)

# group by levels of learner_id, return columns:
# - mean rank of col 'rank_train' (per level of learner_id)
# - mean rank of col 'rank_test' (per level of learner_id)
ranks = ranks[, .(mrank_train = mean(rank_train), mrank_test = mean(rank_test)), by = learner_id]

# print the final table, ordered by mean rank of AUC test
ranks[order(mrank_test)]

autoplot(bmr) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

bmr_small = bmr$clone()$filter(task_id = "german_credit")
autoplot(bmr_small, type = "roc")

# Train- & Testsplit -----------------------------------------------------------
# Check resampling strategies
as.data.table(mlr_resamplings)

resampling = rsmp("holdout", ratio = 0.8)
resampling = rsmp("cv", folds = 3)

rr = resample(task_AMS, learner, resampling, store_models = TRUE)

rr$aggregate(msr("classif.ce"))
rr$score(msr("classif.ce"))

# boxplot of AUC values across the 10 folds
autoplot(rr, measure = msr("classif.auc"))
# ROC curve, averaged over 10 folds
autoplot(rr, type = "roc")
# All available plot types are listed on the manual page of autoplot.ResampleResult().

# Modeling =====================================================================

# Original AMS-Model -----------------------------------------------------------

# Logistic regression model with all variables step-procedure

# Restricted logistic regression model with all variables

# Classification Tree ----------------------------------------------------------
rpart = lrn("classif.rpart", predict_type = "prob")
rpart$train(task_AMS, row_ids = tain_set)
pred = rpart$predict(task_AMS, row_ids = test_set)

autoplot(pred)
autoplot(pred, type = "roc")
autoplot(pred, type = "prc") # PRCs are preferred to ROCs for imbalanced populations
pred$set_threshold(0.66)
pred$confusion
pred$score(measures)

# Other models
randomForest::randomForest()
xgboost::xgboost()
