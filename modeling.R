# MODELLING ####################################################################
# Preparation ==================================================================
# Libraries --------------------------------------------------------------------
library(tidyverse)
#library(modelr)
library(mlr3verse) # machine learning set-up for modeling
options(na.action = na.warn)
options(scipen=100) # prevent scientific notion (e)
set.seed(2507)

# Load data --------------------------------------------------------------------
# load preperad dataset
data <- readRDS("data/JuSAW_prepared.rds")

# Load other scripts -----------------------------------------------------------
source("variable_selection.R") # for predefined feature sets

# Task set-up ==================================================================
# Validation and Train-/Test Split ---------------------------------------------
# Training set for tuning, test set for final evaluation on untouched data
train_test_ratio = .8
data_training = dplyr::sample_frac(tbl = data, size = train_test_ratio)
data_test = dplyr::anti_join(x = data, y = data_training, by = "case")


# Set up different tasks with different variable sets --------------------------
# Original AMS-Model
task_ams_youth = as_task_classif(data_training[ams_youth], 
                                 target = "EMPLOYMENTDAYS", positive = ">=90 Tage", id = "AMS")
#task_ams_youth$set_col_roles("case", roles = "name")
print(task_ams_youth)

# Logistic regression model with all variables step-procedure
# Restricted logistic regression model with all variables


# Evaluation measures (use msrs() to get a list of all measures ----------------
measures = msrs(c("classif.acc", "classif.auc", "classif.tpr", "classif.fpr", "classif.ce"))

# measures = list(
#   msr("classif.auc", predict_sets = "train", id = "auc_train"),
#   msr("classif.auc", id = "auc_test"))

# Learners ---------------------------------------------------------------------
learners = lrns(c( "classif.featureless", 
                   "classif.log_reg",
                    "classif.rpart",
                   "classif.ranger"),
                predict_type = "prob") #predict_sets = c("train", "test"))

# Resampling strategy ----------------------------------------------------------
as.data.table(mlr_resamplings)

#resampling = rsmp("holdout", ratio = 0.8)
resampling = rsmp("cv", folds = 3)

#rr = resample(task_AMS, learner, resampling, store_models = TRUE)

# Benchmark design -------------------------------------------------------------
design = benchmark_grid(tasks = task_ams_youth, # more tasks with different variable sets
                        learners = learners,
                        resamplings = resampling)


# Training =====================================================================
execute_start_time <- Sys.time()
bmr = benchmark(design, store_models = TRUE)
evaluation_time <- Sys.time() - execute_start_time 
rm(execute_start_time)


# Evaluation ===================================================================
print(bmr)

# All available plot types are listed on the manual page of autoplot.ResampleResult().
autoplot(bmr)
autoplot(bmr, type = "roc")

(tab = bmr$aggregate(measures))
bmr$score(measures)
# log.reg zum Beispiel ist bei jedem Maß perfekt, wir wählen das als finales Modell

# Ranked Performance------------------------------------------------------------
ranks_performace = tab[, .(learner_id, rank_train = rank(-AUC), rank_test = rank(MMCE))] %>% 
  dplyr::arrange(rank_train)
print(ranks_performace)

# Logistic Regression and Random Forest clear winners


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

#bmr_small = bmr$clone()$filter(task_id = "german_credit")

# Predictions ==================================================================
# Choose the best model and fit on whole dataset 
# Wir hatten oben log.reg ausgewählt, random forest oder knn waren aber genauso gut
# Choose final model
learner_final = lrn("classif.log_reg",predict_type = "prob")

# Train on whole train data
learner_final$train(task_ams_youth)

# Test on never touched test data (20% of whole data splitted at the beginning)
pred = learner_final$predict_newdata(newdata = data_test)
pred$score(measures)


# RESTE ########################################################################
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


# Predictions knn
result_knn = tab$resample_result[[6]]
as.data.table(result_knn$prediction())

# Model Parameter
knn = bmr$score()[learner_id == "classif.kknn.tuned"]$learner
for (i in 1:10){
  print(knn[[i]]$tuning_result$params)
}

ranger = bmr$score()[learner_id == "classif.ranger.tuned"]$learner
for (i in 1:10){
  print(ranger[[i]]$tuning_result$params)
}

# Variable Importance ---------------------------------------------------------
# ranger
# learner_ranger = learners[[5]]
# Variable importance mode, one of 'none', 'impurity', 'impurity_corrected', 
# 'permutation'. The 'impurity' measure is the Gini index for classification, 
# the variance of the responses for regression and the sum of test statistics 
# (see splitrule) for survival.

filter = flt("importance", learner = learner_ranger)
filter$calculate(task_mushrooms)
feature_scores <- as.data.table(filter)

ggplot(data = feature_scores, aes(x = reorder(feature, -score), y = score)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  ggtitle(label = "Variable Importance Mushroom Features") +
  labs(x = "Features", y = "Variable Importance Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = pretty(1:500, 10))

