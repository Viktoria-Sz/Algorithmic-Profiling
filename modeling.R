# MODELLING ####################################################################
# Preparation ==================================================================
# Libraries --------------------------------------------------------------------
library(tidyverse)
#library(modelr)
library(mlr3verse) # machine learning set-up for modeling
library(ranger) # Random Forest
library(mlr3fairness)
options(scipen=100) # prevent scientific notion (e)
set.seed(42)

# Load data --------------------------------------------------------------------
# load preperad dataset
data <- readRDS("data/JuSAW_prepared.rds")
data_ams <- readRDS("data/JuSAW_ams_prepared.rds")

# Load other scripts -----------------------------------------------------------
source("variable_sets.R", encoding="utf-8") # for predefined feature sets

# Task set-up ==================================================================
# Set up different tasks with different variable sets --------------------------
# Original AMS-Model
task_ams_youth = as_task_classif(data[ams_youth], 
                                 target = "EMPLOYMENTDAYS", positive = ">=90 Tage", id = "AMS youth")
#task_ams_youth$set_col_roles("case", roles = "name")

ids = complete.cases(task_ams_youth$data())
sum(!ids)
task_ams_youth$filter(which(ids))
# number of incomplete observations
# should be zero now
ids = complete.cases(task_green$data())
sum(!ids)

# Set protected attribute
task_ams_youth$col_roles$pta = "GENDER_female"

print(task_ams_youth)
# Logistic regression model with all variables step-procedure ------------------
# Restricted logistic regression model with all variables ----------------------

# Grenn variables --------------------------------------------------------------
task_green_all = as_task_classif(data[green_all], 
                                 target = "EMPLOYMENTDAYS", positive = ">=90 Tage", id = "green_all")
task_green = as_task_classif(data[green], 
                             target = "EMPLOYMENTDAYS", positive = ">=90 Tage", id = "green")

ids = complete.cases(task_green$data())
# number of incomplete observations
sum(!ids)
task_green$filter(which(ids))

task_green$col_roles$pta = "GENDER_female"

print(task_green)

# Set-up rest ==================================================================
# Tasks
tasks = c(task_ams_youth
          , task_green
          )

# Evaluation measures (use msrs() to get a list of all measures ----------------
measures = msrs(c("classif.acc"
                  , "classif.auc"
                  , "classif.tpr"
                  , "classif.fpr"
                  , "classif.ce"
                  , "classif.fbeta"
                  ))

# measures = list(
#   msr("classif.auc", predict_sets = "train", id = "auc_train"),
#   msr("classif.auc", id = "auc_test"))

# Resampling strategy ----------------------------------------------------------
#as.data.table(mlr_resamplings)

resampling = rsmp("holdout", ratio = 0.8)
#resampling = rsmp("cv", folds = 3)

#rr = resample(task_AMS, learner, resampling, store_models = TRUE)

# Learners ---------------------------------------------------------------------
learners = lrns(c( "classif.featureless"
                   ,"classif.log_reg" # logistic regression
                   #,"classif.glmnet" # penalized logistic regression
                   ,"classif.rpart" # Decision tree
                   ,"classif.ranger" # Random Forest
                   #,"classif.xgboost"
                   ), 
                predict_type = "prob", predict_sets = c("train", "test"))

# Benchmark design -------------------------------------------------------------
design = benchmark_grid(tasks = tasks, 
                        learners = learners,
                        resamplings = resampling)


# Training =====================================================================
#execute_start_time <- Sys.time()
bmr = benchmark(design, store_models = TRUE)
#evaluation_time <- Sys.time() - execute_start_time 
#rm(execute_start_time)


# Evaluation ===================================================================
print(bmr)

# All available plot types are listed on the manual page of autoplot.ResampleResult().
autoplot(bmr) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

bmr_ams <- bmr$clone(deep = TRUE)$filter(task_id = "AMS youth")
bmr_green <- bmr$clone(deep = TRUE)$filter(task_id = "green")
autoplot(bmr_ams, type = "roc")
autoplot(bmr_green, type = "roc")

(tab = bmr$aggregate(measures))
bmr$score(measures)
# log.reg zum Beispiel ist bei jedem Maß perfekt, wir wählen das als finales Modell

# Ranked Performance------------------------------------------------------------
ranks_performace = tab[, .(learner_id, task_id, rank_train = rank(-classif.auc))] %>% 
  dplyr::arrange(rank_train)
print(ranks_performace)

# Predictions ==================================================================

# RESTE ########################################################################
# Choose the best model and fit on whole dataset 
# Wir hatten oben log.reg ausgewählt, random forest oder knn waren aber genauso gut
# Choose final model
learner_final = lrn("classif.log_reg",predict_type = "prob")

# Train on whole train data
learner_final$train(task_green)

# Test on never touched test data (20% of whole data splitted at the beginning)
ids = complete.cases(data_test[,green])
# number of incomplete observations
sum(!ids)
data_test <- filter(data_test, ids)

pred = learner_final$predict_newdata(newdata = data_test)
pred$confusion
pred$score(measures)
pred$set_threshold(0.66)
pred$confusion
pred$score(measures)


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

