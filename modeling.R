# MODELLING ############################################################################################################
# Preparation ==========================================================================================================
# Libraries ------------------------------------------------------------------------------------------------------------
library(tidyverse)
#library(broom) # für funktion tidy
library(data.table) # für rbindlist
#library(modelr)
library(mlr3verse) # machine learning set-up for modeling # , options(mlr3.allow_utf8_names = TRUE)
#library(ranger) # Random Forest
library(mlr3fairness)
#library(DALEX)
#library(DALEXtra)
#library(fairmodels)
set.seed(42)
#set.seed(42, "L'Ecuyer-CMRG")

progressr::handlers(global = TRUE) # for progress bars
progressr::handlers("progress")

# runs the outer loop sequentially and the inner loop in parallel
#future::plan(list("multisession"))
# future::plan("multiprocess")

# Load other scripts ---------------------------------------------------------------------------------------------------
source("tasks.R", encoding="utf-8") # for predefined feature sets
source("learners.R", encoding="utf-8") # for predefined feature sets


# Set-up modeling ======================================================================================================
# Tasks ----------------------------------------------------------------------------------------------------------------
tasks = list(#task_ams_youth
             #task_ams
             #task_ams_ext
             #task_diverse
             # task_filtering_disr
             # task_filtering_mrmr
             # task_filtering_jmi
             # task_filtering_cmim
              task_filtering_relief
             # task_behavior
             # task_attitudes
             # task_personality
             # task_characteristics_filter
             # task_otherPES
             )

# Evaluation measures (use msrs() to get a list of all measures --------------------------------------------------------
performance_measures = msrs(c("classif.acc"
                  , "classif.auc" # id = "AUC"
                  , "classif.tpr" # Wie viele Leute wurden richtig in H Kategorie gruppiert
                  , "classif.tnr" # Wie viele Leute wurden richtig in M Kategorie gruppiert
                  , "classif.fpr" # Wie viele Leute wurden fälschlich in H Kategorie gruppiert
                  , "classif.fnr" # Wie viele Leute wurden fälschlich in M Kategorie gruppiert
                  #, "classif.ce"
                  , "classif.fbeta"
                  ))

# performance_measures_ = c(msr("classif.auc", id = "AUC"), 
#                           msr("classif.acc", id = "accuracy"))

# Fairness Measure
fairness_measures_absdiff = msrs(c("fairness.acc"
                           , "fairness.tpr"
                           , "fairness.tnr"
                           , "fairness.fomr"
                           , "fairness.ppv"
                           , "fairness.eod"
                           ))

fairness_measures_diff = list(msr("fairness.acc", operation = groupdiff_diff)
                      , msr("fairness.tpr", operation = groupdiff_diff)
                      , msr("fairness.tnr", operation = groupdiff_diff)
                      , msr("fairness.fomr", operation = groupdiff_diff)
                      , msr("fairness.ppv", operation = groupdiff_diff)
                      , msr("fairness.eod", operation = groupdiff_diff)
                      )

# Resampling strategy --------------------------------------------------------------------------------------------------
#as.data.table(mlr_resamplings)

#resampling_outer_ho = rsmps("holdout", ratio = 0.8)

# Costum resampling
resampling_costum = rsmp("custom")


# Learners -------------------------------------------------------------------------------------------------------------
learners = list(lrn("classif.featureless", id = "Featureless", predict_type = "prob", predict_sets = "test")
              , lrn("classif.log_reg", id = "Logistic Regression", predict_type = "prob", predict_sets = "test")
              , graph_glmnet
              , tuner_rpart
              , tuner_ranger
              , graph_xgboost
              , tuner_kknn
              # , graph_svm
              )


# Benchmark design -----------------------------------------------------------------------------------------------------
set.seed(42)
# design = benchmark_grid(tasks = tasks 
#                         , learners = c(learners, graph_glmnet, graph_xgboost, graph_svm)
#                         , resamplings = resampling_costum
#                         )

grid = data.table::CJ(task = tasks, 
                      learner = learners, #c(learners, graph_glmnet, graph_xgboost, graph_svm), 
                      resampling = list(resampling_costum), sorted = FALSE)

# manual instantiation, since I have a costum test set
Map(function(task, resampling) 
  {resampling$instantiate(task, train_sets = list(setdiff(task$row_ids, test_ids)), test_sets = list(test_ids))}, 
    task =  grid$task, resampling =  grid$resampling)


# Training =============================================================================================================
execute_start_time <- Sys.time()
bmr = benchmark(grid, store_models = TRUE)
evaluation_time <- Sys.time() - execute_start_time 
rm(execute_start_time)
evaluation_time

# Evaluation ===========================================================================================================
print(bmr)

saveRDS(bmr, "models/bmr_AllFilterRelief_RS1000_acc.Rds")

# Measure results
# I guess aggregate is only important for cv -> check
#bmr$aggregate(c(performance_measures, fairness_measures))
bmr$score(c(performance_measures)) # fairness_measures_diff

# set threshold before calculating measures!!
#bmr$aggregate()[,resample_result]$predictions()$set_threshold(0.66)
