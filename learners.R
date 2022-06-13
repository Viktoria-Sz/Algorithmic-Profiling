# LEARNERS #############################################################################################################
library(mlr3verse) # machine learning set-up for modeling
set.seed(42)

# Preperation ==========================================================================================================
# 3 fold cross validation for inner loop
resampling_inner_3CV = rsmp("cv", folds = 3L)

measures_tuning = msr("classif.auc")

# Choose optimization algorithm:
# no need to etra randomize, try to go every step
#tuner_grid_search_ranger = tnr("grid_search", resolution = 5)  
tuner = tnr("random_search")

# Set the budget (when to terminate):
terminator = trm("evals", n_evals = 5)


# use mlr3tuningspaces for default tuning search spaces
# Collection of search spaces for hyperparameter tuning. 
# Includes various search spaces that can be directly applied to an mlr3 learner. 
# Additionally, meta information about the search space can be queried.
as.data.table(mlr_tuning_spaces)

# Tuning spaces from the Bischl (2021) article.

# Learners =============================================================================================================
# glmnet: Penalized logistic regression --------------------------------------------------------------------------------
learner_glmnet = lrn("classif.glmnet", predict_type = "prob", predict_sets = "test")

tuning_space_glmnet = lts("classif.glmnet.rbv2")
learner_glmnet$param_set$values = tuning_space_glmnet$values

learner_glmnet$param_set$values

tuner_glmnet = AutoTuner$new(
  learner = learner_glmnet,
  resampling = resampling_inner_3CV,
  measure = measures_tuning, 
  #search_space = param_ranger, 
  terminator = terminator,
  tuner = tuner
)

fencoder = po("encode", method = "treatment", affect_columns = selector_type("factor"))
ord_to_int = po("colapply", applicator = as.integer, affect_columns = selector_type("ordered"))
graph = fencoder %>>% ord_to_int %>>% tuner_glmnet

graph_glmnet = as_learner(graph)

# Tune lambda
# https://mlr3learners.mlr-org.com/reference/mlr_learners_classif.glmnet.html 

# Decision Tree --------------------------------------------------------------------------------------------------------
learner_rpart = lrn("classif.rpart", id = "Decision Tree", predict_type = "prob", predict_sets = "test")
learner_rpart$param_set

tuning_space_rpart = lts("classif.rpart.default")
learner_rpart$param_set$values = tuning_space_rpart$values

# The complexity hyperparameter cp that controls when the learner considers introducing another branch.
# The minsplit hyperparameter that controls how many observations must be present in a leaf for another split to be attempted.

tuner_rpart = AutoTuner$new(
  learner = learner_rpart,
  resampling = resampling_inner_3CV,
  measure = measures_tuning, 
  #search_space = param_ranger, 
  terminator = terminator,
  tuner = tuner
)


# Random Forest --------------------------------------------------------------------------------------------------------
# Define learner:
learner_ranger = lrn("classif.ranger", id = "Random Forest", predict_type = "prob", predict_sets = "test")

# possible parameter to tune:
learner_ranger$param_set

# get tuning space from default tuning search spaces
tuning_space_ranger = lts("classif.ranger.default")

learner_ranger$param_set$values = tuning_space_ranger$values


# Set up autotuner instance with the predefined setups
tuner_ranger = AutoTuner$new(
  learner = learner_ranger,
  resampling = resampling_inner_3CV,
  measure = measures_tuning, 
  #search_space = param_ranger, 
  terminator = terminator,
  tuner = tuner
)
#tuner_ranger$train(task_ams)

# xgboost --------------------------------------------------------------------------------------------------------------
# Define learner:
learner_xgboost = lrn("classif.xgboost", predict_type = "prob", predict_sets = "test")

# tune nrounds, eta and booster:
learner_xgboost$param_set

# Parameters to tune according to mlr3 website:
# nrounds

# get tuning space from default tuning search spaces
tuning_space_xgboost = lts("classif.xgboost.default")

learner_xgboost$param_set$values = tuning_space_xgboost$values
tuning_space_xgboost$values


# Set up autotuner instance with the predefined setups
tuner_xgboost = AutoTuner$new(
  learner = learner_xgboost,
  resampling = resampling_inner_3CV,
  measure = measures_tuning, 
  terminator = terminator,
  tuner = tuner
)

graph = fencoder %>>% ord_to_int %>>% tuner_xgboost
graph_xgboost = as_learner(graph)

# SVM ------------------------------------------------------------------------------------------------------------------
learner_svm = lrn("classif.svm", predict_type = "prob", predict_sets = "test")

# Possible tuning parameter
learner_svm$param_set

# get tuning space from default tuning search spaces
tuning_space_svm = lts("classif.svm.rbv2")

learner_svm$param_set$values = tuning_space_svm$values
tuning_space_svm$values


# Set up autotuner instance with the predefined setups
tuner_svm = AutoTuner$new(
  learner = learner_svm,
  resampling = resampling_inner_3CV,
  measure = measures_tuning, 
  terminator = terminator,
  tuner = tuner
)

graph = fencoder %>>% ord_to_int %>>% tuner_svm
graph_svm = as_learner(graph)


# Error in self$assert(xs) : 
#   Assertion on 'xs' failed: The parameter 'cost' can only be set if the following condition is met 'type = C-classification'. Instead the parameter value for 'type' is not set at all. Try setting 'type' to a value that satisfies the condition.
# This happened PipeOp classif.svm.tuned's $train()

# Autotune kknn --------------------------------------------------------------------------------------------------------
# Define learner:
learner_kknn = lrn("classif.kknn", predict_type = "prob", predict_sets = "test")

# tune k in knn:
learner_kknn$param_set

# get tuning space from default tuning search spaces
tuning_space_kknn = lts("classif.kknn.rbv2")

learner_kknn$param_set$values = tuning_space_kknn$values
tuning_space_kknn$values


# Set up autotuner instance with the predefined setups
tuner_kknn = AutoTuner$new(
  learner = learner_kknn,
  resampling = resampling_inner_3CV,
  measure = measures_tuning, 
  terminator = terminator,
  tuner = tuner
)


# resampling_outer_ho = rsmps("holdout", ratio = 0.8)
# design = benchmark_grid(
#   tasks = task_ams,
#   learners = c(tuner_rpart, tuner_ranger, graph_glmnet, graph_xgboost, tuner_kknn),
#   resamplings = resampling_outer_ho
# )
# bmr = benchmark(design)
# bmr$score(performance_measures)

