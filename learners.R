# LEARNERS #############################################################################################################
library(mlr3verse) # machine learning set-up for modeling
set.seed(42)

# Preperation ----------------------------------------------------------------------------------------------------------
# 3 fold cross validation for inner loop
resampling_inner_3CV = rsmp("cv", folds = 3L)

measures_tuning = msr("classif.auc")

# Learners -------------------------------------------------------------------------------------------------------------
# Penalized logistic regression glmnet _________________________________________________________________________________
learner_glmnet = lrn("classif.glmnet", predict_type = "prob", predict_sets = "test")
fencoder = po("encode", method = "treatment", affect_columns = selector_type("factor"))
ord_to_int = po("colapply", applicator = as.integer, affect_columns = selector_type("ordered"))
graph = fencoder %>>% ord_to_int %>>% learner_glmnet

graph_glmnet = as_learner(graph)

# xgboost ______________________________________________________________________________________________________________
# Define learner:
learner_xgboost = lrn("classif.xgboost", predict_type = "prob", predict_sets = "test")
graph = fencoder %>>% ord_to_int %>>% learner_xgboost

graph_xgboost = as_learner(graph)

# tune nrounds, eta and booster:
learner_xgboost$param_set

# Hyperparameter subset of XGBoost
param_xgboost = ps(
  nrounds = p_int(lower = 1, upper = 16, tags = "budget"),
  eta = p_dbl(lower = 0, upper = 1),
  booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
)

# Choose optimization algorithm:
# no need to etra randomize, try to go every step
tuner_grid_search_ranger = tnr("grid_search", resolution = 30)  

# Set the budget (when to terminate):
terminator_xgboost = trm("evals", n_evals = 30)

# Set up autotuner instance with the predefined setups
tuner_ranger = AutoTuner$new(
  learner = learner_xgboost,
  resampling = resampling_inner_3CV,
  measure = measures_tuning, 
  search_space = param_xgboost, 
  terminator = terminator_xgboost,
  tuner = tuner_grid_search_ranger
)


# SVM __________________________________________________________________________________________________________________
learner_svm = lrn("classif.svm", predict_type = "prob", predict_sets = "test")
graph = fencoder %>>% ord_to_int %>>% learner_svm

graph_svm = as_learner(graph)

# tune mtry for the random forest:
learner_svm$param_set

search_space = ps(cost = p_dbl(0.1, 10), kernel = p_fct(c("polynomial", "radial")))
# see searchspace grid
# rbindlist(generate_design_grid(search_space, 3)$transpose())

# The Support Vector Machine (SVM), for example, has the degree parameter that is only valid when kernel is "polynomial"
# search_space = ps(
#   cost = p_dbl(-1, 1, trafo = function(x) 10^x),
#   kernel = p_fct(c("polynomial", "radial")),
#   degree = p_int(1, 3, depends = kernel == "polynomial")
# )
# rbindlist(generate_design_grid(search_space, 3)$transpose(), fill = TRUE)


# we will try all configurations: 1 to 21 features.
param_mtry = ps(mtry = p_int(lower = 1, upper = 30)) 

# Choose optimization algorithm:
# no need to etra randomize, try to go every step
tuner_grid_search_ranger = tnr("grid_search", resolution = 30)  

# Set the budget (when to terminate):
terminator_mtry = trm("evals", n_evals = 30)

# Set up autotuner instance with the predefined setups
tuner_ranger = AutoTuner$new(
  learner = learner_svm,
  resampling = resampling_inner_3CV,
  measure = measures_tuning, 
  search_space = param_mtry, 
  terminator = terminator_mtry,
  tuner = tuner_grid_search_ranger
)


# Regression Tree ______________________________________________________________________________________________________
# The complexity hyperparameter cp that controls when the learner considers introducing another branch.
# The minsplit hyperparameter that controls how many observations must be present in a leaf for another split to be attempted.

search_space = ps(
  cp = p_dbl(lower = 0.001, upper = 0.1),
  minsplit = p_int(lower = 1, upper = 10)
)

learner_tree = lrn("classif.rpart",
                   predict_type = "prob",
                   "cp" = 0.001) 
# set cp super low to enforce new splits so we get FPR < 1%

# Autotune knn _________________________________________________________________________________________________________
# Define learner:
learner_knn = lrn("classif.kknn", predict_type = "prob", predict_sets = "test")

# tune k in knn:
learner_knn$param_set

# tune the chosen hyperparameters with these boundaries:
param_k = ps(k = p_int(lower = 1, upper = 50))

# Choose optimization algorithm:
# no need to randomize, try to go every step
tuner_grid_search_knn = tnr("grid_search", resolution = 50)

# Set the budget (when to terminate):
# test every candidate
terminator_knn = trm("evals", n_evals = 50)

# Set up autotuner instance with the predefined setups
tuner_knn = AutoTuner$new(
  learner = learner_knn,
  resampling = resampling_inner_3CV,
  measure = measures_tuning, 
  search_space = param_k, 
  terminator = terminator_knn,
  tuner = tuner_grid_search_knn
)

# Autotune Random Forest _______________________________________________________________________________________________
# Define learner:
learner_ranger = lrn("classif.ranger", predict_type = "prob", predict_sets = "test") #, importance = "impurity")

# tune mtry for the random forest:
learner_ranger$param_set

# we will try all configurations: 1 to 21 features.
param_mtry = ps(mtry = p_int(lower = 1, upper = 30)) 

# Choose optimization algorithm:
# no need to etra randomize, try to go every step
tuner_grid_search_ranger = tnr("grid_search", resolution = 30)  

# Set the budget (when to terminate):
terminator_mtry = trm("evals", n_evals = 30)

# Set up autotuner instance with the predefined setups
tuner_ranger = AutoTuner$new(
  learner = learner_ranger,
  resampling = resampling_inner_3CV,
  measure = measures_tuning, 
  search_space = param_mtry, 
  terminator = terminator_mtry,
  tuner = tuner_grid_search_ranger
)



