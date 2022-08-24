# VARIABLE SELECTION ###################################################################################################

# Preparation ==========================================================================================================
# Libraries ------------------------------------------------------------------------------------------------------------
library("mlr3verse")

# Load other scripts ---------------------------------------------------------------------------------------------------
source("tasks.R", encoding="utf-8") # for predefined feature sets
set.seed(42)

# Validation and Train-/Test Split ---------------------------------------------
# Training set for tuning, test set for final evaluation on untouched data
# train_test_ratio = .8
# data_training = dplyr::sample_frac(tbl = data, size = train_test_ratio)
# data_test = dplyr::anti_join(x = data, y = data_training, by = "case")
# 
# ggplot(data_test, aes(x = EMPLOYMENTDAYS)) +
#   geom_bar()

# Selection procedures =================================================================================================
# Criteria for variable selection: (s.76)
# 1. Significance tests
# 2. AIC
# 3. BIC
# 4. Bayesian procedures – Bayes factors (e.g. BAS package)
# 5. Deviance information criterion (DIC in WinBUGS)
# Other methods:
# 1. Ridge regression
# 2. Lasso and shrinkage methods
# 3. Bayesian variable selection and model search algorithms


# Filter methods =======================================================================================================
# Pure filter methods --------------------------------------------------------------------------------------------------
# Avaible filters: 
mlr_filters
# only for numeric y variables
# "anova", "kruskal_test", "auc", "variance", "find_correlation", "information_gain",

# Learner methods:
# "importance", "performance", "permutation",

# General methods:
# "selected_features",

filter_all = c("disr", "cmim", "jmi", "jmim", "mim", "mrmr", "njmim", "relief")

filter_used = lapply(filter_all, flt)
# lapply(filter_used, function(i) i$calculate(task_all))
# lapply(filter_used, function(i) i$calculate(task_green_big))
lapply(filter_used, function(i) i$calculate(task_all))

filter_scores = lapply(filter_used, as.data.table)
features_15 = lapply(filter_scores, function(i) i$feature[1:15])
names(features_15) = filter_all
features_50 = lapply(filter_scores, function(i) i$feature[1:50])
names(features_50) = filter_all
filter_features = Reduce(intersect, features_50[1:7])
filter_features_disr = features_15$disr
filter_features_cmim = features_15$cmim
filter_features_jmi = features_15$jmi
filter_features_mrmr = features_15$mrmr
filter_features_relief = features_15$relief

#write(filter_features, file = "variable sets/filter_features_all.txt")
# write(filter_features_disr, file = "variable sets/filter_features_disr.txt")
# write(filter_features_jmi, file = "variable sets/filter_features_jmi.txt")
# write(filter_features_mrmr, file = "variable sets/filter_features_mrmr.txt")
# write(filter_features_cmim, file = "variable sets/filter_features_cmim.txt")

# write(filter_features_relief, file = "variable sets/filter_features_relief.txt")

write(filter_features, file = "variable sets/filter_characteristics_all.txt")


# filter = flt("relief")
# filter$calculate(task_green_big)
# as.data.table(filter)

# ----------------------------------------------------------------------------------------------------------------------

# Learner
ranger_cmim = po("filter", flt("cmim"), filter.nfeat = 3) %>>% lrn("classif.ranger", predict_type = "prob")
# nfeat determines the minimum number of features to score (see details), and defaults to the number of features in task. 

# Tune over the filter parameter. It steers how many features are kept by the Filter
search_space = ps(cmim.filter.nfeat = p_int(lower = 1, upper = length(task_green_big$feature_names)))

resampling = rsmp("holdout", ratio = 0.8)

# The problem is one-dimensional (i.e. only one parameter is used). Thus we make use of a grid search.
instance = tune(
  method = "grid_search",
  task = task_green_big,
  learner = ranger_cmim,
  resampling = resampling,
  measure = msr("classif.auc"),
  search_space = search_space
)
autoplot(instance, type = "marginal")

# Variable Importance Filters in learners ------------------------------------------------------------------------------
# The following learners allow the extraction of variable importance and therefore are supported by FilterImportance
# "classif.ranger" , "classif.rpart" , "classif.xgboost"

# Classification Tree selection ________________________________________________________________________________________
lrn_rpart_imp = lrn("classif.rpart")
filter_rpart_imp = flt("importance", learner = lrn_rpart_imp)
filter_rpart_imp$calculate(task_green_big)
filter_rpart_imp = as.data.table(filter_rpart_imp)

filter_scores = append(filter_scores, list(filter_rpart_imp))

# Random Forest selection ______________________________________________________________________________________________
lrn_ranger_imp = lrn("classif.ranger", importance = "impurity")
filter_ranger_imp = flt("importance", learner = lrn_ranger_imp)
filter_ranger_imp$calculate(task_green_big)
filter_ranger_imp = as.data.table(filter_ranger_imp)

filter_scores = append(filter_scores, list(filter_ranger_imp))



# Wrapper methods ======================================================================================================
learner = lrn("classif.rpart", predict_type = "prob")
hout = rsmp("holdout")
measure = msr("classif.auc")
evals20 = trm("evals", n_evals = 20)
fselector = fs("random_search")

instance = FSelectInstanceSingleCrit$new(
  task = task_green,
  learner = learner,
  resampling = hout,
  measure = measure,
  terminator = evals20
)
instance
# reduce logging output
lgr::get_logger("bbotk")$set_threshold("warn")

fselector$optimize(instance)
instance$result_feature_set
instance$result_y
#as.data.table(instance$archive)

# Now the optimized feature subset can be used to subset the task and fit the model on all observations
task$select(instance$result_feature_set)
learner$train(task)



# RESTE ################################################################################################################
# Classification Tree selection ________________________________________________________________________________________
# QUESTION: What measure does it use to select features?
lrn_rpart_imp = lrn("classif.rpart", predict_type = "prob")

# ensure that the learner selects features
stopifnot("selected_features" %in% learner$properties)

# fit a simple classification tree
learner = learner$train(task_green_big)

# extract all features used in the classification tree:
learner$selected_features()


# xgboost selection ____________________________________________________________________________________________________
learner_xgboost = lrn("classif.xgboost")
fencoder = po("encode", method = "treatment", affect_columns = selector_type("factor"))
ord_to_int = po("colapply", applicator = as.integer, affect_columns = selector_type("ordered"))
graph = fencoder %>>% ord_to_int %>>% learner_xgboost
learner_xgboost_imp = as_learner(graph)

learner_xgboost_imp
learner_xgboost$importance()

filter_imp = flt("importance", learner = learner_xgboost)
graph = fencoder %>>% ord_to_int %>>% filter_imp
learner_xgboost_imp = as_learner(graph)

filter_imp$calculate(task_green_big)
as.data.table(filter_imp)
