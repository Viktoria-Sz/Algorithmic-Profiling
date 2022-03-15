# VARIABLE SELECTION ###########################################################

# Preparation ==================================================================
# Libraries --------------------------------------------------------------------
library("mlr3verse")

# Load data --------------------------------------------------------------------
# load preperad dataset
data <- readRDS("data/JuSAW_prepared.rds")


# Validation and Train-/Test Split ---------------------------------------------
# Training set for tuning, test set for final evaluation on untouched data
train_test_ratio = .8
data_training = dplyr::sample_frac(tbl = data, size = train_test_ratio)
data_test = dplyr::anti_join(x = data, y = data_training, by = "case")

ggplot(data_test, aes(x = EMPLOYMENTDAYS)) +
  geom_bar()

# Selection procedures =========================================================
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

# Classification Tree selection ------------------------------------------------
# QUESTION: What measure does it use to select features?
task = as_task_classif(data[green_all], 
                      target = "EMPLOYMENTDAYS", positive = ">=90 Tage", id = "selection_tree")

learner = lrn("classif.rpart", predict_type = "prob")

# ensure that the learner selects features
stopifnot("selected_features" %in% learner$properties)

# fit a simple classification tree
learner = learner$train(task)

# extract all features used in the classification tree:
learner$selected_features()

#----------------------
hout = rsmp("holdout")
measure = msr("classif.auc")
evals20 = trm("evals", n_evals = 20)

instance = FSelectInstanceSingleCrit$new(
  task = task,
  learner = learner,
  resampling = hout,
  measure = measure,
  terminator = evals20
)
instance
fselector = fs("random_search")
# reduce logging output
lgr::get_logger("bbotk")$set_threshold("warn")

fselector$optimize(instance)
instance$result_feature_set
instance$result_y
# as.data.table(instance$archive)

# Random Forest selection ------------------------------------------------------
lrn = lrn("classif.ranger", importance = "impurity")
filter = flt("importance", learner = lrn)
filter$calculate(task)
as.data.table(filter)

# Filtering methods ------------------------------------------------------------
filter = flt("jmim")
filter$calculate(task)
as.data.table(filter)


filter_cor = flt("correlation")
filter_cor$param_set
# change parameter 'method'
# filter_cor$param_set$values = list(method = "spearman")
# filter_cor$param_set
filter_cor$calculate(task)
as.data.table(filter_cor)