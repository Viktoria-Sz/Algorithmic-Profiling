# VARIABLE SELECTION ###################################################################################################

# Preparation ==========================================================================================================
# Libraries ------------------------------------------------------------------------------------------------------------
library("mlr3verse")

# Load other scripts ---------------------------------------------------------------------------------------------------
source("tasks.R", encoding="utf-8") # for predefined feature sets
set.seed(42)

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
#filter_features = Reduce(intersect, features_50[1:7])
filter_features_disr = features_15$disr
filter_features_cmim = features_15$cmim
filter_features_jmi = features_15$jmi
filter_features_mrmr = features_15$mrmr
filter_features_relief = features_15$relief

#write(filter_features, file = "variable sets/filter_features_all.txt")
write(filter_features_disr, file = "variable sets/filter_features_disr.txt")
write(filter_features_jmi, file = "variable sets/filter_features_jmi.txt")
write(filter_features_mrmr, file = "variable sets/filter_features_mrmr.txt")
write(filter_features_cmim, file = "variable sets/filter_features_cmim.txt")
write(filter_features_relief, file = "variable sets/filter_features_relief.txt")

# write(filter_features, file = "variable sets/filter_characteristics_all.txt")
