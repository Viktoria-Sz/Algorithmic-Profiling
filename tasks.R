# TASKS ###############################################################################################################
library(mlr3verse) # machine learning set-up for modeling
#set.seed(42)
set.seed(42)

# Load data ------------------------------------------------------------------------------------------------------------
# load preperad dataset
data <- readRDS("data/JuSAW_prepared_clean.rds")


# Load other scripts ---------------------------------------------------------------------------------------------------
source("variable_sets.R", encoding="utf-8") # for predefined feature sets

# Load test ids
#write(ids, file = "test_ids.txt")
test_ids = scan("test_ids.txt")
test_ids_case = scan("test_ids_case.txt")

# Task set-up ##########################################################################################################
# Set up different tasks with different variable sets 
# AMS Taks ==========================================================================================================
# Original AMS-Model youth ---------------------------------------------------------------------------------------------------------
task_ams_youth = as_task_classif(data[c("case", ams_youth)], target = "employmentdays", positive = ">=90 Days", id = "AMS youth")
#task_ams_youth = as_task_classif(data[ams_youth], target = "r_besch", positive = 1, id = "AMS youth")

task_ams_youth$set_col_roles("case", roles = "name")

# Set protected attribute
task_ams_youth$col_roles$pta = "gender"

# remove incomplete rows
ids = complete.cases(task_ams_youth$data())
sum(!ids)
task_ams_youth$filter(which(ids))

print(task_ams_youth) # 950 Beobachtungen mit 10 Variablen
# Set test set holdout rows
#task_ams_youth$set_row_roles(test_ids, roles = "holdout")
#print(task_ams_youth)

# check missings in holdout
# ids = complete.cases(task_ams_youth$data(test_ids))
# sum(!ids)

# Original AMS full ---------------------------------------------------------------------------------------------------------
task_ams = as_task_classif(data[c("case", ams)], target = "employmentdays", positive = ">=90 Days", id = "AMS full")

task_ams$set_col_roles("case", roles = "name")

ids = complete.cases(task_ams$data())
sum(!ids)
task_ams$filter(which(ids))

# Set protected attribute
task_ams$col_roles$pta = "gender"

# Set test set holdout rows
#task_ams$set_row_roles(test_ids, roles = "holdout")

print(task_ams) # 950 Beobachtungen mit 10 Variablen

# AMS extended ---------------------------------------------------------------------------------------------------------
task_ams_ext = as_task_classif(data[c("case", ams_ext)], target = "employmentdays", positive = ">=90 Days", id = "AMS extended")

task_ams_ext$set_col_roles("case", roles = "name")

ids = complete.cases(task_ams_ext$data())
sum(!ids)
task_ams_ext$filter(which(ids))

# Set protected attribute
task_ams_ext$col_roles$pta = "gender"


# Set test set holdout rows
#task_ams_ext$set_row_roles(test_ids, roles = "holdout")

print(task_ams_ext) # 1009 Beobachtungen mit 16 Variablen
unique(task_ams_ext$feature_types$type)

# All ==================================================================================================================
# all filters ----------------------------------------------------------------------------------------------------------
task_all = as_task_classif(data[c("case", all)], target = "employmentdays", positive = ">=90 Days", id = "all")

task_all$set_col_roles("case", roles = "name")

ids = complete.cases(task_all$data())
sum(!ids)
task_all$filter(which(ids))

# Set protected attribute
task_all$col_roles$pta = "gender"

# Set test set holdout rows
#task_all$set_row_roles(test_ids, roles = "holdout")

print(task_all) # 343 Beobachtungen mit 120 Variablen
unique(task_all$feature_types$type)

# check missings in test set
#ids = complete.cases(task_all$data(test_ids))
#sum(!ids)

# all ------------------------------------------------------------------------------------------------------------------
task_filtering_all = as_task_classif(data[c("case", "employmentdays", "gender", filtering_all)], 
                                     target = "employmentdays", positive = ">=90 Days", id = "filtering_all")

task_filtering_all$set_col_roles("case", roles = "name")


ids = complete.cases(task_filtering_all$data())
sum(!ids)
task_filtering_all$filter(which(ids))

task_filtering_all$col_roles$pta = "gender"
# Set test set holdout rows
#task_green$set_row_roles(test_ids, roles = "holdout")

print(task_filtering_all) # 924 Beobachtungen mit 30 Variablen


unique(task_filtering_all$feature_types$type)

# check missings in test set
#ids = complete.cases(task_filtering_all$data(test_ids))
#sum(!ids)

# disr -----------------------------------------------------------------------------------------------------------------
task_filtering_disr = as_task_classif(data[c("case", "employmentdays", "gender", filtering_disr)], 
                                      target = "employmentdays", positive = ">=90 Days", id = "filtering_disr")

task_filtering_disr$set_col_roles("case", roles = "name")


ids = complete.cases(task_filtering_disr$data())
sum(!ids)
task_filtering_disr$filter(which(ids))

task_filtering_disr$col_roles$pta = "gender"
# Set test set holdout rows
#task_green$set_row_roles(test_ids, roles = "holdout")

print(task_filtering_disr) # 972  Beobachtungen mit 30 Variablen


unique(task_filtering_disr$feature_types$type)

# check missings in test set
ids = complete.cases(task_filtering_disr$data(test_ids))
sum(!ids)

# cmim  ----------------------------------------------------------------------------------------------------------------
task_filtering_cmim = as_task_classif(data[c("case", "employmentdays", "gender", filtering_cmim)], 
                                      target = "employmentdays", positive = ">=90 Days", id = "filtering_cmim")

task_filtering_cmim$set_col_roles("case", roles = "name")


ids = complete.cases(task_filtering_cmim$data())
sum(!ids)
task_filtering_cmim$filter(which(ids))

task_filtering_cmim$col_roles$pta = "gender"
# Set test set holdout rows
#task_green$set_row_roles(test_ids, roles = "holdout")

print(task_filtering_cmim) # 953  Beobachtungen mit 30 Variablen


unique(task_filtering_cmim$feature_types$type)

# check missings in test set
ids = complete.cases(task_filtering_cmim$data(test_ids))
sum(!ids)

# relief ---------------------------------------------------------------------------------------------------------------
task_filtering_relief = as_task_classif(data[c("case", "employmentdays", "gender", filtering_relief)], 
                                        target = "employmentdays", positive = ">=90 Days", id = "filtering_relief")

task_filtering_relief$set_col_roles("case", roles = "name")


ids = complete.cases(task_filtering_relief$data())
sum(!ids)
task_filtering_relief$filter(which(ids))

task_filtering_relief$col_roles$pta = "gender"
# Set test set holdout rows
#task_green$set_row_roles(test_ids, roles = "holdout")

print(task_filtering_relief) # 915  Beobachtungen mit 30 Variablen


unique(task_filtering_relief$feature_types$type)

# check missings in test set
ids = complete.cases(task_filtering_relief$data(test_ids))
sum(!ids)


# Handpicked ===========================================================================================================
# Green big ------------------------------------------------------------------------------------------------------------------
task_green_big = as_task_classif(data[c("case", green_big)], target = "employmentdays", positive = ">=90 Days", id = "green big")

task_green_big$set_col_roles("case", roles = "name")

ids = complete.cases(task_green_big$data())
sum(!ids)
task_green_big$filter(which(ids))

# Set protected attribute
task_green_big$col_roles$pta = "gender"

# Set test set holdout rows
#task_green_big$set_row_roles(test_ids, roles = "holdout")

print(task_green_big) # 616 Beobachtungen mit 16 Variablen
unique(task_green_big$feature_types$type)

# check missings in test set
#ids = complete.cases(task_green_big$data(test_ids))
#sum(!ids)

# filter green big -----------------------------------------------------------------------------------------------------
task_filtering_green = as_task_classif(data[c("case", "employmentdays", "gender", filtering_green)], 
                                       target = "employmentdays", positive = ">=90 Days", id = "filtering_green")

task_filtering_green$set_col_roles("case", roles = "name")


ids = complete.cases(task_filtering_green$data())
sum(!ids)
task_filtering_green$filter(which(ids))

task_filtering_green$col_roles$pta = "gender"
# Set test set holdout rows
#task_green$set_row_roles(test_ids, roles = "holdout")

print(task_filtering_green) # 924 Beobachtungen mit 30 Variablen


unique(task_filtering_green$feature_types$type)

# check missings in test set
#ids = complete.cases(task_filtering_green$data(test_ids))
#sum(!ids)



# diverse ------------------------------------------------------------------------------------------------------------------
task_diverse = as_task_classif(data[c("case", diverse)], target = "employmentdays", positive = ">=90 Days", id = "diverse")

task_diverse$set_col_roles("case", roles = "name")

ids = complete.cases(task_diverse$data())
sum(!ids)
task_diverse$filter(which(ids))

task_diverse$col_roles$pta = "gender"
# Set test set holdout rows
#task_diverse$set_row_roles(test_ids, roles = "holdout")

print(task_diverse) # 743 Beobachtungen mit 30 Variablen


unique(task_diverse$feature_types$type)

# check missings in test set
# ids = complete.cases(task_diverse$data(test_ids))
# sum(!ids)



# characteristics ------------------------------------------------------------------------------------------------------------
task_characteristics = as_task_classif(data[c("case", "employmentdays", characteristics)], 
                                       target = "employmentdays", positive = ">=90 Days", id = "characteristics")

task_characteristics$set_col_roles("case", roles = "name")

ids = complete.cases(task_characteristics$data())
sum(!ids)
task_characteristics$filter(which(ids))

# Set protected attribute
# task_characteristics$col_roles$pta = "gender"

# Set test set holdout rows
#task_characteristics$set_row_roles(test_ids, roles = "holdout")

print(task_characteristics) # 1082 x 72
unique(task_characteristics$feature_types$type)

# check missings in test set
ids = complete.cases(task_characteristics$data(test_ids))
sum(!ids)

# characteristics filter ------------------------------------------------------------------------------------------------------------
task_characteristics_filter = as_task_classif(data[c("case", "employmentdays", filtering_characteristics)], 
                                       target = "employmentdays", positive = ">=90 Days", id = "characteristics_filter")

task_characteristics_filter$set_col_roles("case", roles = "name")

ids = complete.cases(task_characteristics_filter$data())
sum(!ids)
task_characteristics_filter$filter(which(ids))

# Set protected attribute
# task_characteristics_filter$col_roles$pta = "gender"

# Set test set holdout rows
#task_characteristics_filter$set_row_roles(test_ids, roles = "holdout")

print(task_characteristics_filter) # 1090 x 26
unique(task_characteristics_filter$feature_types$type)

# check missings in test set
ids = complete.cases(task_characteristics_filter$data(test_ids))
sum(!ids)



# behavior ------------------------------------------------------------------------------------------------------------
task_behavior = as_task_classif(data[c("case", "employmentdays", behavior)],
                                target = "employmentdays", positive = ">=90 Days", id = "behavior")

task_behavior$set_col_roles("case", roles = "name")

ids = complete.cases(task_behavior$data())
sum(!ids)
task_behavior$filter(which(ids))

# Set protected attribute
# task_behavior$col_roles$pta = "gender"

# Set test set holdout rows
#task_behavior$set_row_roles(test_ids, roles = "holdout")

print(task_behavior) # 1112 x 13
unique(task_behavior$feature_types$type)

# check missings in test set
ids = complete.cases(task_behavior$data(test_ids))
sum(!ids)

# attitudes ------------------------------------------------------------------------------------------------------------
task_attitudes = as_task_classif(data[c("case", "employmentdays", attitudes)], 
                                 target = "employmentdays", positive = ">=90 Days", id = "attitudes")

task_attitudes$set_col_roles("case", roles = "name")

ids = complete.cases(task_attitudes$data())
sum(!ids)
task_attitudes$filter(which(ids))

# Set protected attribute
# task_attitudes$col_roles$pta = "gender"

# Set test set holdout rows
#task_attitudes$set_row_roles(test_ids, roles = "holdout")

print(task_attitudes) # 1133 x 24
unique(task_attitudes$feature_types$type)

# check missings in test set
ids = complete.cases(task_attitudes$data(test_ids))
sum(!ids)

# personality ------------------------------------------------------------------------------------------------------------
task_personality = as_task_classif(data[c("case", "employmentdays", personality)], 
                                   target = "employmentdays", positive = ">=90 Days", id = "personality")

task_personality$set_col_roles("case", roles = "name")

ids = complete.cases(task_personality$data())
sum(!ids)
task_personality$filter(which(ids))

# Set protected attribute
# task_personality$col_roles$pta = "gender"

# Set test set holdout rows
#task_personality$set_row_roles(test_ids, roles = "holdout")

print(task_personality) # 1082 x 24
unique(task_personality$feature_types$type)

# check missings in test set
ids = complete.cases(task_personality$data(test_ids))
sum(!ids)

# Make Test set ========================================================================================================
# task_test = as_task_classif(data[c("case", green_big, filtering)], target = "employmentdays", positive = ">=90 Days", id = "test")
# 
# #task_test$set_col_roles("case", roles = "name")
# 
# 
# ids = complete.cases(task_test$data())
# sum(!ids)
# task_test$filter(which(ids))
# 
# print(task_test) # 616 Beobachtungen mit 30 Variablen
# 
# # Make test_ids with complete cases for a lot of variables
# ho = rsmp("holdout", ratio = 0.74) # 0.677 ist 160/616 Beobachtungen in task_green_big
# learner = lrn("classif.featureless", id = "Only M Group")
# ho$instantiate(task_test)
# ho$test_set(1)
# length(ho$test_set(1))
# 
# test_ids = ho$test_set(1)
# task_test$data(ho$test_set(1), "case")
# 
# case_ids = task_test$data(test_ids)$case
# 
# write(ho$test_set(1), file = "test_ids2.txt")
# write(case_ids, file = "test_ids_case2.txt")
# 
# # Check test set equality ==============================================================================================
# #setequal(task_ams_youth$row_roles$holdout, task_green$row_roles$holdout)
# 
# 
# #setequal(task_green_big$data(test_ids), task_filtering$data(test_ids))
# #setdiff(task_ams_youth$data(test_ids), task_filtering$data(test_ids))
# #is.element(task_green_big$data(), task_filtering$data())
# 
# 
# # setequal(task_green_big$data(which(is.element(task_green_big$row_names$row_name, case_ids))), 
# #          task_filtering$data(which(is.element(task_filtering$row_names$row_name, case_ids))))
# 
# 
#is.element(test_ids, task_filtering$row_names$row_id)

