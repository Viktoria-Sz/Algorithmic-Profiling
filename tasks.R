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

# Task set-up ==========================================================================================================
# Set up different tasks with different variable sets 
# AMS Taks -------------------------------------------------------------------------------------------------------------
# Original AMS-Model youth _____________________________________________________________________________________________
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

# Original AMS full ____________________________________________________________________________________________________
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

# AMS extended _________________________________________________________________________________________________________
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

# Explorative Analysis variables ---------------------------------------------------------------------------------------
# All __________________________________________________________________________________________________________________
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


# Green big ____________________________________________________________________________________________________________
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


# Green small __________________________________________________________________________________________________________
task_green = as_task_classif(data[c("case", green)], target = "employmentdays", positive = ">=90 Days", id = "green small")

task_green$set_col_roles("case", roles = "name")

ids = complete.cases(task_green$data())
sum(!ids)
task_green$filter(which(ids))

task_green$col_roles$pta = "gender"
# Set test set holdout rows
#task_green$set_row_roles(test_ids, roles = "holdout")

print(task_green) # 743 Beobachtungen mit 30 Variablen


unique(task_green$feature_types$type)

# check missings in test set
# ids = complete.cases(task_green$data(test_ids))
# sum(!ids)

# Filtering ------------------------------------------------------------------------------------------------------------
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

# all __________________________________________________________________________________________________________________
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

# disr __________________________________________________________________________________________________________________
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

# cmim  __________________________________________________________________________________________________________________
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

# relief  ______________________________________________________________________________________________________________
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

