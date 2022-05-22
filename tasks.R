# TASKS ###############################################################################################################
library(mlr3verse) # machine learning set-up for modeling
set.seed(42)

# Load data ------------------------------------------------------------------------------------------------------------
# load preperad dataset
data <- readRDS("data/JuSAW_prepared.rds")

# Load other scripts ---------------------------------------------------------------------------------------------------
source("variable_sets.R", encoding="utf-8") # for predefined feature sets

# Load test ids
#write(ids, file = "test_ids.txt")
test_ids = scan("test_ids.txt")

# Task set-up ==========================================================================================================
# Set up different tasks with different variable sets 
# AMS Taks -------------------------------------------------------------------------------------------------------------
# Original AMS-Model youth _____________________________________________________________________________________________
task_ams_youth = as_task_classif(data[ams_youth], target = "EMPLOYMENTDAYS", positive = ">=90 Days", id = "AMS youth")
#task_ams_youth = as_task_classif(data[ams_youth], target = "r_besch", positive = 1, id = "AMS youth")

#task_ams_youth$set_col_roles("case", roles = "name")

# Set protected attribute
task_ams_youth$col_roles$pta = "GENDER"

# remove incomplete rows
ids = complete.cases(task_ams_youth$data())
sum(!ids)
task_ams_youth$filter(which(ids))

print(task_ams_youth) # 950 Beobachtungen mit 10 Variablen
# Set test set holdout rows
task_ams_youth$set_row_roles(test_ids, roles = "holdout")
print(task_ams_youth)

# check missings in holdout
ids = complete.cases(task_ams_youth$data(test_ids))
sum(!ids)

# Original AMS full ____________________________________________________________________________________________________
task_ams = as_task_classif(data[ams], target = "EMPLOYMENTDAYS", positive = ">=90 Days", id = "AMS")


ids = complete.cases(task_ams$data())
sum(!ids)
task_ams$filter(which(ids))

# Set protected attribute
task_ams$col_roles$pta = "GENDER"

# Set test set holdout rows
task_ams$set_row_roles(test_ids, roles = "holdout")

print(task_ams) # 950 Beobachtungen mit 10 Variablen

# AMS extended _________________________________________________________________________________________________________
task_ams_ext = as_task_classif(data[ams_ext], target = "EMPLOYMENTDAYS", positive = ">=90 Days", id = "AMS extended")


ids = complete.cases(task_ams_ext$data())
sum(!ids)
task_ams_ext$filter(which(ids))

# Set protected attribute
task_ams_ext$col_roles$pta = "GENDER"


# Set test set holdout rows
task_ams_ext$set_row_roles(test_ids, roles = "holdout")

print(task_ams_ext) # 1009 Beobachtungen mit 16 Variablen
unique(task_ams_ext$feature_types$type)

# Explorative Analysis variables ---------------------------------------------------------------------------------------
# Green big ____________________________________________________________________________________________________________
task_green_big = as_task_classif(data[green_big], target = "EMPLOYMENTDAYS", positive = ">=90 Days", id = "green big")


ids = complete.cases(task_green_big$data())
sum(!ids)
task_green_big$filter(which(ids))

# Set protected attribute
task_green_big$col_roles$pta = "GENDER"

# Set test set holdout rows
task_green_big$set_row_roles(test_ids, roles = "holdout")

print(task_green_big) # 1009 Beobachtungen mit 16 Variablen
unique(task_green_big$feature_types$type)

# check missings in holdout
ids = complete.cases(task_green_big$data(test_ids))
sum(!ids)


# Make test_ids with complete cases for a lot of variables
# ho = rsmp("holdout", ratio = 0.677) # 0.677 ist 160/496 Beobachtungen in task_green_big
# learner = lrn("classif.featureless", id = "Only M Group", predict_type = "prob", predict_sets = "test")
# ho$instantiate(task_green_big)
# ho$test_set(1)
# write(ho$test_set(1), file = "test_ids.txt")

# Green small __________________________________________________________________________________________________________
task_green = as_task_classif(data[green], target = "EMPLOYMENTDAYS", positive = ">=90 Days", id = "green")



ids = complete.cases(task_green$data())
sum(!ids)
task_green$filter(which(ids))

task_green$col_roles$pta = "GENDER"
# Set test set holdout rows
task_green$set_row_roles(test_ids, roles = "holdout")

print(task_green) # 743 Beobachtungen mit 30 Variablen


unique(task_green$feature_types$type)

# check missings in holdout
ids = complete.cases(task_green_big$data(test_ids))
sum(!ids)

# Check test set equality ==============================================================================================
setequal(task_ams_youth$row_roles$holdout, task_green$row_roles$holdout)


#setequal(task_ams_youth$data(test_ids), task_green_big$data(test_ids))
#setdiff(task_ams_youth$data(test_ids), task_green_big$data(test_ids))
