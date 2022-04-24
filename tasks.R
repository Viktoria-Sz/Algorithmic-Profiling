# TASKS ###############################################################################################################
library(mlr3verse) # machine learning set-up for modeling
set.seed(42)

# Load data ------------------------------------------------------------------------------------------------------------
# load preperad dataset
data <- readRDS("data/JuSAW_prepared.rds")

# Load other scripts ---------------------------------------------------------------------------------------------------
source("variable_sets.R", encoding="utf-8") # for predefined feature sets

# Task set-up ==========================================================================================================
# Set up different tasks with different variable sets 
# AMS Taks -------------------------------------------------------------------------------------------------------------
# Original AMS-Model youth _____________________________________________________________________________________________
task_ams_youth = as_task_classif(data[ams_youth], target = "EMPLOYMENTDAYS", positive = ">=90 Days", id = "AMS youth")
#task_ams_youth = as_task_classif(data[ams_youth], target = "r_besch", positive = 1, id = "AMS youth")

#task_ams_youth$set_col_roles("case", roles = "name")

ids = complete.cases(task_ams_youth$data())
sum(!ids)
task_ams_youth$filter(which(ids))

# Set protected attribute
task_ams_youth$col_roles$pta = "GENDER"
print(task_ams_youth) # 950 Beobachtungen mit 10 Variablen

# Original AMS full ____________________________________________________________________________________________________
task_ams = as_task_classif(data[ams], target = "EMPLOYMENTDAYS", positive = ">=90 Days", id = "AMS")
ids = complete.cases(task_ams$data())
sum(!ids)
task_ams$filter(which(ids))

# Set protected attribute
task_ams$col_roles$pta = "GENDER"
print(task_ams) # 950 Beobachtungen mit 10 Variablen

# AMS extended _________________________________________________________________________________________________________
task_ams_ext = as_task_classif(data[ams_ext], target = "EMPLOYMENTDAYS", positive = ">=90 Days", id = "AMS extended")
ids = complete.cases(task_ams_ext$data())
sum(!ids)
task_ams_ext$filter(which(ids))

# Set protected attribute
task_ams_ext$col_roles$pta = "GENDER"
print(task_ams_ext) # 1009 Beobachtungen mit 16 Variablen
unique(task_ams_ext$feature_types$type)

# Explorative Analysis variables ---------------------------------------------------------------------------------------
# Green big ____________________________________________________________________________________________________________
task_green_big = as_task_classif(data[green_big], target = "EMPLOYMENTDAYS", positive = ">=90 Days", id = "green big")

# Green small __________________________________________________________________________________________________________
task_green = as_task_classif(data[green], target = "EMPLOYMENTDAYS", positive = ">=90 Days", id = "green")

ids = complete.cases(task_green$data())
sum(!ids)
task_green$filter(which(ids))

task_green$col_roles$pta = "GENDER"
print(task_green) # 743 Beobachtungen mit 30 Variablen


unique(task_green$feature_types$type)
