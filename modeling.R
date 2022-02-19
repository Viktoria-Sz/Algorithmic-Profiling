# MODELLING ####################################################################
# Preparation ==================================================================
# Libraries --------------------------------------------------------------------
library(tidyverse)
#library(modelr)
library(mlr3) # machine learning set-up for modeling
library(mlr3viz)
options(na.action = na.warn)
options(scipen=100) # prevent scientific notion (e)

# Load data --------------------------------------------------------------------
# load preperad dataset
data <- readRDS("data/JuSAW_prepared.rds")
attach(data)

# Load other scripts -----------------------------------------------------------

# Task set-up ------------------------------------------------------------------
task_AMS = as_task_clasif(data, target = "mpg", id = "AMS")
print(task_AMS)

# Train- & Testsplit -----------------------------------------------------------

# Modeling =====================================================================

# Original AMS-Model -----------------------------------------------------------

# Logistic regression model with all variables step-procedure

# Restricted logistic regression model with all variables

# Other models
rpart::rpart()
randomForest::randomForest()
xgboost::xgboost()
