# PREDICTON DATAFRAMES #################################################################################################
# Preparation ==========================================================================================================
# Libraries ------------------------------------------------------------------------------------------------------------
library(tidyverse)

source("tasks.R", encoding="utf-8") # for predefined feature sets

# Load data ------------------------------------------------------------------------------------------------------------
data <- readRDS("data/JuSAW_prepared_clean.rds")

data_test = data[test_ids,]
#setequal(data_test$EMPLOYMENTDAYS, df3$truth)
data_test$test_ids = test_ids

# load benchmarks and combine them
bmr_ams = readRDS("models/bmr_Ams_RS1000_auc.Rds") # ams full und youth für die coefficients
bmr_ams_full = readRDS("models/bmr_AmsFull_RS1000_acc.Rds")
bmr_ams_youth = readRDS("models/bmr_AmsYouth_RS1000_acc.Rds")
bmr_ams_ext = readRDS("models/bmr_AmsExt_RS1000_acc.Rds")
# bmr_green = readRDS("models/bmr_GreenSmall_RS1000_acc.Rds")
# bmr_green_big = readRDS("models/bmr_green_big.Rds")
# bmr_green_filtering = readRDS("models/bmr_GreenFilter_RS1000_acc.Rds")
# bmr_all_filtering = readRDS("models/bmr_AllFilter_RS1000_acc.Rds")
bmr_all_filteringRelief = readRDS("models/bmr_AllFilterRelief_RS1000_acc.Rds")
bmr_all_filteringCmim = readRDS("models/bmr_AllFilterCMIM_RS1000_acc.Rds")
bmr_all_filteringDisr = readRDS("models/bmr_AllFilterDisr_RS1000_acc.Rds")
bmr_all_filteringJMI = readRDS("models/bmr_AllFilterJMI_RS1000_acc.Rds")
bmr_all_filteringMRMR = readRDS("models/bmr_AllFilterMRMR_RS1000_acc.Rds")
bmr_diverse = readRDS("models/bmr_diverse_RS1000_acc.Rds") # ams full und youth für die coefficients
bmr_behavior = readRDS("models/bmr_behavior_RS1000_acc.Rds") # ams full und youth für die coefficients
bmr_attitudes = readRDS("models/bmr_attitudes_RS1000_acc.Rds") # ams full und youth für die coefficients
bmr_personality = readRDS("models/bmr_personality_RS1000_acc.Rds") # ams full und youth für die coefficients
bmr_characteristics = readRDS("models/bmr_characteristics_RS1000_acc.Rds") # ams full und youth für die coefficients
bmr_otherPES = readRDS("models/bmr_otherPES_RS1000_acc.Rds") # ams full und youth für die coefficients

bmr = bmr_ams_youth
bmr$combine(bmr_ams_full)
bmr$combine(bmr_ams_ext)
# bmr$combine(bmr_green)
# bmr$combine(bmr_green_big)
# bmr$combine(bmr_green_filtering)
# bmr$combine(bmr_all_filtering)
bmr$combine(bmr_all_filteringRelief)
bmr$combine(bmr_all_filteringCmim)
bmr$combine(bmr_all_filteringDisr)
bmr$combine(bmr_all_filteringJMI)
bmr$combine(bmr_all_filteringMRMR)
bmr$combine(bmr_diverse)
bmr$combine(bmr_behavior)
bmr$combine(bmr_attitudes)
bmr$combine(bmr_personality)
bmr$combine(bmr_characteristics)
bmr$combine(bmr_otherPES)

#saveRDS(bmr, "models/bmr_full.Rds")

# Make data table from benchmark result for further evaluation ---------------------------------------------------------
tab_bmr <- as.data.table(bmr)

# Set threshold to 66% as in the ams paper -----------------------------------------------------------------------------
lapply(tab_bmr$prediction, function(i) i$set_threshold(0.66))

# Get model and task namelist combination for the following computations
task_ids = lapply(tab_bmr$task, function(i) i$id)
learner_ids = lapply(tab_bmr$learner, function(i) i$id)
task_learner_ids = paste(task_ids, learner_ids, sep=", ")

# Predcitions for all models
prediction_list = tab_bmr$prediction
names(prediction_list) = task_learner_ids

models = mlr3misc::map(tab_bmr$learner, "model")
names(models) = task_learner_ids



# Make Dataframes with all predictions + Documentation OR predictions ==================================================
# Predict AMS doc ------------------------------------------------------------------------------------------------------
# AMS full _____________________________________________________________________________________________________________
f_ams <- as.formula(paste("employmentdays", paste(ams[-1], collapse = "+"), sep = "~"))
model_ams_OR <- glm(f_ams, family = "binomial", data = data)
summary(model_ams_OR)
OR_coefs <- c(0.48, -0.05, 0.0, 0.0, -0.01, 0.04, 0.10, 0.03, -0.08, # RGS-Typ 2: -0.10, 
              -0.16, -0.10, -0.33, 0.07, -0.31, 0.20, 0.36, 0.63, -0.21, -0.19, -0.03)
names(OR_coefs) <- names(model_ams_OR$coefficients)
model_ams_OR$coefficients <- OR_coefs

# Predictions 
ams_full_OR <- predict(model_ams_OR, data_test, type="response")


# AMS youth ____________________________________________________________________________________________________________
f_ams_youth <- as.formula(paste("employmentdays", paste(ams_youth[-1], collapse = "+"), sep = "~"))
model_ams_OR_youth <- glm(f_ams_youth, family = "binomial", data = data)
summary(model_ams_OR_youth)
OR_coefs_youth <- c(-0.13, 0.09, 0.13, 0.00 # Altersgruppe 25-28 rausnehemn?
                    ,0.48, 0.40, -0.46, -0.17, -0.15, 
                    -0.36, -0.03, 0.01, -0.02, -0.06, -0.21, -0.14)
names(OR_coefs_youth) <- names(model_ams_OR_youth$coefficients)
model_ams_OR_youth$coefficients <- OR_coefs_youth

# Predictions 
ams_youth_OR <- predict(model_ams_OR_youth, data_test, type="response")


# Dataframes with predictions ------------------------------------------------------------------------------------------
# Dataframe variant 2 __________________________________________________________________________________________________
df2 = data.frame(task = unlist(task_ids))
df2 = expand(df2, task, test_ids) # sollte tasks Anzahl mal test rows sein

#truth = data.frame(test_ids = test_ids, truth = prediction_list[[2]]$truth)
df2 = full_join(df2, data_test, by = "test_ids")
df2$truth_01 <- as.factor(ifelse(df2$employmentdays == '>=90 Days', 1, 0))

probs = lapply(prediction_list, function(i) i$prob[,1])
probs = as.data.frame(do.call(cbind, probs))

df_list = list()
for(i in unique(unlist(task_ids))){
  probs_task = probs[str_subset(names(probs), i)]
  names(probs_task) = unique(unlist(learner_ids))
  df_list = append(df_list, list(probs_task))
}
names(df_list) = unique(unlist(task_ids))

for(i in 1:length(unique(unlist(task_ids)))){
  df_list[[i]] = mutate(df_list[[i]], task = unique(unlist(task_ids))[i], test_ids = test_ids)
}
probs_df = bind_rows(df_list)

df2 = full_join(df2, probs_df, by = c("task","test_ids"))

# Add OR predictions
ams_full_OR_ = data.frame(test_ids, task = "AMS full", OR = ams_full_OR)
ams_youth_OR_ = data.frame(test_ids, task = "AMS youth", OR = ams_youth_OR)
ams_OR = rbind(ams_full_OR_, ams_youth_OR_)

df2 = full_join(df2, ams_OR, by = c("test_ids", "task"))

save(df2, file="data/df2.Rda")

# Dataframe variant 1 __________________________________________________________________________________________________
df1 = data.frame(task = unlist(task_ids), model = unlist(learner_ids))
df1[nrow(df1) + 1,] <- c("AMS youth", "OR")
df1[nrow(df1) + 1,] <- c("AMS full", "OR")
df1 = expand(df1, task, model, test_ids) # sollte tasks, model Anzahl mal test rows sein

#truth = data.frame(test_ids = test_ids, truth = prediction_list[[2]]$truth)
df1 = full_join(df1, data_test, by = "test_ids")
df1$truth_01 <- as.factor(ifelse(df1$employmentdays == '>=90 Days', 1, 0))

#sapply(df_list, function(i) pivot_longer(i, unique(unlist(learner_ids)), names_to = "model", values_to = "probabilities"))

df1_probs = pivot_longer(df_list[[1]], unique(unlist(learner_ids)), names_to = "model", values_to = "probabilities")
for(i in 2:length(df_list)){
  df1_other = pivot_longer(df_list[[i]], unique(unlist(learner_ids)), names_to = "model", values_to = "probabilities")
  df1_probs = rbind(df1_probs, df1_other)
}

# Add OR predictions
ams_full_OR_ = data.frame(test_ids, model = "OR", task = "AMS full", probabilities = ams_full_OR)
ams_youth_OR_ = data.frame(test_ids, model = "OR", task = "AMS youth", probabilities = ams_youth_OR)

df1_probs = bind_rows(df1_probs, ams_full_OR_, ams_youth_OR_)


df1 = full_join(df1, df1_probs, by = c("task", "model", "test_ids"))


# make estimate column with 0.66 and 0.5 and 0.25
df1$estimate_0.66 = as.factor(ifelse(df1$probabilities >= 0.66, 1, 0))
df1$estimate_0.5 = as.factor(ifelse(df1$probabilities >= 0.5, 1, 0))
df1$estimate_0.25 = as.factor(ifelse(df1$probabilities >= 0.25, 1, 0))

save(df1, file="data/df1.Rda")

# Dataframe variante 3 __________________________________________________________________________________________________
df3 = data.frame(test_ids = prediction_list[[1]]$row_ids)

data_test$ams_youth_OR = ams_youth_OR
data_test$ams_full_OR = ams_full_OR

df3 = full_join(df3, data_test, by = "test_ids")
df3$truth_01 <- as.factor(ifelse(df3$employmentdays == '>=90 Days', 1, 0))

probs = lapply(prediction_list, function(i) i$prob[,1])
probs = as.data.frame(do.call(cbind, probs))

df3 = cbind(df3, probs)

save(df3, file="data/df3.Rda")

