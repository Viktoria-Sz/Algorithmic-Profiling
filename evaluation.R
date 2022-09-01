# EVALUATION ###########################################################################################################
# Preparation ==========================================================================================================
# Libraries ------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(fairness) # fairness
library(yardstick) # performance
library(jtools) # for plotting coefficients
#library(pROC)

# Load other scripts ---------------------------------------------------------------------------------------------------
source("functions.R", encoding="utf-8") # for predefined feature sets
source("tasks.R", encoding="utf-8") # for predefined feature sets

# Load data ------------------------------------------------------------------------------------------------------------
load("data/df1.Rda")
load("data/df2.Rda")
load("data/df3.Rda")
data <- readRDS("data/JuSAW_prepared_clean.rds")

data_test = data[test_ids,]
#setequal(data_test$EMPLOYMENTDAYS, df3$truth)
data_test$test_ids = test_ids

# load full benchmark
#bmr = readRDS("models/bmr_full.Rds") 


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


# Get selected feature sets --------------------------------------------------------------------------------------------
# GLMnet
models[[3]]$classif.glmnet.tuned$model$learner

for (i in grep("glmnet" , names(models))) {
  print(names(models[i]))
  print(models[[i]]$classif.glmnet.tuned$model$learner$selected_features())
}

# Decision Tree
for (i in grep("Decision Tree" , names(models))) {
  print(names(models[i]))
  print(models[[i]]$learner$selected_features())
}
for (i in grep("Decision Tree" , names(models))) {
  print(names(models[i]))
  print(models[[i]]$learner$importance())
}

# Random Forest
models[[5]]$learner$importance()

# xgboost
models[[6]]$classif.xgboost.tuned$model$learner$importance()

for (i in grep("xgboost" , names(models))) {
  print(names(models[i]))
  print(models[[i]]$classif.xgboost.tuned$model$learner$importance())
}


# Coefficients =========================================================================================================
tab_bmr_ams$learner[[2]]$model$coefficients
tab_bmr_ams$task
# AMS full _____________________________________________________________________________________________________________
# AMS full Training ----------------------------------------------------------------------------------------------------
f_ams <- as.formula(paste("employmentdays", paste(ams[-1], collapse = "+"), sep = "~"))
model_ams <- glm(f_ams, family = "binomial", data = data)
summary(model_ams)
model_ams$coefficients <- tab_bmr_ams$learner[[9]]$model$coefficients


# Plotting coefficients
plot_summs(model_ams) +
  ggtitle("Coefficients AMS full")

# AMS full OR ----------------------------------------------------------------------------------------------------------
model_ams_OR <- glm(f_ams, family = "binomial", data = data)
summary(model_ams_OR)
OR_coefs <- c(0.48, -0.05, 0.0, 0.0, -0.01, 0.04, 0.10, 0.03, -0.08, # RGS-Typ 2: -0.10, 
              -0.16, -0.10, -0.33, 0.07, -0.31, 0.20, 0.36, 0.63, -0.21, -0.19, -0.03)
names(OR_coefs) <- names(model_ams_OR$coefficients)
model_ams_OR$coefficients <- OR_coefs

# Plot -----------------------------------------------------------------------------------------------------------------
label_data_ams1 <- as.data.frame(summ(model_ams_OR)$coeftable)
label_data_ams1$Coeff <- rownames(label_data_ams1)
label_data_ams1$model <- "AMS full documentation"

label_data_ams2 <- as.data.frame(summ(model_ams)$coeftable)
label_data_ams2$Coeff <- rownames(label_data_ams2)
label_data_ams2$model <- "AMS full training"

label_data_ams <- rbind(label_data_ams2, label_data_ams1)


plot_summs(model_ams_OR) +
  ggtitle("Coefficients AMS full documentation")

plot_summs(model_ams, model_ams_OR, ci_level = 0,
           model.names = c("AMS full training", "AMS full documentation")) +
  geom_text(inherit.aes = FALSE, #position=ggstance::position_dodgev(height=1),
            #position = position_dodge(width = 0.5),
            data = label_data_ams[label_data_ams$Coeff != "(Intercept)",],
            aes(x = `Est.`, y = Coeff, label = round(`Est.`,2), color = model), vjust = -1) +
  ggtitle("Coefficients AMS full + documentation")

# AMS youth ____________________________________________________________________________________________________________
# AMS youth ____________________________________________________________________________________________________________
# AMS youth Training ---------------------------------------------------------------------------------------------------
# 25-28 jährige rausnehmen
# data_youth <- data[!data$AGEGROUP == "25-28",]
# table(data_youth$AGEGROUP)

f_ams_youth <- as.formula(paste("employmentdays", paste(ams_youth[-1], collapse = "+"), sep = "~"))
model_ams_youth <- glm(f_ams_youth, family = "binomial", data = data)
summary(model_ams_youth)

model_ams_youth$coefficients <-  summary(tab_bmr_ams$learner[[2]]$model)$coefficients[,1]

# Plotting coefficients
plot_summs(model_ams_youth, ci_level = 0) +
  ggtitle("Coefficients AMS youth")

# AMS youth OR ---------------------------------------------------------------------------------------------------------
model_ams_youth_OR <- glm(f_ams_youth, family = "binomial", data = data)
summary(model_ams_youth_OR)
OR_coefs_youth <- c(-0.13, 0.09, 0.13, 0.00, 0.48, 0.40, -0.46, -0.17, -0.15, 
                    -0.36, -0.03, 0.01, -0.02, -0.06, -0.21, -0.14)
names(OR_coefs_youth) <- names(model_ams_youth_OR$coefficients)
model_ams_youth_OR$coefficients <- OR_coefs_youth

plot_summs(model_ams_OR) +
  ggtitle("Coefficients AMS youth documentation")

# Plot -----------------------------------------------------------------------------------------------------------------
label_data_ams_youth1 <- as.data.frame(summ(model_ams_youth_OR)$coeftable)
label_data_ams_youth1$Coeff <- rownames(label_data_ams_youth1)
label_data_ams_youth1$model <- "AMS youth documentation"

label_data_ams_youth2 <- as.data.frame(summ(model_ams_youth)$coeftable)
label_data_ams_youth2$Coeff <- rownames(label_data_ams_youth2)
label_data_ams_youth2$model <- "AMS youth training"

label_data_ams_youth <- rbind(label_data_ams_youth2, label_data_ams_youth1)


plot_summs(model_ams_youth, model_ams_youth_OR, ci_level = 0,
           model.names = c("AMS youth training", "AMS youth documentation")) +
  geom_text(inherit.aes = FALSE, #position=ggstance::position_dodgev(height=1),
            #position = position_dodge(width = 0.5),
            data = label_data_ams_youth[label_data_ams_youth$Coeff != "(Intercept)",],
            aes(x = `Est.`, y = Coeff, label = round(`Est.`,2), color = model), vjust = -1) +
  ggtitle("Coefficients AMS youth + documentation")


# Performance ==========================================================================================================
# Confucion Matrix -----------------------------------------------------------------------------------------------------
# 0.66 Threshold
confmat_list_0.66 = list()
for(i in unique(df1$task)){
  conf_mats_0.66 = subset(df1, subset = (task == i)) %>%
    group_by(model) %>%
    conf_mat(truth_01, estimate = estimate_0.66)
  
  names(conf_mats_0.66$conf_mat) = conf_mats_0.66$model
  
  confmat_list_0.66 = append(confmat_list_0.66, list(conf_mats_0.66))
  #conf_mats_0.66$conf_mat
}
names(confmat_list_0.66) = unique(df1$task)

# 0.5 Threshold
confmat_list_0.5 = list()
for(i in unique(df1$task)){
  conf_mats_0.5 = subset(df1, subset = (task == i)) %>%
    group_by(model) %>%
    conf_mat(truth_01, estimate = estimate_0.5)
  
  names(conf_mats_0.5$conf_mat) = conf_mats_0.5$model
  
  confmat_list_0.5 = append(confmat_list_0.5, list(conf_mats_0.5))
  #conf_mats_0.5$conf_mat
}
names(confmat_list_0.5) = unique(df1$task)


# ROC and PRC Curves ---------------------------------------------------------------------------------------------------
roc_list = list()
for(i in unique(df1$task)){
  sub = subset(df1, subset = (task == i)) %>%
    filter(!is.na(probabilities)) %>% # since not all tasks have OR results
    #filter(model == "OR" | model == "Logistic Regression" | model == "encode.colapply.classif.glmnet") %>%
    group_by(model)
  
   roc_plot = autoplot(roc_curve(sub, truth_01, probabilities, event_level = 'second'))  +
    ggtitle(i)
   
   roc_list = append(roc_list, list(roc_plot))
}

# PRC curve
prc_list = list()
for(i in unique(df1$task)){
  sub = subset(df1, subset = (task == i)) %>%
    filter(!is.na(probabilities)) %>%
    #filter(model == "OR" | model == "Logistic Regression" | model == "encode.colapply.classif.glmnet") %>%
    group_by(model)
  
  prc_plot = autoplot(pr_curve(sub, truth_01, probabilities, event_level = 'second')) +
    ggtitle(i)
  
  prc_list = append(prc_list, list(prc_plot))
  
}


# Measures -------------------------------------------------------------------------------------------------------------
task_list <- unique(df1$task)

# use self-made function for performance measure computation
# 66 % low and middle group
measure_list = list()
results_0.66_gender <- performance(df1, tasks = task_list, label = estimate_0.66,
                    protected = gender, privileged = "male", unprivileged = "female", measure_list)
results_0.66_gender_df <- bind_rows(results_0.66_gender, .id = "task")

measure_list = list()
results_0.66_stategroup <- performance(df1, tasks = task_list, label = estimate_0.66,
                    protected = stategroup01, privileged = "AUT", unprivileged = "nAUT", measure_list)
results_0.66_stategroup_df <- bind_rows(results_0.66_stategroup, .id = "task")


# 25 % low and middle group
measure_list = list()
results_0.25_gender <- performance(df1, tasks = task_list, label = estimate_0.25,
                                   protected = gender, privileged = "male", unprivileged = "female", measure_list)
results_0.25_gender_df <- bind_rows(results_0.25_gender, .id = "task")

measure_list = list()
results_0.25_stategroup <- performance(df1, tasks = task_list, label = estimate_0.25,
                                       protected = stategroup01, privileged = "AUT", unprivileged = "nAUT", measure_list)
results_0.25_stategroup_df <- bind_rows(results_0.25_stategroup, .id = "task")



# Averages -------------------------------------------------------------------------------------------------------------
models <- c("LogisticRegression", "PenalizedLR", "RandomForest", "xgboost", "KKNN")

library("writexl")

# Averages by task _____________________________________________________________________________________________________
round <- 3
averages_0.66_gender <- results_0.66_gender_df %>%
  filter(model %in% models) %>%
  group_by(task, .metric) %>%
  summarise(mean_all =round(mean(.estimate), round),
            mean_male = round(mean(male), round), mean_female = round(mean(female),round), mean_diff = round(mean(priv_diff),round))
write_xlsx(averages_0.66_gender,"models\\averages\\averages_0.66_gender.xlsx")


averages_0.66_stategroup <- results_0.66_stategroup_df %>%
  filter(model %in% models) %>%
  group_by(task, .metric) %>%
  summarise(mean_all = round(mean(.estimate),round),
            mean_AUT = round(mean(AUT),round), mean_nAUT = round(mean(nAUT),round), mean_diff = round(mean(priv_diff),round))
write_xlsx(averages_0.66_stategroup,"models\\averages\\averages_0.66_stategroup.xlsx")


averages_0.25_gender <- results_0.25_gender_df %>%
  filter(model %in% models) %>%
  group_by(task, .metric) %>%
  summarise(mean_all =round(mean(.estimate), round),
            mean_male = round(mean(male), round), mean_female = round(mean(female),round), mean_diff = round(mean(priv_diff),round))
write_xlsx(averages_0.25_gender,"models\\averages\\averages_0.25_gender.xlsx")

averages_0.25_stategroup <- results_0.25_stategroup_df %>%
  filter(model %in% models) %>%
  group_by(task, .metric) %>%
  summarise(mean_all = round(mean(.estimate),round),
            mean_AUT = round(mean(AUT),round), mean_nAUT = round(mean(nAUT),round), mean_diff = round(mean(priv_diff),round))
write_xlsx(averages_0.25_stategroup,"models\\averages\\averages_0.25_stategroup.xlsx")

# Averages by model ____________________________________________________________________________________________________
averages_0.66_gender_model <- results_0.66_gender_df %>%
  group_by(model, .metric) %>%
  summarise(mean_all =round(mean(.estimate), round),
            mean_male = round(mean(male), round), mean_female = round(mean(female),round), mean_diff = round(mean(priv_diff),round))
write_xlsx(averages_0.66_gender_model,"models\\averages\\averages_0.66_gender_model.xlsx")

averages_0.66_stategroup_model <- results_0.66_stategroup_df %>%
  group_by(model, .metric) %>%
  summarise(mean_all = round(mean(.estimate),round),
            mean_AUT = round(mean(AUT),round), mean_nAUT = round(mean(nAUT),round), mean_diff = round(mean(priv_diff),round))
write_xlsx(averages_0.66_stategroup_model,"models\\averages\\averages_0.66_stategroup_model.xlsx")


averages_0.25_gender_model <- results_0.25_gender_df %>%
  group_by(model, .metric) %>%
  summarise(mean_all =round(mean(.estimate), round),
            mean_male = round(mean(male), round), mean_female = round(mean(female),round), mean_diff = round(mean(priv_diff),round))
write_xlsx(averages_0.25_gender_model,"models\\averages\\averages_0.25_gender_model.xlsx")

averages_0.25_stategroup_model <- results_0.25_stategroup_df %>%
  group_by(model, .metric) %>%
  summarise(mean_all = round(mean(.estimate),round),
            mean_AUT = round(mean(AUT),round), mean_nAUT = round(mean(nAUT),round), mean_diff = round(mean(priv_diff),round))
write_xlsx(averages_0.25_stategroup_model,"models\\averages\\averages_0.25_stategroup_model.xlsx")

# Heatmap --------------------------------------------------------------------------------------------------------------
# use self-made function for heatmap generation with ggplot

# 0.66 
heatmap_list_0.66 = list()
for(i in task_list){
  p = heatmap(results_0.66_gender[[i]], model, .metric, .estimate) +
    ggtitle(i)
  heatmap_list_0.66 = append(heatmap_list_0.66,  list(p))
}

# 0.66 % middle and high group with gender ________________________________________________________________________________
heatmap_list_0.66_male = list()
for(i in task_list){
  p = heatmap(results_0.66_gender[[i]], model, .metric, male) +
    ggtitle(i)
  heatmap_list_0.66_male = append(heatmap_list_0.66_male,  list(p))
}
heatmap_list_0.66_female = list()
for(i in task_list){
  p = heatmap(results_0.66_gender[[i]], model, .metric, female) +
    ggtitle(i)
  heatmap_list_0.66_female = append(heatmap_list_0.66_female,  list(p))
}
heatmap_list_0.66_genderdiff = list()
for(i in task_list){
  p = heatmap_diff(results_0.66_gender[[i]], model, .metric, priv_diff) +
    ggtitle(i)
  heatmap_list_0.66_genderdiff = append(heatmap_list_0.66_genderdiff,  list(p))
}

# 0.66 % middle and high group with stategroup ________________________________________________________________________________
heatmap_list_0.66_AUT = list()
for(i in task_list){
  p = heatmap(results_0.66_stategroup[[i]], model, .metric, AUT) +
    ggtitle(i)
  heatmap_list_0.66_AUT = append(heatmap_list_0.66_AUT,  list(p))
}
heatmap_list_0.66_nAUT = list()
for(i in task_list){
  p = heatmap(results_0.66_stategroup[[i]], model, .metric, nAUT) +
    ggtitle(i)
  heatmap_list_0.66_nAUT = append(heatmap_list_0.66_nAUT,  list(p))
}
heatmap_list_0.66_statediff = list()
for(i in task_list){
  p = heatmap_diff(results_0.66_stategroup[[i]], model, .metric, priv_diff) +
    ggtitle(i)
  heatmap_list_0.66_statediff = append(heatmap_list_0.66_statediff,  list(p))
}


# 0.25 % ---------------------------------------------------------------------------------------------------------------
heatmap_list_0.25 = list()
for(i in task_list){
  p = heatmap(results_0.25_gender[[i]], model, .metric, .estimate) +
    ggtitle(i)
  heatmap_list_0.25 = append(heatmap_list_0.25,  list(p))
}

# 0.25 % middle and low group with gender ________________________________________________________________________________
heatmap_list_0.25_male = list()
for(i in task_list){
  p = heatmap(results_0.25_gender[[i]], model, .metric, male) +
    ggtitle(i)
  heatmap_list_0.25_male = append(heatmap_list_0.25_male,  list(p))
}
heatmap_list_0.25_female = list()
for(i in task_list){
  p = heatmap(results_0.25_gender[[i]], model, .metric, female) +
    ggtitle(i)
  heatmap_list_0.25_female = append(heatmap_list_0.25_female,  list(p))
}
heatmap_list_0.25_genderdiff = list()
for(i in task_list){
  p = heatmap_diff(results_0.25_gender[[i]], model, .metric, priv_diff) +
    ggtitle(i)
  heatmap_list_0.25_genderdiff = append(heatmap_list_0.25_genderdiff,  list(p))
}

# 0.25 % middle and low group with stategroup ________________________________________________________________________________
heatmap_list_0.25_AUT = list()
for(i in task_list){
  p = heatmap(results_0.25_stategroup[[i]], model, .metric, AUT) +
    ggtitle(i)
  heatmap_list_0.25_AUT = append(heatmap_list_0.25_AUT,  list(p))
}
heatmap_list_0.25_nAUT = list()
for(i in task_list){
  p = heatmap(results_0.25_stategroup[[i]], model, .metric, nAUT) +
    ggtitle(i)
  heatmap_list_0.25_nAUT = append(heatmap_list_0.25_nAUT,  list(p))
}
heatmap_list_0.25_statediff = list()
for(i in task_list){
  p = heatmap_diff(results_0.25_stategroup[[i]], model, .metric, priv_diff) +
    ggtitle(i)
  heatmap_list_0.25_statediff = append(heatmap_list_0.25_statediff,  list(p))
}

# All heatmaps ---------------------------------------------------------------------------------------------------------
heatmap_list_0.66

heatmap_list_0.66_male
heatmap_list_0.66_female
heatmap_list_0.66_genderdiff
heatmap_list_0.66_AUT
heatmap_list_0.66_nAUT
heatmap_list_0.66_statediff


heatmap_list_0.25

heatmap_list_0.25_male
heatmap_list_0.25_female
heatmap_list_0.25_genderdiff
heatmap_list_0.25_AUT
heatmap_list_0.25_nAUT
heatmap_list_0.25_statediff

# Test heatmap
library(RColorBrewer)
ggplot(results_0.25_stategroup[[1]], aes(y= model, x = .metric, fill = priv_diff)) +
  geom_tile() +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) + 
  geom_text(aes(label = round(priv_diff, 2))) +
  scale_fill_distiller(palette = colorRampPalette(brewer.pal(9, "RdYlBu"))(12)[6:12] # "RdYlBu"
                       , direction = 1, limits = c(-1,1))

# RESTE ################################################################################################################
df1[1:10, c("task", "model", "probabilities")]

subset = subset(df1, subset = (task == "AMS youth"))
subset(subset, subset = ( model == "OR" ), c(probabilities,truth_01, test_ids, task))
subset(df1, subset = (task == "AMS youth" & model == "OR" ), probabilities)
subset(df2, subset = (task == "AMS youth"), OR)
subset(df3, subset = (task == "AMS youth" & model != "OR" ))


autoplot(roc_curve(subset(df1, subset = (task == "AMS youth" & model == "Logistic Regression")), 
                   truth_01, probabilities, event_level = "second"))

# ROC Curve
subset(df1, subset = (task == "AMS youth")) %>%
  #filter(model == "OR" | model == "Logistic Regression" | model == "encode.colapply.classif.glmnet") %>%
  group_by(model) %>%
  roc_curve(truth_01, probabilities, event_level = 'second') %>%
  autoplot() +
  ggtitle("AMS youth")


for(i in unique(df1$task)){
  subset(df1, subset = (task == i)) %>%
    filter(!is.na(probabilities)) %>%
    #filter(model == "OR" | model == "Logistic Regression" | model == "encode.colapply.classif.glmnet") %>%
    group_by(model) %>%
    roc_curve(truth_01, probabilities, event_level = 'second') %>%
    autoplot() +
    ggtitle(i)
}

