# EVALUATION ###########################################################################################################
# Preparation ==========================================================================================================
# Libraries ------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(fairness) # fairness
library(yardstick) # performance
library(jtools) # for plotting coefficients
#library(pROC)
library(writexl)
library(ggpubr)

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

df1 <- filter(df1, model != "DecisionTree")

task_list <- unique(df1$task)
models <- c("LogisticRegression", "PenalizedLR", "RandomForest", "xgboost", "KNN")
selected_measures <- c("Prevalence",
                       "Accuracy",
                       "ROC_AUC",
                       "TPR_Recall_Sens", # sensitivity, recall, TPR
                       "FNR",
                       "TNR_Spec", # specificity, TNR
                       "FPR",
                       "PPV_Precision", # Precision, Positive predictive value PPV
                       "NPV"
                       )

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


# Prevalence Prediction / Demographic Parity ---------------------------------------------------------------------------
# 0.66 threshold _______________________________________________________________________________________________________
binom_stats_pred <- function(x, ...) {
  x <- x$estimate_0.66[!is.na(x$estimate_0.66)]
  res <- prop.test(x = sum(x == 1), n = length(x), ...)
  data.frame(Proportion  = unname(res$estimate), 
             Lower = res$conf.int[1],
             Upper = res$conf.int[2])
}

rates_gender <- df1 %>%
  filter(task == "AMS extended") %>%
  filter(model %in% models) %>%
  group_by(gender) %>%
  do(binom_stats_pred(.)) %>%
  arrange(Proportion) %>%
  ungroup() %>%
  rename(char = gender) %>%
  mutate(task = "AMS extended", var = "gender")

jobpred_rate_0.66 <- mean(rates_gender$Proportion, na.rm = TRUE)

rates_mig <- df1 %>%
  filter(task == "AMS extended") %>%
  filter(model %in% models) %>%
  group_by(stategroup01) %>%
  do(binom_stats_pred(.)) %>%
  arrange(Proportion) %>%
  ungroup() %>%
  rename(char = stategroup01) %>%
  mutate(task = "AMS extended", var = "stategroup")

for(i in task_list[-1]){
  rates_new_gender <- df1 %>%
    filter(task == i) %>%
    filter(model %in% models) %>%
    group_by(gender) %>%
    do(binom_stats_pred(.)) %>%
    arrange(Proportion) %>%
    ungroup() %>%
    rename(char = gender) %>%
    mutate(task = i, var = "gender")
  rates_gender <- rbind(rates_gender, rates_new_gender)
  
  rates_new_mig <- df1 %>%
    filter(task == i) %>%
    filter(model %in% models) %>%
    group_by(stategroup01) %>%
    do(binom_stats_pred(.)) %>%
    arrange(Proportion) %>%
    ungroup() %>%
    rename(char = stategroup01) %>%
    mutate(task = i, var = "stategroup")
  rates_mig <- rbind(rates_mig, rates_new_mig)
}
rates = rbind(rates_gender, rates_mig)
rates_selected <- filter(rates, task == "AMS youth" | task == "diverse" | task == "other_PES" 
                         | task == "personality" | task == "filtering_disr")

demographic_parity_0.66 <- ggplot(rates_selected, aes(x = char, y = Proportion, colour = var)) +
  geom_hline(yintercept = jobpred_rate_0.66, col = "red", alpha = .35, lty = 2) + 
  theme_bw(15) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)
                , width = 0.3, size = 2) +
  scale_color_manual(values=c("#999999", "#E69F00"))+
  theme(axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0, 0.15), breaks = scales::pretty_breaks(n = 5)) +
  # facet_grid(cols = vars(task), scales = "free") +
  facet_grid(rows = vars(task), scales = "free") +
  coord_flip() +
  labs(title = "Demographic parity for gender and stategroup with 66% threshold") +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
demographic_parity_0.66

ggsave("plots/demographic_parity_0.66_selected.png", demographic_parity_0.66, width = 9.50, height = 14.40, dpi = 600)

# 0.25 threshold _______________________________________________________________________________________________________
binom_stats_pred <- function(x, ...) {
  x <- x$estimate_0.25[!is.na(x$estimate_0.25)]
  res <- prop.test(x = sum(x == 1), n = length(x), ...)
  data.frame(Proportion  = unname(res$estimate), 
             Lower = res$conf.int[1],
             Upper = res$conf.int[2])
}

rates_gender <- df1 %>%
  filter(task == "AMS extended") %>%
  filter(model %in% models) %>%
  group_by(gender) %>%
  do(binom_stats_pred(.)) %>%
  arrange(Proportion) %>%
  ungroup() %>%
  rename(char = gender) %>%
  mutate(task = "AMS extended", var = "gender")

jobpred_rate_0.25 <- mean(rates_gender$Proportion, na.rm = TRUE)

rates_mig <- df1 %>%
  filter(task == "AMS extended") %>%
  filter(model %in% models) %>%
  group_by(stategroup01) %>%
  do(binom_stats_pred(.)) %>%
  arrange(Proportion) %>%
  ungroup() %>%
  rename(char = stategroup01) %>%
  mutate(task = "AMS extended", var = "stategroup")

for(i in task_list[-1]){
  rates_new_gender <- df1 %>%
    filter(task == i) %>%
    filter(model %in% models) %>%
    group_by(gender) %>%
    do(binom_stats_pred(.)) %>%
    arrange(Proportion) %>%
    ungroup() %>%
    rename(char = gender) %>%
    mutate(task = i, var = "gender")
  rates_gender <- rbind(rates_gender, rates_new_gender)
  
  rates_new_mig <- df1 %>%
    filter(task == i) %>%
    filter(model %in% models) %>%
    group_by(stategroup01) %>%
    do(binom_stats_pred(.)) %>%
    arrange(Proportion) %>%
    ungroup() %>%
    rename(char = stategroup01) %>%
    mutate(task = i, var = "stategroup")
  rates_mig <- rbind(rates_mig, rates_new_mig)
}
rates = rbind(rates_gender, rates_mig)
rates_selected <- filter(rates, task == "AMS youth" | task == "diverse" | task == "other_PES" 
                         | task == "personality" | task == "filtering_disr")

demographic_parity_0.25 <- ggplot(rates_selected, aes(x = char, y = Proportion, colour = var)) +
  geom_hline(yintercept = jobpred_rate_0.25, col = "red", alpha = .35, lty = 2) + 
  theme_bw(15) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)
                , width = 0.3, size = 2) +
  scale_color_manual(values=c("#999999", "#E69F00"))+
  theme(axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0.4, 1), breaks = scales::pretty_breaks(n = 5)) +
  # facet_grid(cols = vars(task), scales = "free") +
  facet_grid(rows = vars(task), scales = "free") +
  coord_flip() +
  labs(title = "Demographic parity for gender and stategroup with 25% threshold") +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
demographic_parity_0.25

ggsave("plots/demographic_parity_0.25_selected.png", demographic_parity_0.25, width = 9.50, height = 14.40, dpi = 600)


# Measures -------------------------------------------------------------------------------------------------------------
tasks_gender <- c("AMS full", "AMS youth", "AMS ext", "diverse", "other_PES")
tasks_wo_gender <- c("filtering_cmim" ,"filtering_disr", "filtering_jmi", "filtering_mrmr", "filtering_relief")

tasks_stategroup <- c("AMS full", "AMS youth", "AMS ext", "diverse", "other_PES", 
                      "filtering_cmim" ,"filtering_disr", "filtering_jmi", "filtering_mrmr" )
tasks_wo_stategroup <- c("filtering_relief")

tasks_AMS <- c("AMS full", "AMS youth", "AMS ext")
tasks_diverse <- c("diverse", "other_PES")
tasks_character <- c("attitudes", "behavior", "characteristics_filter", "personality")
tasks_filtering <- c("filtering_cmim" ,"filtering_disr", "filtering_jmi", "filtering_mrmr", "filtering_relief")

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


# Best accuracy ________________________________________________________________________________________________________
# worst accuracy
results_0.66_gender_df %>%
  filter(.metric == "Accuracy") %>%
  arrange(.estimate)
# best accuracy
results_0.66_gender_df %>%
  filter(.metric == "Accuracy") %>%
  arrange(desc(.estimate)) -> best_accuracy
# write_xlsx(best_accuracy,"models\\averages\\best_accuracy.xlsx")

# Best ROC_AUC ________________________________________________________________________________________________________
# worst
results_0.66_gender_df %>%
  filter(.metric == "ROC_AUC") %>%
  arrange(.estimate)
# best
results_0.66_gender_df %>%
  filter(.metric == "ROC_AUC") %>%
  arrange(desc(.estimate)) -> best_auc
# write_xlsx(best_auc,"models\\averages\\best_auc.xlsx")


# Means by task groups -------------------------------------------------------------------------------------------------
# AMS
results_0.66_gender_df %>%
  filter(task %in% tasks_AMS) %>%
  filter(.metric == "Accuracy" | .metric == "ROC_AUC") %>%
  group_by(.metric) %>%
  summarise(mean = mean(.estimate))

# Diverse
results_0.66_gender_df %>%
  filter(task %in% tasks_diverse) %>%
  filter(.metric == "Accuracy" | .metric == "ROC_AUC") %>%
  group_by(.metric) %>%
  summarise(mean = mean(.estimate))

# Character
results_0.66_gender_df %>%
  filter(task %in% tasks_character) %>%
  filter(.metric == "Accuracy" | .metric == "ROC_AUC") %>%
  group_by(.metric) %>%
  summarise(mean = mean(.estimate))

# Filtering
results_0.66_gender_df %>%
  filter(task %in% tasks_filtering) %>%
  filter(.metric == "Accuracy" | .metric == "ROC_AUC") %>%
  group_by(.metric) %>%
  summarise(mean = mean(.estimate))

# Fairness through unawarness: With gender
results_0.66_gender_df %>%
  filter(task %in% tasks_gender) %>%
  group_by(task) %>%
  summarise(sum = sum(abs(priv_diff), na.rm = TRUE)) %>%
  summarise(mean = mean(sum))

# Fairness through unawarness: Without gender
results_0.66_gender_df %>%
  filter(task %in% tasks_wo_gender) %>%
  group_by(task) %>%
  summarise(sum = sum(abs(priv_diff), na.rm = TRUE)) %>%
  summarise(mean = mean(sum))

# Fairness through unawarness: With stategroup
results_0.66_stategroup_df %>%
  filter(task %in% tasks_stategroup) %>%
  group_by(task) %>%
  summarise(sum = sum(abs(priv_diff), na.rm = TRUE)) %>%
  summarise(mean = mean(sum))

# Fairness through unawarness: Without stategroup
results_0.66_stategroup_df %>%
  filter(task %in% tasks_wo_stategroup) %>%
  group_by(task) %>%
  summarise(sum = sum(abs(priv_diff), na.rm = TRUE)) %>%
  summarise(mean = mean(sum))

# 25 % low and middle group --------------------------------------------------------------------------------------------
measure_list = list()
results_0.25_gender <- performance(df1, tasks = task_list, label = estimate_0.25,
                                   protected = gender, privileged = "male", unprivileged = "female", measure_list)
results_0.25_gender_df <- bind_rows(results_0.25_gender, .id = "task")

measure_list = list()
results_0.25_stategroup <- performance(df1, tasks = task_list, label = estimate_0.25,
                                       protected = stategroup01, privileged = "AUT", unprivileged = "nAUT", measure_list)
results_0.25_stategroup_df <- bind_rows(results_0.25_stategroup, .id = "task")


# Averages =============================================================================================================
round <- 3

# Averages by task -----------------------------------------------------------------------------------------------------
averages_0.66_gender <- results_0.66_gender_df %>%
  filter(model %in% models) %>%
  group_by(task, .metric) %>%
  summarise(mean_all =round(mean(.estimate), round),
            mean_male = round(mean(male), round), mean_female = round(mean(female),round), mean_diff = round(mean(priv_diff),round))
# write_xlsx(averages_0.66_gender,"models\\averages\\averages_0.66_gender.xlsx")


averages_0.66_stategroup <- results_0.66_stategroup_df %>%
  filter(model %in% models) %>%
  group_by(task, .metric) %>%
  summarise(mean_all = round(mean(.estimate),round),
            mean_AUT = round(mean(AUT),round), mean_nAUT = round(mean(nAUT),round), mean_diff = round(mean(priv_diff),round))
# write_xlsx(averages_0.66_stategroup,"models\\averages\\averages_0.66_stategroup.xlsx")


averages_0.25_gender <- results_0.25_gender_df %>%
  filter(model %in% models) %>%
  group_by(task, .metric) %>%
  summarise(mean_all =round(mean(.estimate), round),
            mean_male = round(mean(male), round), mean_female = round(mean(female),round), mean_diff = round(mean(priv_diff),round))
# write_xlsx(averages_0.25_gender,"models\\averages\\averages_0.25_gender.xlsx")

averages_0.25_stategroup <- results_0.25_stategroup_df %>%
  filter(model %in% models) %>%
  group_by(task, .metric) %>%
  summarise(mean_all = round(mean(.estimate),round),
            mean_AUT = round(mean(AUT),round), mean_nAUT = round(mean(nAUT),round), mean_diff = round(mean(priv_diff),round))
# write_xlsx(averages_0.25_stategroup,"models\\averages\\averages_0.25_stategroup.xlsx")

# Best measures models _________________________________________________________________________________________________
# Best accuracy
averages_0.66_gender %>%
  group_by(task) %>%
  filter(.metric == "Accuracy") %>%
  arrange(desc(mean_all)) #-> accuracy_task
#write_xlsx(accuracy_task,"models\\averages\\accuracy_task.xlsx")

# Best ROC-AUC
averages_0.66_gender %>%
  group_by(task) %>%
  filter(.metric == "ROC_AUC") %>%
  arrange(desc(mean_all)) #-> auc_task
#write_xlsx(auc_task,"models\\averages\\auc_task.xlsx")


# Gender diffs
averages_0.66_gender %>%
  group_by(task) %>%
  summarise(sum = sum(abs(mean_diff), na.rm = TRUE)) %>%
  arrange(sum) #-> genderdiff_task
#write_xlsx(genderdiff_task,"models\\averages\\genderdiff_task.xlsx")

# stategroup diffs
averages_0.66_stategroup %>%
  group_by(task) %>%
  summarise(sum = sum(abs(mean_diff), na.rm = TRUE)) %>%
  arrange(sum) #-> stategroupdiff_task
#write_xlsx(stategroupdiff_task,"models\\averages\\stategroupdiff_task.xlsx")


# Averages by model 0.66 -------------------------------------------------------------------------------------------------
averages_0.66_gender_model <- results_0.66_gender_df %>%
  group_by(model, .metric) %>%
  summarise(mean_all =round(mean(.estimate), round),
            mean_male = round(mean(male), round), mean_female = round(mean(female),round), mean_diff = round(mean(priv_diff),round))
# write_xlsx(averages_0.66_gender_model,"models\\averages\\averages_0.66_gender_model.xlsx")

averages_0.66_stategroup_model <- results_0.66_stategroup_df %>%
  group_by(model, .metric) %>%
  summarise(mean_all = round(mean(.estimate),round),
            mean_AUT = round(mean(AUT),round), mean_nAUT = round(mean(nAUT),round), mean_diff = round(mean(priv_diff),round))
# write_xlsx(averages_0.66_stategroup_model,"models\\averages\\averages_0.66_stategroup_model.xlsx")

# Best measures models _________________________________________________________________________________________________
# Best accuracy
averages_0.66_gender_model %>%
  group_by(model) %>%
  filter(.metric == "Accuracy") %>%
  arrange(desc(mean_all)) #-> accuracy_model
# write_xlsx(accuracy_model,"models\\averages\\accuracy_model.xlsx")

# Best ROC-AUC
averages_0.66_gender_model %>%
  group_by(model) %>%
  filter(.metric == "ROC_AUC") %>%
  arrange(desc(mean_all)) #-> auc_model
#write_xlsx(auc_model,"models\\averages\\auc_model.xlsx")


# Gender diffs
averages_0.66_gender_model %>%
  group_by(model) %>%
  summarise(sum = sum(abs(mean_diff), na.rm = TRUE)) %>%
  arrange(sum) #-> genderdiff_model
#write_xlsx(genderdiff_model,"models\\averages\\genderdiff_model.xlsx")

# stategroup diffs
averages_0.66_stategroup_model %>%
  group_by(model) %>%
  summarise(sum = sum(abs(mean_diff), na.rm = TRUE)) %>%
  arrange(sum) #-> stategroupdiff_model
#write_xlsx(stategroupdiff_model,"models\\averages\\stategroupdiff_model.xlsx")


# Averages by model 0.25 -----------------------------------------------------------------------------------------------
averages_0.25_gender_model <- results_0.25_gender_df %>%
  group_by(model, .metric) %>%
  summarise(mean_all =round(mean(.estimate), round),
            mean_male = round(mean(male), round), mean_female = round(mean(female),round), mean_diff = round(mean(priv_diff),round))
# write_xlsx(averages_0.25_gender_model,"models\\averages\\averages_0.25_gender_model.xlsx")

averages_0.25_stategroup_model <- results_0.25_stategroup_df %>%
  group_by(model, .metric) %>%
  summarise(mean_all = round(mean(.estimate),round),
            mean_AUT = round(mean(AUT),round), mean_nAUT = round(mean(nAUT),round), mean_diff = round(mean(priv_diff),round))
# write_xlsx(averages_0.25_stategroup_model,"models\\averages\\averages_0.25_stategroup_model.xlsx")

# Best measures models _________________________________________________________________________________________________
# Best accuracy
averages_0.25_gender_model %>%
  group_by(model) %>%
  filter(.metric == "Accuracy") %>%
  arrange(desc(mean_all))

# Best FNR
averages_0.25_gender_model %>%
  group_by(model) %>%
  filter(.metric == "FNR") %>%
  arrange(mean_all)

# Gender diffs
averages_0.25_gender_model %>%
  group_by(model) %>%
  summarise(sum = sum(abs(mean_diff), na.rm = TRUE)) %>%
  arrange(sum)

# stategroup diffs
averages_0.25_stategroup_model %>%
  group_by(model) %>%
  summarise(sum = sum(abs(mean_diff), na.rm = TRUE)) %>%
  arrange(sum)

# Heatmap --------------------------------------------------------------------------------------------------------------
# use self-made function for heatmap generation with ggplot

# 0.66 
heatmap_list_0.66 = list()
for(i in task_list){
  p = heatmap(results_0.66_gender[[i]], model, .metric, .estimate) +
    ggtitle(i)
  # name = paste0("plots/", i, "_0.66_heatmap_one.png")
  # ggsave(name, p, dpi = 200)
  heatmap_list_0.66 = append(heatmap_list_0.66,  list(p))
}
names(heatmap_list_0.66) <- task_list


# 0.66 % middle and high group with gender ________________________________________________________________________________
heatmap_list_0.66_male = list()
for(i in task_list){
  p = heatmap(results_0.66_gender[[i]], model, .metric, male) +
    ggtitle(i)
  # name = paste0("plots/", i, "_0.66_heatmap_one_male.png")
  # ggsave(name, p, dpi = 200)
  heatmap_list_0.66_male = append(heatmap_list_0.66_male,  list(p))
}
heatmap_list_0.66_female = list()
for(i in task_list){
  p = heatmap(results_0.66_gender[[i]], model, .metric, female) +
    ggtitle(i)
  # name = paste0("plots/", i, "_0.66_heatmap_one_female.png")
  # ggsave(name, p, dpi = 200)
  heatmap_list_0.66_female = append(heatmap_list_0.66_female,  list(p))
}
heatmap_list_0.66_genderdiff = list()
for(i in task_list){
  p = heatmap_diff(results_0.66_gender[[i]], model, .metric, priv_diff) +
    labs(subtitle = "Gender differences (Male - Female)")
  # name = paste0("plots/", i, "_0.66_heatmap_one_diff.png")
  # ggsave(name, p, dpi = 200)
  heatmap_list_0.66_genderdiff = append(heatmap_list_0.66_genderdiff,  list(p))
}
names(heatmap_list_0.66_genderdiff) <- task_list

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
    labs(subtitle = "Stategroup differences (Austrian - non-Austraian)") 
  # name = paste0("plots/", i, "_0.66_heatmap_one_diff_stategroup.png")
  # ggsave(name, p, dpi = 200)
  heatmap_list_0.66_statediff = append(heatmap_list_0.66_statediff,  list(p))
}
names(heatmap_list_0.66_statediff) <- task_list

# All together _________________________________________________________________________________________________________
heatmap_list_0.66_grid = list()
for(i in task_list){
  p <- ggarrange(heatmap_list_0.66[[i]], 
                 heatmap_list_0.66_genderdiff[[i]], 
                 heatmap_list_0.66_statediff[[i]], ncol = 1)
  name = paste0("plots/", i, "_0.66_heatmap.png")
  # ggsave(name, p, width = 7.50, height = 11.40, dpi = 1000)
  ggsave(name, p, width = 9.50, height = 14.40, dpi = 600)
  heatmap_list_0.66_grid = append(heatmap_list_0.66_grid,  list(p))
}

ggarrange(heatmap_list_0.66[[1]],
          heatmap_list_0.66_genderdiff[[1]],
          heatmap_list_0.66_statediff[[1]], ncol = 1)

# 0.25 % ---------------------------------------------------------------------------------------------------------------
heatmap_list_0.25 =list()
for(i in task_list){
  df_filtered <- filter(results_0.25_gender[[i]], model != "Documentation")
  p = heatmap(results_0.25_gender[[i]], model, .metric, .estimate) +
    ggtitle(i)
  # name = paste0("plots/", i, "_0.25_heatmap_one.png")
  # ggsave(name, p, dpi = 200)
  heatmap_list_0.25 = append(heatmap_list_0.25,  list(p))
}
names(heatmap_list_0.25) <- task_list

# 0.25 % middle and low group with gender ________________________________________________________________________________
heatmap_list_0.25_male = list()
for(i in task_list){
  df_filtered <- filter(results_0.25_gender[[i]], model != "Documentation")
  p = heatmap(results_0.25_gender[[i]], model, .metric, male) +
    ggtitle(i)
  heatmap_list_0.25_male = append(heatmap_list_0.25_male,  list(p))
}
heatmap_list_0.25_female = list()
for(i in task_list){
  df_filtered <- filter(results_0.25_gender[[i]], model != "Documentation")
  p = heatmap(results_0.25_gender[[i]], model, .metric, female) +
    ggtitle(i)
  heatmap_list_0.25_female = append(heatmap_list_0.25_female,  list(p))
}
heatmap_list_0.25_genderdiff = list()
for(i in task_list){
  df_filtered <- filter(results_0.25_gender[[i]], model != "Documentation")
  p = heatmap_diff(results_0.25_gender[[i]], model, .metric, priv_diff) +
    labs(subtitle = "Gender differences (Male - Female)")
  # name = paste0("plots/", i, "_0.25_heatmap_one_diff_gender.png")
  # ggsave(name, p, dpi = 200)
  heatmap_list_0.25_genderdiff = append(heatmap_list_0.25_genderdiff,  list(p))
}
names(heatmap_list_0.25_genderdiff) <- task_list

# 0.25 % middle and low group with stategroup ________________________________________________________________________________
heatmap_list_0.25_AUT = list()
for(i in task_list){
  df_filtered <- filter(results_0.25_stategroup[[i]], model != "Documentation")
  p = heatmap(results_0.25_stategroup[[i]], model, .metric, AUT) +
    ggtitle(i)
  heatmap_list_0.25_AUT = append(heatmap_list_0.25_AUT,  list(p))
}
heatmap_list_0.25_nAUT = list()
for(i in task_list){
  df_filtered <- filter(results_0.25_stategroup[[i]], model != "Documentation")
  p = heatmap(results_0.25_stategroup[[i]], model, .metric, nAUT) +
    ggtitle(i)
  heatmap_list_0.25_nAUT = append(heatmap_list_0.25_nAUT,  list(p))
}
heatmap_list_0.25_statediff = list()
for(i in task_list){
  df_filtered <- filter(results_0.25_stategroup[[i]], model != "Documentation")
  p = heatmap_diff(results_0.25_stategroup[[i]], model, .metric, priv_diff) +
    labs(subtitle = "Stategroup differences (Austrian - non-Austraian)") 
  # name = paste0("plots/", i, "_0.25_heatmap_one_diff_stategroup.png")
  # ggsave(name, p, dpi = 200)
  heatmap_list_0.25_statediff = append(heatmap_list_0.25_statediff,  list(p))
}
names(heatmap_list_0.25_statediff) <- task_list

# All together _________________________________________________________________________________________________________
heatmap_list_0.25_grid = list()
for(i in task_list){
  p <- ggarrange(heatmap_list_0.25[[i]], 
                 heatmap_list_0.25_genderdiff[[i]], 
                 heatmap_list_0.25_statediff[[i]], ncol = 1)
  name = paste0("plots/", i, "_0.25_heatmap.png")
  ggsave(name, p, width = 9.50, height = 14.40, dpi = 600)
  
  heatmap_list_0.25_grid = append(heatmap_list_0.25_grid,  list(p))
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


# Full benchmark stuff =================================================================================================
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


# AMS Regression Coefficients ==========================================================================================
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

