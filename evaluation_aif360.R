# EVALUATION ###########################################################################################################
# Preparation ==========================================================================================================
# Libraries ------------------------------------------------------------------------------------------------------------
library(tidyverse)

# AIF360
library(aif360)
#install_aif360(envname = "miniconda-test")
reticulate::use_miniconda(condaenv = "miniconda-test", required = TRUE)
load_aif360_lib()

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

df1$gender01 <- as.factor(ifelse(df1$gender == "female", 0, 1))

# load full benchmark
#bmr = readRDS("models/bmr_full.Rds") 

# Performance ==========================================================================================================
# Measures -------------------------------------------------------------------------------------------------------------
# Specify the protected class and privileged & unprivileged classes
privileged <- list("gender01", 1)
unprivileged <- list("gender01", 0)

# df1 without OR
df1_oOR <- subset(df1, model != "OR")

i <- "AMS youth"
j <- "Logistic Regression"


cm_list = list()
bm_truth_list = list()
bm_pred_list = list()

for(i in unique(df1$task)){
  for(j in unique(df1_oOR$model)){
    
    truth_aif_data = df1 %>%
      subset(model == j & task == i, c(truth_01, gender01)) %>%
      rename(label = truth_01)
    
    pred_aif_data = df1 %>%
      subset(model == j & task ==i, c(estimate_0.66, gender01)) %>%
      rename(label = estimate_0.66)
    
    pred_aif <- aif360::binary_label_dataset(data_path = pred_aif_data,
                                             favor_label = 0,
                                             unfavor_label = 1,
                                             unprivileged_protected_attribute = 0,
                                             privileged_protected_attribute = 1,
                                             target_column = "label",
                                             protected_attribute = "gender01")
    truth_aif <- aif360::binary_label_dataset(data_path = truth_aif_data,
                                              favor_label = 0,
                                              unfavor_label = 1,
                                              unprivileged_protected_attribute = 0,
                                              privileged_protected_attribute = 1,
                                              target_column = "label",
                                              protected_attribute = "gender01")
    
    cm <- list(classification_metric(truth_aif, pred_aif, privileged, unprivileged))
    bm_truth <- list(binary_label_dataset_metric(truth_aif, privileged, unprivileged))
    bm_pred <- list(binary_label_dataset_metric(pred_aif, privileged, unprivileged))
    
    task_learner_id = paste(j, i, sep=", ")
    
    names(cm) <- task_learner_id
    names(bm_truth) <- task_learner_id
    names(bm_pred) <- task_learner_id
    
    cm_list = append(cm_list, cm)
    bm_truth_list = append(bm_truth_list, bm_truth)
    bm_pred_list = append(bm_pred_list, bm_pred)
    
  }
}


# Measures
# Accuracy, TPR, TNR, FNR, PPV, F1-score -> diff men and women
lapply(cm_list, function(i) i$performance_measures())
lapply(cm_list, function(i) i$performance_measures(privileged = TRUE))
lapply(cm_list, function(i) i$performance_measures(privileged = FALSE))

# Statistical Parity Difference, Consistency
lapply(bm_pred_list, function(i) i$statistical_parity_difference())
lapply(bm_pred_list, function(i) i$consistency())

# RESTE AIF360 #########################################################################################################
# Test -----------------------------------------------------------------------------------------------------------------
truth_yLR <- subset(df1, model == "Logistic Regression" & task == "AMS youth", c(truth_01, gender01))
names(truth_yLR)[1] <- "label"
pred_yLR <- subset(df1, model == "Logistic Regression" & task == "AMS youth", c(estimate_0.66, gender01))
names(pred_yLR)[1] <- "label"


pred_aif <- aif360::binary_label_dataset(data_path = pred_yLR,
                                         favor_label = 1,
                                         unfavor_label = 0,
                                         unprivileged_protected_attribute = 0,
                                         privileged_protected_attribute = 1,
                                         target_column = "label",
                                         protected_attribute = "gender01")
truth_aif <- aif360::binary_label_dataset(data_path = truth_yLR,
                                          favor_label = 1,
                                          unfavor_label = 0,
                                          unprivileged_protected_attribute = 0,
                                          privileged_protected_attribute = 1,
                                          target_column = "label",
                                          protected_attribute = "gender01")
# classification measures ----------------------------------------------------------------------------------------------
# accuracy
cm$accuracy() # ACC=(TP+TN)/(P+N)
cm$accuracy(privileged = TRUE) # ACC=(TP+TN)/(P+N)
cm$accuracy(privileged = FALSE) # ACC=(TP+TN)/(P+N)

# error rate
cm$error_rate() # ERR=(FP+FN)/(P+N)
cm$error_rate(privileged = TRUE) # ERR=(FP+FN)/(P+N)
cm$error_rate(privileged = FALSE) # ERR=(FP+FN)/(P+N)
cm$error_rate_difference()

# False discovery rate
cm$false_omission_rate() # FDR= FP/(TP+FP)
cm$false_omission_rate(privileged= TRUE)
cm$false_omission_rate(privileged= FALSE)
cm$false_omission_rate_difference()

# False omission rate
cm$false_discovery_rate() # FOR= FN/(TN+FN)
cm$false_discovery_rate(privileged= TRUE)
cm$false_discovery_rate(privileged= FALSE)
cm$false_discovery_rate_difference()

# False negative rate
cm$false_negative_rate() # FNR= FN/P
cm$false_negative_rate(privileged= TRUE)
cm$false_negative_rate(privileged= FALSE)
cm$false_negative_rate_difference()

# False positive rate
cm$false_positive_rate() # FPR= FP/N 
cm$false_positive_rate(privileged= TRUE)
cm$false_positive_rate(privileged= FALSE)
cm$false_positive_rate_difference()

# Negative predictive value
cm$negative_predictive_value() # NPV=TN/(TN+FN)
cm$negative_predictive_value(privileged= TRUE)
cm$negative_predictive_value(privileged= FALSE)

# Positive predictive value; # cm$precision()
cm$positive_predictive_value() # PPV= TP/(TP+FP)
cm$positive_predictive_value(privileged= TRUE)
cm$positive_predictive_value(privileged= FALSE)

# True negative rate
cm$true_negative_rate() # cm$specificity()
cm$true_negative_rate(privileged= TRUE) # cm$specificity()
cm$true_negative_rate(privileged= FALSE) # cm$specificity()

# True positive rate
cm$true_positive_rate()  # cm$recall() # cm$sensitivity()
cm$true_positive_rate(privileged= TRUE)
cm$true_positive_rate(privileged= FALSE)
cm$true_positive_rate_difference() #equal_opportunity_difference()


#Average of difference in FPR and TPR for unprivileged and privileged groups; A value of 0 indicates equality of odds.
cm$average_odds_difference()  # 1/2[(FPRD=unprivileged−FPRD=privileged)+(TPRD=unprivileged−TPRD=privileged))


# Classification metric ------------------------------------------------------------------------------------------------
cm <- classification_metric(truth_aif, pred_aif, privileged, unprivileged)


cm$performance_measures()
cm$performance_measures(privileged = TRUE)
cm$performance_measures(privileged = FALSE)

# confusion matrix
cm$binary_confusion_matrix()
cm$binary_confusion_matrix(privileged = TRUE)
cm$binary_confusion_matrix(privileged = FALSE)


# Binary label metric --------------------------------------------------------------------------------------------------
bm_truth <- binary_label_dataset_metric(truth_aif, privileged, unprivileged)
bm_pred <- binary_label_dataset_metric(pred_aif, privileged, unprivileged)

# Base rate 
bm_truth$base_rate() # Pr(Y=1)=P/(P+N)
bm_truth$base_rate(privileged = TRUE) # Base rate truth for HIGH for men
bm_truth$base_rate(privileged = FALSE) #  Base rate truth for HIGH for women

bm_pred$base_rate() # Pr(Y=1)=P/(P+N)
bm_pred$base_rate(privileged = TRUE) # Base rate prediction for HIGH for men
bm_pred$base_rate(privileged = FALSE) #  Base rate prediction for HIGH for women

bm_pred$statistical_parity_difference() # difference of base rate women and men

# Individual fairness metric from [1] that measures how similar the labels are for similar instances.
bm_pred$consistency() # with n_neighbours = 5


