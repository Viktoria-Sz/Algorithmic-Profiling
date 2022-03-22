# MODELLING ####################################################################
# Preparation ==================================================================
# Libraries --------------------------------------------------------------------
library(tidyverse)
library(broom) # für funktion tidy
library(data.table) # für rbindlist
#library(modelr)
library(mlr3verse) # machine learning set-up for modeling
library(ranger) # Random Forest
library(mlr3fairness)
library(DALEX)
library(DALEXtra)
library(fairmodels)
set.seed(42)

# Load data --------------------------------------------------------------------
# load preperad dataset
data <- readRDS("data/JuSAW_prepared.rds")

# Load other scripts -----------------------------------------------------------
source("variable_sets.R", encoding="utf-8") # for predefined feature sets

# Validation and Train-/Test Split ---------------------------------------------
# Training set for tuning, test set for final evaluation on untouched data
train_test_ratio = .8
data_training = dplyr::sample_frac(tbl = data, size = train_test_ratio)
data_test = dplyr::anti_join(x = data, y = data_training, by = "case")

ggplot(data_test, aes(x = EMPLOYMENTDAYS)) +
  geom_bar()

# Task set-up ==================================================================
# Set up different tasks with different variable sets --------------------------
# Original AMS-Model
task_ams_youth = as_task_classif(data[ams_youth], 
                                 target = "EMPLOYMENTDAYS", 
                                 positive = ">=90 Tage", id = "AMS youth")
task_ams_youth_train = as_task_classif(data_training[ams_youth], 
                                 target = "EMPLOYMENTDAYS", 
                                 positive = ">=90 Tage", id = "AMS youth train")
task_ams_youth_test = as_task_classif(data_test[ams_youth], 
                                 target = "EMPLOYMENTDAYS", 
                                 positive = ">=90 Tage", id = "AMS youth test")
#task_ams_youth$set_col_roles("case", roles = "name")

ids = complete.cases(task_ams_youth$data())
sum(!ids)
task_ams_youth$filter(which(ids))

ids = complete.cases(task_ams_youth_train$data())
sum(!ids)
task_ams_youth_train$filter(which(ids))

ids = complete.cases(task_ams_youth_test$data())
sum(!ids)
task_ams_youth_test$filter(which(ids))
# number of incomplete observations
# should be zero now
#ids = complete.cases(task_green$data())
#sum(!ids)

# Set protected attribute
task_ams_youth$col_roles$pta = "GENDER_female"
task_ams_youth_train$col_roles$pta = "GENDER_female"
task_ams_youth_test$col_roles$pta = "GENDER_female"

print(task_ams_youth)
# Logistic regression model with all variables step-procedure ------------------
# Restricted logistic regression model with all variables ----------------------

# Grenn variables --------------------------------------------------------------
task_green_all = as_task_classif(data[green_all], 
                                 target = "EMPLOYMENTDAYS", 
                                 positive = ">=90 Tage", id = "green_all")
task_green = as_task_classif(data[green], 
                             target = "EMPLOYMENTDAYS", 
                             positive = ">=90 Tage", id = "green")
task_green_train = as_task_classif(data_training[green], 
                             target = "EMPLOYMENTDAYS", 
                             positive = ">=90 Tage", id = "green train")
task_green_test = as_task_classif(data_test[green], 
                             target = "EMPLOYMENTDAYS", 
                             positive = ">=90 Tage", id = "green test")

ids = complete.cases(task_green$data())
sum(!ids)
task_green$filter(which(ids))

ids = complete.cases(task_green_train$data())
sum(!ids)
task_green_train$filter(which(ids))

ids = complete.cases(task_green_test$data())
sum(!ids)
task_green_test$filter(which(ids))

task_green$col_roles$pta = "GENDER_female"
task_green_train$col_roles$pta = "GENDER_female"
task_green_test$col_roles$pta = "GENDER_female"


print(task_green)

# Set-up rest ==================================================================
# Tasks
tasks = c(task_ams_youth
          , task_green
          )

# Evaluation measures (use msrs() to get a list of all measures ----------------
performance_measures = msrs(c("classif.acc"
                  , "classif.auc"
                  , "classif.tpr"
                  , "classif.fpr"
                  , "classif.ce"
                  , "classif.fbeta"
                  ))

# Fairness Measure
fairness_measures = msrs(c("fairness.acc"
                           , "fairness.fpr"
                           , "fairness.fnr"
                           , "fairness.fomr"
                           , "fairness.ppv"
                           , "fairness.eod"
                           ))
# fairness.acc: Absolute differences in accuracy across groups
# fairness.fpr: Absolute differences in false positive rates across groups
# -> In my case the FPR represents the unemployed who falsely got into the H group
#    and wont get any help, even though they need it. 
#    This rate should not be higher for men
# -> TNR represents the rate at which people got into the M group and do indeed
#    need help finding a job.
#    This rate should not be higher for men
# fairness.fnr: Absolute differences in false negative rates across groups
# -> In my case the FNR represents the rate at which unemployed get into the M group
#    and probably have to participate in support measures even though they dont
#    need them and it would only prolong their unemployment period but not help 
#    them to find a job, since they would find one either way.
# -> TPR would represent the rate at which unemployed rightly got into the h group,
#    that means  they dont need help and dont get help.
#    This rate should not be lower for women
#    This rate is probably higher for highly motivated people
# fairness.fomr: Absolute differences in false omission rates across groups
# fairness.ppv: Absolute differences in positive predictive values across groups
# fairness.eod: Equalized Odds: Sum of absolute differences between true positive and false positive rates across groups

# measures = list(
#   msr("classif.auc", predict_sets = "train", id = "auc_train"),
#   msr("classif.auc", id = "auc_test"))

# Resampling strategy ----------------------------------------------------------
#as.data.table(mlr_resamplings)

resampling = rsmp("holdout", ratio = 0.8)
#resampling = rsmp("cv", folds = 3)

#rr = resample(task_AMS, learner, resampling, store_models = TRUE)

# Learners ---------------------------------------------------------------------
learners = lrns(c( "classif.featureless"
                   ,"classif.log_reg" # logistic regression
                   #,"classif.glmnet" # penalized logistic regression
                   ,"classif.rpart" # Decision tree
                   ,"classif.ranger" # Random Forest
                   #,"classif.xgboost"
                   ), 
                predict_type = "prob", predict_sets = "test") #  "train", "holdout"

# Benchmark design -------------------------------------------------------------
design = benchmark_grid(tasks = tasks, 
                        learners = learners,
                        resamplings = resampling)


# Training =====================================================================
#execute_start_time <- Sys.time()
bmr = benchmark(design, store_models = TRUE)
#evaluation_time <- Sys.time() - execute_start_time 
#rm(execute_start_time)

bmr_ams <- bmr$clone(deep = TRUE)$filter(task_id = "AMS youth train")
bmr_green <- bmr$clone(deep = TRUE)$filter(task_id = "green train")

# Evaluation ===================================================================
print(bmr)

# set threshold before calculating measures!!
#bmr$aggregate()[,resample_result]$predictions()$set_threshold(0.66)

# Measure results
# I guess aggregate is only important for cv -> check
#bmr$aggregate(c(performance_measures, fairness_measures))
bmr$score(c(performance_measures, fairness_measures))

# Set threshold to 66% as in the ams paper
tab_bmr <- as.data.table(bmr)
lapply(tab_bmr$prediction, function(i) i$set_threshold(0.66))


# Aggregate measures as in bmr$score
tab_bmr$task
ams_youth_task_measures <- lapply(tab_bmr$prediction[1:4], 
       function(i) i$score(measures = c(performance_measures, fairness_measures),
                                               task = task_ams_youth))
green_task_measures <- lapply(tab_bmr$prediction[5:8], 
       function(i) i$score(measures = c(performance_measures, fairness_measures),
                           task = task_green))

ams_youth_task <- rbindlist(lapply(ams_youth_task_measures, tidy), idcol=TRUE)
ams_youth_task <- ams_youth_task %>%
  rename(learner = .id,measure_id = names, measure_value = x) %>%
  mutate(learner = as.factor(learner), task = "AMS youth")
levels(ams_youth_task$learner) <- c("featureless", "Log_reg", "rpart", "ranger")

green_task <- rbindlist(lapply(green_task_measures, tidy), idcol=TRUE)
green_task <- green_task %>%
  rename(learner = .id, measure_id = names, measure_value = x) %>%
  mutate(learner = as.factor(learner), task = "Green")
levels(green_task$learner) <- c("featureless", "Log_reg", "rpart", "ranger")

measures_table_0.66 <- rbind(ams_youth_task, green_task)
# measures_table_0.5 <- rbind(ams_youth_task, green_task)
# measures_table_0.5 <- unite(measures_table_0.5, "id", c("task", "learner"), remove = FALSE)
measures_table_0.66 <- unite(measures_table_0.66, "id", c("task", "learner"), remove = FALSE)


tab_bmr$learner
# Predictions
tab_bmr$prediction # als Tabelle?

#confusion_list_0.5 <- lapply(tab_bmr$prediction, function(i) i$confusion)
#lapply(tab_bmr$prediction, function(i) i$set_threshold(0.5))
confusion_list_0.66 <- lapply(tab_bmr$prediction, function(i) i$confusion)
# names(confusion_list_0.5) <- c("ams featureless", "ams Log_reg", "ams rpart", "ams ranger",
#                            "green featureless", "green Log_reg", "green rpart", "green ranger")
names(confusion_list_0.66) <- c("ams featureless", "ams Log_reg", "ams rpart", "ams ranger",
                               "green featureless", "green Log_reg", "green rpart", "green ranger")


# Heatmap with meausres
p_0.66 <- ggplot(measures_table_0.66, aes(measure_id, id, fill = measure_value)) +          # Specify colors manually
  geom_tile() +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) + 
  geom_text(aes(label = round(measure_value, 2))) +
  scale_fill_distiller(palette = "OrRd", direction = 1, limits = c(0,1)) +
  ggtitle(label = "Heatmap with performance and fairness measures for threshold 0.66")

p_0.5 <- ggplot(measures_table_0.5, aes(measure_id, id, fill = measure_value)) +          # Specify colors manually
  geom_tile() +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 70, vjust = 1, hjust=1)) + 
  geom_text(aes(label = round(measure_value, 2))) +
  scale_fill_distiller(palette = "OrRd", direction = 1, limits = c(0,1)) +
  ggtitle(label = "Heatmap with performance and fairness measures for threshold 0.5")

library(plotly)
ggplotly(p_0.66)

# Visualisation of measures ----------------------------------------------------
# All available plot types are listed on the manual page of autoplot.ResampleResult().
# CE Plot
autoplot(bmr) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

# ROC curves for tasks
autoplot(bmr_ams, type = "roc")
autoplot(bmr_green, type = "roc")

# Ranked Performance
ranks_performace = tab[, .(learner_id, task_id, rank_train = rank(-classif.auc))] %>% 
  dplyr::arrange(rank_train)
print(ranks_performace)

# Fairness stuff
fairness_accuracy_tradeoff(bmr)
compare_metrics(bmr, fairness_measure)
fairness_prediction_density(bmr)

# Heatmaps =====================================================================
ams_youth_heatmap <- tab[task_id == "AMS youth", c(4, 7:14)]
ams_youth_heatmap <- column_to_rownames(ams_youth_heatmap, var="learner_id")
ams_youth_heatmap <- as.matrix(ams_youth_heatmap)
heatmap(ams_youth_heatmap, scale = "column")

# Predictions ==================================================================

# Explain and fairmodels =======================================================
# Explain objects --------------------------------------------------------------
tab_bmr$learner[[2]]$model
mlr3misc::map(tab_bmr$learner, "model")

ids = complete.cases(data[ams])
log_explain <- explain_mlr3(tab_bmr$learner[[2]]$model,
                            data = data[ids, ams_youth[-1]],
                            y = data$r_besch[ids],
                            label = "Log ams youth")
rpart_explain <- explain_mlr3(tab_bmr$learner[[3]]$model,
                              data = data[ids, ams_youth[-1]],
                              y = data$r_besch[ids],
                              label = "rpart ams youth")
ranger_explain <- explain_mlr3(tab_bmr$learner[[4]]$model,
                               data = data[ids, ams_youth[-1]],
                               y = data$r_besch[ids],
                               label = "ranger ams youth")

# Fairmodels -------------------------------------------------------------------
fobject <- fairness_check(log_explain, rpart_explain, ranger_explain,
                          protected = data$GENDER_female[ids],
                          privileged = "male",
                          cutoff = 0.66,
                          verbose = FALSE)
fobject$parity_loss_metric_data
fobject$fairness_check_data
plot(fobject)
plot_density(fobject)
plot(metric_scores(fobject))
sm <- stack_metrics(fobject)
plot(sm)
fair_pca <- fairness_pca(fobject)
print(fair_pca)
plot(fair_pca)

fheatmap <- fairness_heatmap(fobject)
plot(fheatmap)

fap <- performance_and_fairness(fobject, fairness_metric = "TPR")
plot(fap)

ac <- all_cutoffs(fobject)
plot(ac)



# RESTE ########################################################################
# Choose the best model and fit on whole dataset 
# Wir hatten oben log.reg ausgewählt, random forest oder knn waren aber genauso gut
# Choose final model
learner_final = lrn("classif.log_reg",predict_type = "prob")

# Train on whole train data
learner_final$train(task_green)

# Test on never touched test data (20% of whole data splitted at the beginning)
ids = complete.cases(data_test[,green])
# number of incomplete observations
sum(!ids)
data_test <- filter(data_test, ids)

pred = learner_final$predict_newdata(newdata = data_test)
pred$confusion
pred$score(measures)
pred$set_threshold(0.66)
pred$confusion
pred$score(measures)


# Classification Tree ----------------------------------------------------------
rpart = lrn("classif.rpart", predict_type = "prob")
rpart$train(task_AMS, row_ids = tain_set)
pred = rpart$predict(task_AMS, row_ids = test_set)

autoplot(pred)
autoplot(pred, type = "roc")
autoplot(pred, type = "prc") # PRCs are preferred to ROCs for imbalanced populations
pred$set_threshold(0.66)
pred$confusion
pred$score(measures)

# Other models
randomForest::randomForest()
xgboost::xgboost()


# Predictions knn
result_knn = tab$resample_result[[6]]
as.data.table(result_knn$prediction())

# Model Parameter
knn = bmr$score()[learner_id == "classif.kknn.tuned"]$learner
for (i in 1:10){
  print(knn[[i]]$tuning_result$params)
}

ranger = bmr$score()[learner_id == "classif.ranger.tuned"]$learner
for (i in 1:10){
  print(ranger[[i]]$tuning_result$params)
}

# Variable Importance ---------------------------------------------------------
# ranger
# learner_ranger = learners[[5]]
# Variable importance mode, one of 'none', 'impurity', 'impurity_corrected', 
# 'permutation'. The 'impurity' measure is the Gini index for classification, 
# the variance of the responses for regression and the sum of test statistics 
# (see splitrule) for survival.

filter = flt("importance", learner = learner_ranger)
filter$calculate(task_mushrooms)
feature_scores <- as.data.table(filter)

ggplot(data = feature_scores, aes(x = reorder(feature, -score), y = score)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  ggtitle(label = "Variable Importance Mushroom Features") +
  labs(x = "Features", y = "Variable Importance Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = pretty(1:500, 10))

# Predictions ==================================================================
# Data frame with all predictions?
green_rpart = bmr$aggregate()[learner_id == "classif.rpart" & task_id == "green train", resample_result][[1]]
pred_green_rpart <- green_rpart$predictions()[[1]]
pred_green_rpart$set_threshold(0.66)

# confusion matrix
pred_green_rpart$confusion

rr = bmr$aggregate()[learner_id == "classif.rpart"]
bmr$predictions()


predictions = learners$predict(task_ams_youth_test)
predictions = learners$predict_newdata(data_test, task = "AMS youth")


# Datasheets and Modelcards ====================================================
rmdfile = report_datasheet()
rmarkdown::render(rmdfile)
