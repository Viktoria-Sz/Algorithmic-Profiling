# MODELLING ############################################################################################################
# Preparation ==========================================================================================================
# Libraries ------------------------------------------------------------------------------------------------------------
library(tidyverse)
#library(broom) # für funktion tidy
library(data.table) # für rbindlist
#library(modelr)
library(mlr3verse) # machine learning set-up for modeling # , options(mlr3.allow_utf8_names = TRUE)
#library(ranger) # Random Forest
library(mlr3fairness)
#library(DALEX)
#library(DALEXtra)
#library(fairmodels)
set.seed(42)
#set.seed(42, "L'Ecuyer-CMRG")

progressr::handlers(global = TRUE) # for progress bars
progressr::handlers("progress")

# runs the outer loop sequentially and the inner loop in parallel
#future::plan(list("multisession"))
# future::plan("multiprocess")

# Load other scripts ---------------------------------------------------------------------------------------------------
source("tasks.R", encoding="utf-8") # for predefined feature sets
source("learners.R", encoding="utf-8") # for predefined feature sets


# Set-up modeling ======================================================================================================
# Tasks ----------------------------------------------------------------------------------------------------------------
tasks = list(#task_ams_youth
             #task_ams
             #task_ams_ext
             #task_diverse
             # task_filtering_disr
             # task_filtering_mrmr
             # task_filtering_jmi
             # task_filtering_cmim
              task_filtering_relief
             # task_behavior
             # task_attitudes
             # task_personality
             # task_characteristics_filter
             # task_otherPES
             )

# Evaluation measures (use msrs() to get a list of all measures --------------------------------------------------------
performance_measures = msrs(c("classif.acc"
                  , "classif.auc" # id = "AUC"
                  , "classif.tpr" # Wie viele Leute wurden richtig in H Kategorie gruppiert
                  , "classif.tnr" # Wie viele Leute wurden richtig in M Kategorie gruppiert
                  , "classif.fpr" # Wie viele Leute wurden fälschlich in H Kategorie gruppiert
                  , "classif.fnr" # Wie viele Leute wurden fälschlich in M Kategorie gruppiert
                  #, "classif.ce"
                  , "classif.fbeta"
                  ))

# performance_measures_ = c(msr("classif.auc", id = "AUC"), 
#                           msr("classif.acc", id = "accuracy"))

# Fairness Measure
fairness_measures_absdiff = msrs(c("fairness.acc"
                           , "fairness.tpr"
                           , "fairness.tnr"
                           , "fairness.fomr"
                           , "fairness.ppv"
                           , "fairness.eod"
                           ))

fairness_measures_diff = list(msr("fairness.acc", operation = groupdiff_diff)
                      , msr("fairness.tpr", operation = groupdiff_diff)
                      , msr("fairness.tnr", operation = groupdiff_diff)
                      , msr("fairness.fomr", operation = groupdiff_diff)
                      , msr("fairness.ppv", operation = groupdiff_diff)
                      , msr("fairness.eod", operation = groupdiff_diff)
                      )

#group_measures = groupwise_metrics(msr("classif.acc"), task_ams_youth)

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

# Resampling strategy --------------------------------------------------------------------------------------------------
#as.data.table(mlr_resamplings)

#resampling_outer_ho = rsmps("holdout", ratio = 0.8)

# Costum resampling
resampling_costum = rsmp("custom")


# Holdout with prespecified holdout row_roles for test and train
#insample = rsmp("insample")

# Learners -------------------------------------------------------------------------------------------------------------
# learners = lrns(c( "classif.featureless" # method: mode
#                    ,"classif.log_reg" # logistic regression
#                    ,"classif.log_reg" # logistic regression
#                    #,"classif.glmnet" # penalized logistic regression
#                    ,"classif.rpart" # Decision tree
#                    ,"classif.ranger" # Random Forest
#                    #,"classif.xgboost"
#                    , "classif.kknn"
#                    #, "classif.svm"
#                    ), 
#                 predict_type = "prob", predict_sets = "test") #  "train", "test"

learners = list(lrn("classif.featureless", id = "Featureless", predict_type = "prob", predict_sets = "test")
              , lrn("classif.log_reg", id = "Logistic Regression", predict_type = "prob", predict_sets = "test")
              , graph_glmnet
              , tuner_rpart
              , tuner_ranger
              , graph_xgboost
              , tuner_kknn
              # , graph_svm
              )

#lapply(learners, function(i) i$feature_types)


# Benchmark design -----------------------------------------------------------------------------------------------------
# Wenn ich das neu aufrufe kommen andere Ergebnisse raus...
# Holdout sample ist immer neu
# Jetzt mit set.seed direkt davor geht es, aber weiter beobachten
set.seed(42)
# design = benchmark_grid(tasks = tasks 
#                         , learners = c(learners, graph_glmnet, graph_xgboost, graph_svm)
#                         , resamplings = resampling_costum
#                         )

grid = data.table::CJ(task = tasks, 
                      learner = learners, #c(learners, graph_glmnet, graph_xgboost, graph_svm), 
                      resampling = list(resampling_costum), sorted = FALSE)

# manual instantiation, since I have a costum test set
Map(function(task, resampling) 
  {resampling$instantiate(task, train_sets = list(setdiff(task$row_ids, test_ids)), test_sets = list(test_ids))}, 
    task =  grid$task, resampling =  grid$resampling)


# Training =============================================================================================================
execute_start_time <- Sys.time()
bmr = benchmark(grid, store_models = TRUE)
evaluation_time <- Sys.time() - execute_start_time 
rm(execute_start_time)
evaluation_time

# Evaluation ===========================================================================================================
print(bmr)

saveRDS(bmr, "models/bmr_AllFilterRelief_RS1000_acc.Rds")
#bmr_ams = readRDS("models/bmr_ams_youth.Rds")


# set threshold before calculating measures!!
#bmr$aggregate()[,resample_result]$predictions()$set_threshold(0.66)

# Measure results
# I guess aggregate is only important for cv -> check
#bmr$aggregate(c(performance_measures, fairness_measures))
bmr$score(c(performance_measures)) # fairness_measures_diff
# fairness measures: male - female -> positiv male ist größer, negativ female ist größer
bmr$score(group_measures)



# Visualizations =======================================================================================================
# Other ----------------------------------------------------------------------------------------------------------------
fairness_prediction_density(prediction_list[[2]], task_ams_youth)
fairness_prediction_density(bmr_ams_youth)

fairness_accuracy_tradeoff(bmr_ams_youth, msr("fairness.fpr"))

compare_metrics(bmr_ams_youth, msrs(c("classif.acc", "classif.auc", "fairness.fpr")))+
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

# Heatmaps -------------------------------------------------------------------------------------------------------------
# Heatmap for performance metrics ______________________________________________________________________________________
scores_list[[2]][1:4]
names(scores_list)[2]

# Performance data
table_performance <- data.frame(matrix(ncol = 3, nrow = 0))
names <- c("measure", "score", "model")
colnames(table_performance) <- names

for(i in 1:32){
  model_scores <- scores_list[[i]][1:4]
  table <- rownames_to_column(as.data.frame(model_scores), "measure") %>%
    rename(score = "model_scores") %>%
    mutate(model = names(scores_list)[i])
  table_performance <- rbind(table_performance, table)
}

# Heatmap performance AMS youth
heatmap(table_performance[1:32,], model, measure, score) +
  ggtitle(label = "Heatmap with performance measures for threshold 0.66 for AMS Youth")


# Heatmap for fairness metrics _________________________________________________________________________________________
scores_list[[2]][5:7]
names(scores_list)[2]

# Fairness data
table_fairness <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(table_fairness) <- names

for(i in 1:32){
  model_scores <- scores_list[[i]][5:7]
  table <- rownames_to_column(as.data.frame(model_scores), "measure") %>%
    rename(score = "model_scores") %>%
    mutate(model = names(scores_list)[i])
  table_fairness <- rbind(table_fairness, table)
}

# Heatmap fairness AMS Youth
heatmap(table_fairness[1:32,], model, measure, score) +
  ggtitle(label = "Heatmap with performance measures for threshold 0.66 for AMS Youth")


# Explain and fairmodels ===============================================================================================
# Explain objects ------------------------------------------------------------------------------------------------------
tab_bmr$learner[[2]]$model
mlr3misc::map(tab_bmr$learner, "model")

# AMS Youth ____________________________________________________________________________________________________________
ids_ams_youth = complete.cases(data[ams_youth])
log_explain_ams_youth <- explain_mlr3(tab_bmr$learner[[2]]$model,
                            data = data[ids_ams_youth, ams_youth[-1]],
                            y = data$r_besch[ids_ams_youth],
                            label = "Log ams youth")
rpart_explain_ams_youth <- explain_mlr3(tab_bmr$learner[[3]]$model,
                              data = data[ids_ams_youth, ams_youth[-1]],
                              y = data$r_besch[ids_ams_youth],
                              label = "rpart ams youth")
ranger_explain_ams_youth <- explain_mlr3(tab_bmr$learner[[4]]$model,
                               data = data[ids_ams_youth, ams_youth[-1]],
                               y = data$r_besch[ids_ams_youth],
                               label = "ranger ams youth")
kknn_explain_ams_youth <- explain_mlr3(tab_bmr$learner[[5]]$model,
                                         data = data[ids_ams_youth, ams_youth[-1]],
                                         y = data$r_besch[ids_ams_youth],
                                         label = "kknn ams youth")
glmnet_explain_ams_youth <- explain_mlr3(tab_bmr$learner[[6]]$model,
                                         data = data[ids_ams_youth, ams_youth[-1]],
                                         y = data$r_besch[ids_ams_youth],
                                         label = "glmnet ams youth")
xgboost_explain_ams_youth <- explain_mlr3(tab_bmr$learner[[7]]$model,
                                         data = data[ids_ams_youth, ams_youth[-1]],
                                         y = data$r_besch[ids_ams_youth],
                                         label = "xgboost ams youth")
svm_explain_ams_youth <- explain_mlr3(tab_bmr$learner[[8]]$model,
                                          data = data[ids_ams_youth, ams_youth[-1]],
                                          y = data$r_besch[ids_ams_youth],
                                          label = "svm ams youth")

# graph_glmnet, graph_xgboost, graph_svm

fobject_ams_youth <- fairness_check(log_explain_ams_youth, rpart_explain_ams_youth, ranger_explain_ams_youth,
                                    #kknn_explain_ams_youth, glmnet_explain_ams_youth, xgboost_explain_ams_youth, svm_explain_ams_youth,
                          protected = data$GENDER[ids_ams_youth],
                          privileged = "male",
                          cutoff = 0.66,
                          verbose = FALSE)

plot(fobject_ams_youth)
fheatmap_ams_youth <- fairness_heatmap(fobject_ams_youth)
plot(fheatmap_ams_youth)

# AMS full _____________________________________________________________________________________________________________
ids_ams = complete.cases(data[ams])
log_explain_ams <- explain_mlr3(tab_bmr$learner[[6]]$model,
                            data = data[ids_ams, ams[-1]],
                            y = data$r_besch[ids_ams],
                            label = "Log ams")
rpart_explain_ams <- explain_mlr3(tab_bmr$learner[[7]]$model,
                              data = data[ids_ams, ams[-1]],
                              y = data$r_besch[ids_ams],
                              label = "rpart ams")
ranger_explain_ams <- explain_mlr3(tab_bmr$learner[[8]]$model,
                               data = data[ids_ams, ams[-1]],
                               y = data$r_besch[ids_ams],
                               label = "ranger ams")

fobject_ams <- fairness_check(log_explain_ams, rpart_explain_ams, ranger_explain_ams,
                          protected = data$GENDER[ids_ams],
                          privileged = "male",
                          cutoff = 0.66,
                          verbose = FALSE)

fobject_ams$parity_loss_metric_data
fobject_ams$groups_data
fobject_ams$fairness_check_data

plot(fobject_ams)
plot_density(fobject_ams)
plot(metric_scores(fobject_ams))
plot(stack_metrics(fobject_ams))
plot(choose_metric(fobject_ams, "FPR"))
plot(performance_and_fairness(fobject_ams, fairness_metric = "FPR"))
plot(group_metric(fobject, fairness_metric = "FPR"))

fheatmap_ams <- fairness_heatmap(fobject_ams)
plot(fheatmap_ams)

# AMS extended _________________________________________________________________________________________________________
ids_ams_ext = complete.cases(data[ams_ext])
log_explain_ams_ext <- explain_mlr3(tab_bmr$learner[[10]]$model,
                            data = data[ids_ams_ext, ams_ext[-1]],
                            y = data$r_besch[ids_ams_ext],
                            label = "Log ams ext")
rpart_explain_ams_ext <- explain_mlr3(tab_bmr$learner[[11]]$model,
                              data = data[ids_ams_ext, ams_ext[-1]],
                              y = data$r_besch[ids_ams_ext],
                              label = "rpart ams ext")
ranger_explain_ams_ext <- explain_mlr3(tab_bmr$learner[[12]]$model,
                               data = data[ids_ams_ext, ams_ext[-1]],
                               y = data$r_besch[ids_ams_ext],
                               label = "ranger ams ext")

fobject <- fairness_check(log_explain_ams_ext, rpart_explain_ams_ext, ranger_explain_ams_ext,
                          protected = data$GENDER[ids_ams_ext],
                          privileged = "male",
                          cutoff = 0.66,
                          verbose = FALSE)

fheatmap <- fairness_heatmap(fobject)
plot(fheatmap)

# Green ________________________________________________________________________________________________________________
ids_green = complete.cases(data[green])
log_explain_green <- explain_mlr3(tab_bmr$learner[[14]]$model,
                            data = data[ids_green, green[-1]],
                            y = data$r_besch[ids_green],
                            label = "Log green")
rpart_explain_green <- explain_mlr3(tab_bmr$learner[[15]]$model,
                              data = data[ids_green, green[-1]],
                              y = data$r_besch[ids_green],
                              label = "rpart green")
ranger_explain_green <- explain_mlr3(tab_bmr$learner[[16]]$model,
                               data = data[ids_green, green[-1]],
                               y = data$r_besch[ids_green],
                               label = "ranger green")

fobject <- fairness_check(log_explain_green, rpart_explain_green, ranger_explain_green,
                          protected = data$GENDER[ids_green],
                          privileged = "male",
                          cutoff = 0.66,
                          verbose = FALSE)

fheatmap <- fairness_heatmap(fobject)
plot(fheatmap)

# Fairmodels -----------------------------------------------------------------------------------------------------------
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



########################################################################################################################
# RESTE ################################################################################################################
# Benchmark threshold heatmaps zeug ------------------------------------------------------------------------------------
# Save log_reg coefficients
# ams_youth_logcoefs <- tab_bmr$learner[[2]]$model$coefficients
# ams_logcoefs <- tab_bmr$learner[[6]]$model$coefficients
# saveRDS(ams_youth_logcoefs, "models/ams_youth_log_coefs.rds")
# saveRDS(ams_logcoefs, "models/ams_log_coefs.rds")

# Aggregate measures as in bmr$score
ams_youth_task_measures <- lapply(tab_bmr$prediction[1:4], 
                                  function(i) i$score(measures = c(performance_measures, fairness_measures), task = task_ams_youth))
green_task_measures <- lapply(tab_bmr$prediction[5:8], 
                              function(i) i$score(measures = c(performance_measures, fairness_measures), task = task_green))

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

# Visualisation of measures --------------------------------------------------------------------------------------------
# All available plot types are listed on the manual page of autoplot.ResampleResult().
# CE Plot
autoplot(bmr) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

# ROC curves for tasks
autoplot(bmr_ams, type = "roc") +
  ggtitle("AMS full")
autoplot(bmr_ams_youth, type = "roc") +
  ggtitle("AMS Youth")
autoplot(bmr_ams_ext, type = "roc") +
  ggtitle("AMS extended")
autoplot(bmr_green, type = "roc") +
  ggtitle("Green")

# Ranked Performance
ranks_performace = tab[, .(learner_id, task_id, rank_train = rank(-classif.auc))] %>% 
  dplyr::arrange(rank_train)
print(ranks_performace)

# Fairness stuff
fairness_accuracy_tradeoff(bmr)
compare_metrics(bmr, fairness_measure)
fairness_prediction_density(bmr)

# Heatmaps =============================================================================================================
ams_youth_heatmap <- tab[task_id == "AMS youth", c(4, 7:14)]
ams_youth_heatmap <- column_to_rownames(ams_youth_heatmap, var="learner_id")
ams_youth_heatmap <- as.matrix(ams_youth_heatmap)
heatmap(ams_youth_heatmap, scale = "column")



# Choose the best model and fit on whole dataset -----------------------------------------------------------------------
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

# Predictions ==========================================================================================================
# Data frame with all predictions? -------------------------------------------------------------------------------------
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

# Validation and Train-/Test Split ---------------------------------------------
# Training set for tuning, test set for final evaluation on untouched data
train_test_ratio = .8
data_training = dplyr::sample_frac(tbl = data, size = train_test_ratio)
data_test = dplyr::anti_join(x = data, y = data_training, by = "case")

ggplot(data_test, aes(x = EMPLOYMENTDAYS)) +
  geom_bar()


# try predifined coeficients -------------------------------------------------------------------------------------------
OR_coefs_youth <- c(-0.13, # Intercept
                    0.13, # altersgruppe 20-24
                    0.00, # altersgruppe 25-28 rausnehmen?
                    0.01, # BUSINESSCASEFREQ1
                    -0.02, # BUSINESSCASEFREQ2
                    -0.06, # BUSINESSCASEFREQ3
                    -0.46, # Childcare_ja
                    0.48, # EducationL
                    0.40, # Education M
                    0.09, # Gender female
                    -0.36, # Impairment strong yes
                    -0.03, # occupationgroup P
                    -0.17, # RGS 3
                    -0.15, # RGS 4
                    -0.21,# Supportmeasure 1 qual
                    -0.14# Supportmeasure 1 Bförd
)
names(OR_coefs_youth) <- names(tab_bmr$learner[[3]]$model$coefficients)
tab_bmr$learner[[3]]$model$coefficients <- OR_coefs_youth



# list stuff -----------------------------------------------------------------------------------------------------------
unlist(probs, recursive = TRUE)

lapply(probs, tidy)
do.call(cbind, lapply(probs, tidy))


tidyframe = lapply(probs, tidy)
cbind(tidyframe[1], tidyframe[2])

# Costum resampling ----------------------------------------------------------------------------------------------------
for(i in 1:length(tasks)){
  test_set = list(test_ids)
  train_set = list(setdiff(tasks[[i]]$row_ids, test_ids))
  resampling_costum$instantiate(tasks[[i]], train_set, test_set)
}
# Part Benchmarks for individual tasks with roc curves -----------------------------------------------------------------
bmr_ams_youth <- bmr$clone(deep = TRUE)$filter(task_id = "AMS youth")
bmr_ams <- bmr$clone(deep = TRUE)$filter(task_id = "AMS full")
bmr_ams_ext <- bmr$clone(deep = TRUE)$filter(task_id = "AMS extended")
bmr_green <- bmr$clone(deep = TRUE)$filter(task_id = "green")

# ROC and PRC Curves 
autoplot(bmr, type = "boxplot", measure = msr("classif.auc")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

# ROC curve
# Sensitivity = Recall = TPR = Wie viele Leute wurden richtig in H Kategorie gruppiert TP/(TP + FN)
# Specificity = TNR = Wie viele Leute wurden richtig in M Kategorie gruppiert = TN/(TN + FP)
# 1 – specificity = False positive rate (FPR) = Wie viele wurden fälschlich in Kategorie H gruppiert
autoplot(bmr_ams_youth, type = "roc")
autoplot(bmr_ams, type = "roc")
autoplot(bmr_ams_ext, type = "roc")
autoplot(bmr_green, type = "roc")
# ROC curve mit bestem Modell je Task und dann zusammen?

# Precision, Recall Plot
# Recall = Sensitivity = TPR = Wie viele Leute wurden richtig in H Kategorie gruppiert
# Precision = Positive predictive value = TP/(TP + FP)
autoplot(bmr_ams_youth, type = "prc")
autoplot(bmr_ams, type = "prc")
autoplot(bmr_ams_ext, type = "prc")
autoplot(bmr_green, type = "prc")
# The ratio of positives and negatives defines the baseline. What is it?

# Stuff ----------------------------------------------------------------------------------------------------------------
tab_bmr$prediction[[1]]$confusion
tab_bmr$prediction[[1]]$score(measures = c(performance_measures, fairness_measures), task = tab_bmr$task[[1]])
tab_bmr$task[[2]]$id
tab_bmr$task[[2]]$col_roles$pta
tab_bmr$learner[[2]]$id

# Confusion Matrix for all models
confusion_list = lapply(tab_bmr$prediction, function(i) i$confusion)
names(confusion_list) <- task_learner_ids

# Scores for all models
scores_list = lapply(tab_bmr$prediction, 
                     function(i) i$score(measures = c(performance_measures, fairness_measures_absdiff), task = task_ams_youth))
names(scores_list) <- task_learner_ids


# Get logistic regression coefficients for ams full and ams youth ------------------------------------------------------
# Plotting coefficients
models[[2]]$coefficients
tab_bmr$learner[[2]]$model

plot_summs(tab_bmr$learner[[2]]$model)
plot_summs(models[[2]], ci_level = 0) +
  ggtitle("Koeffizienten AMS full")

summary(models[[2]])

plot_coefs(models[[2]])

models[[2]]$formula

models[[2]]$call$formula <- f_ams