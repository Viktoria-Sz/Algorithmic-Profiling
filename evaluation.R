# EVALUATION ###################################################################
# Preparation ==================================================================
# Libraries --------------------------------------------------------------------
library(fairness)
library(fairmodels)

# Load data --------------------------------------------------------------------


# Load other scripts -----------------------------------------------------------


# Load trained models ----------------------------------------------------------

# Performance ==================================================================

# Fairness =====================================================================

# Predictive Rate Parity
res1 <- pred_rate_parity(data = data, outcome = "truth", outcome_base = "0", 
                         group = "GESCHLECHT_WEIBLICH", probs = "prob_ams", cutoff = 0.66, base = "male")
res1$Metric
res1$Metric_plot
res1$Probability_plot

# Demographic Parity
res_dem <- dem_parity(data = data, outcome = "truth", outcome_base = "0", 
                      group = "GESCHLECHT_WEIBLICH", probs = "prob_ams", cutoff = 0.66, base = "male")
res_dem$Metric
res_dem$Metric_plot
res_dem$Probability_plot

# Proportional Parity
res_prop <- prop_parity(data = data, outcome = "truth", outcome_base = "0", 
                        group = "GESCHLECHT_WEIBLICH", probs = "prob_ams", cutoff = 0.66, base = "male")
res_prop$Metric
res_prop$Metric_plot

# Equalized Odds
res_eq <- equal_odds(data = data, outcome = "truth", outcome_base = "0", 
                     group = "GESCHLECHT_WEIBLICH", probs = "prob_ams", cutoff = 0.66, base = "male")
res_eq$Metric
res_eq$Metric_plot
res_eq$Probability_plot

# Accuracy Parity
res_acc <- acc_parity(data = data, outcome = "truth", outcome_base = "0", 
                      group = "GESCHLECHT_WEIBLICH", probs = "prob_ams", cutoff = 0.66, base = "male")
res_acc$Metric
res_acc$Metric_plot
res_acc$Probability_plot

# False Negative Rate Parity
res_fnr <- fnr_parity(data = data, outcome = "truth", outcome_base = "0", 
                      group = "GESCHLECHT_WEIBLICH", probs = "predicted_prob1", cutoff = 0.66, base = "male")
res_fnr$Metric
res_fnr$Metric_plot

# False Positive Rate Parity
res_fpr <- fpr_parity(data = data, outcome = "truth", outcome_base = "0", 
                      group = "GESCHLECHT_WEIBLICH", probs = "predicted_prob1", cutoff = 0.66, base = "male")
res_fpr$Metric
res_fpr$Metric_plot

# Negative Predictive Value Parity
res_npv <- npv_parity(data = data, outcome = "truth", outcome_base = "0", 
                      group = "GESCHLECHT_WEIBLICH", probs = "predicted_prob1", cutoff = 0.66, base = "male")
res_npv$Metric
res_npv$Metric_plot

# Specificity Parity
res_sp <- spec_parity(data = data, outcome = "truth", outcome_base = "0", 
                      group = "GESCHLECHT_WEIBLICH", probs = "predicted_prob1", cutoff = 0.66, base = "male")
res_sp$Metric
res_sp$Metric_plot

# ROC AUC Parity
res_auc <- roc_parity(data = data, outcome = "truth", 
                      group = "GESCHLECHT_WEIBLICH", probs = "predicted_prob1", base = "male")
res_auc$Metric
res_auc$Metric_plot
res_auc$ROCAUC_plot

# Matthews correlation coefficient Parity
res_mcc <- mcc_parity(data = data, outcome = "truth", outcome_base = "0", 
                      group = "GESCHLECHT_WEIBLICH", probs = "predicted_prob1", cutoff = 0.66, base = "male")
res_mcc$Metric
res_mcc$Metric_plot
