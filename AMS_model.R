# AMS MODEL REBUILD ############################################################

# Libraries --------------------------------------------------------------------
library(tidyverse)
library(modelr)
options(na.action = na.warn)

# Load data --------------------------------------------------------------------
data <- readRDS("dataJuSAW.rds")
attach(data)

# Data preparation for AMS -----------------------------------------------------

# Variables for AMAS
# Geschlecht weiblich, mit 0= männlich, 1=weiblich
table(r_geschlecht)
table(female, r_geschlecht) # lieber female verwenden, hat weniger missings
table(female)
data$GESCHLECHT_WEIBLICH <- factor(female, levels = c("0", "1"), labels = c("male", "female"), ordered = FALSE)
table(data$GESCHLECHT_WEIBLICH)

# Ausbildung
table(r_ausbildung) 
data$AUSBILDUNG <- factor(r_ausbildung, levels = c("L", "M", "P"), labels =  c("L", "M", "P"), ordered = FALSE)
data$AUSBILDUNG <- relevel(data$AUSBILDUNG, ref = "P")
table(data$AUSBILDUNG)

table(ausb_t1)
table(eduhöchst4)
table(eduhöchst4_t1)

# Betreuungspflichten (nur für Frauen)
table(r_betreuungspflichten)
table(r_betreuungspflichten, kinder) 
table(r_betreuungspflichten, female)
table(kinder, female)
table(kinder)

data$BETREUUNGSPFLICHTEN_BOTH <-factor(kinder, levels = c("ja", "nein"), labels =  c("ja", "nein"), ordered = FALSE)
data$BETREUUNGSPFLICHTEN <- ifelse(data$GESCHLECHT_WEIBLICH == "female" & data$BETREUUNGSPFLICHTEN_BOTH =="ja", "ja", "nein")
data$BETREUUNGSPFLICHTEN <- factor(data$BETREUUNGSPFLICHTEN, ordered = FALSE)
data$BETREUUNGSPFLICHTEN_BOTH <- relevel(data$BETREUUNGSPFLICHTEN_BOTH, ref = "nein")
data$BETREUUNGSPFLICHTEN <- relevel(data$BETREUUNGSPFLICHTEN, ref = "nein")
table(data$BETREUUNGSPFLICHTEN_BOTH)
table(data$BETREUUNGSPFLICHTEN)
table(data$BETREUUNGSPFLICHTEN, data$GESCHLECHT_WEIBLICH)
table(data$BETREUUNGSPFLICHTEN_BOTH, data$GESCHLECHT_WEIBLICH)

# RGS Typ
table(r_rgstyp)
data$RGS_TYP <-factor(r_rgstyp, levels = c("1", "2", "3", "4"), labels =  c("1", "2", "3", "4"), ordered = FALSE)
is.ordered(data$RGS_TYP)
table(data$RGS_TYP)

# Beeinträchtigt
table(shealth)
table(lhealth) # wahrscheinlich beste Wahl
table(longill) 
data$BEEINTRÄCHTIGT_order <-factor(lhealth, levels = c("ja stark", "ja ein wenig", "nein"), 
                                   labels =  c("ja stark", "ja ein wenig", "nein"), ordered =  TRUE)
data$BEEINTRÄCHTIGT <-ifelse(data$BEEINTRÄCHTIGT_order == "ja stark" | data$BEEINTRÄCHTIGT_order == "ja ein wenig", "ja", "nein")
data$BEEINTRÄCHTIGT <- factor(data$BEEINTRÄCHTIGT, ordered = FALSE)
data$BEEINTRÄCHTIGT <- relevel(data$BEEINTRÄCHTIGT, ref = "nein")
is.ordered(data$BEEINTRÄCHTIGT_order)
is.ordered(data$BEEINTRÄCHTIGT)

# Was machen mit den den "keine Angabe" Leuten?
table(data$BEEINTRÄCHTIGT_order)
table(data$BEEINTRÄCHTIGT_order, lhealth, useNA = "always")
ordered(data$BEEINTRÄCHTIGT_order)
table(data$BEEINTRÄCHTIGT)

# Berufsgruppe Produktion oder Service
table(r_berufsgruppe_ams1)
table(r_berufsgruppe)
table(r_berufsgruppe_ams1, r_berufsgruppe) # Was ist mit 9? Wird nicht berücksichtigt im AMS Methoden paper
data$BERUFSGRUPPE_all <- factor(r_berufsgruppe_ams1, ordered = FALSE)
data$BERUFSGRUPPE <- factor(r_berufsgruppe, ordered = FALSE)

# WICHTIG: Für junge Leute unter 25:
# Für diese Population werden die Merkmale STAATENGRUPPE, GESCHÄFTSFALLDAUER und BESCHÄFTIGUNGSVERLAUF nicht für die Schätzung verwendet.
# Beschäftigungsverlauf vor AL
table(r_beschverl_voral)
data$BESCHÄFTIGUNGSVERLAUF <- factor(r_beschverl_voral, levels = c(1, 2), labels = c(">75%", "<75%"), ordered = FALSE)
table(data$BESCHÄFTIGUNGSVERLAUF)

table(r_monate_erw_j1voral, useNA = "always")
table(r_monate_erw_j2voral, useNA = "always")
table(r_monate_erw_j3voral, useNA = "always")
table(r_monate_erw_j4voral, useNA = "always")

# Geschäftsfalldauer
# 0 = kein Geschäftsfall mit Dauer >= 180 Tage; 1 = 1 oder mehrere Geschäftsfälle mit Dauer >= 180 Tage -> halbes Jahr
table(r_geschfalldau_voral)
table(r_geschfalldau3m_voral)
data$GESCHÄFTSFALLDAUER <- factor(r_geschfalldau_voral, levels = c(0, 1), labels = c("kein GF>=180", "GF>=180"), ordered = FALSE)

table(r_geschaeftsfall_j1voral, useNA = "always")
table(r_geschaeftsfall_j2voral, useNA = "always")
table(r_geschaeftsfall_j3voral, useNA = "always")
table(r_geschaeftsfall_j4voral, useNA = "always")

# Geschäftsfallfrequenz
table(r_geschfallfreq_voral)
data$GESCHÄFTSFALLFREQ <- factor(r_geschfallfreq_voral)
data$GESCHÄFTSFALLFREQ_order <- factor(r_geschfallfreq_voral, levels = c(0, 1, 2, 3),  ordered = TRUE)
table(data$GESCHÄFTSFALLFREQ_order)
is.ordered(data$GESCHÄFTSFALLFREQ_order)

# Maßnahmenteilnahme
table(r_maßnahmenteilnahme)
data$MASSNAHMENTEILNAHME <- factor(r_maßnahmenteilnahme, levels = c(0, 1, 2, 3), 
                                   labels = c("kM", "min 1 unterst.", "min 1 qual", "min 1 Bförd"), ordered = FALSE)
data$MASSNAHMENTEILNAHME_order <- factor(data$MASSNAHMENTEILNAHME, ordered = TRUE)
table(data$MASSNAHMENTEILNAHME_order)
is.ordered(data$MASSNAHMENTEILNAHME_order)

# Altersgruppe entfällt, da alle unter 30
# the characteristic AGE GROUP is redefined: less than 20 years (0), 20 to 24 years (20).
table(ageg)
data$ALTERSGRUPPE <- factor(ageg)
table(data$ALTERSGRUPPE)

# Staatengruppe
table(r_staatengruppe)  
data$STAATENGRUPPE <- factor(r_staatengruppe, levels = c("AUT", "DRITT", "EU"), labels =  c("AUT", "DRITT", "EU"), ordered = FALSE)
table(data$STAATENGRUPPE)

table(birthAT)
table(birthAT_v)
table(birthAT_m)
table(mighint12g_new)

# Abhängige Variable kurzfristiges Kriterium
# innerhalb von 7 Monaten nach "Meilenstein" insgesamt 90 Tage in ungeförderter Beschäftigung stehend (1), sonst (0)
table(r_besch)
data$BESCHÄFTIGUNGSTAGE <- factor(r_besch, levels = c(0, 1), labels = c("<90 Tage", ">=90 Tage"), ordered = FALSE)
table(data$BESCHÄFTIGUNGSTAGE)


# AMS MODEL ####################################################################
library(caret) # Confusion Matrix
library(InformationValue) # Optimal cutoff threshold
# library(ISLR)

# Import other scripts (like data preperation)

# Split in Training and Test data ----------------------------------------------

# Train Original AMS logistic model with train-data ----------------------------
model_ams <- glm(BESCHÄFTIGUNGSTAGE ~ GESCHLECHT_WEIBLICH + STAATENGRUPPE + ALTERSGRUPPE 
                 + AUSBILDUNG + BETREUUNGSPFLICHTEN + RGS_TYP
                 + BEEINTRÄCHTIGT + BERUFSGRUPPE + BESCHÄFTIGUNGSVERLAUF
                 + GESCHÄFTSFALLDAUER + GESCHÄFTSFALLFREQ + MASSNAHMENTEILNAHME, 
                 family = "binomial", data = data)
summary(model_ams)
# What about missing values? -> Look at discriptives
# Beschäftigungsverlauf -> einzelne Jahre

# Predictions 
data$prob_ams <- predict(model_ams, data, type="response")

# Confusion Matrix -------------------------------------------------------------
data$class_pred <-factor(ifelse(data$prob_ams > 0.66, 1, 0))
table(data$class_pred)
data$truth <- factor(ifelse(data$BESCHÄFTIGUNGSTAGE == ">=90 Tage", 1, 0))
table(data$truth)

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(data$truth, data$class_pred)[1]

confusionMatrix(data$class_pred, data$truth)

#calculate sensitivity
sensitivity(data$truth, data$class_pred)
#calculate specificity
specificity(data$truth, data$class_pred)
#calculate total misclassification error rate
misClassError(data$truth, data$class_pred, threshold=optimal)

# Checking ROC
library(Epi)
ROC(form = BESCHÄFTIGUNGSTAGE ~ GESCHLECHT_WEIBLICH + ALTERSGRUPPE + STAATENGRUPPE 
    + AUSBILDUNG + BETREUUNGSPFLICHTEN + RGS_TYP
    + BEEINTRÄCHTIGT + BERUFSGRUPPE + BESCHÄFTIGUNGSVERLAUF
    + GESCHÄFTSFALLDAUER + GESCHÄFTSFALLFREQ + MASSNAHMENTEILNAHME, data=data,plot="ROC")

# Looking at variable effects
library(effects)
plot(allEffects(model1))

# Checking model assumptions
# 1. Normality of errors (qqplot)
plot(model1, which=2)
# 2. Constant variance (residual plots, tests)
plot(model1, which=3)
# 3. Independence of errors (sequence plot, DW test)
plot(rstudent(model1), type='l')
# 4. Outliers (leverage, Cooks distance)



# Train alternativ models ------------------------------------------------------
# Logistic regression model with "better" AMS variables
model_alt <- glm(BESCHÄFTIGUNGSTAGE ~ GESCHLECHT_WEIBLICH + ALTERSGRUPPE + STAATENGRUPPE 
             + AUSBILDUNG + BETREUUNGSPFLICHTEN_BOTH + RGS_TYP
             + BEEINTRÄCHTIGT + BERUFSGRUPPE_all + BESCHÄFTIGUNGSVERLAUF
             + GESCHÄFTSFALLDAUER + GESCHÄFTSFALLFREQ + MASSNAHMENTEILNAHME, 
             family = "binomial", data = data)
summary(model1)

# Logistic regression model with all variables step-procedure

# Restricted logistic regression model with all variables

# Other models
rpart::rpart()
randomForest::randomForest()
xgboost::xgboost()

# Predictions
predicted_prob_alt <- predict(model1, data, type="response")


# Performance and Fairness Measures ############################################
library(fairness)

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
