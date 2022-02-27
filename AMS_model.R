# AMS MODEL REBUILD ############################################################

# Libraries --------------------------------------------------------------------
library(tidyverse)
library(modelr)
options(na.action = na.warn)
options(scipen=100) # prevent scientific notion (e)

# Load data --------------------------------------------------------------------
data <- readRDS("data/dataJuSAW.rds")
attach(data)

# Data preparation for AMS -----------------------------------------------------

# Variables for AMAS
# GESCHLECHT_WEIBLICH: Geschlecht weiblich, mit 0= männlich, 1=weiblich
table(r_geschlecht)
table(female, r_geschlecht) # lieber female verwenden, hat weniger missings
table(female)
data$GENDER_female <- factor(female, levels = c("0", "1"), labels = c("male", "female"), ordered = FALSE)
table(data$GENDER_female)

# AUSBILDUNG: Ausbildung
table(r_ausbildung) 
data$EDUCATION <- factor(r_ausbildung, levels = c("L", "M", "P"), labels =  c("L", "M", "P"), ordered = FALSE)
data$EDUCATION <- relevel(data$EDUCATION, ref = "P")
table(data$EDUCATION)

table(ausb_t1)
table(eduhöchst4)
table(eduhöchst4_t1)

# Betreuungspflichten (nur für Frauen)
table(r_betreuungspflichten)
table(r_betreuungspflichten, kinder) 
table(r_betreuungspflichten, female)
table(kinder, female)
table(kinder)

data$CHILDCARE_both <-factor(kinder, levels = c("ja", "nein"), labels =  c("ja", "nein"), ordered = FALSE)
data$CHILDCARE <- ifelse(data$GENDER_female == "female" & data$CHILDCARE_both =="ja", "ja", "nein")
data$CHILDCARE <- factor(data$CHILDCARE, ordered = FALSE)
data$CHILDCARE_both <- relevel(data$CHILDCARE_both, ref = "nein")
data$CHILDCARE <- relevel(data$CHILDCARE, ref = "nein")
table(data$CHILDCARE_both)
table(data$CHILDCARE)
table(data$CHILDCARE, data$GENDER_female)
table(data$CHILDCARE_both, data$GENDER_female)

# RGS Typ
table(r_rgstyp)
data$RGS <-factor(r_rgstyp, levels = c("1", "2", "3", "4"), labels =  c("1", "2", "3", "4"), ordered = FALSE)
is.ordered(data$RGS)
table(data$RGS)

# Beeinträchtigt
table(shealth)
table(lhealth) # wahrscheinlich beste Wahl
table(longill) 
data$IMPAIRMENT_order <-factor(lhealth, levels = c("ja stark", "ja ein wenig", "nein"), 
                                   labels =  c("yes, very", "yes, a little", "no"), ordered =  TRUE)
data$IMPAIRMENT <-ifelse(data$IMPAIRMENT_order == "yes, very" | data$IMPAIRMENT_order == "yes, a little", "yes", "no")
data$IMPAIRMENT <- factor(data$IMPAIRMENT, ordered = FALSE)
data$IMPAIRMENT <- relevel(data$IMPAIRMENT, ref = "no")
is.ordered(data$IMPAIRMENT_order)
is.ordered(data$IMPAIRMENT)

# Was machen mit den den "keine Angabe" Leuten?
table(data$IMPAIRMENT_order)
table(data$IMPAIRMENT_order, lhealth, useNA = "always")
ordered(data$IMPAIRMENT_order)
table(data$IMPAIRMENT)

# Berufsgruppe Produktion oder Service
table(r_berufsgruppe_ams1)
table(r_berufsgruppe)
table(r_berufsgruppe_ams1, r_berufsgruppe) # Was ist mit 9? Wird nicht berücksichtigt im AMS Methoden paper
data$OCCUPATIONGROUP_all <- factor(r_berufsgruppe_ams1, ordered = FALSE)
data$OCCUPATIONGROUP <- factor(r_berufsgruppe, ordered = FALSE)

# WICHTIG: Für junge Leute unter 25:
# Für diese Population werden die Merkmale STAATENGRUPPE, GESCHÄFTSFALLDAUER und BESCHÄFTIGUNGSVERLAUF nicht für die Schätzung verwendet.
# BESCHÄFTIGUNGSVERLAUF: Beschäftigungsverlauf vor AL
table(r_beschverl_voral)
data$EMPLOYMENT <- factor(r_beschverl_voral, levels = c(1, 2), labels = c(">75%", "<75%"), ordered = FALSE)
table(data$EMPLOYMENT)

table(r_monate_erw_j1voral, useNA = "always")
table(r_monate_erw_j2voral, useNA = "always")
table(r_monate_erw_j3voral, useNA = "always")
table(r_monate_erw_j4voral, useNA = "always")

# Geschäftsfalldauer
# 0 = kein Geschäftsfall mit Dauer >= 180 Tage; 1 = 1 oder mehrere Geschäftsfälle mit Dauer >= 180 Tage -> halbes Jahr
table(r_geschfalldau_voral)
table(r_geschfalldau3m_voral)
data$BUSINESSCASEDUR <- factor(r_geschfalldau_voral, levels = c(0, 1), labels = c("kein GF>=180", "GF>=180"), ordered = FALSE)

table(r_geschaeftsfall_j1voral, useNA = "always")
table(r_geschaeftsfall_j2voral, useNA = "always")
table(r_geschaeftsfall_j3voral, useNA = "always")
table(r_geschaeftsfall_j4voral, useNA = "always")

# Geschäftsfallfrequenz
table(r_geschfallfreq_voral)
data$BUSINESSCASEFREQ <- factor(r_geschfallfreq_voral)
data$BUSINESSCASEFREQ_order <- factor(r_geschfallfreq_voral, levels = c(0, 1, 2, 3),  ordered = TRUE)
table(data$BUSINESSCASEFREQ_order)
is.ordered(data$BUSINESSCASEFREQ_order)

# Maßnahmenteilnahme
table(r_maßnahmenteilnahme)
data$SUPPORTMEASURE <- factor(r_maßnahmenteilnahme, levels = c(0, 1, 2, 3), 
                                   labels = c("kM", "min 1 unterst.", "min 1 qual", "min 1 Bförd"), ordered = FALSE)
data$SUPPORTMEASURE_order <- factor(data$SUPPORTMEASURE, ordered = TRUE)
table(data$SUPPORTMEASURE_order)
is.ordered(data$SUPPORTMEASURE_order)

# Altersgruppe entfällt, da alle unter 30
# the characteristic AGE GROUP is redefined: less than 20 years (0), 20 to 24 years (20).
table(ageg)
data$AGEGROUP <- factor(ageg)
table(data$AGEGROUP)

# Staatengruppe
table(r_staatengruppe)  
data$STATEGROUP <- factor(r_staatengruppe, levels = c("AUT", "DRITT", "EU"), labels =  c("AUT", "DRITT", "EU"), ordered = FALSE)
table(data$STATEGROUP)

table(birthAT)
table(birthAT_v)
table(birthAT_m)
table(mighint12g_new)

# Abhängige Variable kurzfristiges Kriterium
# innerhalb von 7 Monaten nach "Meilenstein" insgesamt 90 Tage in ungeförderter Beschäftigung stehend (1), sonst (0)
table(r_besch)
data$EMPLOYMENTDAYS <- factor(r_besch, levels = c(0, 1), labels = c("<90 Tage", ">=90 Tage"), ordered = FALSE)
table(data$EMPLOYMENTDAYS)
data$truth <- ifelse(data$EMPLOYMENTDAYS == ">=90 Tage", 1, 0)
table(data$truth)

# Save dataset =================================================================
saveRDS(data, "data/JuSAW_prepared.rds")

# AMS MODEL ####################################################################
library(caret) # Confusion Matrix
library(InformationValue) # Optimal cutoff threshold
# library(ISLR)

# Import other scripts (like data preperation)

# Split in Training and Test data ----------------------------------------------

# Train Original AMS logistic model with train-data ----------------------------
model_ams <- glm(EMPLOYMENTDAYS ~ GENDER_female + STATEGROUP + AGEGROUP 
                 + EDUCATION + CHILDCARE + RGS
                 + IMPAIRMENT + OCCUPATIONGROUP + EMPLOYMENT
                 + BUSINESSCASEDUR + BUSINESSCASEFREQ + SUPPORTMEASURE, 
                 family = "binomial", data = data)
summary(model_ams)
# What about missing values? -> Look at discriptives
# Beschäftigungsverlauf -> einzelne Jahre

# Predictions 
data$prob_ams <- predict(model_ams, data, type="response")

# Confusion Matrix -------------------------------------------------------------
data$class_pred <-ifelse(data$prob_ams > 0.66, 1, 0)
table(data$class_pred)
data$truth <- ifelse(data$EMPLOYMENTDAYS == ">=90 Tage", 1, 0)
table(data$truth)

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(data$truth, data$prob_ams, optimiseFor = "misclasserror")[1]

# Confusion matrix
confusionMatrix(data$class_pred, data$truth)

#calculate sensitivity, specificity, total misclassification error rate
sensitivity(data$truth, data$class_pred)
specificity(data$truth, data$class_pred)
misClassError(data$truth, data$class_pred, threshold=0.66)

# Checking ROC
library(Epi)
ROC(form = EMPLOYMENTDAYS ~ GENDER_female + AGEGROUP + STATEGROUP 
    + EDUCATION + CHILDCARE + RGS
    + IMPAIRMENT + OCCUPATIONGROUP + EMPLOYMENT
    + BUSINESSCASEDUR + BUSINESSCASEFREQ + SUPPORTMEASURE, data=data,plot="ROC")

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
model_exp <- glm(EMPLOYMENTDAYS ~ GENDER_female + AGEGROUP + STATEGROUP 
                 + EDUCATION + CHILDCARE_both + RGS
                 + IMPAIRMENT + OCCUPATIONGROUP_all + EMPLOYMENT
                 + BUSINESSCASEDUR + BUSINESSCASEFREQ + SUPPORTMEASURE, 
                 family = "binomial", data = data)
summary(model_exp)


# Predictions
predicted_prob_alt <- predict(model_exp, data, type="response")

# Odds Ratios from AMS documentation ###########################################
# Kurzfristiges Kriterium: Partiell valide schätzbare jugendliche Population
model_ams_OR_youth <- glm(EMPLOYMENTDAYS ~ GENDER_female + AGEGROUP 
                 + EDUCATION + CHILDCARE + RGS
                 + IMPAIRMENT + OCCUPATIONGROUP 
                 + BUSINESSCASEFREQ + SUPPORTMEASURE, 
                 family = "binomial", data = data)
summary(model_ams_OR_youth)
OR_coefs_youth <- c(-0.13, 0.09, 0.13, 0.0, 0.48, 0.40, -0.46, -0.10, -0.17, -0.15, 
              -0.36, -0.03, 0.01, -0.02, -0.06, -0.21, -0.14)
model_ams_OR_youth$coefficients <- OR_coefs_youth

# Predictions 
data$prob_ams_OR_youth <- predict(model_ams_OR_youth, data, type="response")

data$class_pred_OR_youth <-ifelse(data$prob_ams_OR_youth > 0.66, 1, 0)
table(data$class_pred_OR_youth)


# Kurzfristiges Kriterium: Voll valide shcätzbare Population
model_ams_OR <- glm(EMPLOYMENTDAYS ~ GENDER_female + AGEGROUP + STATEGROUP
                    + EDUCATION + CHILDCARE + RGS + IMPAIRMENT 
                    + OCCUPATIONGROUP + EMPLOYMENT
                    + BUSINESSCASEFREQ + BUSINESSCASEDUR + SUPPORTMEASURE, 
                    family = "binomial", data = data)
summary(model_ams_OR)
OR_coefs <- c(0.48, -0.05, 0.0, 0.0, -0.01, 0.04, 0.10, 0.03, -0.08, -0.10, 
              -0.16, -0.10, -0.33, 0.07, -0.31, 0.20, 0.36, 0.63, -0.21, -0.19, -0.03)
model_ams_OR$coefficients <- OR_coefs

# Predictions 
data$prob_ams_OR <- predict(model_ams_OR, data, type="response")

data$class_pred_OR <-ifelse(data$prob_ams_OR > 0.66, 1, 0)
table(data$class_pred_OR)
