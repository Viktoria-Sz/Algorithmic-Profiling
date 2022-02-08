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
data$GESCHLECHT_WEIBLICH <- factor(female, levels = c("0", "1"), labels = c("male", "female"))
table(data$GESCHLECHT_WEIBLICH)

# Ausbildung
table(r_ausbildung) 
data$AUSBILDUNG <- factor(r_ausbildung, levels = c("L", "M", "P"), labels =  c("L", "M", "P"))
table(data$AUSBILDUNG )

table(ausb_t1)
table(eduhöchst4)
table(eduhöchst4_t1)

# Betreuungspflichten (nur für Frauen)
table(r_betreuungspflichten)
table(r_betreuungspflichten, kinder) 
table(r_betreuungspflichten, female)
table(kinder, female)
table(kinder)

data$BETREUUNGSPFLICHTEN_BOTH <-factor(kinder, levels = c("ja", "nein"), labels =  c("ja", "nein"))
data$BETREUUNGSPFLICHTEN <- ifelse(data$GESCHLECHT_WEIBLICH == "female" & data$BETREUUNGSPFLICHTEN_BOTH =="ja", "ja", "nein")
table(data$BETREUUNGSPFLICHTEN_BOTH)
table(data$BETREUUNGSPFLICHTEN)
table(data$BETREUUNGSPFLICHTEN, data$GESCHLECHT_WEIBLICH)
table(data$BETREUUNGSPFLICHTEN_BOTH, data$GESCHLECHT_WEIBLICH)

# RGS Typ
table(r_rgstyp)
data$RGS_TYP <-factor(r_rgstyp, levels = c("1", "2", "3", "4"), labels =  c("1", "2", "3", "4"), ordered = is.ordered(r_rgstyp))
table(data$RGS_TYP)
ordered(data$RGS_TYP)

# Beeinträchtigt
table(shealth)
table(lhealth) # wahrscheinlich beste Wahl
table(longill) 
data$BEEINTRÄCHTIGT_order <-factor(lhealth, levels = c("ja stark", "ja ein wenig", "nein"), 
                                   labels =  c("ja stark", "ja ein wenig", "nein"), ordered = is.ordered(r_rgstyp))
data$BEEINTRÄCHTIGT <-ifelse(data$BEEINTRÄCHTIGT_order == "ja stark" | data$BEEINTRÄCHTIGT_order == "ja ein wenig", "ja", "nein")

# Was machen mit den den "keine Angabe" Leuten?
table(data$BEEINTRÄCHTIGT_order)
table(data$BEEINTRÄCHTIGT_order, lhealth, useNA = "always")
ordered(data$BEEINTRÄCHTIGT_order)
table(data$BEEINTRÄCHTIGT)

# Berufsgruppe Produktion oder Service
table(r_berufsgruppe_ams1)
table(r_berufsgruppe)
table(r_berufsgruppe_ams1, r_berufsgruppe) # Was ist mit 9? Wird nicht berücksichtigt im AMS Methoden paper
data$BERUFSGRUPPE_all <- factor(r_berufsgruppe_ams1)
data$BERUFSGRUPPE <- factor(r_berufsgruppe)

# WICHTIG: Für junge Leute unter 25:
# Für diese Population werden die Merkmale STAATENGRUPPE, GESCHÄFTSFALLDAUER und BESCHÄFTIGUNGSVERLAUF nicht für die Schätzung verwendet.
# Beschäftigungsverlauf vor AL
table(r_beschverl_voral)
data$BESCHÄFTIGUNGSVERLAUF <- factor(r_beschverl_voral, levels = c(1, 2), labels = c(">75%", "<75%"))
table(data$BESCHÄFTIGUNGSVERLAUF)

table(r_monate_erw_j1voral, useNA = "always")
table(r_monate_erw_j2voral, useNA = "always")
table(r_monate_erw_j3voral, useNA = "always")
table(r_monate_erw_j4voral, useNA = "always")

# Geschäftsfalldauer
# 0 = kein Geschäftsfall mit Dauer >= 180 Tage; 1 = 1 oder mehrere Geschäftsfälle mit Dauer >= 180 Tage -> halbes Jahr
table(r_geschfalldau_voral)
table(r_geschfalldau3m_voral)
data$GESCHÄFTSFALLDAUER <- factor(r_geschfalldau_voral, levels = c(0, 1), labels = c("kein GF>=180", "GF>=180"))

table(r_geschaeftsfall_j1voral, useNA = "always")
table(r_geschaeftsfall_j2voral, useNA = "always")
table(r_geschaeftsfall_j3voral, useNA = "always")
table(r_geschaeftsfall_j4voral, useNA = "always")

# Geschäftsfallfrequenz
table(r_geschfallfreq_voral)
data$GESCHÄFTSFALLFREQ <- factor(r_geschfallfreq_voral)
data$GESCHÄFTSFALLFREQ_order <- factor(r_geschfallfreq_voral, levels = c(0, 1, 2, 3),  
                                       ordered = is.ordered(r_geschfallfreq_voral))
table(data$GESCHÄFTSFALLFREQ_order)
ordered(data$GESCHÄFTSFALLFREQ_order)

# Maßnahmenteilnahme
table(r_maßnahmenteilnahme)
data$MASSNAHMENTEILNAHME <- factor(r_maßnahmenteilnahme, levels = c(0, 1, 2, 3), 
                                   labels = c("kM", "min 1 unterst.", "min 1 qual", "min 1 Bförd"))
data$MASSNAHMENTEILNAHME_order <- factor(r_maßnahmenteilnahme, levels = c(0, 1, 2, 3), 
                                   labels = c("kM", "min 1 unterst.", "min 1 qual", "min 1 Bförd"),
                                   ordered = is.ordered(r_maßnahmenteilnahme))
table(data$MASSNAHMENTEILNAHME_order)

# Altersgruppe entfällt, da alle unter 30
# the characteristic AGE GROUP is redefined: less than 20 years (0), 20 to 24 years (20).
table(ageg)
data$ALTERSGRUPPE <- factor(ageg)
table(data$ALTERSGRUPPE)

# Staatengruppe
table(r_staatengruppe)  
data$STAATENGRUPPE <- factor(r_staatengruppe, levels = c("AUT", "DRITT", "EU"), labels =  c("AUT", "DRITT", "EU"))
table(data$STAATENGRUPPE)

table(birthAT)
table(birthAT_v)
table(birthAT_m)
table(mighint12g_new)

# Abhängige Variable kurzfristiges Kriterium
# innerhalb von 7 Monaten nach "Meilenstein" insgesamt 90 Tage in ungeförderter Beschäftigung stehend (1), sonst (0)
table(r_besch)
data$BESCHÄFTIGUNGSTAGE <- factor(r_besch, levels = c(0, 1), labels = c("<90 Tage", ">=90 Tage"))
table(data$BESCHÄFTIGUNGSTAGE)


# AMS Model --------------------------------------------------------------------
# Refernzkategorie ändern + was ist los mit RGS?
model1 <- glm(BESCHÄFTIGUNGSTAGE ~ GESCHLECHT_WEIBLICH + STAATENGRUPPE + ALTERSGRUPPE 
             + AUSBILDUNG + BETREUUNGSPFLICHTEN + RGS_TYP
             + BEEINTRÄCHTIGT + BERUFSGRUPPE + BESCHÄFTIGUNGSVERLAUF
             + GESCHÄFTSFALLDAUER + GESCHÄFTSFALLFREQ + MASSNAHMENTEILNAHME, 
             family = "binomial", data = data)
summary(model1)

# Alternative (ordered) Variablen
model_alt <- glm(BESCHÄFTIGUNGSTAGE ~ GESCHLECHT_WEIBLICH + ALTERSGRUPPE + STAATENGRUPPE 
             + AUSBILDUNG + BETREUUNGSPFLICHTEN_BOTH + RGS_TYP
             + BEEINTRÄCHTIGT + BERUFSGRUPPE_all + BESCHÄFTIGUNGSVERLAUF
             + GESCHÄFTSFALLDAUER + GESCHÄFTSFALLFREQ + MASSNAHMENTEILNAHME, 
             family = "binomial", data = data)
summary(model_alt)

predicted_prob1 <- predict(model1, data, type="response")
predicted_prob_alt <- predict(model_alt, data, type="response")

# Checking ROC
library(Epi)
ROC(form = BESCHÄFTIGUNGSTAGE ~ GESCHLECHT_WEIBLICH + ALTERSGRUPPE + STAATENGRUPPE 
    + AUSBILDUNG + BETREUUNGSPFLICHTEN + RGS_TYP
    + BEEINTRÄCHTIGT + BERUFSGRUPPE + BESCHÄFTIGUNGSVERLAUF
    + GESCHÄFTSFALLDAUER + GESCHÄFTSFALLFREQ + MASSNAHMENTEILNAHME, data=data,plot="ROC")

# Confusion Matrix
library(caret)
class_prediction <-factor(ifelse(predicted_prob > 0.66, 1, 0))
table(class_prediction)
truth <- factor(ifelse(data$BESCHÄFTIGUNGSTAGE == ">=90 Tage", 1, 0))
table(truth)
confusionMatrix(class_prediction, truth)

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


# Other models
rpart::rpart()
randomForest::randomForest()
xgboost::xgboost()
