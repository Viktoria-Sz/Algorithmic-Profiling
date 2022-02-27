# PREPARATTION #################################################################

# Libraries --------------------------------------------------------------------
library(tidyverse)
library(psych)
library(corrplot) # use corrplot(cor(...)) for nice correlation plots

# Load data --------------------------------------------------------------------
data <- readRDS("data/dataJuSAW.rds")
attach(data)

# Steps ========================================================================
# Steps for analysis
# • Analysis for each variable
# • Visualization for bivariate associations (e.g. Scatter‐plots & boxplots)
# • Correlations
# • Regression model
# • Covariate Selection
# • Checking for collinearity
# • Checking for diagnostics (residual analysis)


# Plot functions ===============================================================
df_t0t1 <- function(data, variable, variable_t1){
  var <- enquo(variable)
  var_t1 <- enquo(variable_t1)
  
  var_name <- as_label(var)
  var_t1_name <- as_label(var_t1)
  
  df_t0t1 <- data %>%
    select(!!var, !!var_t1) %>%
    pivot_longer(c(expr(!!var_name), expr(!!var_t1_name)), names_to = "time", values_to= expr(!!var_name), values_drop_na = TRUE)
  
  df_t0t1$time <- factor(df_t0t1$time, levels = c(expr(!!var_name), expr(!!var_t1_name)), labels =  c("t0", "t1"), ordered = FALSE) %>%
    relevel(ref = "t0")
  
  return(df_t0t1)
}
# df_t0t1(data, lottery, lottery_t1)
# plot_prob(df_t0t1(data, lottery, lottery_t1), lottery)

plot_prob <- function(data, variable){
  var <- enquo(variable) # Variable verwendbar machen in ggplot
  
  ggplot(data, aes(x = time, fill = !!var)) +
    geom_bar(position = "fill") +
    theme_bw(base_size = 15)
}
# plot_prob(lottery_df, lottery) 

plot_count <- function(data, variable){
  var <- enquo(variable) # Variable verwendbar machen in ggplot
  
  ggplot(data, aes(x = !!var, fill = time)) +
    geom_bar(position = position_dodge(width = 0.5)) +
    theme_bw(base_size = 15)
}
# plot_count(lottery_df, lottery) 

# Exploring the data ===========================================================
str(data)

names(data)
summary(data)
describe(data)


numeric.only <-sapply(data,class)=='numeric'
describe(data[,numeric.only])

# data %>% 
#   skimr::skim(dest, carrier) 


# Variable exploration =========================================================

# Klarstellungen
# Codebuch für survey Variablen
# t0 Zeitpunkt erstes Interview -> month
# t1 Zeitpunkt zweites Interview -> ein Jahr später month?
# Was bedeutet l?
# Nur r_ Variablen sind aus den Registerdaten?
# r_betreuungspflichten registerdaten weniger als kinder


# To Dos
# AMS Model nachbilden
# Relative Angaben für t0-t1 Unterschiede - bzw. Veränderungen irgendwie darstellen
# Korrelationsmatrix mit Job?


# General ------------------------------------------------------------------------------------------
case
table(ams_t1) # Derzeitiger Status: beim AMS gemeldet?
table(month_of_interview) # Monat erstes Interview?
table(month_of_interview_t1) # Monat zweites Interview ca. ein Jahr später

# Job ----------------------------------------------------------------------------------------------
# Job at second interview
table(job_t1)
table(r_besch, job_t1) # Kombination aus beidem für eine neue Variable?

# How many were employed before/ for at least 3 months?
table(job3) # 3 Monate Job gehabt?
table(dauer_längster) # Keine 249 Personen -> was sind das für Kategorien? In welcher Einheit ist das?

# What job was the last job and what contract?
table(statuslastjob)
table(angest_level_l) 
table(beruf_letzt_isco1_t0)
table(beruf_letzt_isco1_t1) # Ist hiermit ein Job zwischen t0 und t1 gemeint, der aber schon wieder beendet ist?
table(beruf_isco1_t1) # Unterschied zu vorheriger Variable?

table(match)
table(match_l_t1)
table(match_t1)

table(jobsat)
table(jobsat_l_t1)
table(jobsat_t1)

table(vertrag)
table(vertrag_l_t1)
table(vertrag_t1)

table(firmsize)

# Why did it end?
table(endlastjob)
table(endlastjob_t1)
table(endreason) # Zusammenfassung

# Unemployment experience
table(ALexp)
table(exp) #  Erwerbstätigkeit Lifetime Gesamtdauer - Summe aus Jahren und Monaten Erfahrung - in Jahren

# Next job -----------------------------------------------------------------------------------------
table(zusage) #!!! Genauer analysiern, eventuell alle mit Zusage rausnehmen
table(zusage_t1)

# Job search
table(bewerbung_no)      
table(vorstell_no)
table(suche_specific)   
table(suchintens)


# Jobattributpräferenz -----------------------------------------------------------------------------
# Motivation
table(effortmot)

table(jap_jobsec)
table(jap_income)
table(jap_career)
table(jap_anerkennung)
table(jap_freizeit)
table(jap_independent)
table(jap_creative)
table(jap_selfdevel)
table(jap_learnopp)
table(jap_interest)
table(jap_social)
table(jap_help)

data$intrins <- (jap_selfdevel +jap_learnopp +jap_independent+ jap_creative +jap_interest)/5
data$extrins <- (jap_jobsec+jap_income +jap_career + jap_anerkennung)/4 #values 1-4
table(data$intrins)

# Work Attitudes
table(lottery)
table(leisurevalue)
table(familyvalue)
table(lifesat)
table(prefcat)

table(fertint) # Haben Sie vor, in den nächsten drei Jahren ein (weiteres) Kind zu bekommen?
table(fertint_t1)

# Sinn von Arbeit: Geld verdienen, dazugehören, interessante Tätigkeiten ausführen
table(a_instrumental)
table(a_belong)
table(a_interest)

# Reservationslohn (Nettoverdienst)
table(reserve) 
table(reserveDK) # Don't know

# Personality stuff  -------------------------------------------------------------------------------
# Selbstwert
table(sw_träumer) 
table(sw_wertlos)
table(sw_selbstzweifel)
table(sw_zukunftsangst)
table(sw_mitmirzufrieden)
table(sw_selbstwert) # nochmal nachschauen wieso so komische Zahlen rauskommen - zusammengeseztes Ktiterium?

table(risk) # Risikobereitschaft
table(trust)

# Big Five Personality
table(p_introvertiert) # Was ist 1 und was ist 5?
table(p_trust)
table(p_faul)
table(p_laidback)
table(p_goaloriented)
table(p_noculture) # was war das?
table(p_extravert)
table(p_sorgen)
table(p_kritisieren)
table(p_gründlich)
table(p_insecure)
table(p_fantasie)
table(p_tüchtig)

table(gewissenh) # p_tüchtig + p_gründlich + p_goaloriented)/3

# locus of control
table(locus_self)
table(locus_luck)
table(locus_work)
table(locus_selbstzweifel)
table(locus_nocontrol)

# Depression
table(dep_gelassen)
table(dep_einsam)
table(dep_ärgerlich)
table(dep_niedergeschlagen)
table(dep_glücklich)
table(dep_nervös)
table(dep_ängstlich)
table(dep_traurig)
table(dep_energie)
table(depress) # Summenindex ((6-dep_gelassen)+dep_einsam+dep_ärgerlich+dep_niedergeschlagen+(6-dep_glücklich)+dep_nervös+dep_ängstlich+dep_traurig+(6-dep_energie)) divided by 9 - 1 to 5
table(depress_WHO) # Sumenindex wie depress, auf einer anderen Skala 0-36
table(depress10_WHO) # 0 to 10
table(depressrisk_WHO) # Indikator für Risiko wenn depress_WHO >18

depress_d # Differenz Summenindex Welle2-Welle 1, N=584 nur vollständige Angaben

# Behavior -----------------------------------------------------------------------------------
table(alkohol)
table(alccut) # Haben Sie schon einmal das Gefühl gehabt, dass Sie Ihren Alkoholkonsum verringern sollten?
table(rauchen)
table(schlaf) # Was bedeuten Auswahlmöglichkeiten?
table(sport)
table(ernährung)
table(fz_fernsehen_video)
table(fz_computerspiele)
table(fz_internetsurfen)
table(fz_musikhören)
table(fz_lesen)

# Time structure --------------------------------------------------------------------------------
# nochmal nachschauen was das jahoda zeug ist und was ist das p - p heißt vergangen
table(jahoda_timestruc_aufst)
table(jahoda_timestruc_termine)
table(jahoda_timestruc_tag)
table(jahoda_timestruc_bett)
table(jahoda_timestruc)

# Social
table(jahoda_social_treff)
table(jahoda_social_einsam)
table(jahoda_social_kolleg)
table(jahoda_social)
table(jahoda_purpose_lang)

table(jahoda_purpose_zeit)

table(jahoda_stigma_unan)
table(jahoda_stigma_sorg)

table(jahoda_depress_down_p)
table(jahoda_depress_interest_p)
table(jahoda_depress_p)

# Finanzen
table(jahoda_finanz1_p)
table(jahoda_finanz2_p)
table(jahoda_finanz_p)

# Social
table(socialmeet)
table(socialcomp) # In Relation zu was?
table(freunde)
table(soc_konflikt) # nachschauen
table(soc_versteh)

# Health
table(shealth) # Wie schätzen Sie Ihren allgemeinen Gesundheitszustand ein?
table(lhealth) # Werden Sie bei Ihren täglichen Aktivitäten in irgendeiner Weise von einer längeren Krankheit oder einer Behinderung beeinträchtigt?
table(longill) # Kam es im letzten Jahr vor, dass Sie länger als 6 Wochen ununterbrochen krank waren?

# Finances
table(finanzgut)
table(finanzschlecht)
table(finanzgut_t1)
table(finanzschlecht_t1)
table(efinanzgut)
table(efinanzschlecht)
table(e_unterstütz)
table(e_unterstütz_t1)

# Education ----------------------------------------------------------------------------------------
table(ausb_t1)
table(eduhöchst4)
table(eduhöchst4_t1)
table(notede) # Schulnote letztes Zeugnis: Deutsch
table(notema) # Schulnote letztes Zeugnis: Mathematik

table(elternschulint)
table(yrsedu)
table(edumore)
table(abbruch01)

# Ability ------------------------------------------------------------------------------------------
table(SDT)  # Symbol Zahlen Test           
table(tiere_no) 
table(recall)
table(rechnencorr1) 
table(rechnencorr2)
table(kast1corra)
table(kast2corra)
table(drecall)
# Ability Variable Zusammenfassung?

# Characteristics ----------------------------------------------------------------------------------
# Age
table(ageg)

# Gender
table(female)

# Migration background
table(birthAT)
table(birthAT_v) # Vater: Geburtsland Österreich
table(birthAT_m) # Mutter: Geburtsland Österreich
table(mighint12g_new)
table(migklasse)
table(schuleat) # nachschauen
table(deutsch5)
table(deutschpex) # nachschauen

# Religion
table(relig)
table(relig2) # Wie wichtig ist Religion

# Discrimination
table(diskrim)
table(diskrim_nat)
table(diskrim_rel)
table(diskrim_lan)
table(diskrim_col)
table(diskrim_age)
table(diskrim_sex)
table(diskrim_phy)
table(diskrim_other)

table(ges_status) # Gesellschaftlicher Status - Position in Hierarchie


# Relationships -------------------------------------------------------------------------------
# Relationship
table(beziehung)
table(beziehung_t1)

# Children
table(kinder) 

# Siblings
table(geschwist)

# Parents
table(edudad)
table(dadwork)
table(dadberuf)
table(edumum)
table(mumwork)
table(mumberuf)
table(varbeitgerne)
table(varbeitwichtig)
table(marbeitgerne)
table(marbeitwichtig)
table(arbeitswerte_elt)
table(mobilityv)
table(mobilitym)
table(mobility_both)
table(mobility_both2)

# Who's living in the same household
# Gibts auch alles mit t1
table(hhvater)
table(hhstiefvater)
table(hhmutter)
table(hhstiefmutter)
table(hhpartner)
table(hhkinder)
table(hhkinder, female, r_betreuungspflichten)
table(hhkinderpartner)
table(hhgeschwister)
table(hhgroßeltern)
table(hhwg)
table(hhalleine)
table(hhsonst)
table(hhsize)


# Save dataset =================================================================
saveRDS(data, "data/JuSAW_prepared.rds")


# t0-t1 Plot Barplott ##########################################################
table(lottery)
table(lottery_t1)

lottery_df <- data %>%
  select(lottery, lottery_t1) %>%
  pivot_longer(c("lottery", "lottery_t1"), names_to = "time", values_to="lottery", values_drop_na = TRUE)
lottery_df$time <- factor(lottery_df$time, levels = c("lottery", "lottery_t1"), labels =  c("t0", "t1"), ordered = FALSE) %>%
  relevel(ref = "t0")

ggplot(lottery_df, aes(x = lottery, fill = time)) +
  geom_bar(position = position_dodge(width = 0.5)) +
  #scale_alpha_discrete(range = c(0.5,1)) +
  theme_bw(base_size = 15)

ggplot(lottery_df, aes(x = time, fill = lottery)) +
  geom_bar(position = "fill") +
  theme_bw(base_size = 15)

