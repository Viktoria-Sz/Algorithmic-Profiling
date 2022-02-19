# PREPARATTION #################################################################

# Libraries --------------------------------------------------------------------
library(tidyverse)
library(psych)
library(corrplot) # use corrplot(cor(...)) for nice correlation plots

# Load data --------------------------------------------------------------------
data <- readRDS("data/dataJuSAW.rds")

# Steps ========================================================================
# Steps for analysis
# • Analysis for each variable
# • Visualization for bivariate associations (e.g. Scatter‐plots & boxplots)
# • Correlations
# • Regression model
# • Covariate Selection
# • Checking for collinearity
# • Checking for diagnostics (residual analysis)


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
attach(data)

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
table(ams_t1) # ??
table(month_of_interview) # Monat erstes Interview?
table(month_of_interview_t1) # Monat zweites Interview ca. ein Jahr später

# Job ----------------------------------------------------------------------------------------------
# Job at second interview
table(job_t1)

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
table(firmsize_l_t1)
table(firmsize_t1)

# Why did it end?
table(endlastjob)
table(endlastjob_t1)
table(endreason) # Unterschied zu vorherigen vars?

# Unemployment experience
table(ALexp)
table(exp) # nachschauen -> Erwerbstätigkeit Lifetime Gesamtdauer?

# Next job -----------------------------------------------------------------------------------------
table(zusage)
table(zusage_t1)

# Job search
table(bewerbung_no)      
table(vorstell_no)
table(suche_specific)   
table(suchintens)



# Jobattributpräferenz -----------------------------------------------------------------------------
# Motivation
table(effortmot)
table(effortmot_t1)

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

table(jap_jobsec_t1)
table(jap_income_t1)
table(jap_career_t1)
table(jap_anerkennung_t1)
table(jap_freizeit_t1)
table(jap_independent_t1)
table(jap_creative_t1)
table(jap_selfdevel_t1)
table(jap_learnopp_t1)
table(jap_interest_t1)
table(jap_social_t1)
table(jap_help_t1)

# Work Attitudes
table(lottery)
table(lottery_t1)
table(leisurevalue)
table(familyvalue)
table(leisurevalue_t1)
table(familyvalue_t1)
table(lifesat)
table(lifesat_t1)
table(prefcat)

# Sinn von Arbeit: Geld verdienen, dazugehören, interessante Tätigkeiten ausführen
table(a_instrumental)
table(a_belong)
table(a_interest)

table(a_instrumental_t1)
table(a_belong_t1)
table(a_interest_t1)

# Reservationslohn?
table(reserve) # Vielleicht wie viel Geld Reserve? Reservationslohn?
table(reserveDK)

# Personality stuff  -------------------------------------------------------------------------------
# Selbstwert
table(sw_träumer) 
table(sw_wertlos)
table(sw_selbstzweifel)
table(sw_zukunftsangst)
table(sw_mitmirzufrieden)
table(sw_selbstwert) # nochmal nachschauen wieso so komische Zahlen rauskommen - zusammengeseztes Ktiterium?

table(sw_träumer_t1)
table(sw_wertlos_t1)
table(sw_selbstzweifel_t1)
table(sw_zukunftsangst_t1)
table(sw_mitmirzufrieden_t1)
table(sw_selbstwert_t1)

table(risk) # bedeutet, dass Sie sich als „gar nicht risikobereit“ einschätzen und 10 bedeutet, dass Sie sich als „sehr risikobereit“
table(trust)
table(risk_t1) 
table(trust_t1)

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

table(gewissenh) # Zusammengesetzes Kriterium?

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
table(depress) # Nochmal nachschauen wie die berechnet werden
table(depress_WHO)
table(depress10_WHO)
table(depressrisk_WHO)

table(dep_gelassen_t1)
table(dep_einsam_t1)
table(dep_ärgerlich_t1)
table(dep_niedergeschlagen_t1)
table(dep_glücklich_t1)
table(dep_nervös_t1)
table(dep_ängstlich_t1)
table(dep_traurig_t1)
table(dep_energie_t1)
table(depress_t1)
table(depress_WHO_t1)
table(depress10_WHO_t1)
table(depressrisk_WHO_t1)

depress_d

# Behavior -----------------------------------------------------------------------------------
table(alkohol)
table(alccut) # was war das nochmal?
table(alkohol_t1)
table(alccut_t1)
table(rauchen)
table(rauchen_t1)
table(schlaf) # Was bedeuten Auswahlmöglichkeiten?
table(sport)
table(ernährung)
table(fz_fernsehen_video)
table(fz_computerspiele)
table(fz_internetsurfen)
table(fz_musikhören)
table(fz_lesen)

table(schlaf_t1)
table(sport_t1)
table(ernährung_t1)
table(fz_fernsehen_video_t1)
table(fz_computerspiele_t1)
table(fz_internetsurfen_t1)
table(fz_musikhören_t1)
table(fz_lesen_t1)

# Time structure --------------------------------------------------------------------------------
# nochmal nachschauen was das jahoda zeug ist und was ist das p
table(jahoda_timestruc_aufst)
table(jahoda_timestruc_termine)
table(jahoda_timestruc_tag)
table(jahoda_timestruc_bett)
table(jahoda_timestruc)
table(jahoda_timestruc_aufst_p)
table(jahoda_timestruc_termine_p)
table(jahoda_timestruc_tag_p)
table(jahoda_timestruc_bett_p)
table(jahoda_timestruc_p)

# Social
table(jahoda_social_treff)
table(jahoda_social_einsam)
table(jahoda_social_kolleg)
table(jahoda_social)
table(jahoda_purpose_lang)
table(jahoda_social_treff_p)
table(jahoda_social_einsam_p)
table(jahoda_social_kolleg_p)
table(jahoda_social_p)

table(jahoda_purpose_zeit)
table(jahoda_purpose_lang_p)
table(jahoda_purpose_zeit_p)

table(jahoda_stigma_unan)
table(jahoda_stigma_sorg)
table(jahoda_stigma_unan_p)
table(jahoda_stigma_sorg_p)

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
table(socialmeet_t1)
table(socialcomp_t1)
table(freunde_t1)
table(soc_konflikt) # nachschauen
table(soc_versteh)

# Health
table(shealth) # Wie schätzen Sie Ihren allgemeinen Gesundheitszustand ein?
table(shealth_t1)
table(lhealth) # Werden Sie bei Ihren täglichen Aktivitäten in irgendeiner Weise von einer längeren Krankheit oder einer Behinderung beeinträchtigt?
table(lhealth_t1)
table(longill) # Kam es im letzten Jahr vor, dass Sie länger als 6 Wochen ununterbrochen krank waren?
table(longill_t1)

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
table(notede) # NOte?
table(notema)

table(elternschulint)
table(yrsedu)
table(edumore)
table(abbruch01)

# Ability ------------------------------------------------------------------------------------------
table(recall)
table(recall_t1)
table(rechnencorr1) 
table(rechnencorr2)
table(kast1corra)
table(kast2corra)
table(drecall)
table(drecall_t1)


# Characteristics ----------------------------------------------------------------------------------
# Age
table(ageg)

# Gender
table(female)

# Migration background
table(birthAT)
table(birthAT_v)
table(birthAT_m)
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

table(hhvater_t1)
table(hhstiefvater_t1)
table(hhmutter_t1)
table(hhstiefmutter_t1)
table(hhpartner_t1)
table(hhkinder_t1)
table(hhkinder, female, r_betreuungspflichten_t1)
table(hhkinderpartner_t1)
table(hhgeschwister_t1)
table(hhgroßeltern_t1)
table(hhwg_t1)
table(hhalleine_t1)
table(hhsonst_t1)
table(hhsize_t1)

table(tiere_no) # so viele Tiere??
table(tiere_no_t1)


# Frage??
table(fertint)
table(fertint_t1)

table(SDT)             
table(SDT_t1)
table(ges_status)

# Save dataset =================================================================
saveRDS(data, "data/JuSAW_prepared.rds")
