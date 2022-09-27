# VARIABLE SETS ########################################################################################################
library(janitor)
# AMS ==================================================================================================================
# Original AMS variables -----------------------------------------------------------------------------------------------
# WICHTIG: Für junge Leute unter 25:
# Für diese Population werden die Merkmale STAATENGRUPPE, GESCHÄFTSFALLDAUER und BESCHÄFTIGUNGSVERLAUF nicht für die Schätzung verwendet.
ams_youth <- c("EMPLOYMENTDAYS", "GENDER", "AGEGROUP", "EDUCATION", "CHILDCARE", "RGS", 
               "IMPAIRMENT_strong", "OCCUPATIONGROUP", "BUSINESSCASEFREQ", "SUPPORTMEASURE_order")
ams_youth <- janitor::make_clean_names(ams_youth)

ams <- c("EMPLOYMENTDAYS", "GENDER", "AGEGROUP", "STATEGROUP", "EDUCATION", "CHILDCARE", "RGS", 
         "IMPAIRMENT_strong", "OCCUPATIONGROUP", "EMPLOYMENTHIST", "BUSINESSCASEFREQ", 
         "BUSINESSCASEDUR", "SUPPORTMEASURE_order")
ams <- janitor::make_clean_names(ams)

# Expanded AMS Variables -----------------------------------------------------------------------------------------------
ams_ext <- c("EMPLOYMENTDAYS", "GENDER", "AGEGROUP", "STATEGROUP", "EDUCATION", "CHILDCARE_both", "RGS", 
             "IMPAIRMENT_order", "OCCUPATIONGROUP_all",  #"EMPLOYMENTHIST",
             "r_monate_erw_j1voral", "r_monate_erw_j2voral", "r_monate_erw_j3voral", "r_monate_erw_j4voral",
             "BUSINESSCASEFREQ_order" , "BUSINESSCASEDUR", "SUPPORTMEASURE_order"
              )

ams_ext <- janitor::make_clean_names(ams_ext)


# Explorative ==========================================================================================================
# All useful vars ------------------------------------------------------------------------------------------------------
all = c("EMPLOYMENTDAYS", "GENDER", "AGEGROUP", "CHILDCARE", "CHILDCARE_both", "IMPAIRMENT", "IMPAIRMENT_order",
        "STATEGROUP", "RGS", "OCCUPATIONGROUP", "OCCUPATIONGROUP_all", "EDUCATION",
         "EMPLOYMENTHIST", "r_monate_erw_j1voral","r_monate_erw_j2voral", "r_monate_erw_j3voral", "r_monate_erw_j4voral",
        "BUSINESSCASEDUR", "BUSINESSCASEFREQ_order", "SUPPORTMEASURE_order",
        "migklasse", "schuleat", "mighint12g_new",
        "notede", "notema", "elternschulint", "edumore", #"abbruch01",
        "SDT", "tiere_no", "recall", "drecall", "rechnencorr1", "rechnencorr2", "kast1corra", "kast2corra", "ability",
        "ALexp", "exp", "statuslastjob", "zusage", "effortmot", "prefcat",
        "jap_jobsec", "jap_income", "jap_career", "jap_anerkennung", "jap_freizeit", "jap_independent", 
        "jap_creative", "jap_selfdevel", "jap_learnopp", "jap_interest", "jap_social", "jap_help", 
        "intrins", "extrins", "intrins_min_extrins",
        "lottery", "leisurevalue", "familyvalue", "lifesat", "fertint", "a_instrumental", "a_belong", "a_interest", "reserve",
        "sw_träumer", "sw_wertlos", "sw_selbstzweifel", "sw_zukunftsangst", "sw_mitmirzufrieden", "sw_selbstwert", "risk", "trust",
        "p_introvertiert", "p_trust", "p_faul", "p_laidback", "p_goaloriented", "p_noculture", "p_extravert", 
        "p_sorgen", "p_kritisieren", "p_gründlich", "p_insecure", "p_fantasie", "p_tüchtig", 
        "gewissenh", #-> # p_tüchtig + p_gründlich + p_goaloriented)/3
        "locus_self", "locus_luck", "locus_work", "locus_selbstzweifel", "locus_nocontrol",
        "dep_gelassen", "dep_einsam", "dep_ärgerlich", "dep_niedergeschlagen", "dep_glücklich", "dep_nervös", 
        "dep_ängstlich", "dep_traurig", "dep_energie", 
        "depress", # Summenindex ((6-dep_gelassen)+dep_einsam+dep_ärgerlich+dep_niedergeschlagen+(6-dep_glücklich)+dep_nervös+dep_ängstlich+dep_traurig+(6-dep_energie)) divided by 9 - 1 to 5
        "depressrisk_WHO",  # Sumenindex wie depress, auf einer anderen Skala 0-36
        #-> depress10_WHO # 0-10
        "alkohol", "rauchen", "schlaf", "sport", "ernährung", "fz_fernsehen_video", "fz_computerspiele", 
        "fz_internetsurfen", "fz_musikhören", "fz_lesen",
        "socialmeet", "socialcomp", "freunde", "beziehung", "geschwist", "hhalleine",
        #"finanzgut", "finanzschlecht", "efinanzgut", "efinanzschlecht",
        "edudad", "dadwork", "edumum", "mumwork", "arbeitswerte_elt0",  "e_unterstütz")
        #"varbeitgerne", "varbeitwichtig",
        #"marbeitgerne", "marbeitwichtig", 

all <- janitor::make_clean_names(all)

# Green Variables ------------------------------------------------------------------------------------------------------
green_big <- c(ams, "zusage", "match", "jobsat", "vertrag", "endreason", "ALexp", "exp",
               "intrins", "extrins", "lottery", "lifesat", "prefcat", "endreason01",
               "a_instrumental", "a_belong", "a_interest", "reserve", 
               "sw_träumer", "sw_wertlos", "sw_selbstzweifel", "sw_zukunftsangst",
               "sw_mitmirzufrieden", "sw_selbstwert", "risk", "trust", 
               "p_introvertiert", "p_trust", "p_faul", "p_laidback", "p_goaloriented",
               "p_noculture", "p_extravert", "p_sorgen", "p_kritisieren", 
               "p_gründlich", "p_insecure", "p_fantasie", "p_tüchtig", "gewissenh",
               "locus_self", "locus_luck", "locus_work", "locus_selbstzweifel", "locus_nocontrol",
               "depressrisk_WHO", "alkohol", "rauchen", "ernährung", 
               "fz_computerspiele", "fz_lesen", "fz_internetsurfen",
               "socialcomp", "shealth", "finanzgut", "e_unterstütz",
               "notede", "notema", "dadwork", "mumwork",
               "SDT", "recall", "tiere_no", "rechnencorr1", "rechnencorr2", 
               "kast1corra", "kast2corra", "drecall", "relig_islam", "ability"
)
green_big <- janitor::make_clean_names(green_big)

green <- c(ams, "zusage", "a_lexp",
           "intrins", "extrins", "prefcat", "a_instrumental", "trust",
           "reserve", "sw_selbstwert", "gewissenh",
           "depressrisk_WHO", "alkohol", "socialcomp", 
           "SDT", "recall" , "relig_islam", "ability"
)
green <- janitor::make_clean_names(green)

diverse <- c(ams, "zusage", "a_lexp", "prefcat", 
             "intrins", "extrins",  "a_instrumental", 
             "notede", "notema", "ability",
              "sw_selbstwert", "gewissenh", "trust", "risk",
             "depressrisk_WHO", "alkohol", "socialcomp",
             "edudad", "dadwork", "edumum", "mumwork"
              
)
diverse <- janitor::make_clean_names(diverse)

# Attitudes/Character variable set =====================================================================================
characteristics = c("jap_jobsec", "jap_income", "jap_career", "jap_anerkennung", "jap_freizeit", "jap_independent",
                 "jap_creative", "jap_selfdevel", "jap_learnopp","jap_interest","jap_social", "jap_help",
                 "intrins","extrins","intrins_min_extrins","effortmot","lottery","leisurevalue","familyvalue",
                 "lifesat","a_instrumental", "a_belong", "a_interest", "sw_träumer", "sw_wertlos", "sw_selbstzweifel",
                 "sw_zukunftsangst","sw_mitmirzufrieden", "sw_selbstwert", "risk", "trust",
                 "dep_gelassen",  "dep_einsam", "dep_ärgerlich", "dep_niedergeschlagen", "dep_glücklich", "dep_nervös",
                 "dep_ängstlich", "dep_traurig",  "dep_energie", "depress", "depressrisk_WHO",  
                 "p_introvertiert", "p_trust", "p_faul",  "p_laidback",  "p_goaloriented", "p_noculture",
                 "p_extravert", "p_sorgen", "p_kritisieren", "p_gründlich", "p_insecure", "p_fantasie", "p_tüchtig",
                 "gewissenh", "locus_self", "locus_luck", "locus_work", "locus_selbstzweifel", "locus_nocontrol",
                 "alkohol",  "rauchen", "schlaf", "sport", "ernährung",
                 "fz_fernsehen_video", "fz_computerspiele", "fz_internetsurfen", "fz_musikhören", "fz_lesen"
)
characteristics <- janitor::make_clean_names(characteristics)

# attitudes variable set ===============================================================================================
attitudes = c("jap_jobsec", "jap_income", "jap_career", "jap_anerkennung", "jap_freizeit", "jap_independent",
              "jap_creative", "jap_selfdevel", "jap_learnopp","jap_interest","jap_social", "jap_help",
              "intrins","extrins","intrins_min_extrins","effortmot","lottery","leisurevalue","familyvalue",
              "lifesat","a_instrumental", "a_belong", "a_interest")
attitudes <- janitor::make_clean_names(attitudes)

# personality variable set ===============================================================================================
personality = c("sw_selbstwert", "risk", "trust", "depressrisk_WHO",  
                "p_introvertiert", "p_trust", "p_faul",  "p_laidback",  "p_goaloriented", "p_noculture",
                "p_extravert", "p_sorgen", "p_kritisieren", "p_gründlich", "p_insecure", "p_fantasie", "p_tüchtig",
                "gewissenh", "locus_self", "locus_luck", "locus_work", "locus_selbstzweifel", "locus_nocontrol")
personality <- janitor::make_clean_names(personality)

# behavior variable set ===============================================================================================
behavior = c("alkohol",  "rauchen", "schlaf",  "sport",  "ernährung",
             "fz_fernsehen_video", "fz_computerspiele", "fz_internetsurfen", "fz_musikhören", "fz_lesen",
             "socialmeet", "socialcomp"
)
behavior <- janitor::make_clean_names(behavior)

# Variable sets other paper ============================================================================================
otherPES <- c("AGEGROUP", "BUSINESSCASEDUR", "BUSINESSCASEFREQ_order", "CHILDCARE", "beziehung", "hhalleine",
              "EDUCATION", "EMPLOYMENTHIST", "endreason01", "GENDER", "IMPAIRMENT",
              "intrins", "extrins", "sw_selbstwert", "leisurevalue", "locus_luck", "locus_nocontrol",
              "locus_selbstzweifel", "locus_self", "locus_work",
              "STATEGROUP", "notede", "notema", "ability",
              "OCCUPATIONGROUP_all", "prefcat", "RGS"
)
otherPES <- janitor::make_clean_names(otherPES)

# Variable selection methods ===========================================================================================
# Variable sets from filtering -----------------------------------------------------------------------------------------
filtering_green = scan("variable sets/filter_features.txt", what="character")
filtering_green <- janitor::make_clean_names(filtering_green)

filtering_characteristics = scan("variable sets/filter_characteristics_all.txt", what="character")
filtering_characteristics <- janitor::make_clean_names(filtering_characteristics)

filtering_all = scan("variable sets/filter_features_all.txt", what="character")
filtering_all <- janitor::make_clean_names(filtering_all)

filtering_disr = scan("variable sets/filter_features_disr.txt", what="character")
filtering_disr <- janitor::make_clean_names(filtering_disr)

filtering_cmim = scan("variable sets/filter_features_cmim.txt", what="character")
filtering_cmim <- janitor::make_clean_names(filtering_cmim)

filtering_jmi = scan("variable sets/filter_features_jmi.txt", what="character")
filtering_jmi <- janitor::make_clean_names(filtering_jmi)

filtering_mrmr = scan("variable sets/filter_features_mrmr.txt", what="character")
filtering_mrmr <- janitor::make_clean_names(filtering_mrmr)

filtering_relief = scan("variable sets/filter_features_relief.txt", what="character")
filtering_relief <- janitor::make_clean_names(filtering_relief)

