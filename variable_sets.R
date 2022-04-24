# VARIABLE SETS ################################################################

# Variable sets ================================================================
# Original AMS variables -------------------------------------------------------
# WICHTIG: Für junge Leute unter 25:
# Für diese Population werden die Merkmale STAATENGRUPPE, GESCHÄFTSFALLDAUER und BESCHÄFTIGUNGSVERLAUF nicht für die Schätzung verwendet.
ams_youth <- c("EMPLOYMENTDAYS", "GENDER", "AGEGROUP", "EDUCATION", "CHILDCARE", "RGS", 
               "IMPAIRMENT_strong", "OCCUPATIONGROUP", "BUSINESSCASEFREQ", "SUPPORTMEASURE")

ams <- c("EMPLOYMENTDAYS", "GENDER", "AGEGROUP", "STATEGROUP", "EDUCATION", "CHILDCARE", "RGS", 
         "IMPAIRMENT_strong", "OCCUPATIONGROUP", "EMPLOYMENTHIST", "BUSINESSCASEFREQ", 
         "BUSINESSCASEDUR", "SUPPORTMEASURE")

# Expanded AMS Variables -------------------------------------------------------
ams_ext <- c("EMPLOYMENTDAYS", "GENDER", "AGEGROUP", "STATEGROUP", "EDUCATION", "CHILDCARE_both", "RGS", 
             "IMPAIRMENT_order", "OCCUPATIONGROUP_all",  #"EMPLOYMENTHIST",
             "r_monate_erw_j1voral", "r_monate_erw_j2voral", "r_monate_erw_j3voral", "r_monate_erw_j4voral",
             #"r_geschaeftsfall_j1voral", "r_geschaeftsfall_j2voral", "r_geschaeftsfall_j3voral", "r_geschaeftsfall_j4voral",
             "BUSINESSCASEFREQ_order" , "BUSINESSCASEDUR", "SUPPORTMEASURE_order"
             
             )

# Green Variables --------------------------------------------------------------
green_big <- c(ams, "zusage", "match", "jobsat", "vertrag", "endreason", "ALexp", "exp",
               "intrins", "extrins", "lottery", "lifesat", "prefcat",
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
green_t1 <- c("deutsch5", "bewerbung_no", "vorstell_no", "suche_specific", "suchintens",  "soc_konflikt", "soc_versteh")

green <- c(ams, "zusage", "ALexp",
           "intrins", "extrins", "prefcat", "a_instrumental", "trust",
           "reserve", "sw_selbstwert", "gewissenh",
           "depressrisk_WHO", "alkohol", "socialcomp", 
           "SDT", "recall" , "relig_islam", "ability"
)

# Variable sets other paper ----------------------------------------------------

# Influencable variable set ----------------------------------------------------