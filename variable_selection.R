# VARIABLE SETS ################################################################

# Preparation ==================================================================
# Libraries --------------------------------------------------------------------

# Load data --------------------------------------------------------------------
# load preperad dataset
data <- readRDS("data/JuSAW_prepared.rds")
attach(data)

# Variable sets ================================================================
# Original AMS variables -------------------------------------------------------
# WICHTIG: Für junge Leute unter 25:
# Für diese Population werden die Merkmale STAATENGRUPPE, GESCHÄFTSFALLDAUER und BESCHÄFTIGUNGSVERLAUF nicht für die Schätzung verwendet.
ams_youth <- c("EMPLOYMENTDAYS", "GENDER_female", "AGEGROUP", "EDUCATION", "CHILDCARE", "RGS", 
               "IMPAIRMENT", "OCCUPATIONGROUP", "BUSINESSCASEFREQ", "SUPPORTMEASURE")

ams <- c("EMPLOYMENTDAYS", "GENDER_female", "AGEGROUP", "STATEGROUP", "EDUCATION", "CHILDCARE", "RGS", 
         "IMPAIRMENT", "OCCUPATIONGROUP", "EMPLOYMENT", "BUSINESSCASEFREQ", 
         "BUSINESSCASEDUR", "SUPPORTMEASURE")

# Expanded AMS Variables -------------------------------------------------------
ams_exp <- c("EMPLOYMENTDAYS", "GENDER_female", "AGEGROUP", "EDUCATION", "CHILDCARE_both", "RGS", 
             "IMPAIRMENT", "OCCUPATIONGROUP_all", "BUSINESSCASEFREQ" , "SUPPORTMEASURE")

# Variable sets other paper ----------------------------------------------------

# Influencable variable set ----------------------------------------------------

# Selection procedures ---------------------------------------------------------
# Criteria for variable selection: (s.76)
# 1. Significance tests
# 2. AIC
# 3. BIC
# 4. Bayesian procedures – Bayes factors (e.g. BAS package)
# 5. Deviance information criterion (DIC in WinBUGS)
# Other methods:
# 1. Ridge regression
# 2. Lasso and shrinkage methods
# 3. Bayesian variable selection and model search algorithms