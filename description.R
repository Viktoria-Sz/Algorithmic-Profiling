# DESCRIPTIVES #########################################################################################################
# Preparation ==========================================================================================================
# Libraries ------------------------------------------------------------------------------------------------------------
library(tidyverse)
#library(broom) # für funktion tidy
library(data.table) # für rbindlist
library(rcompanion) # für cramers V
library(DiscriMiner) # for correlation ratio between numeric and categorical variable
library(DescTools) # for pairwise calculations
library(corrplot)
library(wesanderson) # for nicer plot colors

# Load data ------------------------------------------------------------------------------------------------------------
# load preperad dataset
data <- readRDS("data/JuSAW_prepared.rds")
data_pre <- readRDS("data/dataJuSAW.rds")

# Load other scripts ---------------------------------------------------------------------------------------------------
source("variable_sets.R", encoding="utf-8") # for predefined feature sets
source("functions.R", encoding="utf-8")

# Simple descriptions ##################################################################################################
# General Vars =========================================================================================================
# Employmentdays - dependent variable ----------------------------------------------------------------------------------
ggplot(data, aes(x = as.factor(r_besch), fill = as.factor(r_besch))) +
  geom_bar() +
  scale_fill_manual(values=c("#FFCC00", "#999999"))+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  theme_bw(14) +
  # labs(title = "Days in employment more of less than 90 in last 7 months",
  #      y = "count", x = "EMPLOYMENTDAYS",
  #      fill = "EMPLOYMENTDAYS") +
  labs(title = "Beschäftigungszeitraum mehr oder weniger als 90 Tage in 7 Monaten",
       y = "Beobachtungen", x = "Beschäftigungstage",
       fill = "EMPLOYMENTDAYS") +
  scale_x_discrete(labels = c("1" = ">=90 Tage", "0" = "<90 Tage")) +
  theme(legend.position = "none")

# Gender ---------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = GENDER, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")
ggplot(data, aes(x = GENDER, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, position = position_dodge(width = 0.5)) +
  labs(title = "Days in employment dependent on gender")

# ci_plot
ci_plot(data, GENDER)
# Vielleicht noch einheitliche Skala von 0 bis 1
  
# The effect of gender on having more than 90 days of work in 7 months
# We would like to test for the equality of proportions of males or females
# Η0: “Independence between gender and more than 90 workdays” vs
# Η1: “there is association between gender and workdays”
tab_gender <- table(data$GENDER, data$EMPLOYMENTDAYS)
round(100*prop.table(tab_gender, 1),1)

# prop.test implements the Pearson’s chi-square statistics for independence
chisq.test(data$GENDER, data$EMPLOYMENTDAYS)
fisher.test(tab_gender)

tab_gender
cramerV(tab_gender)
cramerV(data$GENDER, data$EMPLOYMENTDAYS)

# Agegroup -------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = AGEGROUP, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")
ggplot(data, aes(x = AGEGROUP, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))

ci_plot(data, AGEGROUP)


# Childcare ------------------------------------------------------------------------------------------------------------
# Argument für childcare both?
ggplot(data, aes(x = CHILDCARE_both, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")
ggplot(data, aes(x = CHILDCARE_both, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = CHILDCARE, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")
ggplot(data, aes(x = CHILDCARE, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = CHILDCARE_men, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")
ggplot(data, aes(x = CHILDCARE_men, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))

# Health ---------------------------------------------------------------------------------------------------------------
# Impariment is "yes, very"; impairment_strong is "yes, very" and "yes, little"
ggplot(data, aes(x = IMPAIRMENT, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")
ggplot(data, aes(x = IMPAIRMENT, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = IMPAIRMENT_strong, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")
ggplot(data, aes(x = IMPAIRMENT_strong, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))

# Migration background -------------------------------------------------------------------------------------------------
# Staatengruppe
ggplot(data, aes(x = STATEGROUP, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")
ggplot(data, aes(x = STATEGROUP, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))

# Religion
table(data$relig, useNA = "always")
ggplot(data, aes(x = relig, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")
ggplot(data, aes(x = relig, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
table(data$relig2, useNA = "always") # Wie wichtig ist Religion

# Variable Isalm und other
data$relig_islam <- as.factor(ifelse(data$relig == "Islam", "Islam", "other"))
ggplot(data, aes(x = relig_islam, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = relig_islam, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")

# Migration background
table(data$mighint12g_new, useNA = "always")
ggplot(data, aes(x = mighint12g_new, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = mighint12g_new, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")


ci_plot(data, STATEGROUP)
ci_plot(data, relig) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))
ci_plot(data, mighint12g_new)


chisq.test(data$stategroup01, data$EMPLOYMENTDAYS)


# RGS Typ --------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = RGS, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = RGS, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")


# Hard Skills ==========================================================================================================
# Education ------------------------------------------------------------------------------------------------------------
# Grade German
table(data$notede, useNA = "always") # Schulnote letztes Zeugnis: Deutsch
ggplot(data, aes(x = as.factor(notede), group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = as.factor(notede), group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")

# Grade Maths
table(data$notema, useNA = "always") # Schulnote letztes Zeugnis: Mathematik
ggplot(data, aes(x = as.factor(notema), group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = as.factor(notema), group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")

# Education
ggplot(data, aes(x = EDUCATION, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = EDUCATION, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")
# korreliert eindeutig

# Ability --------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = EMPLOYMENTDAYS, y = ability)) +
  geom_boxplot()


# Job ------------------------------------------------------------------------------------------------------------------
# Occupationgroup
ggplot(data, aes(x = OCCUPATIONGROUP_all, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = OCCUPATIONGROUP_all, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")
ggplot(data, aes(x = OCCUPATIONGROUP, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = OCCUPATIONGROUP, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")

# Employmenthistory
ggplot(data, aes(x = EMPLOYMENTHIST, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = EMPLOYMENTHIST, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")

# Unemployment history
ggplot(data, aes(x = ALexp, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = ALexp, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")

# Businesscase duration
ggplot(data, aes(x = BUSINESSCASEDUR, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = BUSINESSCASEDUR, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")

# Businesscase frequency
ggplot(data, aes(x = BUSINESSCASEFREQ, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = BUSINESSCASEFREQ, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")

# Supportmeasures
ggplot(data, aes(x = SUPPORTMEASURE, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = SUPPORTMEASURE, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")

# Soft Skilss ==========================================================================================================
# Jobattributpräferenz -------------------------------------------------------------------------------------------------
# Motivation
ggplot(data, aes(x = EMPLOYMENTDAYS, y = intrins)) +
  geom_boxplot()
ggplot(data, aes(x = EMPLOYMENTDAYS, y = extrins)) +
  geom_boxplot()
ggplot(data, aes(x = EMPLOYMENTDAYS, y = intrins_min_extrins)) +
  geom_boxplot()

# Preferred working hours
ggplot(data, aes(x = prefcat, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = prefcat, group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")


# Personality ----------------------------------------------------------------------------------------------------------
# Depressionrisk
ggplot(data, aes(x = as.factor(depressrisk_WHO), group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = position_dodge(width = 0.5))
ggplot(data, aes(x = as.factor(depressrisk_WHO), group = EMPLOYMENTDAYS, fill = EMPLOYMENTDAYS)) +
  geom_bar(position = "fill")


# Other descriptives ###################################################################################################
# CI-Plots =============================================================================================================
jobfind_rate <- mean(data$EMPLOYMENTDAYS == ">=90 Days")

var_list <- c("STATEGROUP", "AGEGROUP", "CHILDCARE", "IMPAIRMENT", "EDUCATION")
data_rates <- filter(data, !is.na(EDUCATION))

rates <- data_rates %>%
  group_by(GENDER) %>%
  do(binom_stats(.)) %>%
  arrange(Proportion) %>%
  ungroup() %>%
  rename(char = GENDER) %>%
  mutate(var = "GENDER")

for(i in var_list){
  rates_new <- data_rates %>%
    group_by(data_rates[i]) %>%
    do(binom_stats(.)) %>%
    arrange(Proportion) %>%
    ungroup() %>%
    rename(char = i) %>%
    mutate(var = i)
  rates <- rbind(rates, rates_new)
}


prevalence <- ggplot(rates, aes(x = char, y = Proportion, colour = var)) +
  geom_hline(yintercept = jobfind_rate, col = "red", alpha = .35, lty = 2) + 
  geom_point(size = 5) +
  theme_bw() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)
                , width = 0.3, size = 2) +
  scale_color_manual(values=c("#FFDB6D", "#C4961A", "#D16103", "#52854C", "#4E84C4", "#293352"))+
  theme(axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0, 0.6), breaks = scales::pretty_breaks(n = 5)) +
  #scale_fill_manual(values=wes_palette(n = 6,name="Rushmore")) +
  #scale_fill_brewer(palette="Accent") +
  facet_grid(cols = vars(var), scales = "free") +
  # facet_grid(rows = vars(var), scales = "free") +
  # coord_flip() +
  #labs(title = "Anteile der Arbeitssuchenden mit erfülltem Zielkriterium in verschiedenen demographischen Gruppen") +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
prevalence

# ggsave("plots/CI_plot_prevalence_vertical.png", prevalence, width = 9.50, height = 14.40, dpi = 1500)
# ggsave("plots/CI_plot_prevalence_horizontal.png", prevalence, width = 9.50, height = 5.45, dpi = 1500)



# Associations with Cramers V ==========================================================================================
# AMS ------------------------------------------------------------------------------------------------------------------
cramerv_ams <- DescTools::PairApply(data[, c(ams[-length(ams)], "SUPPORTMEASURE_order")], DescTools::CramerV)
#cramerv_ams <- DescTools::PairApply(data[, ams], DescTools::CramerV)

cramerV(data$EMPLOYMENTDAYS, data$SUPPORTMEASURE)

corrplot(cramerv_ams, is.corr = F, diag = F, type = 'lower',  col.lim = c(0, 1),  method = 'color',  tl.col = 'black'
         , addCoef.col = 'grey50', number.cex = 0.8, tl.srt = 45, na.label = "NA"
         #,order = 'hclust'
         )

# AMS ext ------------------------------------------------------------------------------------------------------------
cramerv_ams_ext <- DescTools::PairApply(data[, ams_ext], DescTools::CramerV)

corrplot(cramerv_ams_ext, is.corr = F, diag = F, type = 'lower',  col.lim = c(0, 1),  method = 'color',  tl.col = 'black'
         , addCoef.col = 'grey50', number.cex = 0.8, tl.srt = 45, na.label = "NA"
         #,order = 'hclust'
         )


# AMS youth ------------------------------------------------------------------------------------------------------------
cramerv_ams_youth <- DescTools::PairApply(data[, c(ams_youth[-length(ams_youth)], "SUPPORTMEASURE_order")], DescTools::CramerV)

corrplot(cramerv_ams_youth, is.corr = F, diag = F, type = 'lower',  col.lim = c(0, 1),  method = 'color',  tl.col = 'black'
         , addCoef.col = 'grey50', number.cex = 0.8, tl.srt = 45, na.label = "NA"
         #,order = 'hclust'
         )


# Green ----------------------------------------------------------------------------------------------------------------
str(data[,green_big])
green_big_num <- dplyr::select_if(data[,green_big], is.numeric)
green_big_cat <- dplyr::select_if(data[,green_big], is.factor)
cor_greenbig_num <- cor(green_big_num,use = "pairwise.complete.obs") 
cramerv_greenbig_cat <- DescTools::PairApply(green_big_cat, DescTools::CramerV)

# Categorical
corrplot(cramerv_greenbig_cat, is.corr = F, diag = F, type = 'lower',  col.lim = c(0, 1),  method = 'color',  tl.col = 'black'
         , tl.srt = 45, na.label = "NA"
         #, number.cex = 0.6, , addCoef.col = 'grey50'
         )

# Numeric
corrplot(cor_greenbig_num, method = 'color', diag = F, type = 'lower', tl.col = 'black'
         , tl.srt = 45, na.label = "NA" #, order = 'hclust'
         #,addCoef.col = 'grey50', number.cex = 0.5
         )

str(data[,green])
green_num <- dplyr::select_if(data[,green], is.numeric)
green_cat <- dplyr::select_if(data[,green], is.factor)
cor_green_num <- cor(green_num,use = "pairwise.complete.obs") 
cramerv_green_cat <- DescTools::PairApply(green_cat, DescTools::CramerV)

corrplot(cramerv_green_cat, is.corr = F, diag = F, type = 'lower',  col.lim = c(0, 1),  method = 'color',  tl.col = 'black'
         , addCoef.col = 'grey50', number.cex = 0.7, tl.srt = 45, na.label = "NA")
corrplot(cor_green_num, method = 'color', diag = F, type = 'lower', tl.col = 'black',
         addCoef.col = 'grey50', number.cex = 1, tl.srt = 45, na.label = "NA", order = 'hclust')

# All vars -------------------------------------------------------------------------------------------------------------
str(data[,all])
all_num <- dplyr::select_if(data[,all], is.numeric)
all_cat <- dplyr::select_if(data[,all], is.factor) %>%
  select(-socialmeet) # ist aus irgendeinem Grund NA
cor_all_num <- cor(all_num, use = "pairwise.complete.obs") 
cramerv_all_cat <- DescTools::PairApply(all_cat, DescTools::CramerV)

corrplot(cramerv_all_cat, is.corr = F, diag = F, type = 'lower',  col.lim = c(0, 1),  method = 'color',  tl.col = 'black'
         , tl.srt = 45, na.label = "NA", number.cex = 0.3
         #,order = 'hclust' , addCoef.col = 'grey50', 
)
corrplot(cor_all_num, method = 'color', diag = F, type = 'lower', tl.col = 'black'
         , tl.srt = 45, na.label = "NA" #, order = 'hclust'
         #,addCoef.col = 'grey50', number.cex = 0.5
)
