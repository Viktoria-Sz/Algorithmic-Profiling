# DESCRIPTIVES #################################################################
# Preparation ==================================================================
# Libraries --------------------------------------------------------------------
library(tidyverse)
library(broom) # für funktion tidy
library(data.table) # für rbindlist
library(corrplot)

# Load data --------------------------------------------------------------------
# load preperad dataset
data <- readRDS("data/JuSAW_prepared.rds")

# Load other scripts -----------------------------------------------------------
source("variable_sets.R", encoding="utf-8") # for predefined feature sets

# The effect of gender on having more than 90 days of work in 7 months
# We would like to test for the equality of proportions of males or females
# Η0: “Independence between gender and more than 90 workdays” vs
# Η1: “there is association between gender and workdays”
tab_gender <- table(data$GENDER_female, data$EMPLOYMENTDAYS)
round(100*prop.table(tab_gender, 1),1)

# prop.test implements the Pearson’s chi-square statistics for independence
chisq.test(data$GENDER_female, data$EMPLOYMENTDAYS)
fisher.test(tab_gender)


#===============================================================================
# Heatmap plot für alle p-values von chisquared test Variablen gegenseitig
# Mit base heatmap() -----------------------------------------------------------
CHIS_employ <- lapply(data[, ams], function(x) chisq.test(data[,ams[1]], x)$p.value)
pvalues_ams_list <- rbindlist(lapply(CHIS_employ, tidy), idcol=TRUE)

chis <- list()
for(i in 2:length(ams)){
  chis <- lapply(data[, ams], function(x) chisq.test(data[,ams[i]], x)$p.value)
  values <- rbindlist(lapply(chis, tidy), idcol=TRUE)
  pvalues_ams_list <- full_join(pvalues_ams_list, values, by = ".id")
}
pvalues_ams_mat <- column_to_rownames(pvalues_ams_list, var=".id")
names(pvalues_ams_mat) <- ams
pvalues_ams_heatmap <- round(as.matrix(pvalues_ams_mat),5)
heatmap(pvalues_ams_heatmap, Colv = NA, Rowv = NA)


# Mit ggplot -------------------------------------------------------------------
CHIS_employ <- lapply(data[, ams], function(x) chisq.test(data[,ams[1]], x)$p.value)
pvalues_ams <- rbindlist(lapply(CHIS_employ, tidy), idcol=TRUE)
pvalues_ams <- mutate(pvalues_ams, id1 = ams[1])

chis <- list()
for(i in 2:length(ams)){
  chis <- lapply(data[, ams], function(x) chisq.test(data[,ams[i]], x)$p.value)
  values <- rbindlist(lapply(chis, tidy), idcol=TRUE)
  values <- mutate(values, id1 = ams[i])
  pvalues_ams <- rbind(pvalues_ams, values)
}
pvalues_ams <- pvalues_ams %>%
  rename(id2 = .id, pvalues = x) %>%
  mutate(pvalues = round(pvalues, 3))

pvalues_ams$groups <- cut(pvalues_ams$pvalues,               # Add group column
                       breaks = c(-1, 0, 0.01, 0.05, 0.1, 1))

ggplot(pvalues_ams, aes(id1, id2, fill = groups)) +          # Specify colors manually
  geom_tile() +
  scale_fill_manual(breaks = levels(pvalues_ams$groups),
                    values = c("white", "yellow", "orange", "red", "darkred")) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))

# Mit Corrplot -----------------------------------------------------------------
testRes = cor.mtest(mtcars, conf.level = 0.95)

## specialized the insignificant value according to the significant level
corrplot(M, p.mat = testRes$p, sig.level = 0.10, order = 'hclust', addrect = 2)

# ==============================================================================
# Heatmap plot für alle Anteile je Variablenkategorie an denen die >90 Tage haben
ams_freq <- data %>%
  group_by_at(vars(one_of(c(ams[2], ams[1])))) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  filter(EMPLOYMENTDAYS == ">=90 Tage") %>%
  select(1,3,4) %>%
  rename(characteristic = 1)

for(i in 3:length(ams)){
  ams_freq_new <- data %>%
    group_by_at(vars(one_of(c(ams[i], ams[1])))) %>%
    summarise(n = n()) %>%
    mutate(freq = n/sum(n)) %>%
    filter(EMPLOYMENTDAYS == ">=90 Tage") %>%
    select(1,3,4) %>%
    rename(characteristic = 1)
  ams_freq <- rbind(ams_freq, ams_freq_new)
}

ggplot(ams_freq, aes(x = characteristic, y = freq)) +
  geom_bar()
