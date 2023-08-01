###########
# Library #
###########

library(dplyr)
library(factoextra)
library(ggfortify)
library(ggplot2)
library(gridExtra)
library(openxlsx)
library(psych)
library(readr)
library(readxl)
library(SKAT)
library(tidyr)
library(xlsx)

############
# Datasets #
############

lung <- read_csv("C:/Users/knigh/OneDrive/Desktop/Github/TCR-Project/Datasets/Lung Data/dt.analysis.lung.csv",
                 show_col_types = FALSE)
lungenes <- read_csv("C:/Users/knigh/OneDrive/Desktop/Github/TCR-Project/Datasets/Lung Data/vjGene.p.all.bytime.csv",
                     show_col_types = FALSE)

#############
# Summaries #
#############

lung <- lung %>% 
  mutate_at(c('Patient.Status', 'Phase', 'Disease.Type', 'TRTP', 'DISTYP', 
              'PDL1_TC', 'PDL1STAT', 'AGEU', 'AGEGR1', 'SEX', 'RACE', 
              'RACEGR1', 'SMOKHIST', 'SMOKGR1', 'COUNTRY', 'REGION1', 
              'LINTXGR1', 'PRPLAT', 'PLATMTYP', 'Status', 'LIVERBL',
              'Response', 'Response.bin1', 'Response.bin2'), as.factor)
cat("Summary of lung as factors: \n\n")
summary(lung)

lungenes <- lungenes %>% 
  mutate_at(c('...1'), as.factor)
cat("Summary of lungenes as factors: \n\n")
summary(lungenes)



