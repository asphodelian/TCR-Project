###########
# Library #
###########

library(dplyr)
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

C19vj <- read_csv("D:/Coding/R Storage/Summer TCR Project/TCR Datasets/dt.COVID_TCR.vjGene.p.csv",
                  show_col_types = FALSE)
vj <- read_csv("D:/Coding/R Storage/Summer TCR Project/TCR Datasets/dt.HD_TCR.vjGene.p.csv",
               show_col_types = FALSE)
patients <- read_excel("D:/Coding/R Storage/Summer TCR Project/TCR Datasets/dt.info_edited.xlsx")

#############
# Summaries #
#############

C19vj <- C19vj %>% 
  mutate_at(c('vjGene'), as.factor)
cat("Summary of C19vj: \n")
summary(C19vj)

vj <- vj %>% 
  mutate_at(c('vjGene'), as.factor)
cat("Summary of vj: \n\n")
summary(vj)

patients <- patients %>% 
  mutate_at(c('Sample.ID', 'diseae.stage', 'days.from.first.symptoms', 
              'patient.ID', 'time', 'choose', '...7', 'comment'), 
            as.factor)
cat("Summary of patients as factor: \n\n")
summary(patients)

############
# Describe #
############

describe(C19vj)
describe(vj)
describe(patients)

#############
# Data Prep #
#############

# combine the vj datasets
combine <- merge(C19vj, vj, by = "vjGene")
# replace NAs w/ extremely small number (10e-8)
combine[is.na(combine)] <- 1e-7

# dim check
dim(C19vj)
dim(vj)
dim(patients)
dim(combine)

# combine prep
colnames(combine) <- c("vjGene", "1_1", "1_2", "1_3", "1_4", "1_5", "1_6",
                       "1_7", "1_8", "1_9", "10_1", "10_2", "10_3", "10_4", 
                       "10_5", "10_6", "10_7", "10_8", "11_1", "12_1", "13_1", 
                       "14_1", "15_1", "16_1", "17_1", "18_1", "19_1", "2_1", 
                       "2_2", "2_3", "20_1", "21_1", "22_1", "23_1", "24_1", 
                       "25_1", "25_2", "26_1", "27_1", "28_1", "29_1", "3_1", 
                       "3_2", "32_1", "33_1", "34_1", "35_1", "38_1", "39_1", 
                       "40_1", "41_1", "44_1", "45_1", "5_1", "5_2", "5_3", 
                       "5_4", "5_5", "5_6", "5_7", "5_8", "6_1", "6_2", "7_1", 
                       "7_2", "7_3", "7_4", "8_1", "8_2", "9_1", "9_2", "HD01",
                       "HD02", "HD03", "HD04", "HD05", "HD06", "HD07", "HD08",
                       "HD09", "HD10", "HD11", "HD12", "HD13", "HD14", "HD15", 
                       "HD16", "HD17", "HD18", "HD19", "HD20", "HD21", "HD22", 
                       "HD23", "HD24", "HD25", "HD26", "HD27", "HD28", "HD29",       
                       "HD30", "HD31", "HD32", "HD33", "HD34", "HD35", "HD36",
                       "HD37", "HD38", "HD39")

names(combine)
longComb <- pivot_longer(combine, 
                         cols = c("1_1", "1_2", "1_3", "1_4", "1_5", "1_6", 
                                  "1_7", "1_8", "1_9", "10_1", "10_2", "10_3", 
                                  "10_4", "10_5", "10_6", "10_7", "10_8", 
                                  "11_1", "12_1", "13_1", "14_1", "15_1", 
                                  "16_1", "17_1", "18_1", "19_1", "2_1", "2_2", 
                                  "2_3", "20_1", "21_1", "22_1", "23_1", "24_1",
                                  "25_1", "25_2", "26_1", "27_1", "28_1", 
                                  "29_1", "3_1", "3_2", "32_1", "33_1", "34_1", 
                                  "35_1", "38_1", "39_1", "40_1", "41_1", 
                                  "44_1", "45_1", "5_1", "5_2", "5_3", "5_4", 
                                  "5_5", "5_6", "5_7", "5_8", "6_1", "6_2", 
                                  "7_1", "7_2", "7_3", "7_4", "8_1", "8_2", 
                                  "9_1", "9_2", "HD01", "HD02", "HD03", "HD04", 
                                  "HD05", "HD06", "HD07", "HD08", "HD09", 
                                  "HD10", "HD11", "HD12", "HD13", "HD14", 
                                  "HD15", "HD16", "HD17", "HD18", "HD19", 
                                  "HD20", "HD21", "HD22", "HD23", "HD24", 
                                  "HD25", "HD26", "HD27", "HD28", "HD29", 
                                  "HD30", "HD31", "HD32", "HD33", "HD34", 
                                  "HD35", "HD36", "HD37", "HD38", "HD39"),
                         names_to = "Sample.ID", values_to = "Value")
View(longComb)

# combine patients
full <- merge(longComb, patients, by = "Sample.ID")
full <- full %>%
  arrange(vjGene)

# log transform
full$log.value <- log(full$Value)

# min-max standard/normalization
min_val <- min(full$log.value)
max_val <- max(full$log.value)
full$normLog <- (full$log.value - min_val) / (max_val - min_val)














