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
gene <- read_excel("C:/Users/knigh/OneDrive/Desktop/Github/TCR-Project/Datasets/Disease Data/fullgenes.xlsx")

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

# log transform 
longComb$Value <- log(longComb$Value)
# standardize
longComb$Value <- scale(longComb$Value)

wideComb <- pivot_wider(longComb, names_from = "vjGene", values_from = "Value")

# combine wide & patients
full <- merge(wideComb, patients, by = "Sample.ID", all = TRUE)

#######################
# How we want dataset #
#######################

# Patient | VJ1 | VJ2 |    Y    |   Y1   |
#  Pat 1  | ... | ... |    HD   |   HD   |
#  Pat 2  | ... | ... | Disease | Active |

full <- select(full, patient.ID, Sample.ID, diseae.stage, "TRBV10-1_TRBJ1-1",
               "TRBV10-1_TRBJ1-2", "TRBV10-1_TRBJ1-3", "TRBV10-1_TRBJ1-4",
               "TRBV10-1_TRBJ1-5", "TRBV10-1_TRBJ1-6", "TRBV10-1_TRBJ2-1",
               "TRBV10-1_TRBJ2-2", "TRBV10-1_TRBJ2-3", "TRBV10-1_TRBJ2-4", 
               "TRBV10-1_TRBJ2-5", "TRBV10-1_TRBJ2-6", "TRBV10-1_TRBJ2-7",
               "TRBV10-2_TRBJ1-1", "TRBV10-2_TRBJ1-2", "TRBV10-2_TRBJ1-3",
               "TRBV10-2_TRBJ1-4", "TRBV10-2_TRBJ1-5", "TRBV10-2_TRBJ1-6", 
               "TRBV10-2_TRBJ2-1", "TRBV10-2_TRBJ2-2", "TRBV10-2_TRBJ2-3",
               "TRBV10-2_TRBJ2-4", "TRBV10-2_TRBJ2-5", "TRBV10-2_TRBJ2-6", 
               "TRBV10-2_TRBJ2-7", "TRBV10-3_TRBJ1-1", "TRBV10-3_TRBJ1-2", 
               "TRBV10-3_TRBJ1-3", "TRBV10-3_TRBJ1-4", "TRBV10-3_TRBJ1-5",
               "TRBV10-3_TRBJ1-6", "TRBV10-3_TRBJ2-1", "TRBV10-3_TRBJ2-2", 
               "TRBV10-3_TRBJ2-3", "TRBV10-3_TRBJ2-4", "TRBV10-3_TRBJ2-5", 
               "TRBV10-3_TRBJ2-6", "TRBV10-3_TRBJ2-7", "TRBV11-1_TRBJ1-1",
               "TRBV11-1_TRBJ1-2", "TRBV11-1_TRBJ1-3", "TRBV11-1_TRBJ1-4",
               "TRBV11-1_TRBJ1-5", "TRBV11-1_TRBJ1-6", "TRBV11-1_TRBJ2-1", 
               "TRBV11-1_TRBJ2-2", "TRBV11-1_TRBJ2-3", "TRBV11-1_TRBJ2-4",
               "TRBV11-1_TRBJ2-5", "TRBV11-1_TRBJ2-6", "TRBV11-1_TRBJ2-7",
               "TRBV11-2_TRBJ1-1", "TRBV11-2_TRBJ1-2", "TRBV11-2_TRBJ1-3",
               "TRBV11-2_TRBJ1-4", "TRBV11-2_TRBJ1-5", "TRBV11-2_TRBJ1-6", 
               "TRBV11-2_TRBJ2-1", "TRBV11-2_TRBJ2-2", "TRBV11-2_TRBJ2-3",
               "TRBV11-2_TRBJ2-4", "TRBV11-2_TRBJ2-5", "TRBV11-2_TRBJ2-6",
               "TRBV11-2_TRBJ2-7", "TRBV11-3_TRBJ1-1", "TRBV11-3_TRBJ1-2",
               "TRBV11-3_TRBJ1-3", "TRBV11-3_TRBJ1-4", "TRBV11-3_TRBJ1-5",
               "TRBV11-3_TRBJ1-6", "TRBV11-3_TRBJ2-1", "TRBV11-3_TRBJ2-2",
               "TRBV11-3_TRBJ2-3", "TRBV11-3_TRBJ2-4", "TRBV11-3_TRBJ2-5",
               "TRBV11-3_TRBJ2-6", "TRBV11-3_TRBJ2-7", "TRBV12-1_TRBJ1-1",
               "TRBV12-1_TRBJ1-2", "TRBV12-1_TRBJ1-5", "TRBV12-1_TRBJ2-2",
               "TRBV12-1_TRBJ2-3", "TRBV12-1_TRBJ2-4", "TRBV12-1_TRBJ2-5",
               "TRBV12-1_TRBJ2-7", "TRBV12-3_TRBJ1-1", "TRBV12-3_TRBJ1-2", 
               "TRBV12-3_TRBJ1-3", "TRBV12-3_TRBJ1-4", "TRBV12-3_TRBJ1-5",
               "TRBV12-3_TRBJ1-6", "TRBV12-3_TRBJ2-1", "TRBV12-3_TRBJ2-2",
               "TRBV12-3_TRBJ2-3", "TRBV12-3_TRBJ2-4", "TRBV12-3_TRBJ2-5",
               "TRBV12-3_TRBJ2-6", "TRBV12-3_TRBJ2-7", "TRBV12-4_TRBJ1-1",
               "TRBV12-4_TRBJ1-2", "TRBV12-4_TRBJ1-3", "TRBV12-4_TRBJ1-4",
               "TRBV12-4_TRBJ1-5", "TRBV12-4_TRBJ1-6", "TRBV12-4_TRBJ2-1",
               "TRBV12-4_TRBJ2-2", "TRBV12-4_TRBJ2-3", "TRBV12-4_TRBJ2-4",
               "TRBV12-4_TRBJ2-5", "TRBV12-4_TRBJ2-6", "TRBV12-4_TRBJ2-7",
               "TRBV12-5_TRBJ1-1", "TRBV12-5_TRBJ1-2", "TRBV12-5_TRBJ1-3",
               "TRBV12-5_TRBJ1-4", "TRBV12-5_TRBJ1-5", "TRBV12-5_TRBJ1-6",
               "TRBV12-5_TRBJ2-1", "TRBV12-5_TRBJ2-2", "TRBV12-5_TRBJ2-3",
               "TRBV12-5_TRBJ2-4", "TRBV12-5_TRBJ2-5", "TRBV12-5_TRBJ2-6",
               "TRBV12-5_TRBJ2-7", "TRBV13_TRBJ1-1", "TRBV13_TRBJ1-2", 
               "TRBV13_TRBJ1-3", "TRBV13_TRBJ1-4", "TRBV13_TRBJ1-5", 
               "TRBV13_TRBJ1-6", "TRBV13_TRBJ2-1", "TRBV13_TRBJ2-2", 
               "TRBV13_TRBJ2-3", "TRBV13_TRBJ2-4", "TRBV13_TRBJ2-5", 
               "TRBV13_TRBJ2-6", "TRBV13_TRBJ2-7", "TRBV14_TRBJ1-1", 
               "TRBV14_TRBJ1-2", "TRBV14_TRBJ1-3", "TRBV14_TRBJ1-4", 
               "TRBV14_TRBJ1-5", "TRBV14_TRBJ1-6", "TRBV14_TRBJ2-1",
               "TRBV14_TRBJ2-2", "TRBV14_TRBJ2-3", "TRBV14_TRBJ2-4", 
               "TRBV14_TRBJ2-5", "TRBV14_TRBJ2-6", "TRBV14_TRBJ2-7", 
               "TRBV15_TRBJ1-1", "TRBV15_TRBJ1-2", "TRBV15_TRBJ1-3",
               "TRBV15_TRBJ1-4", "TRBV15_TRBJ1-5", "TRBV15_TRBJ1-6",
               "TRBV15_TRBJ2-1", "TRBV15_TRBJ2-2", "TRBV15_TRBJ2-3",
               "TRBV15_TRBJ2-4", "TRBV15_TRBJ2-5", "TRBV15_TRBJ2-6",
               "TRBV15_TRBJ2-7", "TRBV18_TRBJ1-1", "TRBV18_TRBJ1-2",
               "TRBV18_TRBJ1-3", "TRBV18_TRBJ1-4", "TRBV18_TRBJ1-5", 
               "TRBV18_TRBJ1-6", "TRBV18_TRBJ2-1", "TRBV18_TRBJ2-2",
               "TRBV18_TRBJ2-3", "TRBV18_TRBJ2-4", "TRBV18_TRBJ2-5",
               "TRBV18_TRBJ2-6", "TRBV18_TRBJ2-7", "TRBV19_TRBJ1-1",
               "TRBV19_TRBJ1-2", "TRBV19_TRBJ1-3", "TRBV19_TRBJ1-4", 
               "TRBV19_TRBJ1-5", "TRBV19_TRBJ1-6", "TRBV19_TRBJ2-1",
               "TRBV19_TRBJ2-2", "TRBV19_TRBJ2-3", "TRBV19_TRBJ2-4", 
               "TRBV19_TRBJ2-5", "TRBV19_TRBJ2-6", "TRBV19_TRBJ2-7",
               "TRBV2_TRBJ1-1", "TRBV2_TRBJ1-2", "TRBV2_TRBJ1-3",
               "TRBV2_TRBJ1-4", "TRBV2_TRBJ1-5", "TRBV2_TRBJ1-6",
               "TRBV2_TRBJ2-1", "TRBV2_TRBJ2-2", "TRBV2_TRBJ2-3",
               "TRBV2_TRBJ2-4", "TRBV2_TRBJ2-5", "TRBV2_TRBJ2-6",
               "TRBV2_TRBJ2-7", "TRBV20-1_TRBJ1-1", "TRBV20-1_TRBJ1-2",
               "TRBV20-1_TRBJ1-3", "TRBV20-1_TRBJ1-4", "TRBV20-1_TRBJ1-5",
               "TRBV20-1_TRBJ1-6", "TRBV20-1_TRBJ2-1", "TRBV20-1_TRBJ2-2",
               "TRBV20-1_TRBJ2-3", "TRBV20-1_TRBJ2-4", "TRBV20-1_TRBJ2-5",
               "TRBV20-1_TRBJ2-6", "TRBV20-1_TRBJ2-7", "TRBV21-1_TRBJ1-1",
               "TRBV21-1_TRBJ1-2", "TRBV21-1_TRBJ1-3", "TRBV21-1_TRBJ1-4",
               "TRBV21-1_TRBJ1-5", "TRBV21-1_TRBJ1-6", "TRBV21-1_TRBJ2-1",
               "TRBV21-1_TRBJ2-2", "TRBV21-1_TRBJ2-3", "TRBV21-1_TRBJ2-4",
               "TRBV21-1_TRBJ2-5", "TRBV21-1_TRBJ2-6", "TRBV21-1_TRBJ2-7",
               "TRBV23-1_TRBJ1-1", "TRBV23-1_TRBJ1-2", "TRBV23-1_TRBJ1-4",
               "TRBV23-1_TRBJ1-5", "TRBV23-1_TRBJ1-6", "TRBV23-1_TRBJ2-1",
               "TRBV23-1_TRBJ2-2", "TRBV23-1_TRBJ2-3", "TRBV23-1_TRBJ2-4",
               "TRBV23-1_TRBJ2-5","TRBV23-1_TRBJ2-6", "TRBV23-1_TRBJ2-7",
               "TRBV24-1_TRBJ1-1", "TRBV24-1_TRBJ1-2", "TRBV24-1_TRBJ1-3",
               "TRBV24-1_TRBJ1-4", "TRBV24-1_TRBJ1-5", "TRBV24-1_TRBJ1-6",
               "TRBV24-1_TRBJ2-1", "TRBV24-1_TRBJ2-2", "TRBV24-1_TRBJ2-3",
               "TRBV24-1_TRBJ2-4", "TRBV24-1_TRBJ2-5", "TRBV24-1_TRBJ2-6",
               "TRBV24-1_TRBJ2-7", "TRBV25-1_TRBJ1-1", "TRBV25-1_TRBJ1-2",
               "TRBV25-1_TRBJ1-3", "TRBV25-1_TRBJ1-4", "TRBV25-1_TRBJ1-5",
               "TRBV25-1_TRBJ1-6", "TRBV25-1_TRBJ2-1", "TRBV25-1_TRBJ2-2",
               "TRBV25-1_TRBJ2-3", "TRBV25-1_TRBJ2-4", "TRBV25-1_TRBJ2-5",
               "TRBV25-1_TRBJ2-6", "TRBV25-1_TRBJ2-7", "TRBV27_TRBJ1-1",
               "TRBV27_TRBJ1-2", "TRBV27_TRBJ1-3", "TRBV27_TRBJ1-4", 
               "TRBV27_TRBJ1-5", "TRBV27_TRBJ1-6", "TRBV27_TRBJ2-1",
               "TRBV27_TRBJ2-2", "TRBV27_TRBJ2-3", "TRBV27_TRBJ2-4", 
               "TRBV27_TRBJ2-5", "TRBV27_TRBJ2-6", "TRBV27_TRBJ2-7",
               "TRBV28_TRBJ1-1", "TRBV28_TRBJ1-2", "TRBV28_TRBJ1-3",
               "TRBV28_TRBJ1-4", "TRBV28_TRBJ1-5", "TRBV28_TRBJ1-6",
               "TRBV28_TRBJ2-1", "TRBV28_TRBJ2-2", "TRBV28_TRBJ2-3",
               "TRBV28_TRBJ2-4", "TRBV28_TRBJ2-5", "TRBV28_TRBJ2-6",
               "TRBV28_TRBJ2-7", "TRBV29-1_TRBJ1-1", "TRBV29-1_TRBJ1-2",
               "TRBV29-1_TRBJ1-3", "TRBV29-1_TRBJ1-4", "TRBV29-1_TRBJ1-5",
               "TRBV29-1_TRBJ1-6", "TRBV29-1_TRBJ2-1", "TRBV29-1_TRBJ2-2",
               "TRBV29-1_TRBJ2-3", "TRBV29-1_TRBJ2-4", "TRBV29-1_TRBJ2-5",
               "TRBV29-1_TRBJ2-6", "TRBV29-1_TRBJ2-7", "TRBV3-2_TRBJ1-4",
               "TRBV3-2_TRBJ2-1", "TRBV30_TRBJ1-1", "TRBV30_TRBJ1-2",
               "TRBV30_TRBJ1-3", "TRBV30_TRBJ1-4", "TRBV30_TRBJ1-5",
               "TRBV30_TRBJ1-6", "TRBV30_TRBJ2-1", "TRBV30_TRBJ2-2",
               "TRBV30_TRBJ2-3", "TRBV30_TRBJ2-4", "TRBV30_TRBJ2-5",
               "TRBV30_TRBJ2-6", "TRBV30_TRBJ2-7", "TRBV4-1_TRBJ1-1",
               "TRBV4-1_TRBJ1-2", "TRBV4-1_TRBJ1-3", "TRBV4-1_TRBJ1-4",
               "TRBV4-1_TRBJ1-5", "TRBV4-1_TRBJ1-6", "TRBV4-1_TRBJ2-1",
               "TRBV4-1_TRBJ2-2", "TRBV4-1_TRBJ2-3", "TRBV4-1_TRBJ2-4",
               "TRBV4-1_TRBJ2-5", "TRBV4-1_TRBJ2-6", "TRBV4-1_TRBJ2-7",
               "TRBV4-2_TRBJ1-1", "TRBV4-2_TRBJ1-2", "TRBV4-2_TRBJ1-3",
               "TRBV4-2_TRBJ1-4", "TRBV4-2_TRBJ1-5", "TRBV4-2_TRBJ1-6",
               "TRBV4-2_TRBJ2-1", "TRBV4-2_TRBJ2-2", "TRBV4-2_TRBJ2-3",
               "TRBV4-2_TRBJ2-4", "TRBV4-2_TRBJ2-5", "TRBV4-2_TRBJ2-6",
               "TRBV4-2_TRBJ2-7", "TRBV4-3_TRBJ1-1", "TRBV4-3_TRBJ1-2", 
               "TRBV4-3_TRBJ1-3", "TRBV4-3_TRBJ1-4", "TRBV4-3_TRBJ1-5",
               "TRBV4-3_TRBJ1-6", "TRBV4-3_TRBJ2-1", "TRBV4-3_TRBJ2-2",
               "TRBV4-3_TRBJ2-3", "TRBV4-3_TRBJ2-4", "TRBV4-3_TRBJ2-5",
               "TRBV4-3_TRBJ2-6", "TRBV4-3_TRBJ2-7", "TRBV5-1_TRBJ1-1",
               "TRBV5-1_TRBJ1-2", "TRBV5-1_TRBJ1-3", "TRBV5-1_TRBJ1-4",
               "TRBV5-1_TRBJ1-5", "TRBV5-1_TRBJ1-6", "TRBV5-1_TRBJ2-1",
               "TRBV5-1_TRBJ2-2", "TRBV5-1_TRBJ2-3", "TRBV5-1_TRBJ2-4",
               "TRBV5-1_TRBJ2-5", "TRBV5-1_TRBJ2-6", "TRBV5-1_TRBJ2-7",
               "TRBV5-3_TRBJ1-1", "TRBV5-3_TRBJ1-2", "TRBV5-3_TRBJ1-3",
               "TRBV5-3_TRBJ1-4", "TRBV5-3_TRBJ1-5", "TRBV5-3_TRBJ1-6",
               "TRBV5-3_TRBJ2-1", "TRBV5-3_TRBJ2-2", "TRBV5-3_TRBJ2-3",
               "TRBV5-3_TRBJ2-4", "TRBV5-3_TRBJ2-5", "TRBV5-3_TRBJ2-6",
               "TRBV5-3_TRBJ2-7", "TRBV5-4_TRBJ1-1", "TRBV5-4_TRBJ1-2",
               "TRBV5-4_TRBJ1-3", "TRBV5-4_TRBJ1-4", "TRBV5-4_TRBJ1-5",
               "TRBV5-4_TRBJ1-6", "TRBV5-4_TRBJ2-1", "TRBV5-4_TRBJ2-2",
               "TRBV5-4_TRBJ2-3", "TRBV5-4_TRBJ2-4", "TRBV5-4_TRBJ2-5",
               "TRBV5-4_TRBJ2-6", "TRBV5-4_TRBJ2-7", "TRBV5-5_TRBJ1-1",
               "TRBV5-5_TRBJ1-2", "TRBV5-5_TRBJ1-3", "TRBV5-5_TRBJ1-4",
               "TRBV5-5_TRBJ1-5", "TRBV5-5_TRBJ1-6", "TRBV5-5_TRBJ2-1",
               "TRBV5-5_TRBJ2-2", "TRBV5-5_TRBJ2-3", "TRBV5-5_TRBJ2-4", 
               "TRBV5-5_TRBJ2-5", "TRBV5-5_TRBJ2-6", "TRBV5-5_TRBJ2-7",
               "TRBV5-6_TRBJ1-1", "TRBV5-6_TRBJ1-2", "TRBV5-6_TRBJ1-3",
               "TRBV5-6_TRBJ1-4", "TRBV5-6_TRBJ1-5", "TRBV5-6_TRBJ1-6",
               "TRBV5-6_TRBJ2-1", "TRBV5-6_TRBJ2-2", "TRBV5-6_TRBJ2-3",
               "TRBV5-6_TRBJ2-4", "TRBV5-6_TRBJ2-5", "TRBV5-6_TRBJ2-6",
               "TRBV5-6_TRBJ2-7", "TRBV5-7_TRBJ1-1", "TRBV5-7_TRBJ1-2",
               "TRBV5-7_TRBJ1-3", "TRBV5-7_TRBJ1-4", "TRBV5-7_TRBJ1-5",
               "TRBV5-7_TRBJ1-6", "TRBV5-7_TRBJ2-1", "TRBV5-7_TRBJ2-2",
               "TRBV5-7_TRBJ2-3", "TRBV5-7_TRBJ2-4", "TRBV5-7_TRBJ2-5",
               "TRBV5-7_TRBJ2-6", "TRBV5-7_TRBJ2-7", "TRBV5-8_TRBJ1-1", 
               "TRBV5-8_TRBJ1-2", "TRBV5-8_TRBJ1-3", "TRBV5-8_TRBJ1-4",
               "TRBV5-8_TRBJ1-5", "TRBV5-8_TRBJ1-6", "TRBV5-8_TRBJ2-1",
               "TRBV5-8_TRBJ2-2", "TRBV5-8_TRBJ2-3", "TRBV5-8_TRBJ2-4",
               "TRBV5-8_TRBJ2-5", "TRBV5-8_TRBJ2-6", "TRBV5-8_TRBJ2-7",
               "TRBV6-1_TRBJ1-1", "TRBV6-1_TRBJ1-2", "TRBV6-1_TRBJ1-3",
               "TRBV6-1_TRBJ1-4", "TRBV6-1_TRBJ1-5", "TRBV6-1_TRBJ1-6",
               "TRBV6-1_TRBJ2-1", "TRBV6-1_TRBJ2-2", "TRBV6-1_TRBJ2-3",
               "TRBV6-1_TRBJ2-4", "TRBV6-1_TRBJ2-5", "TRBV6-1_TRBJ2-6",
               "TRBV6-1_TRBJ2-7", "TRBV6-2_TRBJ1-1", "TRBV6-2_TRBJ1-2", 
               "TRBV6-2_TRBJ1-5", "TRBV6-2_TRBJ1-6", "TRBV6-2_TRBJ2-1",
               "TRBV6-2_TRBJ2-2", "TRBV6-2_TRBJ2-3", "TRBV6-2_TRBJ2-5",
               "TRBV6-2_TRBJ2-7", "TRBV6-3_TRBJ1-1", "TRBV6-3_TRBJ1-2",
               "TRBV6-3_TRBJ1-3", "TRBV6-3_TRBJ1-4", "TRBV6-3_TRBJ1-5",
               "TRBV6-3_TRBJ1-6", "TRBV6-3_TRBJ2-1", "TRBV6-3_TRBJ2-2",
               "TRBV6-3_TRBJ2-3", "TRBV6-3_TRBJ2-4", "TRBV6-3_TRBJ2-5",
               "TRBV6-3_TRBJ2-6", "TRBV6-3_TRBJ2-7", "TRBV6-4_TRBJ1-1",
               "TRBV6-4_TRBJ1-2", "TRBV6-4_TRBJ1-3", "TRBV6-4_TRBJ1-4",
               "TRBV6-4_TRBJ1-5", "TRBV6-4_TRBJ1-6", "TRBV6-4_TRBJ2-1",
               "TRBV6-4_TRBJ2-2", "TRBV6-4_TRBJ2-3", "TRBV6-4_TRBJ2-4",
               "TRBV6-4_TRBJ2-5", "TRBV6-4_TRBJ2-6", "TRBV6-4_TRBJ2-7",
               "TRBV6-5_TRBJ1-1", "TRBV6-5_TRBJ1-2", "TRBV6-5_TRBJ1-3",
               "TRBV6-5_TRBJ1-4", "TRBV6-5_TRBJ1-5", "TRBV6-5_TRBJ1-6",
               "TRBV6-5_TRBJ2-1", "TRBV6-5_TRBJ2-2", "TRBV6-5_TRBJ2-3",
               "TRBV6-5_TRBJ2-4", "TRBV6-5_TRBJ2-5", "TRBV6-5_TRBJ2-6",
               "TRBV6-5_TRBJ2-7", "TRBV6-6_TRBJ1-1", "TRBV6-6_TRBJ1-2",
               "TRBV6-6_TRBJ1-3", "TRBV6-6_TRBJ1-4", "TRBV6-6_TRBJ1-5",
               "TRBV6-6_TRBJ1-6", "TRBV6-6_TRBJ2-1", "TRBV6-6_TRBJ2-2",
               "TRBV6-6_TRBJ2-3", "TRBV6-6_TRBJ2-4", "TRBV6-6_TRBJ2-5",
               "TRBV6-6_TRBJ2-6", "TRBV6-6_TRBJ2-7", "TRBV6-7_TRBJ1-1", 
               "TRBV6-7_TRBJ1-2", "TRBV6-7_TRBJ1-3", "TRBV6-7_TRBJ1-4",
               "TRBV6-7_TRBJ1-5", "TRBV6-7_TRBJ1-6", "TRBV6-7_TRBJ2-1",
               "TRBV6-7_TRBJ2-2", "TRBV6-7_TRBJ2-3", "TRBV6-7_TRBJ2-5",
               "TRBV6-7_TRBJ2-6", "TRBV6-7_TRBJ2-7", "TRBV6-8_TRBJ1-1",
               "TRBV6-8_TRBJ1-2", "TRBV6-8_TRBJ1-3", "TRBV6-8_TRBJ1-4",
               "TRBV6-8_TRBJ1-5", "TRBV6-8_TRBJ1-6", "TRBV6-8_TRBJ2-1",
               "TRBV6-8_TRBJ2-2", "TRBV6-8_TRBJ2-3", "TRBV6-8_TRBJ2-4",
               "TRBV6-8_TRBJ2-5", "TRBV6-8_TRBJ2-6", "TRBV6-8_TRBJ2-7",
               "TRBV6-9_TRBJ1-1", "TRBV6-9_TRBJ1-2", "TRBV6-9_TRBJ1-3",
               "TRBV6-9_TRBJ1-4", "TRBV6-9_TRBJ1-5", "TRBV6-9_TRBJ1-6",
               "TRBV6-9_TRBJ2-1", "TRBV6-9_TRBJ2-2", "TRBV6-9_TRBJ2-3",
               "TRBV6-9_TRBJ2-4", "TRBV6-9_TRBJ2-5", "TRBV6-9_TRBJ2-6",
               "TRBV6-9_TRBJ2-7", "TRBV7-2_TRBJ1-1", "TRBV7-2_TRBJ1-2",
               "TRBV7-2_TRBJ1-3", "TRBV7-2_TRBJ1-4", "TRBV7-2_TRBJ1-5",
               "TRBV7-2_TRBJ1-6", "TRBV7-2_TRBJ2-1", "TRBV7-2_TRBJ2-2",
               "TRBV7-2_TRBJ2-3", "TRBV7-2_TRBJ2-4", "TRBV7-2_TRBJ2-5",
               "TRBV7-2_TRBJ2-6", "TRBV7-2_TRBJ2-7", "TRBV7-3_TRBJ1-1",
               "TRBV7-3_TRBJ1-2", "TRBV7-3_TRBJ1-3", "TRBV7-3_TRBJ1-4",
               "TRBV7-3_TRBJ1-5", "TRBV7-3_TRBJ1-6", "TRBV7-3_TRBJ2-1",
               "TRBV7-3_TRBJ2-2", "TRBV7-3_TRBJ2-3", "TRBV7-3_TRBJ2-4",
               "TRBV7-3_TRBJ2-5", "TRBV7-3_TRBJ2-6", "TRBV7-3_TRBJ2-7",
               "TRBV7-4_TRBJ1-1", "TRBV7-4_TRBJ1-2", "TRBV7-4_TRBJ1-3",
               "TRBV7-4_TRBJ1-4", "TRBV7-4_TRBJ1-5", "TRBV7-4_TRBJ1-6",
               "TRBV7-4_TRBJ2-1", "TRBV7-4_TRBJ2-2", "TRBV7-4_TRBJ2-3",
               "TRBV7-4_TRBJ2-4", "TRBV7-4_TRBJ2-5", "TRBV7-4_TRBJ2-6",
               "TRBV7-4_TRBJ2-7", "TRBV7-5_TRBJ1-1", "TRBV7-5_TRBJ1-2",
               "TRBV7-5_TRBJ1-3", "TRBV7-5_TRBJ1-4", "TRBV7-5_TRBJ1-5",
               "TRBV7-5_TRBJ1-6", "TRBV7-5_TRBJ2-1", "TRBV7-5_TRBJ2-2",
               "TRBV7-5_TRBJ2-3", "TRBV7-5_TRBJ2-4", "TRBV7-5_TRBJ2-5",
               "TRBV7-5_TRBJ2-6", "TRBV7-5_TRBJ2-7", "TRBV7-6_TRBJ1-1",
               "TRBV7-6_TRBJ1-2", "TRBV7-6_TRBJ1-3", "TRBV7-6_TRBJ1-4",
               "TRBV7-6_TRBJ1-5", "TRBV7-6_TRBJ1-6", "TRBV7-6_TRBJ2-1",
               "TRBV7-6_TRBJ2-2", "TRBV7-6_TRBJ2-3", "TRBV7-6_TRBJ2-4",
               "TRBV7-6_TRBJ2-5", "TRBV7-6_TRBJ2-6", "TRBV7-6_TRBJ2-7",
               "TRBV7-7_TRBJ1-1", "TRBV7-7_TRBJ1-2", "TRBV7-7_TRBJ1-3",
               "TRBV7-7_TRBJ1-4", "TRBV7-7_TRBJ1-5", "TRBV7-7_TRBJ1-6",
               "TRBV7-7_TRBJ2-1", "TRBV7-7_TRBJ2-2", "TRBV7-7_TRBJ2-3",
               "TRBV7-7_TRBJ2-4", "TRBV7-7_TRBJ2-5", "TRBV7-7_TRBJ2-6",
               "TRBV7-7_TRBJ2-7", "TRBV7-8_TRBJ1-1", "TRBV7-8_TRBJ1-2",
               "TRBV7-8_TRBJ1-3", "TRBV7-8_TRBJ1-4", "TRBV7-8_TRBJ2-1",
               "TRBV7-8_TRBJ2-2", "TRBV7-8_TRBJ2-3", "TRBV7-8_TRBJ2-4",
               "TRBV7-8_TRBJ2-5", "TRBV7-8_TRBJ2-6", "TRBV7-8_TRBJ2-7",
               "TRBV7-9_TRBJ1-1", "TRBV7-9_TRBJ1-2", "TRBV7-9_TRBJ1-3",
               "TRBV7-9_TRBJ1-4", "TRBV7-9_TRBJ1-5", "TRBV7-9_TRBJ1-6",
               "TRBV7-9_TRBJ2-1", "TRBV7-9_TRBJ2-2", "TRBV7-9_TRBJ2-3",
               "TRBV7-9_TRBJ2-4", "TRBV7-9_TRBJ2-5", "TRBV7-9_TRBJ2-6",
               "TRBV7-9_TRBJ2-7", "TRBV9_TRBJ1-1", "TRBV9_TRBJ1-2",
               "TRBV9_TRBJ1-3", "TRBV9_TRBJ1-4", "TRBV9_TRBJ1-5",
               "TRBV9_TRBJ1-6", "TRBV9_TRBJ2-1", "TRBV9_TRBJ2-2",
               "TRBV9_TRBJ2-3", "TRBV9_TRBJ2-4", "TRBV9_TRBJ2-5",
               "TRBV9_TRBJ2-6", "TRBV9_TRBJ2-7", days.from.first.symptoms,
               time, choose, "...7", comment)

update <- full[, -632:-686] 
#update <- update[-c(22, 94:109),] # keep the NA rows

# new col: recover/active = disease; healthy = healthy
new <- update %>%
  mutate(Y = case_when(
    diseae.stage %in% c("active", "recovered") ~ "disease",
    diseae.stage == "healthy" ~ "healthy",
    TRUE ~ NA_character_
  ))

# renaming col
final <- rename(new, Y1 = diseae.stage)

final <- select(final, patient.ID, Sample.ID, "TRBV10-1_TRBJ1-1", 
                "TRBV10-1_TRBJ1-2", "TRBV10-1_TRBJ1-3", "TRBV10-1_TRBJ1-4",
                "TRBV10-1_TRBJ1-5", "TRBV10-1_TRBJ1-6", "TRBV10-1_TRBJ2-1",
                "TRBV10-1_TRBJ2-2", "TRBV10-1_TRBJ2-3", "TRBV10-1_TRBJ2-4",
                "TRBV10-1_TRBJ2-5", "TRBV10-1_TRBJ2-6", "TRBV10-1_TRBJ2-7",
                "TRBV10-2_TRBJ1-1", "TRBV10-2_TRBJ1-2", "TRBV10-2_TRBJ1-3",
                "TRBV10-2_TRBJ1-4", "TRBV10-2_TRBJ1-5", "TRBV10-2_TRBJ1-6",
                "TRBV10-2_TRBJ2-1", "TRBV10-2_TRBJ2-2", "TRBV10-2_TRBJ2-3",
                "TRBV10-2_TRBJ2-4", "TRBV10-2_TRBJ2-5", "TRBV10-2_TRBJ2-6",
                "TRBV10-2_TRBJ2-7", "TRBV10-3_TRBJ1-1", "TRBV10-3_TRBJ1-2",
                "TRBV10-3_TRBJ1-3", "TRBV10-3_TRBJ1-4", "TRBV10-3_TRBJ1-5",
                "TRBV10-3_TRBJ1-6", "TRBV10-3_TRBJ2-1", "TRBV10-3_TRBJ2-2",
                "TRBV10-3_TRBJ2-3", "TRBV10-3_TRBJ2-4", "TRBV10-3_TRBJ2-5", 
                "TRBV10-3_TRBJ2-6", "TRBV10-3_TRBJ2-7", "TRBV11-1_TRBJ1-1",
                "TRBV11-1_TRBJ1-2", "TRBV11-1_TRBJ1-3", "TRBV11-1_TRBJ1-4",
                "TRBV11-1_TRBJ1-5", "TRBV11-1_TRBJ1-6", "TRBV11-1_TRBJ2-1",
                "TRBV11-1_TRBJ2-2", "TRBV11-1_TRBJ2-3", "TRBV11-1_TRBJ2-4",
                "TRBV11-1_TRBJ2-5", "TRBV11-1_TRBJ2-6", "TRBV11-1_TRBJ2-7",
                "TRBV11-2_TRBJ1-1", "TRBV11-2_TRBJ1-2", "TRBV11-2_TRBJ1-3",
                "TRBV11-2_TRBJ1-4", "TRBV11-2_TRBJ1-5", "TRBV11-2_TRBJ1-6",
                "TRBV11-2_TRBJ2-1", "TRBV11-2_TRBJ2-2", "TRBV11-2_TRBJ2-3",
                "TRBV11-2_TRBJ2-4", "TRBV11-2_TRBJ2-5", "TRBV11-2_TRBJ2-6",
                "TRBV11-2_TRBJ2-7", "TRBV11-3_TRBJ1-1", "TRBV11-3_TRBJ1-2",
                "TRBV11-3_TRBJ1-3", "TRBV11-3_TRBJ1-4", "TRBV11-3_TRBJ1-5",
                "TRBV11-3_TRBJ1-6", "TRBV11-3_TRBJ2-1", "TRBV11-3_TRBJ2-2", 
                "TRBV11-3_TRBJ2-3", "TRBV11-3_TRBJ2-4", "TRBV11-3_TRBJ2-5",
                "TRBV11-3_TRBJ2-6", "TRBV11-3_TRBJ2-7", "TRBV12-1_TRBJ1-1",
                "TRBV12-1_TRBJ1-2", "TRBV12-1_TRBJ1-5", "TRBV12-1_TRBJ2-2",
                "TRBV12-1_TRBJ2-3", "TRBV12-1_TRBJ2-4", "TRBV12-1_TRBJ2-5",
                "TRBV12-1_TRBJ2-7", "TRBV12-3_TRBJ1-1", "TRBV12-3_TRBJ1-2",
                "TRBV12-3_TRBJ1-3", "TRBV12-3_TRBJ1-4", "TRBV12-3_TRBJ1-5",
                "TRBV12-3_TRBJ1-6", "TRBV12-3_TRBJ2-1", "TRBV12-3_TRBJ2-2",
                "TRBV12-3_TRBJ2-3", "TRBV12-3_TRBJ2-4", "TRBV12-3_TRBJ2-5",
                "TRBV12-3_TRBJ2-6", "TRBV12-3_TRBJ2-7", "TRBV12-4_TRBJ1-1",
                "TRBV12-4_TRBJ1-2", "TRBV12-4_TRBJ1-3", "TRBV12-4_TRBJ1-4",
                "TRBV12-4_TRBJ1-5", "TRBV12-4_TRBJ1-6", "TRBV12-4_TRBJ2-1",
                "TRBV12-4_TRBJ2-2", "TRBV12-4_TRBJ2-3", "TRBV12-4_TRBJ2-4",
                "TRBV12-4_TRBJ2-5", "TRBV12-4_TRBJ2-6", "TRBV12-4_TRBJ2-7",
                "TRBV12-5_TRBJ1-1", "TRBV12-5_TRBJ1-2", "TRBV12-5_TRBJ1-3",
                "TRBV12-5_TRBJ1-4", "TRBV12-5_TRBJ1-5", "TRBV12-5_TRBJ1-6",
                "TRBV12-5_TRBJ2-1", "TRBV12-5_TRBJ2-2", "TRBV12-5_TRBJ2-3",
                "TRBV12-5_TRBJ2-4", "TRBV12-5_TRBJ2-5", "TRBV12-5_TRBJ2-6",
                "TRBV12-5_TRBJ2-7", "TRBV13_TRBJ1-1", "TRBV13_TRBJ1-2",
                "TRBV13_TRBJ1-3", "TRBV13_TRBJ1-4", "TRBV13_TRBJ1-5",
                "TRBV13_TRBJ1-6", "TRBV13_TRBJ2-1", "TRBV13_TRBJ2-2", 
                "TRBV13_TRBJ2-3", "TRBV13_TRBJ2-4", "TRBV13_TRBJ2-5",
                "TRBV13_TRBJ2-6", "TRBV13_TRBJ2-7", "TRBV14_TRBJ1-1",
                "TRBV14_TRBJ1-2", "TRBV14_TRBJ1-3", "TRBV14_TRBJ1-4",
                "TRBV14_TRBJ1-5", "TRBV14_TRBJ1-6", "TRBV14_TRBJ2-1",
                "TRBV14_TRBJ2-2", "TRBV14_TRBJ2-3", "TRBV14_TRBJ2-4",
                "TRBV14_TRBJ2-5", "TRBV14_TRBJ2-6", "TRBV14_TRBJ2-7",
                "TRBV15_TRBJ1-1", "TRBV15_TRBJ1-2", "TRBV15_TRBJ1-3",
                "TRBV15_TRBJ1-4", "TRBV15_TRBJ1-5", "TRBV15_TRBJ1-6", 
                "TRBV15_TRBJ2-1", "TRBV15_TRBJ2-2", "TRBV15_TRBJ2-3",
                "TRBV15_TRBJ2-4", "TRBV15_TRBJ2-5", "TRBV15_TRBJ2-6",
                "TRBV15_TRBJ2-7", "TRBV18_TRBJ1-1", "TRBV18_TRBJ1-2",
                "TRBV18_TRBJ1-3", "TRBV18_TRBJ1-4", "TRBV18_TRBJ1-5",
                "TRBV18_TRBJ1-6", "TRBV18_TRBJ2-1", "TRBV18_TRBJ2-2",
                "TRBV18_TRBJ2-3", "TRBV18_TRBJ2-4", "TRBV18_TRBJ2-5",
                "TRBV18_TRBJ2-6", "TRBV18_TRBJ2-7", "TRBV19_TRBJ1-1",
                "TRBV19_TRBJ1-2", "TRBV19_TRBJ1-3", "TRBV19_TRBJ1-4",
                "TRBV19_TRBJ1-5", "TRBV19_TRBJ1-6", "TRBV19_TRBJ2-1", 
                "TRBV19_TRBJ2-2",  "TRBV19_TRBJ2-3", "TRBV19_TRBJ2-4",
                "TRBV19_TRBJ2-5", "TRBV19_TRBJ2-6", "TRBV19_TRBJ2-7",
                "TRBV2_TRBJ1-1", "TRBV2_TRBJ1-2", "TRBV2_TRBJ1-3", 
                "TRBV2_TRBJ1-4", "TRBV2_TRBJ1-5", "TRBV2_TRBJ1-6",
                "TRBV2_TRBJ2-1", "TRBV2_TRBJ2-2", "TRBV2_TRBJ2-3",
                "TRBV2_TRBJ2-4", "TRBV2_TRBJ2-5", "TRBV2_TRBJ2-6", 
                "TRBV2_TRBJ2-7", "TRBV20-1_TRBJ1-1", "TRBV20-1_TRBJ1-2", 
                "TRBV20-1_TRBJ1-3", "TRBV20-1_TRBJ1-4", "TRBV20-1_TRBJ1-5",
                "TRBV20-1_TRBJ1-6", "TRBV20-1_TRBJ2-1", "TRBV20-1_TRBJ2-2",
                "TRBV20-1_TRBJ2-3", "TRBV20-1_TRBJ2-4", "TRBV20-1_TRBJ2-5",
                "TRBV20-1_TRBJ2-6", "TRBV20-1_TRBJ2-7", "TRBV21-1_TRBJ1-1",
                "TRBV21-1_TRBJ1-2", "TRBV21-1_TRBJ1-3", "TRBV21-1_TRBJ1-4",
                "TRBV21-1_TRBJ1-5", "TRBV21-1_TRBJ1-6", "TRBV21-1_TRBJ2-1",
                "TRBV21-1_TRBJ2-2", "TRBV21-1_TRBJ2-3", "TRBV21-1_TRBJ2-4",
                "TRBV21-1_TRBJ2-5", "TRBV21-1_TRBJ2-6", "TRBV21-1_TRBJ2-7", 
                "TRBV23-1_TRBJ1-1", "TRBV23-1_TRBJ1-2", "TRBV23-1_TRBJ1-4",
                "TRBV23-1_TRBJ1-5", "TRBV23-1_TRBJ1-6", "TRBV23-1_TRBJ2-1",
                "TRBV23-1_TRBJ2-2", "TRBV23-1_TRBJ2-3", "TRBV23-1_TRBJ2-4",
                "TRBV23-1_TRBJ2-5", "TRBV23-1_TRBJ2-6", "TRBV23-1_TRBJ2-7",
                "TRBV24-1_TRBJ1-1", "TRBV24-1_TRBJ1-2", "TRBV24-1_TRBJ1-3",
                "TRBV24-1_TRBJ1-4", "TRBV24-1_TRBJ1-5", "TRBV24-1_TRBJ1-6",
                "TRBV24-1_TRBJ2-1", "TRBV24-1_TRBJ2-2", "TRBV24-1_TRBJ2-3",
                "TRBV24-1_TRBJ2-4", "TRBV24-1_TRBJ2-5", "TRBV24-1_TRBJ2-6", 
                "TRBV24-1_TRBJ2-7", "TRBV25-1_TRBJ1-1", "TRBV25-1_TRBJ1-2",
                "TRBV25-1_TRBJ1-3", "TRBV25-1_TRBJ1-4", "TRBV25-1_TRBJ1-5",
                "TRBV25-1_TRBJ1-6", "TRBV25-1_TRBJ2-1", "TRBV25-1_TRBJ2-2",
                "TRBV25-1_TRBJ2-3", "TRBV25-1_TRBJ2-4", "TRBV25-1_TRBJ2-5", 
                "TRBV25-1_TRBJ2-6", "TRBV25-1_TRBJ2-7", "TRBV27_TRBJ1-1",
                "TRBV27_TRBJ1-2", "TRBV27_TRBJ1-3", "TRBV27_TRBJ1-4",
                "TRBV27_TRBJ1-5", "TRBV27_TRBJ1-6", "TRBV27_TRBJ2-1",
                "TRBV27_TRBJ2-2", "TRBV27_TRBJ2-3", "TRBV27_TRBJ2-4", 
                "TRBV27_TRBJ2-5", "TRBV27_TRBJ2-6", "TRBV27_TRBJ2-7", 
                "TRBV28_TRBJ1-1", "TRBV28_TRBJ1-2", "TRBV28_TRBJ1-3",
                "TRBV28_TRBJ1-4", "TRBV28_TRBJ1-5", "TRBV28_TRBJ1-6",
                "TRBV28_TRBJ2-1", "TRBV28_TRBJ2-2", "TRBV28_TRBJ2-3", 
                "TRBV28_TRBJ2-4", "TRBV28_TRBJ2-5", "TRBV28_TRBJ2-6",
                "TRBV28_TRBJ2-7", "TRBV29-1_TRBJ1-1", "TRBV29-1_TRBJ1-2",
                "TRBV29-1_TRBJ1-3", "TRBV29-1_TRBJ1-4", "TRBV29-1_TRBJ1-5",
                "TRBV29-1_TRBJ1-6", "TRBV29-1_TRBJ2-1", "TRBV29-1_TRBJ2-2",
                "TRBV29-1_TRBJ2-3", "TRBV29-1_TRBJ2-4", "TRBV29-1_TRBJ2-5",
                "TRBV29-1_TRBJ2-6", "TRBV29-1_TRBJ2-7", "TRBV3-2_TRBJ1-4",
                "TRBV3-2_TRBJ2-1", "TRBV30_TRBJ1-1", "TRBV30_TRBJ1-2",
                "TRBV30_TRBJ1-3", "TRBV30_TRBJ1-4", "TRBV30_TRBJ1-5", 
                "TRBV30_TRBJ1-6", "TRBV30_TRBJ2-1", "TRBV30_TRBJ2-2",
                "TRBV30_TRBJ2-3", "TRBV30_TRBJ2-4", "TRBV30_TRBJ2-5",
                "TRBV30_TRBJ2-6", "TRBV30_TRBJ2-7", "TRBV4-1_TRBJ1-1",
                "TRBV4-1_TRBJ1-2", "TRBV4-1_TRBJ1-3", "TRBV4-1_TRBJ1-4",
                "TRBV4-1_TRBJ1-5", "TRBV4-1_TRBJ1-6", "TRBV4-1_TRBJ2-1",
                "TRBV4-1_TRBJ2-2", "TRBV4-1_TRBJ2-3", "TRBV4-1_TRBJ2-4",
                "TRBV4-1_TRBJ2-5", "TRBV4-1_TRBJ2-6", "TRBV4-1_TRBJ2-7",
                "TRBV4-2_TRBJ1-1", "TRBV4-2_TRBJ1-2",  "TRBV4-2_TRBJ1-3",
                "TRBV4-2_TRBJ1-4", "TRBV4-2_TRBJ1-5", "TRBV4-2_TRBJ1-6",
                "TRBV4-2_TRBJ2-1", "TRBV4-2_TRBJ2-2", "TRBV4-2_TRBJ2-3",
                "TRBV4-2_TRBJ2-4", "TRBV4-2_TRBJ2-5", "TRBV4-2_TRBJ2-6",
                "TRBV4-2_TRBJ2-7", "TRBV4-3_TRBJ1-1", "TRBV4-3_TRBJ1-2", 
                "TRBV4-3_TRBJ1-3", "TRBV4-3_TRBJ1-4", "TRBV4-3_TRBJ1-5",
                "TRBV4-3_TRBJ1-6", "TRBV4-3_TRBJ2-1", "TRBV4-3_TRBJ2-2",
                "TRBV4-3_TRBJ2-3", "TRBV4-3_TRBJ2-4", "TRBV4-3_TRBJ2-5",
                "TRBV4-3_TRBJ2-6", "TRBV4-3_TRBJ2-7", "TRBV5-1_TRBJ1-1",
                "TRBV5-1_TRBJ1-2", "TRBV5-1_TRBJ1-3", "TRBV5-1_TRBJ1-4",
                "TRBV5-1_TRBJ1-5", "TRBV5-1_TRBJ1-6", "TRBV5-1_TRBJ2-1",
                "TRBV5-1_TRBJ2-2", "TRBV5-1_TRBJ2-3", "TRBV5-1_TRBJ2-4",
                "TRBV5-1_TRBJ2-5", "TRBV5-1_TRBJ2-6", "TRBV5-1_TRBJ2-7", 
                "TRBV5-3_TRBJ1-1", "TRBV5-3_TRBJ1-2", "TRBV5-3_TRBJ1-3",
                "TRBV5-3_TRBJ1-4", "TRBV5-3_TRBJ1-5", "TRBV5-3_TRBJ1-6",
                "TRBV5-3_TRBJ2-1", "TRBV5-3_TRBJ2-2", "TRBV5-3_TRBJ2-3",
                "TRBV5-3_TRBJ2-4", "TRBV5-3_TRBJ2-5", "TRBV5-3_TRBJ2-6",
                "TRBV5-3_TRBJ2-7", "TRBV5-4_TRBJ1-1", "TRBV5-4_TRBJ1-2",
                "TRBV5-4_TRBJ1-3", "TRBV5-4_TRBJ1-4",  "TRBV5-4_TRBJ1-5",
                "TRBV5-4_TRBJ1-6", "TRBV5-4_TRBJ2-1", "TRBV5-4_TRBJ2-2",
                "TRBV5-4_TRBJ2-3", "TRBV5-4_TRBJ2-4", "TRBV5-4_TRBJ2-5",
                "TRBV5-4_TRBJ2-6", "TRBV5-4_TRBJ2-7", "TRBV5-5_TRBJ1-1",
                "TRBV5-5_TRBJ1-2", "TRBV5-5_TRBJ1-3", "TRBV5-5_TRBJ1-4",
                "TRBV5-5_TRBJ1-5", "TRBV5-5_TRBJ1-6", "TRBV5-5_TRBJ2-1",
                "TRBV5-5_TRBJ2-2", "TRBV5-5_TRBJ2-3", "TRBV5-5_TRBJ2-4",
                "TRBV5-5_TRBJ2-5", "TRBV5-5_TRBJ2-6", "TRBV5-5_TRBJ2-7",
                "TRBV5-6_TRBJ1-1", "TRBV5-6_TRBJ1-2", "TRBV5-6_TRBJ1-3",
                "TRBV5-6_TRBJ1-4", "TRBV5-6_TRBJ1-5", "TRBV5-6_TRBJ1-6",
                "TRBV5-6_TRBJ2-1", "TRBV5-6_TRBJ2-2", "TRBV5-6_TRBJ2-3",
                "TRBV5-6_TRBJ2-4", "TRBV5-6_TRBJ2-5", "TRBV5-6_TRBJ2-6",
                "TRBV5-6_TRBJ2-7", "TRBV5-7_TRBJ1-1", "TRBV5-7_TRBJ1-2",
                "TRBV5-7_TRBJ1-3", "TRBV5-7_TRBJ1-4", "TRBV5-7_TRBJ1-5",
                "TRBV5-7_TRBJ1-6", "TRBV5-7_TRBJ2-1", "TRBV5-7_TRBJ2-2",
                "TRBV5-7_TRBJ2-3", "TRBV5-7_TRBJ2-4", "TRBV5-7_TRBJ2-5",
                "TRBV5-7_TRBJ2-6", "TRBV5-7_TRBJ2-7", "TRBV5-8_TRBJ1-1",
                "TRBV5-8_TRBJ1-2", "TRBV5-8_TRBJ1-3", "TRBV5-8_TRBJ1-4",
                "TRBV5-8_TRBJ1-5", "TRBV5-8_TRBJ1-6", "TRBV5-8_TRBJ2-1",
                "TRBV5-8_TRBJ2-2", "TRBV5-8_TRBJ2-3", "TRBV5-8_TRBJ2-4",
                "TRBV5-8_TRBJ2-5", "TRBV5-8_TRBJ2-6", "TRBV5-8_TRBJ2-7",
                "TRBV6-1_TRBJ1-1", "TRBV6-1_TRBJ1-2", "TRBV6-1_TRBJ1-3",
                "TRBV6-1_TRBJ1-4", "TRBV6-1_TRBJ1-5", "TRBV6-1_TRBJ1-6",
                "TRBV6-1_TRBJ2-1", "TRBV6-1_TRBJ2-2", "TRBV6-1_TRBJ2-3",
                "TRBV6-1_TRBJ2-4", "TRBV6-1_TRBJ2-5", "TRBV6-1_TRBJ2-6",
                "TRBV6-1_TRBJ2-7", "TRBV6-2_TRBJ1-1", "TRBV6-2_TRBJ1-2",
                "TRBV6-2_TRBJ1-5", "TRBV6-2_TRBJ1-6", "TRBV6-2_TRBJ2-1", 
                "TRBV6-2_TRBJ2-2", "TRBV6-2_TRBJ2-3", "TRBV6-2_TRBJ2-5",
                "TRBV6-2_TRBJ2-7", "TRBV6-3_TRBJ1-1", "TRBV6-3_TRBJ1-2",
                "TRBV6-3_TRBJ1-3", "TRBV6-3_TRBJ1-4", "TRBV6-3_TRBJ1-5",
                "TRBV6-3_TRBJ1-6", "TRBV6-3_TRBJ2-1", "TRBV6-3_TRBJ2-2",
                "TRBV6-3_TRBJ2-3", "TRBV6-3_TRBJ2-4", "TRBV6-3_TRBJ2-5",
                "TRBV6-3_TRBJ2-6", "TRBV6-3_TRBJ2-7", "TRBV6-4_TRBJ1-1",
                "TRBV6-4_TRBJ1-2", "TRBV6-4_TRBJ1-3", "TRBV6-4_TRBJ1-4",
                "TRBV6-4_TRBJ1-5", "TRBV6-4_TRBJ1-6", "TRBV6-4_TRBJ2-1", 
                "TRBV6-4_TRBJ2-2", "TRBV6-4_TRBJ2-3", "TRBV6-4_TRBJ2-4",
                "TRBV6-4_TRBJ2-5", "TRBV6-4_TRBJ2-6", "TRBV6-4_TRBJ2-7",
                "TRBV6-5_TRBJ1-1", "TRBV6-5_TRBJ1-2", "TRBV6-5_TRBJ1-3",
                "TRBV6-5_TRBJ1-4", "TRBV6-5_TRBJ1-5", "TRBV6-5_TRBJ1-6",
                "TRBV6-5_TRBJ2-1", "TRBV6-5_TRBJ2-2", "TRBV6-5_TRBJ2-3",
                "TRBV6-5_TRBJ2-4", "TRBV6-5_TRBJ2-5", "TRBV6-5_TRBJ2-6",
                "TRBV6-5_TRBJ2-7", "TRBV6-6_TRBJ1-1", "TRBV6-6_TRBJ1-2",
                "TRBV6-6_TRBJ1-3", "TRBV6-6_TRBJ1-4", "TRBV6-6_TRBJ1-5",
                "TRBV6-6_TRBJ1-6", "TRBV6-6_TRBJ2-1", "TRBV6-6_TRBJ2-2",
                "TRBV6-6_TRBJ2-3", "TRBV6-6_TRBJ2-4", "TRBV6-6_TRBJ2-5",
                "TRBV6-6_TRBJ2-6", "TRBV6-6_TRBJ2-7", "TRBV6-7_TRBJ1-1",
                "TRBV6-7_TRBJ1-2", "TRBV6-7_TRBJ1-3", "TRBV6-7_TRBJ1-4",
                "TRBV6-7_TRBJ1-5", "TRBV6-7_TRBJ1-6", "TRBV6-7_TRBJ2-1",
                "TRBV6-7_TRBJ2-2", "TRBV6-7_TRBJ2-3", "TRBV6-7_TRBJ2-5",
                "TRBV6-7_TRBJ2-6", "TRBV6-7_TRBJ2-7", "TRBV6-8_TRBJ1-1",
                "TRBV6-8_TRBJ1-2", "TRBV6-8_TRBJ1-3", "TRBV6-8_TRBJ1-4", 
                "TRBV6-8_TRBJ1-5", "TRBV6-8_TRBJ1-6", "TRBV6-8_TRBJ2-1",
                "TRBV6-8_TRBJ2-2", "TRBV6-8_TRBJ2-3", "TRBV6-8_TRBJ2-4",
                "TRBV6-8_TRBJ2-5", "TRBV6-8_TRBJ2-6", "TRBV6-8_TRBJ2-7",
                "TRBV6-9_TRBJ1-1", "TRBV6-9_TRBJ1-2", "TRBV6-9_TRBJ1-3",
                "TRBV6-9_TRBJ1-4", "TRBV6-9_TRBJ1-5", "TRBV6-9_TRBJ1-6",
                "TRBV6-9_TRBJ2-1", "TRBV6-9_TRBJ2-2", "TRBV6-9_TRBJ2-3",
                "TRBV6-9_TRBJ2-4", "TRBV6-9_TRBJ2-5", "TRBV6-9_TRBJ2-6",
                "TRBV6-9_TRBJ2-7", "TRBV7-2_TRBJ1-1", "TRBV7-2_TRBJ1-2",
                "TRBV7-2_TRBJ1-3", "TRBV7-2_TRBJ1-4", "TRBV7-2_TRBJ1-5",
                "TRBV7-2_TRBJ1-6", "TRBV7-2_TRBJ2-1", "TRBV7-2_TRBJ2-2",
                "TRBV7-2_TRBJ2-3", "TRBV7-2_TRBJ2-4", "TRBV7-2_TRBJ2-5",
                "TRBV7-2_TRBJ2-6", "TRBV7-2_TRBJ2-7", "TRBV7-3_TRBJ1-1", 
                "TRBV7-3_TRBJ1-2", "TRBV7-3_TRBJ1-3", "TRBV7-3_TRBJ1-4",
                "TRBV7-3_TRBJ1-5", "TRBV7-3_TRBJ1-6", "TRBV7-3_TRBJ2-1",
                "TRBV7-3_TRBJ2-2", "TRBV7-3_TRBJ2-3", "TRBV7-3_TRBJ2-4",
                "TRBV7-3_TRBJ2-5", "TRBV7-3_TRBJ2-6", "TRBV7-3_TRBJ2-7",
                "TRBV7-4_TRBJ1-1", "TRBV7-4_TRBJ1-2", "TRBV7-4_TRBJ1-3",
                "TRBV7-4_TRBJ1-4", "TRBV7-4_TRBJ1-5", "TRBV7-4_TRBJ1-6",
                "TRBV7-4_TRBJ2-1", "TRBV7-4_TRBJ2-2", "TRBV7-4_TRBJ2-3",
                "TRBV7-4_TRBJ2-4", "TRBV7-4_TRBJ2-5", "TRBV7-4_TRBJ2-6",
                "TRBV7-4_TRBJ2-7", "TRBV7-5_TRBJ1-1", "TRBV7-5_TRBJ1-2", 
                "TRBV7-5_TRBJ1-3", "TRBV7-5_TRBJ1-4", "TRBV7-5_TRBJ1-5", 
                "TRBV7-5_TRBJ1-6", "TRBV7-5_TRBJ2-1", "TRBV7-5_TRBJ2-2", 
                "TRBV7-5_TRBJ2-3", "TRBV7-5_TRBJ2-4", "TRBV7-5_TRBJ2-5", 
                "TRBV7-5_TRBJ2-6", "TRBV7-5_TRBJ2-7", "TRBV7-6_TRBJ1-1", 
                "TRBV7-6_TRBJ1-2", "TRBV7-6_TRBJ1-3", "TRBV7-6_TRBJ1-4", 
                "TRBV7-6_TRBJ1-5", "TRBV7-6_TRBJ1-6", "TRBV7-6_TRBJ2-1", 
                "TRBV7-6_TRBJ2-2", "TRBV7-6_TRBJ2-3", "TRBV7-6_TRBJ2-4", 
                "TRBV7-6_TRBJ2-5", "TRBV7-6_TRBJ2-6", "TRBV7-6_TRBJ2-7", Y, Y1)

# saving as excel file
#write.xlsx(final, 
#           "D:/Coding/R Storage/Summer TCR Project/TCR Datasets/fullgenes.xlsx", 
#           row.names = FALSE)
#write.csv(final, 
#          "D:/Coding/R Storage/Summer TCR Project/TCR Datasets/fullgenes.csv", 
#          row.names = FALSE)

################
# SKAT Attempt #
################

# Note from 6/16/2023 meeting: 
# figure out how to let code read the cols instead me manually doing 
# 710 mini datasets
# However, we run into the problem of the ending numbers if I wanted to do a
# for loop. There are some that don't have dashes, and I have no clue how to
# prep it for that. So, 50 mini datasets made. Not as bad as 710 unique combos

matGene <- as.matrix(gene)
attach(gene)

# Partial string to match in column names
# Only with the v part of the gene
string1 <- "TRBV10-1"
string2 <- "TRBV10-2"
string3 <- "TRBV10-3"
string4 <- "TRBV11-1"
string5 <- "TRBV11-2"
string6 <- "TRBV11-3"
string7 <- "TRBV12-1"
string8 <- "TRBV12-3"
string9 <- "TRBV12-4"
string10 <- "TRBV12-5"

string11 <- "TRBV13"
string12 <- "TRBV14"
string13 <- "TRBV15"
string14 <- "TRBV18"
string15 <- "TRBV19"
string16 <- "TRBV2"
string17 <- "TRBV20-1"
string18 <- "TRBV21-1"
string19 <- "TRBV23-1"
string20 <- "TRBV24-1"

string21 <- "TRBV25-1"
string22 <- "TRBV27"
string23 <- "TRBV28"
string24 <- "TRBV29-1"
string25 <- "TRBV3-2"
string26 <- "TRBV30"
string27 <- "TRBV4-1"
string28 <- "TRBV4-2"
string29 <- "TRBV4-3"
string30 <- "TRBV5-1"

string31 <- "TRBV5-3"
string32 <- "TRBV5-4"
string33 <- "TRBV5-5"
string34 <- "TRBV5-6"
string35 <- "TRBV5-7"
string36 <- "TRBV5-8"
string37 <- "TRBV6-1"
string38 <- "TRBV6-2"
string39 <- "TRBV6-3"
string40 <- "TRBV6-4"

string41 <- "TRBV6-5"
string42 <- "TRBV6-6"
string43 <- "TRBV6-7"
string44 <- "TRBV6-8"
string45 <- "TRBV6-9"
string46 <- "TRBV7-2"
string47 <- "TRBV7-3"
string48 <- "TRBV7-4"
string49 <- "TRBV7-5"
string50 <- "TRBV7-6"

# Find columns that match the partial string
col1 <- grep(string1, names(gene), value = TRUE)
col2 <- grep(string2, names(gene), value = TRUE)
col3 <- grep(string3, names(gene), value = TRUE)
col4 <- grep(string4, names(gene), value = TRUE)
col5 <- grep(string5, names(gene), value = TRUE)
col6 <- grep(string6, names(gene), value = TRUE)
col7 <- grep(string7, names(gene), value = TRUE)
col8 <- grep(string8, names(gene), value = TRUE)
col9 <- grep(string9, names(gene), value = TRUE)
col10 <- grep(string10, names(gene), value = TRUE)

col11 <- grep(string11, names(gene), value = TRUE)
col12 <- grep(string12, names(gene), value = TRUE)
col13 <- grep(string13, names(gene), value = TRUE)
col14 <- grep(string14, names(gene), value = TRUE)
col15 <- grep(string15, names(gene), value = TRUE)
col16 <- grep(string16, names(gene), value = TRUE)
col17 <- grep(string17, names(gene), value = TRUE)
col18 <- grep(string18, names(gene), value = TRUE)
col19 <- grep(string19, names(gene), value = TRUE)
col20 <- grep(string20, names(gene), value = TRUE)

col21 <- grep(string21, names(gene), value = TRUE)
col22 <- grep(string22, names(gene), value = TRUE)
col23 <- grep(string23, names(gene), value = TRUE)
col24 <- grep(string24, names(gene), value = TRUE)
col25 <- grep(string25, names(gene), value = TRUE)
col26 <- grep(string26, names(gene), value = TRUE)
col27 <- grep(string27, names(gene), value = TRUE)
col28 <- grep(string28, names(gene), value = TRUE)
col29 <- grep(string29, names(gene), value = TRUE)
col30 <- grep(string30, names(gene), value = TRUE)

col31 <- grep(string31, names(gene), value = TRUE)
col32 <- grep(string32, names(gene), value = TRUE)
col33 <- grep(string33, names(gene), value = TRUE)
col34 <- grep(string34, names(gene), value = TRUE)
col35 <- grep(string35, names(gene), value = TRUE)
col36 <- grep(string36, names(gene), value = TRUE)
col37 <- grep(string37, names(gene), value = TRUE)
col38 <- grep(string38, names(gene), value = TRUE)
col39 <- grep(string39, names(gene), value = TRUE)
col40 <- grep(string40, names(gene), value = TRUE)

col41 <- grep(string41, names(gene), value = TRUE)
col42 <- grep(string42, names(gene), value = TRUE)
col43 <- grep(string43, names(gene), value = TRUE)
col44 <- grep(string44, names(gene), value = TRUE)
col45 <- grep(string45, names(gene), value = TRUE)
col46 <- grep(string46, names(gene), value = TRUE)
col47 <- grep(string47, names(gene), value = TRUE)
col48 <- grep(string48, names(gene), value = TRUE)
col49 <- grep(string49, names(gene), value = TRUE)
col50 <- grep(string50, names(gene), value = TRUE)

# Subset the dataset with the matching columns
sub1 <- as.matrix(gene[, col1])
sub2 <- as.matrix(gene[, col2])
sub3 <- as.matrix(gene[, col3])
sub4 <- as.matrix(gene[, col4])
sub5 <- as.matrix(gene[, col5])
sub6 <- as.matrix(gene[, col6])
sub7 <- as.matrix(gene[, col7])
sub8 <- as.matrix(gene[, col8])
sub9 <- as.matrix(gene[, col9])
sub10 <- as.matrix(gene[, col10])

sub11 <- as.matrix(gene[, col11])
sub12 <- as.matrix(gene[, col12])
sub13 <- as.matrix(gene[, col13])
sub14 <- as.matrix(gene[, col14])
sub15 <- as.matrix(gene[, col15])
sub16 <- as.matrix(gene[, col16])
sub17 <- as.matrix(gene[, col17])
sub18 <- as.matrix(gene[, col18])
sub19 <- as.matrix(gene[, col19])
sub20 <- as.matrix(gene[, col20])

sub21 <- as.matrix(gene[, col21])
sub22 <- as.matrix(gene[, col22])
sub23 <- as.matrix(gene[, col23])
sub24 <- as.matrix(gene[, col24])
sub25 <- as.matrix(gene[, col25])
sub26 <- as.matrix(gene[, col26])
sub27 <- as.matrix(gene[, col27])
sub28 <- as.matrix(gene[, col28])
sub29 <- as.matrix(gene[, col29])
sub30 <- as.matrix(gene[, col30])

sub31 <- as.matrix(gene[, col31])
sub32 <- as.matrix(gene[, col32])
sub33 <- as.matrix(gene[, col33])
sub34 <- as.matrix(gene[, col34])
sub35 <- as.matrix(gene[, col35])
sub36 <- as.matrix(gene[, col36])
sub37 <- as.matrix(gene[, col37])
sub38 <- as.matrix(gene[, col38])
sub39 <- as.matrix(gene[, col39])
sub40 <- as.matrix(gene[, col40])

sub41 <- as.matrix(gene[, col41])
sub42 <- as.matrix(gene[, col42])
sub43 <- as.matrix(gene[, col43])
sub44 <- as.matrix(gene[, col44])
sub45 <- as.matrix(gene[, col45])
sub46 <- as.matrix(gene[, col46])
sub47 <- as.matrix(gene[, col47])
sub48 <- as.matrix(gene[, col48])
sub49 <- as.matrix(gene[, col49])
sub50 <- as.matrix(gene[, col50])

### SKAT ###

# Use SKATBinary() for this entire thing
# ask what the x was
# in need of help to write out model
# also the dataset needed to be a matrix, so I'm running into errors

# Note 6/22/2023: For the NAs in dataset,fill them in
# try making the SKATBin in a for loop

#outv10.1 <- SKAT(final[,1:13],objv10.1)
#set.na <- c(22,94:109)
#Y <- gene$Y
#Y[set.na] <- "healthy"
#one.vec <- rep(1,length(Y))
#Y.d=rep(0,length(Y))
#Y.d[which(Y=="disease")]=1
#obj.s <- SKAT_Null_Model(Y.d ~ 1, out_type = "D")
#out <- SKATBinary(as.matrix(sub1), obj.s, kernel = "linear.weighted")
#out$p.value
