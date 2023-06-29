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

wideComb <- pivot_wider(longComb, names_from = "vjGene", values_from = "Value")

# combine long & patients
#full <- merge(longComb, patients, by = "Sample.ID")
#full <- full %>%
#  arrange(vjGene)
# log transform
#full$log.value <- log(full$Value)
# min-max standard/normalization
#min_val <- min(full$log.value)
#max_val <- max(full$log.value)
#full$normLog <- (full$log.value - min_val) / (max_val - min_val)
# reorder
#genes <- select(full, patient.ID, choose, Sample.ID, vjGene, Value, log.value, 
#                normLog, diseae.stage, days.from.first.symptoms, time, ...7,
#                comment)  
#View(genes)

# combine wide & patients
full <- merge(wideComb, patients, by = "Sample.ID", all = TRUE)

#######################
# How we want dataset #
#######################

# Patient | VJ1 | VJ2 |    Y    |   Y1   |
#  Pat 1  | ... | ... |    HD   |   HD   |
#  Pat 2  | ... | ... | Disease | Active | 

update <- full[, -c(5, 7:10)]

# new col: recover/active = disease; healthy = healthy
update1 <- update %>%
  mutate(Y = case_when(
    diseae.stage %in% c("active", "recovered") ~ "disease",
    diseae.stage == "healthy" ~ "healthy",
    TRUE ~ NA_character_
  ))

# vjGene columns
update2 <- pivot_wider(update1, names_from = vjGene, values_from = Value) 

# renaming col
update <- rename(update, Y1 = diseae.stage)

# reorder
update <- select(update, patient.ID, Sample.ID, "TRBV10-1_TRBJ1-1",
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
                 "TRBV9_TRBJ2-6", "TRBV9_TRBJ2-7", log.value, normLog, Y, Y1)

View(update)

# saving as excel file
#write.xlsx(genes, 
#           "D:/Coding/R Storage/Summer TCR Project/TCR Datasets/fullgenes.xlsx", 
#           row.names = FALSE)
#write.csv(genes, 
#          "D:/Coding/R Storage/Summer TCR Project/TCR Datasets/fullgenes.csv", 
#          row.names = FALSE)