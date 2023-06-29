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

# reorder
genes <- select(full, patient.ID, choose, Sample.ID, vjGene, Value, log.value, 
                normLog, diseae.stage, days.from.first.symptoms, time, ...7,
                comment)  
View(genes)

#################
# Mini datasets #
#################

vj <- genes$vjGene

mini1 <- genes[vj == "TRBV10-1_TRBJ1-1", ]
mini2 <- genes[vj == "TRBV10-1_TRBJ1-2", ]
mini3 <- genes[vj == "TRBV10-1_TRBJ1-3", ]
mini4 <- genes[vj == "TRBV10-1_TRBJ1-4", ]
mini5 <- genes[vj == "TRBV10-1_TRBJ1-5", ]
mini6 <- genes[vj == "TRBV10-1_TRBJ1-6", ]
mini7 <- genes[vj == "TRBV10-1_TRBJ2-1", ]
mini8 <- genes[vj == "TRBV10-1_TRBJ2-2", ]
mini9 <- genes[vj == "TRBV10-1_TRBJ2-3", ]
mini10 <- genes[vj == "TRBV10-1_TRBJ2-4", ]

mini11 <- genes[vj == "TRBV10-1_TRBJ2-5", ]
mini12 <- genes[vj == "TRBV10-1_TRBJ2-6", ]
mini13 <- genes[vj == "TRBV10-1_TRBJ2-7", ]
mini14 <- genes[vj == "TRBV10-2_TRBJ1-1", ]
mini15 <- genes[vj == "TRBV10-2_TRBJ1-2", ]
mini16 <- genes[vj == "TRBV10-2_TRBJ1-3", ]
mini17 <- genes[vj == "TRBV10-2_TRBJ1-4", ]
mini18 <- genes[vj == "TRBV10-2_TRBJ1-5", ]
mini19 <- genes[vj == "TRBV10-2_TRBJ1-6", ]
mini20 <- genes[vj == "TRBV10-2_TRBJ2-1", ]

mini21 <- genes[vj == "TRBV10-2_TRBJ2-2", ]
mini22 <- genes[vj == "TRBV10-2_TRBJ2-3", ]
mini23 <- genes[vj == "TRBV10-2_TRBJ2-4", ]
mini24 <- genes[vj == "TRBV10-2_TRBJ2-5", ]
mini25 <- genes[vj == "TRBV10-2_TRBJ2-6", ]
mini26 <- genes[vj == "TRBV10-2_TRBJ2-7", ]
mini27 <- genes[vj == "TRBV10-3_TRBJ1-1", ]
mini28 <- genes[vj == "TRBV10-3_TRBJ1-2", ]
mini29 <- genes[vj == "TRBV10-3_TRBJ1-3", ]
mini30 <- genes[vj == "TRBV10-3_TRBJ1-4", ]

mini31 <- genes[vj == "TRBV10-3_TRBJ1-5", ]
mini32 <- genes[vj == "TRBV10-3_TRBJ1-6", ]
mini33 <- genes[vj == "TRBV10-3_TRBJ2-1", ]
mini34 <- genes[vj == "TRBV10-3_TRBJ2-2", ]
mini35 <- genes[vj == "TRBV10-3_TRBJ2-3", ]
mini36 <- genes[vj == "TRBV10-3_TRBJ2-4", ]
mini37 <- genes[vj == "TRBV10-3_TRBJ2-5", ]
mini38 <- genes[vj == "TRBV10-3_TRBJ2-6", ]
mini39 <- genes[vj == "TRBV10-3_TRBJ2-7", ]
mini40 <- genes[vj == "TRBV11-1_TRBJ1-1", ]

mini41 <- genes[vj == "TRBV11-1_TRBJ1-2", ]
mini42 <- genes[vj == "TRBV11-1_TRBJ1-3", ]
mini43 <- genes[vj == "TRBV11-1_TRBJ1-4", ]
mini44 <- genes[vj == "TRBV11-1_TRBJ1-5", ]
mini45 <- genes[vj == "TRBV11-1_TRBJ1-6", ]
mini46 <- genes[vj == "TRBV11-1_TRBJ2-1", ]
mini47 <- genes[vj == "TRBV11-1_TRBJ2-2", ]
mini48 <- genes[vj == "TRBV11-1_TRBJ2-3", ]
mini49 <- genes[vj == "TRBV11-1_TRBJ2-4", ]
mini50 <- genes[vj == "TRBV11-1_TRBJ2-5", ]

mini51 <- genes[vj == "TRBV11-1_TRBJ2-6", ]
mini52 <- genes[vj == "TRBV11-1_TRBJ2-7", ]
mini53 <- genes[vj == "TRBV11-2_TRBJ1-1", ]
mini54 <- genes[vj == "TRBV11-2_TRBJ1-2", ]
mini55 <- genes[vj == "TRBV11-2_TRBJ1-3", ]
mini56 <- genes[vj == "TRBV11-2_TRBJ1-4", ]
mini57 <- genes[vj == "TRBV11-2_TRBJ1-5", ]
mini58 <- genes[vj == "TRBV11-2_TRBJ1-6", ]
mini59 <- genes[vj == "TRBV11-2_TRBJ2-1", ]
mini60 <- genes[vj == "TRBV11-2_TRBJ2-2", ]

mini61 <- genes[vj == "TRBV11-2_TRBJ2-3", ]
mini62 <- genes[vj == "TRBV11-2_TRBJ2-4", ]
mini63 <- genes[vj == "TRBV11-2_TRBJ2-5", ]
mini64 <- genes[vj == "TRBV11-2_TRBJ2-6", ]
mini65 <- genes[vj == "TRBV11-2_TRBJ2-7", ]
mini66 <- genes[vj == "TRBV11-3_TRBJ1-1", ]
mini67 <- genes[vj == "TRBV11-3_TRBJ1-2", ]
mini68 <- genes[vj == "TRBV11-3_TRBJ1-3", ]
mini69 <- genes[vj == "TRBV11-3_TRBJ1-4", ]
mini70 <- genes[vj == "TRBV11-3_TRBJ1-5", ]

mini71 <- genes[vj == "TRBV11-3_TRBJ1-6", ]
mini72 <- genes[vj == "TRBV11-3_TRBJ2-1", ]
mini73 <- genes[vj == "TRBV11-3_TRBJ2-2", ]
mini74 <- genes[vj == "TRBV11-3_TRBJ2-3", ]
mini75 <- genes[vj == "TRBV11-3_TRBJ2-4", ]
mini76 <- genes[vj == "TRBV11-3_TRBJ2-5", ]
mini77 <- genes[vj == "TRBV11-3_TRBJ2-6", ]
mini78 <- genes[vj == "TRBV11-3_TRBJ2-7", ]
mini79 <- genes[vj == "TRBV12-1_TRBJ1-1", ]
mini80 <- genes[vj == "TRBV12-1_TRBJ1-2", ]

mini81 <- genes[vj == "TRBV12-1_TRBJ1-5", ]
mini82 <- genes[vj == "TRBV12-1_TRBJ2-2", ]
mini83 <- genes[vj == "TRBV12-1_TRBJ2-3", ]
mini84 <- genes[vj == "TRBV12-1_TRBJ2-4", ]
mini85 <- genes[vj == "TRBV12-1_TRBJ2-5", ]
mini86 <- genes[vj == "TRBV12-1_TRBJ2-7", ]
mini87 <- genes[vj == "TRBV12-3_TRBJ1-1", ]
mini88 <- genes[vj == "TRBV12-3_TRBJ1-2", ]
mini89 <- genes[vj == "TRBV12-3_TRBJ1-3", ]
mini90 <- genes[vj == "TRBV12-3_TRBJ1-4", ]

mini91 <- genes[vj == "TRBV12-3_TRBJ1-5", ]
mini92 <- genes[vj == "TRBV12-3_TRBJ1-6", ]
mini93 <- genes[vj == "TRBV12-3_TRBJ2-1", ]
mini94 <- genes[vj == "TRBV12-3_TRBJ2-2", ]
mini95 <- genes[vj == "TRBV12-3_TRBJ2-3", ]
mini96 <- genes[vj == "TRBV12-3_TRBJ2-4", ]
mini97 <- genes[vj == "TRBV12-3_TRBJ2-5", ]
mini98 <- genes[vj == "TRBV12-3_TRBJ2-6", ]
mini99 <- genes[vj == "TRBV12-3_TRBJ2-7", ]
mini100 <- genes[vj == "TRBV12-4_TRBJ1-1", ]

mini101 <- genes[vj == "TRBV12-4_TRBJ1-2", ]
mini102 <- genes[vj == "TRBV12-4_TRBJ1-3", ]
mini103 <- genes[vj == "TRBV12-4_TRBJ1-4", ]
mini104 <- genes[vj == "TRBV12-4_TRBJ1-5", ]
mini105 <- genes[vj == "TRBV12-4_TRBJ1-6", ]
mini106 <- genes[vj == "TRBV12-4_TRBJ2-1", ]
mini107 <- genes[vj == "TRBV12-4_TRBJ2-2", ]
mini108 <- genes[vj == "TRBV12-4_TRBJ2-3", ]
mini109 <- genes[vj == "TRBV12-4_TRBJ2-4", ]
mini110 <- genes[vj == "TRBV12-4_TRBJ2-5", ]

mini111 <- genes[vj == "TRBV12-4_TRBJ2-6", ]
mini112 <- genes[vj == "TRBV12-4_TRBJ2-7", ]
mini113 <- genes[vj == "TRBV12-5_TRBJ1-1", ]
mini114 <- genes[vj == "TRBV12-5_TRBJ1-2", ]
mini115 <- genes[vj == "TRBV12-5_TRBJ1-3", ]
mini116 <- genes[vj == "TRBV12-5_TRBJ1-4", ]
mini117 <- genes[vj == "TRBV12-5_TRBJ1-5", ]
mini118 <- genes[vj == "TRBV12-5_TRBJ1-6", ]
mini119 <- genes[vj == "TRBV12-4_TRBJ2-4", ]
mini120 <- genes[vj == "TRBV12-5_TRBJ2-1", ]

mini121 <- genes[vj == "TRBV12-5_TRBJ2-2", ]
mini122 <- genes[vj == "TRBV12-5_TRBJ2-3", ]
mini123 <- genes[vj == "TRBV12-5_TRBJ2-4", ]
mini124 <- genes[vj == "TRBV12-5_TRBJ2-5", ]
mini125 <- genes[vj == "TRBV12-5_TRBJ2-6", ]
mini126 <- genes[vj == "TRBV12-5_TRBJ2-7", ]
mini127 <- genes[vj == "TRBV13_TRBJ1-1", ]
mini128 <- genes[vj == "TRBV13_TRBJ1-2", ]
mini129 <- genes[vj == "TRBV13_TRBJ1-3", ]
mini130 <- genes[vj == "TRBV13_TRBJ1-4", ]

mini131 <- genes[vj == "TRBV13_TRBJ1-5", ]
mini132 <- genes[vj == "TRBV13_TRBJ1-6", ]
mini133 <- genes[vj == "TRBV13_TRBJ2-1", ]
mini134 <- genes[vj == "TRBV13_TRBJ2-2", ]
mini135 <- genes[vj == "TRBV13_TRBJ2-3", ]
mini136 <- genes[vj == "TRBV13_TRBJ2-4", ]
mini137 <- genes[vj == "TRBV13_TRBJ2-5", ]
mini138 <- genes[vj == "TRBV13_TRBJ2-6", ]
mini139 <- genes[vj == "TRBV13_TRBJ2-7", ]
mini140 <- genes[vj == "TRBV14_TRBJ1-1", ]

mini141 <- genes[vj == "TRBV14_TRBJ1-2", ]
mini142 <- genes[vj == "TRBV14_TRBJ1-3", ]
mini143 <- genes[vj == "TRBV14_TRBJ1-4", ]
mini144 <- genes[vj == "TRBV14_TRBJ1-5", ]
mini145 <- genes[vj == "TRBV14_TRBJ1-6", ]
mini146 <- genes[vj == "TRBV14_TRBJ2-1", ]
mini147 <- genes[vj == "TRBV14_TRBJ2-2", ]
mini148 <- genes[vj == "TRBV14_TRBJ2-3", ]
mini149 <- genes[vj == "TRBV14_TRBJ2-4", ]
mini150 <- genes[vj == "TRBV14_TRBJ2-5", ]

mini151 <- genes[vj == "TRBV14_TRBJ2-6", ]
mini152 <- genes[vj == "TRBV14_TRBJ2-7", ]
mini153 <- genes[vj == "TRBV15_TRBJ1-1", ]
mini154 <- genes[vj == "TRBV15_TRBJ1-2", ]
mini155 <- genes[vj == "TRBV15_TRBJ1-3", ]
mini156 <- genes[vj == "TRBV15_TRBJ1-4", ]
mini157 <- genes[vj == "TRBV15_TRBJ1-5", ]
mini158 <- genes[vj == "TRBV15_TRBJ1-6", ]
mini159 <- genes[vj == "TRBV15_TRBJ2-1", ]
mini160 <- genes[vj == "TRBV15_TRBJ2-2", ]

mini161 <- genes[vj == "TRBV15_TRBJ2-3", ]
mini162 <- genes[vj == "TRBV15_TRBJ2-4", ]
mini163 <- genes[vj == "TRBV15_TRBJ2-5", ]
mini164 <- genes[vj == "TRBV15_TRBJ2-6", ]
mini165 <- genes[vj == "TRBV15_TRBJ2-7", ]
mini166 <- genes[vj == "TRBV18_TRBJ1-1", ]
mini167 <- genes[vj == "TRBV18_TRBJ1-2", ]
mini168 <- genes[vj == "TRBV18_TRBJ1-3", ]
mini169 <- genes[vj == "TRBV18_TRBJ1-4", ]
mini170 <- genes[vj == "TRBV18_TRBJ1-5", ]

mini171 <- genes[vj == "TRBV18_TRBJ1-6", ]
mini172 <- genes[vj == "TRBV18_TRBJ2-1", ]
mini173 <- genes[vj == "TRBV18_TRBJ2-2", ]
mini174 <- genes[vj == "TRBV18_TRBJ2-3", ]
mini175 <- genes[vj == "TRBV18_TRBJ2-4", ]
mini176 <- genes[vj == "TRBV18_TRBJ2-5", ]
mini177 <- genes[vj == "TRBV18_TRBJ2-6", ]
mini178 <- genes[vj == "TRBV18_TRBJ2-7", ]
mini179 <- genes[vj == "TRBV19_TRBJ1-1", ]
mini180 <- genes[vj == "TRBV19_TRBJ1-2", ]

mini191 <- genes[vj == "TRBV19_TRBJ1-3", ]
mini192 <- genes[vj == "TRBV19_TRBJ1-4", ]
mini193 <- genes[vj == "TRBV19_TRBJ1-5", ]
mini194 <- genes[vj == "TRBV19_TRBJ1-6", ]
mini195 <- genes[vj == "TRBV19_TRBJ2-1", ]
mini196 <- genes[vj == "TRBV19_TRBJ2-2", ]
mini197 <- genes[vj == "TRBV19_TRBJ2-3", ]
mini198 <- genes[vj == "TRBV19_TRBJ2-4", ]
mini199 <- genes[vj == "TRBV19_TRBJ2-5", ]
mini200 <- genes[vj == "TRBV19_TRBJ2-6", ]

mini201 <- genes[vj == "TRBV19_TRBJ2-7", ]
mini202 <- genes[vj == "TRBV2_TRBJ1-1", ]
mini203 <- genes[vj == "TRBV2_TRBJ1-2", ]
mini204 <- genes[vj == "TRBV2_TRBJ1-3", ]
mini205 <- genes[vj == "TRBV2_TRBJ1-4", ]
mini206 <- genes[vj == "TRBV2_TRBJ1-5", ]
mini207 <- genes[vj == "TRBV2_TRBJ1-6", ]
mini208 <- genes[vj == "TRBV2_TRBJ2-1", ]
mini209 <- genes[vj == "TRBV2_TRBJ2-2", ]
mini210 <- genes[vj == "TRBV2_TRBJ2-3", ]

mini211 <- genes[vj == "TRBV2_TRBJ2-4", ]
mini212 <- genes[vj == "TRBV2_TRBJ2-5", ]
mini213 <- genes[vj == "TRBV2_TRBJ2-6", ]
mini214 <- genes[vj == "TRBV2_TRBJ2-7", ]
mini215 <- genes[vj == "TRBV20-1_TRBJ1-1", ]
mini216 <- genes[vj == "TRBV20-1_TRBJ1-2", ]
mini217 <- genes[vj == "TRBV20-1_TRBJ1-3", ]
mini218 <- genes[vj == "TRBV20-1_TRBJ1-4", ]
mini219 <- genes[vj == "TRBV20-1_TRBJ1-5", ]
mini220 <- genes[vj == "TRBV20-1_TRBJ1-6", ]

mini221 <- genes[vj == "TRBV20-1_TRBJ2-1", ]
mini222 <- genes[vj == "TRBV20-1_TRBJ2-2", ]
mini223 <- genes[vj == "TRBV20-1_TRBJ2-3", ]
mini224 <- genes[vj == "TRBV20-1_TRBJ2-4", ]
mini225 <- genes[vj == "TRBV20-1_TRBJ2-5", ]
mini226 <- genes[vj == "TRBV20-1_TRBJ2-6", ]
mini227 <- genes[vj == "TRBV20-1_TRBJ2-7", ]
mini228 <- genes[vj == "TRBV21-1_TRBJ1-1", ]
mini229 <- genes[vj == "TRBV21-1_TRBJ1-2", ]
mini230 <- genes[vj == "TRBV21-1_TRBJ1-3", ]

mini231 <- genes[vj == "TRBV21-1_TRBJ1-4", ]
mini232 <- genes[vj == "TRBV21-1_TRBJ1-5", ]
mini233 <- genes[vj == "TRBV21-1_TRBJ1-6", ]
mini234 <- genes[vj == "TRBV21-1_TRBJ2-1", ]
mini235 <- genes[vj == "TRBV21-1_TRBJ2-2", ]
mini236 <- genes[vj == "TRBV21-1_TRBJ2-3", ]
mini237 <- genes[vj == "TRBV21-1_TRBJ2-4", ]
mini238 <- genes[vj == "TRBV21-1_TRBJ2-5", ]
mini239 <- genes[vj == "TRBV21-1_TRBJ2-6", ]
mini240 <- genes[vj == "TRBV21-1_TRBJ2-7", ]

mini241 <- genes[vj == "TRBV23-1_TRBJ1-1", ]
mini242 <- genes[vj == "TRBV23-1_TRBJ1-2", ]
mini243 <- genes[vj == "TRBV23-1_TRBJ1-4", ]
mini244 <- genes[vj == "TRBV23-1_TRBJ1-5", ]
mini245 <- genes[vj == "TRBV23-1_TRBJ1-6", ]
mini246 <- genes[vj == "TRBV23-1_TRBJ2-1", ]
mini247 <- genes[vj == "TRBV23-1_TRBJ2-2", ]
mini248 <- genes[vj == "TRBV23-1_TRBJ2-3", ]
mini249 <- genes[vj == "TRBV23-1_TRBJ2-4", ]
mini250 <- genes[vj == "TRBV23-1_TRBJ2-5", ]

mini261 <- genes[vj == "TRBV23-1_TRBJ2-6", ]
mini262 <- genes[vj == "TRBV23-1_TRBJ2-7", ]
mini263 <- genes[vj == "TRBV24-1_TRBJ1-1", ]
mini264 <- genes[vj == "TRBV24-1_TRBJ1-2", ]
mini265 <- genes[vj == "TRBV24-1_TRBJ1-3", ]
mini266 <- genes[vj == "TRBV24-1_TRBJ1-4", ]
mini267 <- genes[vj == "TRBV24-1_TRBJ1-5", ]
mini268 <- genes[vj == "TRBV24-1_TRBJ1-6", ]
mini269 <- genes[vj == "TRBV24-1_TRBJ2-1", ]
mini270 <- genes[vj == "TRBV24-1_TRBJ2-2", ]

mini271 <- genes[vj == "TRBV24-1_TRBJ2-3", ]
mini272 <- genes[vj == "TRBV24-1_TRBJ2-4", ]
mini273 <- genes[vj == "TRBV24-1_TRBJ2-5", ]
mini274 <- genes[vj == "TRBV24-1_TRBJ2-6", ]
mini275 <- genes[vj == "TRBV24-1_TRBJ2-7", ]
mini276 <- genes[vj == "TRBV25-1_TRBJ1-1", ]
mini277 <- genes[vj == "TRBV25-1_TRBJ1-2", ]
mini278 <- genes[vj == "TRBV25-1_TRBJ1-3", ]
mini279 <- genes[vj == "TRBV25-1_TRBJ1-4", ]
mini280 <- genes[vj == "TRBV25-1_TRBJ1-5", ]

mini281 <- genes[vj == "TRBV25-1_TRBJ1-6", ]
mini282 <- genes[vj == "TRBV25-1_TRBJ2-1", ]
mini283 <- genes[vj == "TRBV25-1_TRBJ2-2", ]
mini284 <- genes[vj == "TRBV25-1_TRBJ2-3", ]
mini285 <- genes[vj == "TRBV25-1_TRBJ2-4", ]
mini286 <- genes[vj == "TRBV25-1_TRBJ2-5", ]
mini287 <- genes[vj == "TRBV25-1_TRBJ2-6", ]
mini288 <- genes[vj == "TRBV25-1_TRBJ2-7", ]
mini289 <- genes[vj == "TRBV27_TRBJ1-1", ]
mini290 <- genes[vj == "TRBV27_TRBJ1-2", ]

mini291 <- genes[vj == "TRBV27_TRBJ1-3", ]
mini292 <- genes[vj == "TRBV27_TRBJ1-4", ]
mini293 <- genes[vj == "TRBV27_TRBJ1-5", ]
mini294 <- genes[vj == "TRBV27_TRBJ1-6", ]
mini295 <- genes[vj == "TRBV27_TRBJ2-1", ]
mini296 <- genes[vj == "TRBV27_TRBJ2-2", ]
mini297 <- genes[vj == "TRBV27_TRBJ2-3", ]
mini298 <- genes[vj == "TRBV27_TRBJ2-4", ]
mini299 <- genes[vj == "TRBV27_TRBJ2-5", ]
mini300 <- genes[vj == "TRBV27_TRBJ2-6", ]

mini301 <- genes[vj == "TRBV27_TRBJ2-7", ]
mini302 <- genes[vj == "TRBV28_TRBJ1-1", ]
mini303 <- genes[vj == "TRBV28_TRBJ1-2", ]
mini304 <- genes[vj == "TRBV28_TRBJ1-3", ]
mini305 <- genes[vj == "TRBV28_TRBJ1-4", ]
mini306 <- genes[vj == "TRBV28_TRBJ1-5", ]
mini307 <- genes[vj == "TRBV28_TRBJ1-6", ]
mini308 <- genes[vj == "TRBV28_TRBJ2-1", ]
mini309 <- genes[vj == "TRBV28_TRBJ2-2", ]
mini310 <- genes[vj == "TRBV28_TRBJ2-3", ]

mini311 <- genes[vj == "TRBV28_TRBJ2-4", ]
mini312 <- genes[vj == "TRBV28_TRBJ2-5", ]
mini313 <- genes[vj == "TRBV28_TRBJ2-6", ]
mini314 <- genes[vj == "TRBV28_TRBJ2-7", ]
mini315 <- genes[vj == "TRBV28_TRBJ1-4", ]
mini316 <- genes[vj == "TRBV29-1_TRBJ1-1", ]
mini317 <- genes[vj == "TRBV29-1_TRBJ1-2", ]
mini318 <- genes[vj == "TRBV29-1_TRBJ1-3", ]
mini319 <- genes[vj == "TRBV29-1_TRBJ1-4", ]
mini320 <- genes[vj == "TRBV29-1_TRBJ1-5", ]

mini321 <- genes[vj == "TRBV29-1_TRBJ1-6", ]
mini322 <- genes[vj == "TRBV29-1_TRBJ2-1", ]
mini323 <- genes[vj == "TRBV29-1_TRBJ2-2", ]
mini324 <- genes[vj == "TRBV29-1_TRBJ2-3", ]
mini325 <- genes[vj == "TRBV29-1_TRBJ2-4", ]
mini326 <- genes[vj == "TRBV29-1_TRBJ2-5", ]
mini327 <- genes[vj == "TRBV29-1_TRBJ2-6", ]
mini328 <- genes[vj == "TRBV29-1_TRBJ2-7", ]
mini329 <- genes[vj == "TRBV3-2_TRBJ1-4", ]
mini330 <- genes[vj == "TRBV3-2_TRBJ2-1", ]

mini331 <- genes[vj == "TRBV30_TRBJ1-1", ]
mini332 <- genes[vj == "TRBV30_TRBJ1-2", ]
mini333 <- genes[vj == "TRBV30_TRBJ1-3", ]
mini334 <- genes[vj == "TRBV30_TRBJ1-4", ]
mini335 <- genes[vj == "TRBV30_TRBJ1-5", ]
mini336 <- genes[vj == "TRBV30_TRBJ1-6", ]
mini337 <- genes[vj == "TRBV30_TRBJ2-1", ]
mini338 <- genes[vj == "TRBV30_TRBJ2-2", ]
mini339 <- genes[vj == "TRBV30_TRBJ2-3", ]
mini340 <- genes[vj == "TRBV30_TRBJ2-4", ]

mini341 <- genes[vj == "TRBV30_TRBJ2-5", ]
mini342 <- genes[vj == "TRBV30_TRBJ2-6", ]
mini343 <- genes[vj == "TRBV30_TRBJ2-7", ]
mini344 <- genes[vj == "TRBV4-1_TRBJ1-1", ]
mini345 <- genes[vj == "TRBV4-1_TRBJ1-2", ]
mini346 <- genes[vj == "TRBV4-1_TRBJ1-3", ]
mini347 <- genes[vj == "TRBV4-1_TRBJ1-4", ]
mini348 <- genes[vj == "TRBV4-1_TRBJ1-5", ]
mini349 <- genes[vj == "TRBV4-1_TRBJ1-6", ]
mini350 <- genes[vj == "TRBV4-1_TRBJ2-1", ]

mini351 <- genes[vj == "TRBV4-1_TRBJ2-2", ]
mini352 <- genes[vj == "TRBV4-1_TRBJ2-3", ]
mini353 <- genes[vj == "TRBV4-1_TRBJ2-4", ]
mini354 <- genes[vj == "TRBV4-1_TRBJ2-5", ]
mini355 <- genes[vj == "TRBV4-1_TRBJ2-6", ]
mini356 <- genes[vj == "TRBV4-1_TRBJ2-7", ]
mini357 <- genes[vj == "TRBV4-2_TRBJ1-1", ]
mini358 <- genes[vj == "TRBV4-2_TRBJ1-2", ]
mini359 <- genes[vj == "TRBV4-2_TRBJ1-3", ]
mini360 <- genes[vj == "TRBV4-2_TRBJ1-4", ]

mini361 <- genes[vj == "TRBV4-2_TRBJ1-5", ]
mini362 <- genes[vj == "TRBV4-2_TRBJ1-6", ]
mini363 <- genes[vj == "TRBV4-2_TRBJ2-1", ]
mini364 <- genes[vj == "TRBV4-2_TRBJ2-2", ]
mini365 <- genes[vj == "TRBV4-2_TRBJ2-3", ]
mini366 <- genes[vj == "TRBV4-2_TRBJ2-4", ]
mini367 <- genes[vj == "TRBV4-2_TRBJ2-5", ]
mini368 <- genes[vj == "TRBV4-2_TRBJ2-6", ]
mini369 <- genes[vj == "TRBV4-2_TRBJ2-7", ]
mini370 <- genes[vj == "TRBV4-3_TRBJ1-1", ]

mini371 <- genes[vj == "TRBV4-3_TRBJ1-2", ]
mini372 <- genes[vj == "TRBV4-3_TRBJ1-3", ]
mini373 <- genes[vj == "TRBV4-3_TRBJ1-4", ]
mini374 <- genes[vj == "TRBV4-3_TRBJ1-5", ]
mini375 <- genes[vj == "TRBV4-3_TRBJ1-6", ]
mini376 <- genes[vj == "TRBV4-3_TRBJ2-1", ]
mini377 <- genes[vj == "TRBV4-3_TRBJ2-2", ]
mini378 <- genes[vj == "TRBV4-3_TRBJ2-3", ]
mini379 <- genes[vj == "TRBV4-3_TRBJ2-4", ]
mini380 <- genes[vj == "TRBV4-3_TRBJ2-5", ]

mini381 <- genes[vj == "TRBV4-3_TRBJ2-6", ]
mini382 <- genes[vj == "TRBV4-3_TRBJ2-7", ]
mini383 <- genes[vj == "TRBV5-1_TRBJ1-1", ]
mini384 <- genes[vj == "TRBV5-1_TRBJ1-2", ]
mini385 <- genes[vj == "TRBV5-1_TRBJ1-3", ]
mini386 <- genes[vj == "TRBV5-1_TRBJ1-4", ]
mini387 <- genes[vj == "TRBV5-1_TRBJ1-5", ]
mini388 <- genes[vj == "TRBV5-1_TRBJ1-6", ]
mini389 <- genes[vj == "TRBV5-1_TRBJ2-1", ]
mini390 <- genes[vj == "TRBV5-1_TRBJ2-2", ]

mini391 <- genes[vj == "TRBV5-1_TRBJ2-3", ]
mini392 <- genes[vj == "TRBV5-1_TRBJ2-4", ]
mini393 <- genes[vj == "TRBV5-1_TRBJ2-5", ]
mini394 <- genes[vj == "TRBV5-1_TRBJ2-6", ]
mini395 <- genes[vj == "TRBV5-1_TRBJ2-7", ]
mini396 <- genes[vj == "TRBV5-3_TRBJ1-1", ]
mini397 <- genes[vj == "TRBV5-3_TRBJ1-2", ]
mini398 <- genes[vj == "TRBV5-3_TRBJ1-3", ]
mini399 <- genes[vj == "TRBV5-3_TRBJ1-4", ]
mini400 <- genes[vj == "TRBV5-3_TRBJ1-5", ]

mini401 <- genes[vj == "TRBV5-3_TRBJ1-6", ]
mini402 <- genes[vj == "TRBV5-3_TRBJ2-1", ]
mini403 <- genes[vj == "TRBV5-3_TRBJ2-2", ]
mini404 <- genes[vj == "TRBV5-3_TRBJ2-3", ]
mini405 <- genes[vj == "TRBV5-3_TRBJ2-4", ]
mini406 <- genes[vj == "TRBV5-3_TRBJ2-5", ]
mini407 <- genes[vj == "TRBV5-3_TRBJ2-6", ]
mini408 <- genes[vj == "TRBV5-3_TRBJ2-7", ]
mini409 <- genes[vj == "TRBV5-4_TRBJ1-1", ]
mini410 <- genes[vj == "TRBV5-4_TRBJ1-2", ]

mini411 <- genes[vj == "TRBV5-4_TRBJ1-3", ]
mini412 <- genes[vj == "TRBV5-4_TRBJ1-4", ]
mini413 <- genes[vj == "TRBV5-4_TRBJ1-5", ]
mini414 <- genes[vj == "TRBV5-4_TRBJ1-6", ]
mini415 <- genes[vj == "TRBV5-4_TRBJ2-1", ]
mini416 <- genes[vj == "TRBV5-4_TRBJ2-2", ]
mini417 <- genes[vj == "TRBV5-4_TRBJ2-3", ]
mini418 <- genes[vj == "TRBV5-4_TRBJ2-4", ]
mini419 <- genes[vj == "TRBV5-4_TRBJ2-5", ]
mini420 <- genes[vj == "TRBV5-4_TRBJ2-6", ]

mini421 <- genes[vj == "TRBV5-4_TRBJ2-7", ]
mini422 <- genes[vj == "TRBV5-5_TRBJ1-1", ]
mini423 <- genes[vj == "TRBV5-5_TRBJ1-2", ]
mini424 <- genes[vj == "TRBV5-5_TRBJ1-3", ]
mini425 <- genes[vj == "TRBV5-5_TRBJ1-4", ]
mini426 <- genes[vj == "TRBV5-5_TRBJ1-5", ]
mini427 <- genes[vj == "TRBV5-5_TRBJ1-6", ]
mini428 <- genes[vj == "TRBV5-5_TRBJ2-1", ]
mini429 <- genes[vj == "TRBV5-5_TRBJ2-2", ]
mini430 <- genes[vj == "TRBV5-5_TRBJ2-3", ]

mini431 <- genes[vj == "TRBV5-5_TRBJ2-4", ]
mini432 <- genes[vj == "TRBV5-5_TRBJ2-5", ]
mini433 <- genes[vj == "TRBV5-5_TRBJ2-6", ]
mini434 <- genes[vj == "TRBV5-5_TRBJ2-7", ]
mini435 <- genes[vj == "TRBV5-6_TRBJ1-1", ]
mini436 <- genes[vj == "TRBV5-6_TRBJ1-2", ]
mini437 <- genes[vj == "TRBV5-6_TRBJ1-3", ]
mini438 <- genes[vj == "TRBV5-6_TRBJ1-4", ]
mini439 <- genes[vj == "TRBV5-6_TRBJ1-5", ]
mini440 <- genes[vj == "TRBV5-6_TRBJ1-6", ]

mini441 <- genes[vj == "TRBV5-6_TRBJ2-1", ]
mini442 <- genes[vj == "TRBV5-6_TRBJ2-2", ]
mini443 <- genes[vj == "TRBV5-6_TRBJ2-3", ]
mini444 <- genes[vj == "TRBV5-6_TRBJ2-4", ]
mini445 <- genes[vj == "TRBV5-6_TRBJ2-5", ]
mini446 <- genes[vj == "TRBV5-6_TRBJ2-6", ]
mini447 <- genes[vj == "TRBV5-6_TRBJ2-7", ]
mini448 <- genes[vj == "TRBV5-7_TRBJ1-1", ]
mini449 <- genes[vj == "TRBV5-7_TRBJ1-2", ]
mini450 <- genes[vj == "TRBV5-7_TRBJ1-3", ]

mini451 <- genes[vj == "TRBV5-7_TRBJ1-4", ]
mini452 <- genes[vj == "TRBV5-7_TRBJ1-5", ]
mini453 <- genes[vj == "TRBV5-7_TRBJ1-6", ]
mini454 <- genes[vj == "TRBV5-7_TRBJ2-1", ]
mini455 <- genes[vj == "TRBV5-7_TRBJ2-2", ]
mini456 <- genes[vj == "TRBV5-7_TRBJ2-3", ]
mini457 <- genes[vj == "TRBV5-7_TRBJ2-4", ]
mini458 <- genes[vj == "TRBV5-7_TRBJ2-5", ]
mini459 <- genes[vj == "TRBV5-7_TRBJ2-6", ]
mini460 <- genes[vj == "TRBV5-7_TRBJ2-7", ]

mini461 <- genes[vj == "TRBV5-8_TRBJ1-1", ]
mini462 <- genes[vj == "TRBV5-8_TRBJ1-2", ]
mini463 <- genes[vj == "TRBV5-8_TRBJ1-3", ]
mini464 <- genes[vj == "TRBV5-8_TRBJ1-4", ]
mini465 <- genes[vj == "TRBV5-8_TRBJ1-5", ]
mini466 <- genes[vj == "TRBV5-8_TRBJ1-6", ]
mini467 <- genes[vj == "TRBV5-8_TRBJ2-1", ]
mini468 <- genes[vj == "TRBV5-8_TRBJ2-2", ]
mini469 <- genes[vj == "TRBV5-8_TRBJ2-3", ]
mini470 <- genes[vj == "TRBV5-8_TRBJ2-4", ]

mini471 <- genes[vj == "TRBV5-8_TRBJ2-5", ]
mini472 <- genes[vj == "TRBV5-8_TRBJ2-6", ]
mini473 <- genes[vj == "TRBV5-8_TRBJ2-7", ]
mini474 <- genes[vj == "TRBV6-1_TRBJ1-1", ]
mini475 <- genes[vj == "TRBV6-1_TRBJ1-2", ]
mini476 <- genes[vj == "TRBV6-1_TRBJ1-3", ]
mini477 <- genes[vj == "TRBV6-1_TRBJ1-4", ]
mini478 <- genes[vj == "TRBV6-1_TRBJ1-5", ]
mini479 <- genes[vj == "TRBV6-1_TRBJ1-6", ]
mini480 <- genes[vj == "TRBV6-1_TRBJ2-1", ]

mini481 <- genes[vj == "TRBV6-1_TRBJ2-2", ]
mini482 <- genes[vj == "TRBV6-1_TRBJ2-3", ]
mini483 <- genes[vj == "TRBV6-1_TRBJ2-4", ]
mini484 <- genes[vj == "TRBV6-1_TRBJ2-5", ]
mini485 <- genes[vj == "TRBV6-1_TRBJ2-6", ]
mini486 <- genes[vj == "TRBV6-1_TRBJ2-7", ]
mini487 <- genes[vj == "TRBV6-2_TRBJ1-1", ]
mini488 <- genes[vj == "TRBV6-2_TRBJ1-2", ]
mini489 <- genes[vj == "TRBV6-2_TRBJ1-5", ]
mini490 <- genes[vj == "TRBV6-2_TRBJ1-6", ]

mini491 <- genes[vj == "TRBV6-2_TRBJ2-1", ]
mini492 <- genes[vj == "TRBV6-2_TRBJ2-2", ]
mini493 <- genes[vj == "TRBV6-2_TRBJ2-3", ]
mini494 <- genes[vj == "TRBV6-2_TRBJ2-5", ]
mini495 <- genes[vj == "TRBV6-2_TRBJ2-7", ]
mini496 <- genes[vj == "TRBV6-3_TRBJ1-1", ]
mini497 <- genes[vj == "TRBV6-3_TRBJ1-2", ]
mini498 <- genes[vj == "TRBV6-3_TRBJ1-3", ]
mini499 <- genes[vj == "TRBV6-3_TRBJ1-4", ]
mini500 <- genes[vj == "TRBV6-3_TRBJ1-5", ]

mini501 <- genes[vj == "TRBV6-3_TRBJ1-6", ]
mini502 <- genes[vj == "TRBV6-3_TRBJ2-1", ]
mini503 <- genes[vj == "TRBV6-3_TRBJ2-2", ]
mini504 <- genes[vj == "TRBV6-3_TRBJ2-3", ]
mini505 <- genes[vj == "TRBV6-3_TRBJ2-4", ]
mini506 <- genes[vj == "TRBV6-3_TRBJ2-5", ]
mini507 <- genes[vj == "TRBV6-3_TRBJ2-6", ]
mini508 <- genes[vj == "TRBV6-3_TRBJ2-7", ]
mini509 <- genes[vj == "TRBV6-4_TRBJ1-1", ]
mini510 <- genes[vj == "TRBV6-4_TRBJ1-2", ]

mini511 <- genes[vj == "TRBV6-4_TRBJ1-3", ]
mini512 <- genes[vj == "TRBV6-4_TRBJ1-4", ]
mini513 <- genes[vj == "TRBV6-4_TRBJ1-5", ]
mini514 <- genes[vj == "TRBV6-4_TRBJ1-6", ]
mini515 <- genes[vj == "TRBV6-4_TRBJ2-1", ]
mini516 <- genes[vj == "TRBV6-4_TRBJ2-2", ]
mini517 <- genes[vj == "TRBV6-4_TRBJ2-3", ]
mini518 <- genes[vj == "TRBV6-4_TRBJ2-4", ]
mini519 <- genes[vj == "TRBV6-4_TRBJ2-5", ]
mini520 <- genes[vj == "TRBV6-4_TRBJ2-6", ]

mini521 <- genes[vj == "TRBV6-4_TRBJ2-7", ]
mini522 <- genes[vj == "TRBV6-5_TRBJ1-1", ]
mini523 <- genes[vj == "TRBV6-5_TRBJ1-2", ]
mini524 <- genes[vj == "TRBV6-5_TRBJ1-3", ]
mini525 <- genes[vj == "TRBV6-5_TRBJ1-4", ]
mini526 <- genes[vj == "TRBV6-5_TRBJ1-5", ]
mini527 <- genes[vj == "TRBV6-5_TRBJ1-6", ]
mini528 <- genes[vj == "TRBV6-5_TRBJ2-1", ]
mini529 <- genes[vj == "TRBV6-5_TRBJ2-2", ]
mini530 <- genes[vj == "TRBV6-5_TRBJ2-3", ]

mini531 <- genes[vj == "TRBV6-4_TRBJ2-7", ]
mini532 <- genes[vj == "TRBV6-5_TRBJ1-1", ]
mini533 <- genes[vj == "TRBV6-5_TRBJ1-2", ]
mini534 <- genes[vj == "TRBV6-5_TRBJ1-3", ]
mini535 <- genes[vj == "TRBV6-5_TRBJ1-4", ]
mini536 <- genes[vj == "TRBV6-5_TRBJ1-5", ]
mini537 <- genes[vj == "TRBV6-5_TRBJ1-6", ]
mini538 <- genes[vj == "TRBV6-5_TRBJ2-1", ]
mini539 <- genes[vj == "TRBV6-5_TRBJ2-2", ]
mini540 <- genes[vj == "TRBV6-5_TRBJ2-3", ]

mini541 <- genes[vj == "TRBV6-5_TRBJ2-4", ]
mini542 <- genes[vj == "TRBV6-5_TRBJ2-5", ]
mini543 <- genes[vj == "TRBV6-5_TRBJ2-6", ]
mini544 <- genes[vj == "TRBV6-5_TRBJ2-7", ]
mini545 <- genes[vj == "TRBV6-6_TRBJ1-1", ]
mini546 <- genes[vj == "TRBV6-6_TRBJ1-2", ]
mini547 <- genes[vj == "TRBV6-6_TRBJ1-3", ]
mini548 <- genes[vj == "TRBV6-6_TRBJ1-4", ]
mini549 <- genes[vj == "TRBV6-6_TRBJ1-5", ]
mini550 <- genes[vj == "TRBV6-6_TRBJ1-6", ]

mini551 <- genes[vj == "TRBV6-6_TRBJ2-1", ]
mini552 <- genes[vj == "TRBV6-6_TRBJ2-2", ]
mini553 <- genes[vj == "TRBV6-6_TRBJ2-3", ]
mini554 <- genes[vj == "TRBV6-6_TRBJ2-4", ]
mini555 <- genes[vj == "TRBV6-6_TRBJ2-5", ]
mini556 <- genes[vj == "TRBV6-6_TRBJ2-6", ]
mini557 <- genes[vj == "TRBV6-6_TRBJ2-7", ]
mini558 <- genes[vj == "TRBV6-7_TRBJ1-1", ]
mini559 <- genes[vj == "TRBV6-7_TRBJ1-2", ]
mini560 <- genes[vj == "TRBV6-7_TRBJ1-3", ]

mini561 <- genes[vj == "TRBV6-7_TRBJ1-4", ]
mini562 <- genes[vj == "TRBV6-7_TRBJ1-5", ]
mini563 <- genes[vj == "TRBV6-7_TRBJ1-6", ]
mini564 <- genes[vj == "TRBV6-7_TRBJ2-1", ]
mini565 <- genes[vj == "TRBV6-7_TRBJ2-2", ]
mini566 <- genes[vj == "TRBV6-7_TRBJ2-3", ]
mini567 <- genes[vj == "TRBV6-7_TRBJ2-5", ]
mini568 <- genes[vj == "TRBV6-7_TRBJ2-6", ]
mini569 <- genes[vj == "TRBV6-7_TRBJ2-7", ]
mini570 <- genes[vj == "TRBV6-8_TRBJ1-1", ]

mini571 <- genes[vj == "TRBV6-8_TRBJ1-2", ]
mini572 <- genes[vj == "TRBV6-8_TRBJ1-3", ]
mini573 <- genes[vj == "TRBV6-8_TRBJ1-4", ]
mini574 <- genes[vj == "TRBV6-8_TRBJ1-5", ]
mini575 <- genes[vj == "TRBV6-8_TRBJ1-6", ]
mini576 <- genes[vj == "TRBV6-8_TRBJ2-1", ]
mini577 <- genes[vj == "TRBV6-8_TRBJ2-2", ]
mini578 <- genes[vj == "TRBV6-8_TRBJ2-3", ]
mini579 <- genes[vj == "TRBV6-8_TRBJ2-4", ]
mini580 <- genes[vj == "TRBV6-8_TRBJ2-5", ]

mini581 <- genes[vj == "TRBV6-8_TRBJ2-6", ]
mini582 <- genes[vj == "TRBV6-8_TRBJ2-7", ]
mini583 <- genes[vj == "TRBV6-9_TRBJ1-1", ]
mini584 <- genes[vj == "TRBV6-9_TRBJ1-2", ]
mini585 <- genes[vj == "TRBV6-9_TRBJ1-3", ]
mini586 <- genes[vj == "TRBV6-9_TRBJ1-4", ]
mini587 <- genes[vj == "TRBV6-9_TRBJ1-5", ]
mini588 <- genes[vj == "TRBV6-9_TRBJ1-6", ]
mini589 <- genes[vj == "TRBV6-9_TRBJ2-1", ]
mini590 <- genes[vj == "TRBV6-9_TRBJ2-2", ]

mini591 <- genes[vj == "TRBV6-9_TRBJ2-3", ]
mini592 <- genes[vj == "TRBV6-9_TRBJ2-4", ]
mini593 <- genes[vj == "TRBV6-9_TRBJ2-5", ]
mini594 <- genes[vj == "TRBV6-9_TRBJ2-6", ]
mini595 <- genes[vj == "TRBV6-9_TRBJ2-7", ]
mini596 <- genes[vj == "TRBV7-2_TRBJ1-1", ]
mini597 <- genes[vj == "TRBV7-2_TRBJ1-2", ]
mini598 <- genes[vj == "TRBV7-2_TRBJ1-3", ]
mini599 <- genes[vj == "TRBV7-2_TRBJ1-4", ]
mini600 <- genes[vj == "TRBV7-2_TRBJ1-5", ]

mini601 <- genes[vj == "TRBV7-2_TRBJ1-6", ]
mini602 <- genes[vj == "TRBV7-2_TRBJ2-1", ]
mini603 <- genes[vj == "TRBV7-2_TRBJ2-2", ]
mini604 <- genes[vj == "TRBV7-2_TRBJ2-3", ]
mini605 <- genes[vj == "TRBV7-2_TRBJ2-4", ]
mini606 <- genes[vj == "TRBV7-2_TRBJ2-5", ]
mini607 <- genes[vj == "TRBV7-2_TRBJ2-6", ]
mini608 <- genes[vj == "TRBV7-2_TRBJ2-7", ]
mini609 <- genes[vj == "TRBV7-3_TRBJ1-1", ]
mini610 <- genes[vj == "TRBV7-3_TRBJ1-2", ]

mini611 <- genes[vj == "TRBV7-3_TRBJ1-3", ]
mini612 <- genes[vj == "TRBV7-3_TRBJ1-4", ]
mini613 <- genes[vj == "TRBV7-3_TRBJ1-5", ]
mini614 <- genes[vj == "TRBV7-3_TRBJ1-6", ]
mini615 <- genes[vj == "TRBV7-3_TRBJ2-1", ]
mini616 <- genes[vj == "TRBV7-3_TRBJ2-2", ]
mini617 <- genes[vj == "TRBV7-3_TRBJ2-3", ]
mini618 <- genes[vj == "TRBV7-3_TRBJ2-4", ]
mini619 <- genes[vj == "TRBV7-3_TRBJ2-5", ]
mini620 <- genes[vj == "TRBV7-3_TRBJ2-6", ]

mini621 <- genes[vj == "TRBV7-3_TRBJ2-7", ]
mini622 <- genes[vj == "TRBV7-4_TRBJ1-1", ]
mini623 <- genes[vj == "TRBV7-4_TRBJ1-2", ]
mini624 <- genes[vj == "TRBV7-4_TRBJ1-3", ]
mini625 <- genes[vj == "TRBV7-4_TRBJ1-4", ]
mini626 <- genes[vj == "TRBV7-4_TRBJ1-5", ]
mini627 <- genes[vj == "TRBV7-4_TRBJ1-6", ]
mini628 <- genes[vj == "TRBV7-4_TRBJ2-1", ]
mini629 <- genes[vj == "TRBV7-4_TRBJ2-2", ]
mini630 <- genes[vj == "TRBV7-4_TRBJ2-3", ]

mini631 <- genes[vj == "TRBV7-4_TRBJ2-4", ]
mini632 <- genes[vj == "TRBV7-4_TRBJ2-5", ]
mini633 <- genes[vj == "TRBV7-4_TRBJ2-6", ]
mini634 <- genes[vj == "TRBV7-4_TRBJ2-7", ]
mini635 <- genes[vj == "TRBV7-5_TRBJ1-1", ]
mini636 <- genes[vj == "TRBV7-5_TRBJ1-2", ]
mini637 <- genes[vj == "TRBV7-5_TRBJ1-3", ]
mini638 <- genes[vj == "TRBV7-5_TRBJ1-4", ]
mini639 <- genes[vj == "TRBV7-5_TRBJ1-5", ]
mini640 <- genes[vj == "TRBV7-5_TRBJ1-6", ]

mini641 <- genes[vj == "TRBV7-5_TRBJ2-1", ]
mini642 <- genes[vj == "TRBV7-5_TRBJ2-2", ]
mini643 <- genes[vj == "TRBV7-5_TRBJ2-3", ]
mini644 <- genes[vj == "TRBV7-5_TRBJ2-4", ]
mini645 <- genes[vj == "TRBV7-5_TRBJ2-5", ]
mini646 <- genes[vj == "TRBV7-5_TRBJ2-6", ]
mini647 <- genes[vj == "TRBV7-5_TRBJ2-7", ]
mini648 <- genes[vj == "TRBV7-6_TRBJ1-1", ]
mini649 <- genes[vj == "TRBV7-6_TRBJ1-2", ]
mini650 <- genes[vj == "TRBV7-6_TRBJ1-3", ]

mini651 <- genes[vj == "TRBV7-6_TRBJ1-4", ]
mini652 <- genes[vj == "TRBV7-6_TRBJ1-5", ]
mini653 <- genes[vj == "TRBV7-6_TRBJ1-6", ]
mini654 <- genes[vj == "TRBV7-6_TRBJ2-1", ]
mini655 <- genes[vj == "TRBV7-6_TRBJ2-2", ]
mini656 <- genes[vj == "TRBV7-6_TRBJ2-3", ]
mini657 <- genes[vj == "TRBV7-6_TRBJ2-4", ]
mini658 <- genes[vj == "TRBV7-6_TRBJ2-5", ]
mini659 <- genes[vj == "TRBV7-6_TRBJ2-6", ]
mini660 <- genes[vj == "TRBV7-6_TRBJ2-7", ]

mini661 <- genes[vj == "TRBV7-7_TRBJ1-1", ]
mini662 <- genes[vj == "TRBV7-7_TRBJ1-2", ]
mini663 <- genes[vj == "TRBV7-7_TRBJ1-3", ]
mini664 <- genes[vj == "TRBV7-7_TRBJ1-4", ]
mini665 <- genes[vj == "TRBV7-7_TRBJ1-5", ]
mini666 <- genes[vj == "TRBV7-7_TRBJ1-6", ]
mini667 <- genes[vj == "TRBV7-7_TRBJ2-1", ]
mini668 <- genes[vj == "TRBV7-7_TRBJ2-2", ]
mini669 <- genes[vj == "TRBV7-7_TRBJ2-3", ]
mini670 <- genes[vj == "TRBV7-7_TRBJ2-4", ]

mini671 <- genes[vj == "TRBV7-7_TRBJ2-5", ]
mini672 <- genes[vj == "TRBV7-7_TRBJ2-6", ]
mini673 <- genes[vj == "TRBV7-7_TRBJ2-7", ]
mini674 <- genes[vj == "TRBV7-8_TRBJ1-1", ]
mini675 <- genes[vj == "TRBV7-8_TRBJ1-2", ]
mini676 <- genes[vj == "TRBV7-8_TRBJ1-3", ]
mini677 <- genes[vj == "TRBV7-8_TRBJ1-4", ]
mini678 <- genes[vj == "TRBV7-8_TRBJ2-1", ]
mini679 <- genes[vj == "TRBV7-8_TRBJ2-2", ]
mini680 <- genes[vj == "TRBV7-8_TRBJ2-3", ]

mini681 <- genes[vj == "TRBV7-8_TRBJ2-4", ]
mini682 <- genes[vj == "TRBV7-8_TRBJ2-5", ]
mini683 <- genes[vj == "TRBV7-8_TRBJ2-6", ]
mini684 <- genes[vj == "TRBV7-8_TRBJ2-7", ]
mini685 <- genes[vj == "TRBV7-9_TRBJ1-1", ]
mini686 <- genes[vj == "TRBV7-9_TRBJ1-2", ]
mini687 <- genes[vj == "TRBV7-9_TRBJ1-3", ]
mini688 <- genes[vj == "TRBV7-9_TRBJ1-4", ]
mini689 <- genes[vj == "TRBV7-9_TRBJ1-5", ]
mini690 <- genes[vj == "TRBV7-9_TRBJ1-6", ]

mini691 <- genes[vj == "TRBV7-9_TRBJ2-1", ]
mini692 <- genes[vj == "TRBV7-9_TRBJ2-2", ]
mini693 <- genes[vj == "TRBV7-9_TRBJ2-3", ]
mini694 <- genes[vj == "TRBV7-9_TRBJ2-4", ]
mini695 <- genes[vj == "TRBV7-9_TRBJ2-5", ]
mini696 <- genes[vj == "TRBV7-9_TRBJ2-6", ]
mini697 <- genes[vj == "TRBV7-9_TRBJ2-7", ]
mini698 <- genes[vj == "TRBV9_TRBJ1-1", ]
mini699 <- genes[vj == "TRBV9_TRBJ1-2", ]
mini700 <- genes[vj == "TRBV9_TRBJ1-3", ]

mini701 <- genes[vj == "TRBV9_TRBJ1-4", ]
mini702 <- genes[vj == "TRBV9_TRBJ1-5", ]
mini703 <- genes[vj == "TRBV9_TRBJ1-6", ]
mini704 <- genes[vj == "TRBV9_TRBJ2-1", ]
mini705 <- genes[vj == "TRBV9_TRBJ2-2", ]
mini706 <- genes[vj == "TRBV9_TRBJ2-3", ]
mini707 <- genes[vj == "TRBV9_TRBJ2-4", ]
mini708 <- genes[vj == "TRBV9_TRBJ2-5", ]
mini709 <- genes[vj == "TRBV9_TRBJ2-6", ]
mini710 <- genes[vj == "TRBV9_TRBJ2-7", ]

# saving as excel file
#write.xlsx(genes, 
#           "D:/Coding/R Storage/Summer TCR Project/TCR Datasets/fullgenes.xlsx", 
#           row.names = FALSE)
#write.csv(genes, 
#          "D:/Coding/R Storage/Summer TCR Project/TCR Datasets/fullgenes.csv", 
#          row.names = FALSE)













