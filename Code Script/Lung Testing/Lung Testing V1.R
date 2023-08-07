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

#############
# Dimscribe #
#############

describe(lung)
dim(lung)
describe(lungenes)
dim(lungenes)

################
# Dataset Prep #
################

# no need for screening
lungene <- lungenes[, -c(2:70)]

# pre-infusion
pre <-  lungene[, c(1:62)]

# for NAs and 0s
pre[is.na(pre)] <- 1e-7
pre[pre == 0] <- 1e-7

colnames(pre) <- c("vjGene", "1056201652", "1056201734", "1056201763",
                   "1056201766", "1093501642", "1093501649",  "1093501690",
                   "1245501789", "1322701679", "1351901585",  "1351901590",
                   "1351901743", "1351901799", "1371101668", "1371101712",
                   "2000044446", "2000044709", "2000044739", "2000044755",
                   "2000044784", "2000134826", "2000167902", "2000199725", 
                   "2000206742", "2000206980", "2000210806", "2000210942",
                   "2000211830", "10562011366", "10562011565", "12455011536",
                   "13227011150", "13519011353", "13711011381", "20000891275",
                   "20001081074", "20001331759", "20001351017", "20001351914",
                   "20001352321", "20001371882", "20001971574", "20001971832",
                   "20001991312", "20002061151", "20002061360", "20002062054",
                   "20002081893", "20002091885", "20002101827", "20002111889",
                   "20002131864", "20002181234", "20002181334", "20004391109",
                   "20004391114", "20007341206", "20007341222", "20007341336", 
                   "20007341420", "20011092291")

# log transform 
pre$'1056201652' <- log(pre$'1056201652')
pre$'1056201734' <- log(pre$'1056201734')
pre$'1056201763' <- log(pre$'1056201763')
pre$'1056201766' <- log(pre$'1056201766')
pre$'1093501642' <- log(pre$'1093501642')
pre$'1093501649' <- log(pre$'1093501649')
pre$'1093501690' <- log(pre$'1093501690')
pre$'1245501789' <- log(pre$'1245501789')
pre$'1322701679' <- log(pre$'1322701679')
pre$'1351901585' <- log(pre$'1351901585')

pre$'1351901590' <- log(pre$'1351901590')
pre$'1351901743' <- log(pre$'1351901743')
pre$'1351901799' <- log(pre$'1351901799')
pre$'1371101668' <- log(pre$'1371101668')
pre$'1371101712' <- log(pre$'1371101712')
pre$'2000044446' <- log(pre$'2000044446')
pre$'2000044709' <- log(pre$'2000044709')
pre$'2000044739' <- log(pre$'2000044739')
pre$'2000044755' <- log(pre$'2000044755')
pre$'2000044784' <- log(pre$'2000044784')

pre$'2000134826' <- log(pre$'2000134826')
pre$'2000167902' <- log(pre$'2000167902')
pre$'2000199725' <- log(pre$'2000199725')
pre$'2000206742' <- log(pre$'2000206742')
pre$'2000206980' <- log(pre$'2000206980')
pre$'2000210806' <- log(pre$'2000210806')
pre$'2000210942' <- log(pre$'2000210942')
pre$'2000211830' <- log(pre$'2000211830')
pre$'10562011366' <- log(pre$'10562011366')
pre$'10562011565' <- log(pre$'10562011565')

pre$'12455011536' <- log(pre$'12455011536')
pre$'13227011150' <- log(pre$'13227011150')
pre$'13519011353' <- log(pre$'13519011353')
pre$'13711011381' <- log(pre$'13711011381')
pre$'20000891275' <- log(pre$'20000891275')
pre$'20001081074' <- log(pre$'20001081074')
pre$'20001331759' <- log(pre$'20001331759')
pre$'20001351017' <- log(pre$'20001351017')
pre$'20001351914' <- log(pre$'20001351914')
pre$'20001352321' <- log(pre$'20001352321')

pre$'20001371882' <- log(pre$'20001371882')
pre$'20001971574' <- log(pre$'20001971574')
pre$'20001971832' <- log(pre$'20001971832')
pre$'20001991312' <- log(pre$'20001991312')
pre$'20002061151' <- log(pre$'20002061151')
pre$'20002061360' <- log(pre$'20002061360')
pre$'20002062054' <- log(pre$'20002062054')
pre$'20002081893' <- log(pre$'20002081893')
pre$'20002091885' <- log(pre$'20002091885')
pre$'20002101827' <- log(pre$'20002101827')

pre$'20002111889' <- log(pre$'20002111889')
pre$'20002131864' <- log(pre$'20002131864')
pre$'20002181234' <- log(pre$'20002181234')
pre$'20002181334' <- log(pre$'20002181334')
pre$'20004391109' <- log(pre$'20004391109')
pre$'20004391114' <- log(pre$'20004391114')
pre$'20007341206' <- log(pre$'20007341206')
pre$'20007341222' <- log(pre$'20007341222')
pre$'20007341336' <- log(pre$'20007341336')
pre$'20007341420' <- log(pre$'20007341420')

pre$'20011092291' <- log(pre$'20011092291')

# standardize
pre$'1056201652' <- scale(pre$'1056201652')
pre$'1056201734' <- scale(pre$'1056201734')
pre$'1056201763' <- scale(pre$'1056201763')
pre$'1056201766' <- scale(pre$'1056201766')
pre$'1093501642' <- scale(pre$'1093501642')
pre$'1093501649' <- scale(pre$'1093501649')
pre$'1093501690' <- scale(pre$'1093501690')
pre$'1245501789' <- scale(pre$'1245501789')
pre$'1322701679' <- scale(pre$'1322701679')
pre$'1351901585' <- scale(pre$'1351901585')

pre$'1351901590' <- scale(pre$'1351901590')
pre$'1351901743' <- scale(pre$'1351901743')
pre$'1351901799' <- scale(pre$'1351901799')
pre$'1371101668' <- scale(pre$'1371101668')
pre$'1371101712' <- scale(pre$'1371101712')
pre$'2000044446' <- scale(pre$'2000044446')
pre$'2000044709' <- scale(pre$'2000044709')
pre$'2000044739' <- scale(pre$'2000044739')
pre$'2000044755' <- scale(pre$'2000044755')
pre$'2000044784' <- scale(pre$'2000044784')

pre$'2000134826' <- scale(pre$'2000134826')
pre$'2000167902' <- scale(pre$'2000167902')
pre$'2000199725' <- scale(pre$'2000199725')
pre$'2000206742' <- scale(pre$'2000206742')
pre$'2000206980' <- scale(pre$'2000206980')
pre$'2000210806' <- scale(pre$'2000210806')
pre$'2000210942' <- scale(pre$'2000210942')
pre$'2000211830' <- scale(pre$'2000211830')
pre$'10562011366' <- scale(pre$'10562011366')
pre$'10562011565' <- scale(pre$'10562011565')

pre$'12455011536' <- scale(pre$'12455011536')
pre$'13227011150' <- scale(pre$'13227011150')
pre$'13519011353' <- scale(pre$'13519011353')
pre$'13711011381' <- scale(pre$'13711011381')
pre$'20000891275' <- scale(pre$'20000891275')
pre$'20001081074' <- scale(pre$'20001081074')
pre$'20001331759' <- scale(pre$'20001331759')
pre$'20001351017' <- scale(pre$'20001351017')
pre$'20001351914' <- scale(pre$'20001351914')
pre$'20001352321' <- scale(pre$'20001352321')

pre$'20001371882' <- scale(pre$'20001371882')
pre$'20001971574' <- scale(pre$'20001971574')
pre$'20001971832' <- scale(pre$'20001971832')
pre$'20001991312' <- scale(pre$'20001991312')
pre$'20002061151' <- scale(pre$'20002061151')
pre$'20002061360' <- scale(pre$'20002061360')
pre$'20002062054' <- scale(pre$'20002062054')
pre$'20002081893' <- scale(pre$'20002081893')
pre$'20002091885' <- scale(pre$'20002091885')
pre$'20002101827' <- scale(pre$'20002101827')

pre$'20002111889' <- scale(pre$'20002111889')
pre$'20002131864' <- scale(pre$'20002131864')
pre$'20002181234' <- scale(pre$'20002181234')
pre$'20002181334' <- scale(pre$'20002181334')
pre$'20004391109' <- scale(pre$'20004391109')
pre$'20004391114' <- scale(pre$'20004391114')
pre$'20007341206' <- scale(pre$'20007341206')
pre$'20007341222' <- scale(pre$'20007341222')
pre$'20007341336' <- scale(pre$'20007341336')
pre$'20007341420' <- scale(pre$'20007341420')

pre$'20011092291' <- scale(pre$'20011092291')

# transform
preTrans <- as.data.frame(t(pre))

colnames(preTrans) <- c("TRBV1_TRBJ1-1", "TRBV1_TRBJ1-2", "TRBV1_TRBJ1-3", 
                        "TRBV1_TRBJ1-4", "TRBV1_TRBJ1-5", "TRBV1_TRBJ1-6", 
                        "TRBV1_TRBJ2-1", "TRBV1_TRBJ2-2", "TRBV1_TRBJ2-3", 
                        "TRBV1_TRBJ2-4", "TRBV1_TRBJ2-5", "TRBV1_TRBJ2-6", 
                        "TRBV1_TRBJ2-7", "TRBV10-1_TRBJ1-1", "TRBV10-1_TRBJ1-2",
                        "TRBV10-1_TRBJ1-3", "TRBV10-1_TRBJ1-4", 
                        "TRBV10-1_TRBJ1-5", "TRBV10-1_TRBJ1-6", 
                        "TRBV10-1_TRBJ2-1", "TRBV10-1_TRBJ2-2", 
                        "TRBV10-1_TRBJ2-3", "TRBV10-1_TRBJ2-4", 
                        "TRBV10-1_TRBJ2-5", "TRBV10-1_TRBJ2-6", 
                        "TRBV10-1_TRBJ2-7", "TRBV10-2_TRBJ1-1",
                        "TRBV10-2_TRBJ1-2", "TRBV10-2_TRBJ1-3", 
                        "TRBV10-2_TRBJ1-4", "TRBV10-2_TRBJ1-5",
                        "TRBV10-2_TRBJ1-6", "TRBV10-2_TRBJ2-1", 
                        "TRBV10-2_TRBJ2-2", "TRBV10-2_TRBJ2-3",
                        "TRBV10-2_TRBJ2-4", "TRBV10-2_TRBJ2-5", 
                        "TRBV10-2_TRBJ2-6", "TRBV10-2_TRBJ2-7",
                        "TRBV10-3_TRBJ1-1", "TRBV10-3_TRBJ1-2", 
                        "TRBV10-3_TRBJ1-3", "TRBV10-3_TRBJ1-4",
                        "TRBV10-3_TRBJ1-5", "TRBV10-3_TRBJ1-6", 
                        "TRBV10-3_TRBJ2-1", "TRBV10-3_TRBJ2-2",
                        "TRBV10-3_TRBJ2-3", "TRBV10-3_TRBJ2-4", 
                        "TRBV10-3_TRBJ2-5", "TRBV10-3_TRBJ2-6",
                        "TRBV10-3_TRBJ2-7", "TRBV11-1_TRBJ1-1", 
                        "TRBV11-1_TRBJ1-2", "TRBV11-1_TRBJ1-3",
                        "TRBV11-1_TRBJ1-4", "TRBV11-1_TRBJ1-5", 
                        "TRBV11-1_TRBJ1-6", "TRBV11-1_TRBJ2-1",
                        "TRBV11-1_TRBJ2-2", "TRBV11-1_TRBJ2-3", 
                        "TRBV11-1_TRBJ2-4", "TRBV11-1_TRBJ2-5",
                        "TRBV11-1_TRBJ2-6", "TRBV11-1_TRBJ2-7", 
                        "TRBV11-2_TRBJ1-1", "TRBV11-2_TRBJ1-2",
                        "TRBV11-2_TRBJ1-3", "TRBV11-2_TRBJ1-4", 
                        "TRBV11-2_TRBJ1-5", "TRBV11-2_TRBJ1-6",
                        "TRBV11-2_TRBJ2-1", "TRBV11-2_TRBJ2-2", 
                        "TRBV11-2_TRBJ2-3", "TRBV11-2_TRBJ2-4",
                        "TRBV11-2_TRBJ2-5", "TRBV11-2_TRBJ2-6", 
                        "TRBV11-2_TRBJ2-7", "TRBV11-3_TRBJ1-1",
                        "TRBV11-3_TRBJ1-2", "TRBV11-3_TRBJ1-3", 
                        "TRBV11-3_TRBJ1-4", "TRBV11-3_TRBJ1-5",
                        "TRBV11-3_TRBJ1-6", "TRBV11-3_TRBJ2-1", 
                        "TRBV11-3_TRBJ2-2", "TRBV11-3_TRBJ2-3",
                        "TRBV11-3_TRBJ2-4", "TRBV11-3_TRBJ2-5", 
                        "TRBV11-3_TRBJ2-6", "TRBV11-3_TRBJ2-7",
                        "TRBV12-1_TRBJ1-1", "TRBV12-1_TRBJ1-2", 
                        "TRBV12-1_TRBJ1-3", "TRBV12-1_TRBJ1-4",
                        "TRBV12-1_TRBJ1-5", "TRBV12-1_TRBJ1-6", 
                        "TRBV12-1_TRBJ2-1", "TRBV12-1_TRBJ2-2",
                        "TRBV12-1_TRBJ2-3", "TRBV12-1_TRBJ2-4", 
                        "TRBV12-1_TRBJ2-5", "TRBV12-1_TRBJ2-6",
                        "TRBV12-1_TRBJ2-7", "TRBV12-2_TRBJ1-1", 
                        "TRBV12-2_TRBJ1-2", "TRBV12-2_TRBJ1-3",
                        "TRBV12-2_TRBJ1-4", "TRBV12-2_TRBJ1-5", 
                        "TRBV12-2_TRBJ1-6", "TRBV12-2_TRBJ2-1",
                        "TRBV12-2_TRBJ2-2", "TRBV12-2_TRBJ2-2P", 
                        "TRBV12-2_TRBJ2-3", "TRBV12-2_TRBJ2-4",
                        "TRBV12-2_TRBJ2-5", "TRBV12-2_TRBJ2-6", 
                        "TRBV12-2_TRBJ2-7", "TRBV12-3_TRBJ1-1",
                        "TRBV12-3_TRBJ1-2", "TRBV12-3_TRBJ1-3", 
                        "TRBV12-3_TRBJ1-4", "TRBV12-3_TRBJ1-5",
                        "TRBV12-3_TRBJ1-6", "TRBV12-3_TRBJ2-1", 
                        "TRBV12-3_TRBJ2-2", "TRBV12-3_TRBJ2-3",
                        "TRBV12-3_TRBJ2-4", "TRBV12-3_TRBJ2-5", 
                        "TRBV12-3_TRBJ2-6", "TRBV12-3_TRBJ2-7",
                        "TRBV12-5_TRBJ1-1", "TRBV12-5_TRBJ1-2", 
                        "TRBV12-5_TRBJ1-3", "TRBV12-5_TRBJ1-4",
                        "TRBV12-5_TRBJ1-5", "TRBV12-5_TRBJ1-6", 
                        "TRBV12-5_TRBJ2-1", "TRBV12-5_TRBJ2-2",
                        "TRBV12-5_TRBJ2-3", "TRBV12-5_TRBJ2-4", 
                        "TRBV12-5_TRBJ2-5", "TRBV12-5_TRBJ2-6",
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
                        "TRBV15_TRBJ2-7", "TRBV16_TRBJ1-1", "TRBV16_TRBJ1-2", 
                        "TRBV16_TRBJ1-3", "TRBV16_TRBJ1-4", "TRBV16_TRBJ1-5", 
                        "TRBV16_TRBJ1-6", "TRBV16_TRBJ2-1", "TRBV16_TRBJ2-2",
                        "TRBV16_TRBJ2-3", "TRBV16_TRBJ2-4", "TRBV16_TRBJ2-5", 
                        "TRBV16_TRBJ2-6", "TRBV16_TRBJ2-7", "TRBV17_TRBJ1-1", 
                        "TRBV17_TRBJ1-3", "TRBV17_TRBJ1-5", "TRBV17_TRBJ2-2P", 
                        "TRBV17_TRBJ2-3", "TRBV17_TRBJ2-7", "TRBV18_TRBJ1-1",
                        "TRBV18_TRBJ1-2", "TRBV18_TRBJ1-3", "TRBV18_TRBJ1-4", 
                        "TRBV18_TRBJ1-5", "TRBV18_TRBJ1-6", "TRBV18_TRBJ2-1", 
                        "TRBV18_TRBJ2-2", "TRBV18_TRBJ2-3", "TRBV18_TRBJ2-4", 
                        "TRBV18_TRBJ2-5", "TRBV18_TRBJ2-6", "TRBV18_TRBJ2-7", 
                        "TRBV19_TRBJ1-1", "TRBV19_TRBJ1-2", "TRBV19_TRBJ1-3", 
                        "TRBV19_TRBJ1-4", "TRBV19_TRBJ1-5", "TRBV19_TRBJ1-6", 
                        "TRBV19_TRBJ2-1", "TRBV19_TRBJ2-2", "TRBV19_TRBJ2-2P", 
                        "TRBV19_TRBJ2-3", "TRBV19_TRBJ2-4", "TRBV19_TRBJ2-5",
                        "TRBV19_TRBJ2-6", "TRBV19_TRBJ2-7", "TRBV2_TRBJ1-1", 
                        "TRBV2_TRBJ1-2", "TRBV2_TRBJ1-3", "TRBV2_TRBJ1-4", 
                        "TRBV2_TRBJ1-5", "TRBV2_TRBJ1-6", "TRBV2_TRBJ2-1", 
                        "TRBV2_TRBJ2-2", "TRBV2_TRBJ2-3", "TRBV2_TRBJ2-4", 
                        "TRBV2_TRBJ2-5", "TRBV2_TRBJ2-6", "TRBV2_TRBJ2-7", 
                        "TRBV20-1_TRBJ1-1", "TRBV20-1_TRBJ1-2", 
                        "TRBV20-1_TRBJ1-3", "TRBV20-1_TRBJ1-4", 
                        "TRBV20-1_TRBJ1-5", "TRBV20-1_TRBJ1-6", 
                        "TRBV20-1_TRBJ2-1", "TRBV20-1_TRBJ2-2", 
                        "TRBV20-1_TRBJ2-3", "TRBV20-1_TRBJ2-4", 
                        "TRBV20-1_TRBJ2-5", "TRBV20-1_TRBJ2-6", 
                        "TRBV20-1_TRBJ2-7", "TRBV21-1_TRBJ1-1", 
                        "TRBV21-1_TRBJ1-2", "TRBV21-1_TRBJ1-3", 
                        "TRBV21-1_TRBJ1-4", "TRBV21-1_TRBJ1-5", 
                        "TRBV21-1_TRBJ1-6", "TRBV21-1_TRBJ2-1",
                        "TRBV21-1_TRBJ2-2", "TRBV21-1_TRBJ2-3", 
                        "TRBV21-1_TRBJ2-4", "TRBV21-1_TRBJ2-5", 
                        "TRBV21-1_TRBJ2-6", "TRBV21-1_TRBJ2-7",
                        "TRBV23-1_TRBJ1-1", "TRBV23-1_TRBJ1-2", 
                        "TRBV23-1_TRBJ1-3", "TRBV23-1_TRBJ1-4", 
                        "TRBV23-1_TRBJ1-5", "TRBV23-1_TRBJ1-6", 
                        "TRBV23-1_TRBJ2-1", "TRBV23-1_TRBJ2-2", 
                        "TRBV23-1_TRBJ2-3", "TRBV23-1_TRBJ2-4", 
                        "TRBV23-1_TRBJ2-5", "TRBV23-1_TRBJ2-6", 
                        "TRBV23-1_TRBJ2-7", "TRBV24-1_TRBJ1-1", 
                        "TRBV24-1_TRBJ1-2", "TRBV24-1_TRBJ1-3", 
                        "TRBV24-1_TRBJ1-4", "TRBV24-1_TRBJ1-5", 
                        "TRBV24-1_TRBJ1-6", "TRBV24-1_TRBJ2-1", 
                        "TRBV24-1_TRBJ2-2", "TRBV24-1_TRBJ2-2P", 
                        "TRBV24-1_TRBJ2-3", "TRBV24-1_TRBJ2-4", 
                        "TRBV24-1_TRBJ2-5", "TRBV24-1_TRBJ2-6",
                        "TRBV24-1_TRBJ2-7", "TRBV25-1_TRBJ1-1", 
                        "TRBV25-1_TRBJ1-2", "TRBV25-1_TRBJ1-3",
                        "TRBV25-1_TRBJ1-4", "TRBV25-1_TRBJ1-5", 
                        "TRBV25-1_TRBJ1-6", "TRBV25-1_TRBJ2-1",
                        "TRBV25-1_TRBJ2-2", "TRBV25-1_TRBJ2-3", 
                        "TRBV25-1_TRBJ2-4", "TRBV25-1_TRBJ2-5",
                        "TRBV25-1_TRBJ2-6", "TRBV25-1_TRBJ2-7", 
                        "TRBV27_TRBJ1-1", "TRBV27_TRBJ1-2", "TRBV27_TRBJ1-3", 
                        "TRBV27_TRBJ1-4", "TRBV27_TRBJ1-5", "TRBV27_TRBJ1-6",
                        "TRBV27_TRBJ2-1", "TRBV27_TRBJ2-2", "TRBV27_TRBJ2-3", 
                        "TRBV27_TRBJ2-4", "TRBV27_TRBJ2-5", "TRBV27_TRBJ2-6",
                        "TRBV27_TRBJ2-7", "TRBV28_TRBJ1-1", "TRBV28_TRBJ1-2", 
                        "TRBV28_TRBJ1-3", "TRBV28_TRBJ1-4", "TRBV28_TRBJ1-5",
                        "TRBV28_TRBJ1-6", "TRBV28_TRBJ2-1", "TRBV28_TRBJ2-2", 
                        "TRBV28_TRBJ2-3", "TRBV28_TRBJ2-4", "TRBV28_TRBJ2-5", 
                        "TRBV28_TRBJ2-6", "TRBV28_TRBJ2-7", "TRBV29-1_TRBJ1-1", 
                        "TRBV29-1_TRBJ1-2", "TRBV29-1_TRBJ1-3", 
                        "TRBV29-1_TRBJ1-4", "TRBV29-1_TRBJ1-5", 
                        "TRBV29-1_TRBJ1-6", "TRBV29-1_TRBJ2-1", 
                        "TRBV29-1_TRBJ2-2", "TRBV29-1_TRBJ2-3", 
                        "TRBV29-1_TRBJ2-4", "TRBV29-1_TRBJ2-5", 
                        "TRBV29-1_TRBJ2-6", "TRBV29-1_TRBJ2-7", 
                        "TRBV3-1_TRBJ1-1", "TRBV3-1_TRBJ1-2", "TRBV3-1_TRBJ1-3",
                        "TRBV3-1_TRBJ1-4", "TRBV3-1_TRBJ1-5", "TRBV3-1_TRBJ1-6", 
                        "TRBV3-1_TRBJ2-1", "TRBV3-1_TRBJ2-2", "TRBV3-1_TRBJ2-3", 
                        "TRBV3-1_TRBJ2-4", "TRBV3-1_TRBJ2-5", "TRBV3-1_TRBJ2-6", 
                        "TRBV3-1_TRBJ2-7", "TRBV30_TRBJ1-1", "TRBV30_TRBJ1-2",
                        "TRBV30_TRBJ1-3", "TRBV30_TRBJ1-4", "TRBV30_TRBJ1-5", 
                        "TRBV30_TRBJ1-6", "TRBV30_TRBJ2-1", "TRBV30_TRBJ2-2", 
                        "TRBV30_TRBJ2-2P", "TRBV30_TRBJ2-3", "TRBV30_TRBJ2-4", 
                        "TRBV30_TRBJ2-5", "TRBV30_TRBJ2-6", "TRBV30_TRBJ2-7",
                        "TRBV4-1_TRBJ1-1", "TRBV4-1_TRBJ1-2", "TRBV4-1_TRBJ1-3", 
                        "TRBV4-1_TRBJ1-4", "TRBV4-1_TRBJ1-5", "TRBV4-1_TRBJ1-6", 
                        "TRBV4-1_TRBJ2-1", "TRBV4-1_TRBJ2-2", "TRBV4-1_TRBJ2-3", 
                        "TRBV4-1_TRBJ2-4", "TRBV4-1_TRBJ2-5", "TRBV4-1_TRBJ2-6",
                        "TRBV4-1_TRBJ2-7", "TRBV4-2_TRBJ1-1", "TRBV4-2_TRBJ1-2", 
                        "TRBV4-2_TRBJ1-3", "TRBV4-2_TRBJ1-4", "TRBV4-2_TRBJ1-5", 
                        "TRBV4-2_TRBJ1-6", "TRBV4-2_TRBJ2-1", "TRBV4-2_TRBJ2-2", 
                        "TRBV4-2_TRBJ2-3", "TRBV4-2_TRBJ2-4", "TRBV4-2_TRBJ2-5",
                        "TRBV4-2_TRBJ2-6", "TRBV4-2_TRBJ2-7", "TRBV4-3_TRBJ1-1", 
                        "TRBV4-3_TRBJ1-2", "TRBV4-3_TRBJ1-3", "TRBV4-3_TRBJ1-4", 
                        "TRBV4-3_TRBJ1-5", "TRBV4-3_TRBJ1-6", "TRBV4-3_TRBJ2-1", 
                        "TRBV4-3_TRBJ2-2", "TRBV4-3_TRBJ2-3", "TRBV4-3_TRBJ2-4",
                        "TRBV4-3_TRBJ2-5", "TRBV4-3_TRBJ2-6", "TRBV4-3_TRBJ2-7", 
                        "TRBV5-1_TRBJ1-1", "TRBV5-1_TRBJ1-2", "TRBV5-1_TRBJ1-3", 
                        "TRBV5-1_TRBJ1-4", "TRBV5-1_TRBJ1-5", "TRBV5-1_TRBJ1-6", 
                        "TRBV5-1_TRBJ2-1", "TRBV5-1_TRBJ2-2", "TRBV5-1_TRBJ2-3",
                        "TRBV5-1_TRBJ2-4", "TRBV5-1_TRBJ2-5", "TRBV5-1_TRBJ2-6", 
                        "TRBV5-1_TRBJ2-7", "TRBV5-3_TRBJ1-1", "TRBV5-3_TRBJ1-2",
                        "TRBV5-3_TRBJ1-3", "TRBV5-3_TRBJ1-4", "TRBV5-3_TRBJ1-5", 
                        "TRBV5-3_TRBJ1-6", "TRBV5-3_TRBJ2-1", "TRBV5-3_TRBJ2-2",
                        "TRBV5-3_TRBJ2-3", "TRBV5-3_TRBJ2-4", "TRBV5-3_TRBJ2-5", 
                        "TRBV5-3_TRBJ2-6", "TRBV5-3_TRBJ2-7", "TRBV5-4_TRBJ1-1", 
                        "TRBV5-4_TRBJ1-2", "TRBV5-4_TRBJ1-3", "TRBV5-4_TRBJ1-4", 
                        "TRBV5-4_TRBJ1-5", "TRBV5-4_TRBJ1-6", "TRBV5-4_TRBJ2-1",
                        "TRBV5-4_TRBJ2-2", "TRBV5-4_TRBJ2-3", "TRBV5-4_TRBJ2-4", 
                        "TRBV5-4_TRBJ2-5", "TRBV5-4_TRBJ2-6", "TRBV5-4_TRBJ2-7", 
                        "TRBV5-5_TRBJ1-1", "TRBV5-5_TRBJ1-2", "TRBV5-5_TRBJ1-3", 
                        "TRBV5-5_TRBJ1-4", "TRBV5-5_TRBJ1-5", "TRBV5-5_TRBJ1-6",
                        "TRBV5-5_TRBJ2-1", "TRBV5-5_TRBJ2-2", "TRBV5-5_TRBJ2-3", 
                        "TRBV5-5_TRBJ2-4", "TRBV5-5_TRBJ2-5", "TRBV5-5_TRBJ2-6", 
                        "TRBV5-5_TRBJ2-7", "TRBV5-6_TRBJ1-1", "TRBV5-6_TRBJ1-2", 
                        "TRBV5-6_TRBJ1-3", "TRBV5-6_TRBJ1-4", "TRBV5-6_TRBJ1-5",
                        "TRBV5-6_TRBJ1-6", "TRBV5-6_TRBJ2-1", "TRBV5-6_TRBJ2-2", 
                        "TRBV5-6_TRBJ2-3", "TRBV5-6_TRBJ2-4", "TRBV5-6_TRBJ2-5", 
                        "TRBV5-6_TRBJ2-6", "TRBV5-6_TRBJ2-7", "TRBV5-7_TRBJ1-1", 
                        "TRBV5-7_TRBJ1-2", "TRBV5-7_TRBJ1-3", "TRBV5-7_TRBJ1-4",
                        "TRBV5-7_TRBJ1-5", "TRBV5-7_TRBJ1-6", "TRBV5-7_TRBJ2-1", 
                        "TRBV5-7_TRBJ2-2", "TRBV5-7_TRBJ2-3", "TRBV5-7_TRBJ2-4", 
                        "TRBV5-7_TRBJ2-5", "TRBV5-7_TRBJ2-6", "TRBV5-7_TRBJ2-7",
                        "TRBV5-8_TRBJ1-1", "TRBV5-8_TRBJ1-2", "TRBV5-8_TRBJ1-3",
                        "TRBV5-8_TRBJ1-4", "TRBV5-8_TRBJ1-5", "TRBV5-8_TRBJ1-6", 
                        "TRBV5-8_TRBJ2-1", "TRBV5-8_TRBJ2-2", "TRBV5-8_TRBJ2-3", 
                        "TRBV5-8_TRBJ2-4", "TRBV5-8_TRBJ2-5", "TRBV5-8_TRBJ2-6", 
                        "TRBV5-8_TRBJ2-7", "TRBV6-1_TRBJ1-1", "TRBV6-1_TRBJ1-2",
                        "TRBV6-1_TRBJ1-3", "TRBV6-1_TRBJ1-4", "TRBV6-1_TRBJ1-5", 
                        "TRBV6-1_TRBJ1-6", "TRBV6-1_TRBJ2-1", "TRBV6-1_TRBJ2-2", 
                        "TRBV6-1_TRBJ2-3", "TRBV6-1_TRBJ2-4", "TRBV6-1_TRBJ2-5", 
                        "TRBV6-1_TRBJ2-6", "TRBV6-1_TRBJ2-7", "TRBV6-2_TRBJ1-1",
                        "TRBV6-2_TRBJ1-2", "TRBV6-2_TRBJ1-3", "TRBV6-2_TRBJ1-4", 
                        "TRBV6-2_TRBJ1-5", "TRBV6-2_TRBJ1-6", "TRBV6-2_TRBJ2-1", 
                        "TRBV6-2_TRBJ2-2", "TRBV6-2_TRBJ2-3", "TRBV6-2_TRBJ2-4", 
                        "TRBV6-2_TRBJ2-5", "TRBV6-2_TRBJ2-6", "TRBV6-2_TRBJ2-7",
                        "TRBV6-4_TRBJ1-1", "TRBV6-4_TRBJ1-2", "TRBV6-4_TRBJ1-3", 
                        "TRBV6-4_TRBJ1-4", "TRBV6-4_TRBJ1-5", "TRBV6-4_TRBJ1-6", 
                        "TRBV6-4_TRBJ2-1", "TRBV6-4_TRBJ2-2", 
                        "TRBV6-4_TRBJ2-2P", "TRBV6-4_TRBJ2-3", 
                        "TRBV6-4_TRBJ2-4", "TRBV6-4_TRBJ2-5", "TRBV6-4_TRBJ2-6", 
                        "TRBV6-4_TRBJ2-7", "TRBV6-5_TRBJ1-1", "TRBV6-5_TRBJ1-2", 
                        "TRBV6-5_TRBJ1-3", "TRBV6-5_TRBJ1-4", "TRBV6-5_TRBJ1-5",
                        "TRBV6-5_TRBJ1-6", "TRBV6-5_TRBJ2-1", "TRBV6-5_TRBJ2-2", 
                        "TRBV6-5_TRBJ2-3", "TRBV6-5_TRBJ2-4", "TRBV6-5_TRBJ2-5", 
                        "TRBV6-5_TRBJ2-6", "TRBV6-5_TRBJ2-7", "TRBV6-6_TRBJ1-1", 
                        "TRBV6-6_TRBJ1-2", "TRBV6-6_TRBJ1-3", "TRBV6-6_TRBJ1-4",
                        "TRBV6-6_TRBJ1-5", "TRBV6-6_TRBJ1-6", "TRBV6-6_TRBJ2-1", 
                        "TRBV6-6_TRBJ2-2", "TRBV6-6_TRBJ2-3", "TRBV6-6_TRBJ2-4", 
                        "TRBV6-6_TRBJ2-5", "TRBV6-6_TRBJ2-6", "TRBV6-6_TRBJ2-7", 
                        "TRBV6-7_TRBJ1-1", "TRBV6-7_TRBJ1-2", "TRBV6-7_TRBJ1-3",
                        "TRBV6-7_TRBJ1-4", "TRBV6-7_TRBJ1-5", "TRBV6-7_TRBJ1-6", 
                        "TRBV6-7_TRBJ2-1", "TRBV6-7_TRBJ2-2", "TRBV6-7_TRBJ2-3", 
                        "TRBV6-7_TRBJ2-4", "TRBV6-7_TRBJ2-5", "TRBV6-7_TRBJ2-6", 
                        "TRBV6-7_TRBJ2-7", "TRBV6-8_TRBJ1-1", "TRBV6-8_TRBJ1-2",
                        "TRBV6-8_TRBJ1-3", "TRBV6-8_TRBJ1-4", "TRBV6-8_TRBJ1-5", 
                        "TRBV6-8_TRBJ1-6", "TRBV6-8_TRBJ2-1", "TRBV6-8_TRBJ2-2", 
                         "TRBV6-8_TRBJ2-3", "TRBV6-8_TRBJ2-4", 
                        "TRBV6-8_TRBJ2-5", "TRBV6-8_TRBJ2-6", "TRBV6-8_TRBJ2-7", 
                        "TRBV6-9_TRBJ1-1", "TRBV6-9_TRBJ1-2", "TRBV6-9_TRBJ1-3", 
                        "TRBV6-9_TRBJ1-4", "TRBV6-9_TRBJ1-5", "TRBV6-9_TRBJ1-6", 
                        "TRBV6-9_TRBJ2-1", "TRBV6-9_TRBJ2-2", "TRBV6-9_TRBJ2-3",
                        "TRBV6-9_TRBJ2-4", "TRBV6-9_TRBJ2-5", "TRBV6-9_TRBJ2-6", 
                        "TRBV6-9_TRBJ2-7", "TRBV7-1_TRBJ1-1", "TRBV7-1_TRBJ1-2", 
                        "TRBV7-1_TRBJ1-3", "TRBV7-1_TRBJ1-4", "TRBV7-1_TRBJ1-5", 
                        "TRBV7-1_TRBJ1-6", "TRBV7-1_TRBJ2-1", "TRBV7-1_TRBJ2-2",
                        "TRBV7-1_TRBJ2-3", "TRBV7-1_TRBJ2-4", "TRBV7-1_TRBJ2-5", 
                        "TRBV7-1_TRBJ2-6", "TRBV7-1_TRBJ2-7", "TRBV7-2_TRBJ1-1", 
                        "TRBV7-2_TRBJ1-2", "TRBV7-2_TRBJ1-3", "TRBV7-2_TRBJ1-4", 
                        "TRBV7-2_TRBJ1-5", "TRBV7-2_TRBJ1-6", "TRBV7-2_TRBJ2-1",
                        "TRBV7-2_TRBJ2-2", "TRBV7-2_TRBJ2-3", "TRBV7-2_TRBJ2-4", 
                        "TRBV7-2_TRBJ2-5", "TRBV7-2_TRBJ2-6", "TRBV7-2_TRBJ2-7", 
                        "TRBV7-3_TRBJ1-1", "TRBV7-3_TRBJ1-2", "TRBV7-3_TRBJ1-3", 
                        "TRBV7-3_TRBJ1-4", "TRBV7-3_TRBJ1-5", "TRBV7-3_TRBJ1-6",
                        "TRBV7-3_TRBJ2-1", "TRBV7-3_TRBJ2-2", "TRBV7-3_TRBJ2-3", 
                        "TRBV7-3_TRBJ2-4", "TRBV7-3_TRBJ2-5", "TRBV7-3_TRBJ2-6", 
                        "TRBV7-3_TRBJ2-7", "TRBV7-4_TRBJ1-1", "TRBV7-4_TRBJ1-2", 
                        "TRBV7-4_TRBJ1-3", "TRBV7-4_TRBJ1-4", "TRBV7-4_TRBJ1-5",
                        "TRBV7-4_TRBJ1-6", "TRBV7-4_TRBJ2-1", "TRBV7-4_TRBJ2-2", 
                        "TRBV7-4_TRBJ2-3", "TRBV7-4_TRBJ2-4", "TRBV7-4_TRBJ2-5", 
                        "TRBV7-4_TRBJ2-6", "TRBV7-4_TRBJ2-7", "TRBV7-6_TRBJ1-1", 
                        "TRBV7-6_TRBJ1-2", "TRBV7-6_TRBJ1-3", "TRBV7-6_TRBJ1-4",
                        "TRBV7-6_TRBJ1-5", "TRBV7-6_TRBJ1-6", "TRBV7-6_TRBJ2-1", 
                        "TRBV7-6_TRBJ2-2", "TRBV7-6_TRBJ2-3", "TRBV7-6_TRBJ2-4", 
                        "TRBV7-6_TRBJ2-5", "TRBV7-6_TRBJ2-6", "TRBV7-6_TRBJ2-7", 
                        "TRBV7-7_TRBJ1-1", "TRBV7-7_TRBJ1-2", "TRBV7-7_TRBJ1-3",
                        "TRBV7-7_TRBJ1-4", "TRBV7-7_TRBJ1-5", "TRBV7-7_TRBJ1-6", 
                        "TRBV7-7_TRBJ2-1", "TRBV7-7_TRBJ2-2", "TRBV7-7_TRBJ2-3", 
                        "TRBV7-7_TRBJ2-4", "TRBV7-7_TRBJ2-5", "TRBV7-7_TRBJ2-6", 
                        "TRBV7-7_TRBJ2-7", "TRBV7-8_TRBJ1-1", "TRBV7-8_TRBJ1-2",
                        "TRBV7-8_TRBJ1-3", "TRBV7-8_TRBJ1-4", "TRBV7-8_TRBJ1-5", 
                        "TRBV7-8_TRBJ1-6", "TRBV7-8_TRBJ2-1", "TRBV7-8_TRBJ2-2", 
                        "TRBV7-8_TRBJ2-3", "TRBV7-8_TRBJ2-4", "TRBV7-8_TRBJ2-5", 
                        "TRBV7-8_TRBJ2-6", "TRBV7-8_TRBJ2-7", "TRBV7-9_TRBJ1-1",
                        "TRBV7-9_TRBJ1-2", "TRBV7-9_TRBJ1-3", "TRBV7-9_TRBJ1-4", 
                        "TRBV7-9_TRBJ1-5", "TRBV7-9_TRBJ1-6", "TRBV7-9_TRBJ2-1", 
                        "TRBV7-9_TRBJ2-2", "TRBV7-9_TRBJ2-3", "TRBV7-9_TRBJ2-4", 
                        "TRBV7-9_TRBJ2-5", "TRBV7-9_TRBJ2-6", "TRBV7-9_TRBJ2-7",
                        "TRBV9_TRBJ1-1", "TRBV9_TRBJ1-2", "TRBV9_TRBJ1-3", 
                        "TRBV9_TRBJ1-4", "TRBV9_TRBJ1-5", "TRBV9_TRBJ1-6", 
                        "TRBV9_TRBJ2-1", "TRBV9_TRBJ2-2", "TRBV9_TRBJ2-3", 
                        "TRBV9_TRBJ2-4", "TRBV9_TRBJ2-5", "TRBV9_TRBJ2-6", 
                        "TRBV9_TRBJ2-7")

preTrans <- preTrans[-1,]

preTrans$Patient.ID <- c("1056201652", "1056201734", "1056201763", "1056201766",
                         "1093501642", "1093501649", "1093501690", "1245501789",
                         "1322701679", "1351901585", "1351901590", "1351901743", 
                         "1351901799", "1371101668", "1371101712", "2000044446",
                         "2000044709", "2000044739", "2000044755", "2000044784",
                         "2000134826", "2000167902", "2000199725", "2000206742", 
                         "2000206980", "2000210806", "2000210942", "2000211830",
                         "10562011366", "10562011565", "12455011536",
                         "13227011150", "13519011353", "13711011381",
                         "20000891275", "20001081074", "20001331759",
                         "20001351017", "20001351914", "20001352321",
                         "20001371882", "20001971574", "20001971832",
                         "20001991312", "20002061151", "20002061360",
                         "20002062054", "20002081893", "20002091885",
                         "20002101827", "20002111889", "20002131864",
                         "20002181234", "20002181334", "20004391109",
                         "20004391114", "20007341206", "20007341222",
                         "20007341336", "20007341420", "20011092291")

preTrans <- select(preTrans, Patient.ID, "TRBV1_TRBJ1-1", "TRBV1_TRBJ1-2", 
                   "TRBV1_TRBJ1-3", "TRBV1_TRBJ1-4", "TRBV1_TRBJ1-5", 
                   "TRBV1_TRBJ1-6", "TRBV1_TRBJ2-1", "TRBV1_TRBJ2-2", 
                   "TRBV1_TRBJ2-3", "TRBV1_TRBJ2-4", "TRBV1_TRBJ2-5", 
                   "TRBV1_TRBJ2-6", "TRBV1_TRBJ2-7", "TRBV10-1_TRBJ1-1", 
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
                   "TRBV12-1_TRBJ1-2", "TRBV12-1_TRBJ1-3", "TRBV12-1_TRBJ1-4",
                   "TRBV12-1_TRBJ1-5", "TRBV12-1_TRBJ1-6", "TRBV12-1_TRBJ2-1", 
                   "TRBV12-1_TRBJ2-2", "TRBV12-1_TRBJ2-3", "TRBV12-1_TRBJ2-4", 
                   "TRBV12-1_TRBJ2-5", "TRBV12-1_TRBJ2-6", "TRBV12-1_TRBJ2-7", 
                   "TRBV12-2_TRBJ1-1", "TRBV12-2_TRBJ1-2", "TRBV12-2_TRBJ1-3",
                   "TRBV12-2_TRBJ1-4", "TRBV12-2_TRBJ1-5", "TRBV12-2_TRBJ1-6", 
                   "TRBV12-2_TRBJ2-1", "TRBV12-2_TRBJ2-2", "TRBV12-2_TRBJ2-2P", 
                   "TRBV12-2_TRBJ2-3", "TRBV12-2_TRBJ2-4", "TRBV12-2_TRBJ2-5", 
                   "TRBV12-2_TRBJ2-6", "TRBV12-2_TRBJ2-7", "TRBV12-3_TRBJ1-1",
                   "TRBV12-3_TRBJ1-2", "TRBV12-3_TRBJ1-3", "TRBV12-3_TRBJ1-4", 
                   "TRBV12-3_TRBJ1-5", "TRBV12-3_TRBJ1-6", "TRBV12-3_TRBJ2-1", 
                   "TRBV12-3_TRBJ2-2", "TRBV12-3_TRBJ2-3", "TRBV12-3_TRBJ2-4", 
                   "TRBV12-3_TRBJ2-5", "TRBV12-3_TRBJ2-6", "TRBV12-3_TRBJ2-7",
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
                   "TRBV15_TRBJ2-7", "TRBV16_TRBJ1-1", "TRBV16_TRBJ1-2", 
                   "TRBV16_TRBJ1-3", "TRBV16_TRBJ1-4", "TRBV16_TRBJ1-5", 
                   "TRBV16_TRBJ1-6", "TRBV16_TRBJ2-1", "TRBV16_TRBJ2-2",
                   "TRBV16_TRBJ2-3", "TRBV16_TRBJ2-4", "TRBV16_TRBJ2-5", 
                   "TRBV16_TRBJ2-6", "TRBV16_TRBJ2-7", "TRBV17_TRBJ1-1", 
                   "TRBV17_TRBJ1-3", "TRBV17_TRBJ1-5", "TRBV17_TRBJ2-2P", 
                   "TRBV17_TRBJ2-3", "TRBV17_TRBJ2-7", "TRBV18_TRBJ1-1",
                   "TRBV18_TRBJ1-2", "TRBV18_TRBJ1-3", "TRBV18_TRBJ1-4", 
                   "TRBV18_TRBJ1-5", "TRBV18_TRBJ1-6", "TRBV18_TRBJ2-1", 
                   "TRBV18_TRBJ2-2", "TRBV18_TRBJ2-3", "TRBV18_TRBJ2-4", 
                   "TRBV18_TRBJ2-5", "TRBV18_TRBJ2-6", "TRBV18_TRBJ2-7", 
                   "TRBV19_TRBJ1-1", "TRBV19_TRBJ1-2", "TRBV19_TRBJ1-3", 
                   "TRBV19_TRBJ1-4", "TRBV19_TRBJ1-5", "TRBV19_TRBJ1-6", 
                   "TRBV19_TRBJ2-1", "TRBV19_TRBJ2-2", "TRBV19_TRBJ2-2P", 
                   "TRBV19_TRBJ2-3", "TRBV19_TRBJ2-4", "TRBV19_TRBJ2-5",
                   "TRBV19_TRBJ2-6", "TRBV19_TRBJ2-7", "TRBV2_TRBJ1-1", 
                   "TRBV2_TRBJ1-2", "TRBV2_TRBJ1-3", "TRBV2_TRBJ1-4", 
                   "TRBV2_TRBJ1-5", "TRBV2_TRBJ1-6", "TRBV2_TRBJ2-1", 
                   "TRBV2_TRBJ2-2", "TRBV2_TRBJ2-3", "TRBV2_TRBJ2-4", 
                   "TRBV2_TRBJ2-5", "TRBV2_TRBJ2-6", "TRBV2_TRBJ2-7", 
                   "TRBV20-1_TRBJ1-1", "TRBV20-1_TRBJ1-2", "TRBV20-1_TRBJ1-3", 
                   "TRBV20-1_TRBJ1-4", "TRBV20-1_TRBJ1-5", "TRBV20-1_TRBJ1-6", 
                   "TRBV20-1_TRBJ2-1", "TRBV20-1_TRBJ2-2", "TRBV20-1_TRBJ2-3", 
                   "TRBV20-1_TRBJ2-4", "TRBV20-1_TRBJ2-5", "TRBV20-1_TRBJ2-6", 
                   "TRBV20-1_TRBJ2-7", "TRBV21-1_TRBJ1-1", "TRBV21-1_TRBJ1-2", 
                   "TRBV21-1_TRBJ1-3", "TRBV21-1_TRBJ1-4", "TRBV21-1_TRBJ1-5", 
                   "TRBV21-1_TRBJ1-6", "TRBV21-1_TRBJ2-1", "TRBV21-1_TRBJ2-2", 
                   "TRBV21-1_TRBJ2-3", "TRBV21-1_TRBJ2-4", "TRBV21-1_TRBJ2-5", 
                   "TRBV21-1_TRBJ2-6", "TRBV21-1_TRBJ2-7", "TRBV23-1_TRBJ1-1", 
                   "TRBV23-1_TRBJ1-2", "TRBV23-1_TRBJ1-3", "TRBV23-1_TRBJ1-4", 
                   "TRBV23-1_TRBJ1-5", "TRBV23-1_TRBJ1-6", "TRBV23-1_TRBJ2-1", 
                   "TRBV23-1_TRBJ2-2", "TRBV23-1_TRBJ2-3", "TRBV23-1_TRBJ2-4", 
                   "TRBV23-1_TRBJ2-5", "TRBV23-1_TRBJ2-6", "TRBV23-1_TRBJ2-7", 
                   "TRBV24-1_TRBJ1-1", "TRBV24-1_TRBJ1-2", "TRBV24-1_TRBJ1-3", 
                   "TRBV24-1_TRBJ1-4", "TRBV24-1_TRBJ1-5", "TRBV24-1_TRBJ1-6", 
                   "TRBV24-1_TRBJ2-1", "TRBV24-1_TRBJ2-2", "TRBV24-1_TRBJ2-2P", 
                   "TRBV24-1_TRBJ2-3", "TRBV24-1_TRBJ2-4", "TRBV24-1_TRBJ2-5", 
                   "TRBV24-1_TRBJ2-6", "TRBV24-1_TRBJ2-7", "TRBV25-1_TRBJ1-1", 
                   "TRBV25-1_TRBJ1-2", "TRBV25-1_TRBJ1-3", "TRBV25-1_TRBJ1-4", 
                   "TRBV25-1_TRBJ1-5", "TRBV25-1_TRBJ1-6", "TRBV25-1_TRBJ2-1",
                   "TRBV25-1_TRBJ2-2", "TRBV25-1_TRBJ2-3", "TRBV25-1_TRBJ2-4", 
                   "TRBV25-1_TRBJ2-5", "TRBV25-1_TRBJ2-6", "TRBV25-1_TRBJ2-7", 
                   "TRBV27_TRBJ1-1", "TRBV27_TRBJ1-2", "TRBV27_TRBJ1-3", 
                   "TRBV27_TRBJ1-4", "TRBV27_TRBJ1-5", "TRBV27_TRBJ1-6",
                   "TRBV27_TRBJ2-1", "TRBV27_TRBJ2-2", "TRBV27_TRBJ2-3", 
                   "TRBV27_TRBJ2-4", "TRBV27_TRBJ2-5", "TRBV27_TRBJ2-6",
                   "TRBV27_TRBJ2-7", "TRBV28_TRBJ1-1", "TRBV28_TRBJ1-2", 
                   "TRBV28_TRBJ1-3", "TRBV28_TRBJ1-4", "TRBV28_TRBJ1-5",
                   "TRBV28_TRBJ1-6", "TRBV28_TRBJ2-1", "TRBV28_TRBJ2-2", 
                   "TRBV28_TRBJ2-3", "TRBV28_TRBJ2-4", "TRBV28_TRBJ2-5", 
                   "TRBV28_TRBJ2-6", "TRBV28_TRBJ2-7", "TRBV29-1_TRBJ1-1", 
                   "TRBV29-1_TRBJ1-2", "TRBV29-1_TRBJ1-3", "TRBV29-1_TRBJ1-4", 
                   "TRBV29-1_TRBJ1-5", "TRBV29-1_TRBJ1-6", "TRBV29-1_TRBJ2-1", 
                   "TRBV29-1_TRBJ2-2", "TRBV29-1_TRBJ2-3", "TRBV29-1_TRBJ2-4", 
                   "TRBV29-1_TRBJ2-5", "TRBV29-1_TRBJ2-6", "TRBV29-1_TRBJ2-7", 
                   "TRBV3-1_TRBJ1-1", "TRBV3-1_TRBJ1-2", "TRBV3-1_TRBJ1-3",
                   "TRBV3-1_TRBJ1-4", "TRBV3-1_TRBJ1-5", "TRBV3-1_TRBJ1-6", 
                   "TRBV3-1_TRBJ2-1", "TRBV3-1_TRBJ2-2", "TRBV3-1_TRBJ2-3", 
                   "TRBV3-1_TRBJ2-4", "TRBV3-1_TRBJ2-5", "TRBV3-1_TRBJ2-6", 
                   "TRBV3-1_TRBJ2-7", "TRBV30_TRBJ1-1", "TRBV30_TRBJ1-2",
                   "TRBV30_TRBJ1-3", "TRBV30_TRBJ1-4", "TRBV30_TRBJ1-5", 
                   "TRBV30_TRBJ1-6", "TRBV30_TRBJ2-1", "TRBV30_TRBJ2-2", 
                   "TRBV30_TRBJ2-2P", "TRBV30_TRBJ2-3", "TRBV30_TRBJ2-4", 
                   "TRBV30_TRBJ2-5", "TRBV30_TRBJ2-6", "TRBV30_TRBJ2-7",
                   "TRBV4-1_TRBJ1-1", "TRBV4-1_TRBJ1-2", "TRBV4-1_TRBJ1-3", 
                   "TRBV4-1_TRBJ1-4", "TRBV4-1_TRBJ1-5", "TRBV4-1_TRBJ1-6", 
                   "TRBV4-1_TRBJ2-1", "TRBV4-1_TRBJ2-2", "TRBV4-1_TRBJ2-3", 
                   "TRBV4-1_TRBJ2-4", "TRBV4-1_TRBJ2-5", "TRBV4-1_TRBJ2-6",
                   "TRBV4-1_TRBJ2-7", "TRBV4-2_TRBJ1-1", "TRBV4-2_TRBJ1-2", 
                   "TRBV4-2_TRBJ1-3", "TRBV4-2_TRBJ1-4", "TRBV4-2_TRBJ1-5", 
                   "TRBV4-2_TRBJ1-6", "TRBV4-2_TRBJ2-1", "TRBV4-2_TRBJ2-2", 
                   "TRBV4-2_TRBJ2-3", "TRBV4-2_TRBJ2-4", "TRBV4-2_TRBJ2-5",
                   "TRBV4-2_TRBJ2-6", "TRBV4-2_TRBJ2-7", "TRBV4-3_TRBJ1-1", 
                   "TRBV4-3_TRBJ1-2", "TRBV4-3_TRBJ1-3", "TRBV4-3_TRBJ1-4", 
                   "TRBV4-3_TRBJ1-5", "TRBV4-3_TRBJ1-6", "TRBV4-3_TRBJ2-1", 
                   "TRBV4-3_TRBJ2-2", "TRBV4-3_TRBJ2-3", "TRBV4-3_TRBJ2-4",
                   "TRBV4-3_TRBJ2-5", "TRBV4-3_TRBJ2-6", "TRBV4-3_TRBJ2-7", 
                   "TRBV5-1_TRBJ1-1", "TRBV5-1_TRBJ1-2", "TRBV5-1_TRBJ1-3", 
                   "TRBV5-1_TRBJ1-4", "TRBV5-1_TRBJ1-5", "TRBV5-1_TRBJ1-6", 
                   "TRBV5-1_TRBJ2-1", "TRBV5-1_TRBJ2-2", "TRBV5-1_TRBJ2-3",
                   "TRBV5-1_TRBJ2-4", "TRBV5-1_TRBJ2-5", "TRBV5-1_TRBJ2-6", 
                   "TRBV5-1_TRBJ2-7", "TRBV5-3_TRBJ1-1", "TRBV5-3_TRBJ1-2",
                   "TRBV5-3_TRBJ1-3", "TRBV5-3_TRBJ1-4", "TRBV5-3_TRBJ1-5", 
                   "TRBV5-3_TRBJ1-6", "TRBV5-3_TRBJ2-1", "TRBV5-3_TRBJ2-2",
                   "TRBV5-3_TRBJ2-3", "TRBV5-3_TRBJ2-4", "TRBV5-3_TRBJ2-5", 
                   "TRBV5-3_TRBJ2-6", "TRBV5-3_TRBJ2-7", "TRBV5-4_TRBJ1-1", 
                   "TRBV5-4_TRBJ1-2", "TRBV5-4_TRBJ1-3", "TRBV5-4_TRBJ1-4", 
                   "TRBV5-4_TRBJ1-5", "TRBV5-4_TRBJ1-6", "TRBV5-4_TRBJ2-1",
                   "TRBV5-4_TRBJ2-2", "TRBV5-4_TRBJ2-3", "TRBV5-4_TRBJ2-4", 
                   "TRBV5-4_TRBJ2-5", "TRBV5-4_TRBJ2-6", "TRBV5-4_TRBJ2-7", 
                   "TRBV5-5_TRBJ1-1", "TRBV5-5_TRBJ1-2", "TRBV5-5_TRBJ1-3", 
                   "TRBV5-5_TRBJ1-4", "TRBV5-5_TRBJ1-5", "TRBV5-5_TRBJ1-6",
                   "TRBV5-5_TRBJ2-1", "TRBV5-5_TRBJ2-2", "TRBV5-5_TRBJ2-3", 
                   "TRBV5-5_TRBJ2-4", "TRBV5-5_TRBJ2-5", "TRBV5-5_TRBJ2-6", 
                   "TRBV5-5_TRBJ2-7", "TRBV5-6_TRBJ1-1", "TRBV5-6_TRBJ1-2", 
                   "TRBV5-6_TRBJ1-3", "TRBV5-6_TRBJ1-4", "TRBV5-6_TRBJ1-5",
                   "TRBV5-6_TRBJ1-6", "TRBV5-6_TRBJ2-1", "TRBV5-6_TRBJ2-2", 
                   "TRBV5-6_TRBJ2-3", "TRBV5-6_TRBJ2-4", "TRBV5-6_TRBJ2-5", 
                   "TRBV5-6_TRBJ2-6", "TRBV5-6_TRBJ2-7", "TRBV5-7_TRBJ1-1", 
                   "TRBV5-7_TRBJ1-2", "TRBV5-7_TRBJ1-3", "TRBV5-7_TRBJ1-4",
                   "TRBV5-7_TRBJ1-5", "TRBV5-7_TRBJ1-6", "TRBV5-7_TRBJ2-1", 
                   "TRBV5-7_TRBJ2-2", "TRBV5-7_TRBJ2-3", "TRBV5-7_TRBJ2-4", 
                   "TRBV5-7_TRBJ2-5", "TRBV5-7_TRBJ2-6", "TRBV5-7_TRBJ2-7",
                   "TRBV5-8_TRBJ1-1", "TRBV5-8_TRBJ1-2", "TRBV5-8_TRBJ1-3",
                   "TRBV5-8_TRBJ1-4", "TRBV5-8_TRBJ1-5", "TRBV5-8_TRBJ1-6", 
                   "TRBV5-8_TRBJ2-1", "TRBV5-8_TRBJ2-2", "TRBV5-8_TRBJ2-3", 
                   "TRBV5-8_TRBJ2-4", "TRBV5-8_TRBJ2-5", "TRBV5-8_TRBJ2-6", 
                   "TRBV5-8_TRBJ2-7", "TRBV6-1_TRBJ1-1", "TRBV6-1_TRBJ1-2",
                   "TRBV6-1_TRBJ1-3", "TRBV6-1_TRBJ1-4", "TRBV6-1_TRBJ1-5", 
                   "TRBV6-1_TRBJ1-6", "TRBV6-1_TRBJ2-1", "TRBV6-1_TRBJ2-2", 
                   "TRBV6-1_TRBJ2-3", "TRBV6-1_TRBJ2-4", "TRBV6-1_TRBJ2-5", 
                   "TRBV6-1_TRBJ2-6", "TRBV6-1_TRBJ2-7", "TRBV6-2_TRBJ1-1",
                   "TRBV6-2_TRBJ1-2", "TRBV6-2_TRBJ1-3", "TRBV6-2_TRBJ1-4", 
                   "TRBV6-2_TRBJ1-5", "TRBV6-2_TRBJ1-6", "TRBV6-2_TRBJ2-1", 
                   "TRBV6-2_TRBJ2-2", "TRBV6-2_TRBJ2-3", "TRBV6-2_TRBJ2-4", 
                   "TRBV6-2_TRBJ2-5", "TRBV6-2_TRBJ2-6", "TRBV6-2_TRBJ2-7",
                   "TRBV6-4_TRBJ1-1", "TRBV6-4_TRBJ1-2", "TRBV6-4_TRBJ1-3", 
                   "TRBV6-4_TRBJ1-4", "TRBV6-4_TRBJ1-5", "TRBV6-4_TRBJ1-6", 
                   "TRBV6-4_TRBJ2-1", "TRBV6-4_TRBJ2-2", "TRBV6-4_TRBJ2-2P", 
                   "TRBV6-4_TRBJ2-3", "TRBV6-4_TRBJ2-4", "TRBV6-4_TRBJ2-5", 
                   "TRBV6-4_TRBJ2-6", "TRBV6-4_TRBJ2-7", "TRBV6-5_TRBJ1-1", 
                   "TRBV6-5_TRBJ1-2", "TRBV6-5_TRBJ1-3", "TRBV6-5_TRBJ1-4", 
                   "TRBV6-5_TRBJ1-5", "TRBV6-5_TRBJ1-6", "TRBV6-5_TRBJ2-1", 
                   "TRBV6-5_TRBJ2-2", "TRBV6-5_TRBJ2-3", "TRBV6-5_TRBJ2-4", 
                   "TRBV6-5_TRBJ2-5", "TRBV6-5_TRBJ2-6", "TRBV6-5_TRBJ2-7", 
                   "TRBV6-6_TRBJ1-1", "TRBV6-6_TRBJ1-2", "TRBV6-6_TRBJ1-3", 
                   "TRBV6-6_TRBJ1-4", "TRBV6-6_TRBJ1-5", "TRBV6-6_TRBJ1-6", 
                   "TRBV6-6_TRBJ2-1", "TRBV6-6_TRBJ2-2", "TRBV6-6_TRBJ2-3", 
                   "TRBV6-6_TRBJ2-4", "TRBV6-6_TRBJ2-5", "TRBV6-6_TRBJ2-6", 
                   "TRBV6-6_TRBJ2-7", "TRBV6-7_TRBJ1-1", "TRBV6-7_TRBJ1-2", 
                   "TRBV6-7_TRBJ1-3", "TRBV6-7_TRBJ1-4", "TRBV6-7_TRBJ1-5", 
                   "TRBV6-7_TRBJ1-6", "TRBV6-7_TRBJ2-1", "TRBV6-7_TRBJ2-2", 
                   "TRBV6-7_TRBJ2-3", "TRBV6-7_TRBJ2-4", "TRBV6-7_TRBJ2-5",
                   "TRBV6-7_TRBJ2-6", "TRBV6-7_TRBJ2-7", "TRBV6-8_TRBJ1-1",
                   "TRBV6-8_TRBJ1-2", "TRBV6-8_TRBJ1-3", "TRBV6-8_TRBJ1-4", 
                   "TRBV6-8_TRBJ1-5", "TRBV6-8_TRBJ1-6", "TRBV6-8_TRBJ2-1", 
                   "TRBV6-8_TRBJ2-2", "TRBV6-8_TRBJ2-3", "TRBV6-8_TRBJ2-4", 
                   "TRBV6-8_TRBJ2-5", "TRBV6-8_TRBJ2-6", "TRBV6-8_TRBJ2-7", 
                   "TRBV6-9_TRBJ1-1", "TRBV6-9_TRBJ1-2", "TRBV6-9_TRBJ1-3", 
                   "TRBV6-9_TRBJ1-4", "TRBV6-9_TRBJ1-5", "TRBV6-9_TRBJ1-6", 
                   "TRBV6-9_TRBJ2-1", "TRBV6-9_TRBJ2-2", "TRBV6-9_TRBJ2-3",
                   "TRBV6-9_TRBJ2-4", "TRBV6-9_TRBJ2-5", "TRBV6-9_TRBJ2-6", 
                   "TRBV6-9_TRBJ2-7", "TRBV7-1_TRBJ1-1", "TRBV7-1_TRBJ1-2", 
                   "TRBV7-1_TRBJ1-3", "TRBV7-1_TRBJ1-4", "TRBV7-1_TRBJ1-5", 
                   "TRBV7-1_TRBJ1-6", "TRBV7-1_TRBJ2-1", "TRBV7-1_TRBJ2-2",
                   "TRBV7-1_TRBJ2-3", "TRBV7-1_TRBJ2-4", "TRBV7-1_TRBJ2-5", 
                   "TRBV7-1_TRBJ2-6", "TRBV7-1_TRBJ2-7", "TRBV7-2_TRBJ1-1", 
                   "TRBV7-2_TRBJ1-2", "TRBV7-2_TRBJ1-3", "TRBV7-2_TRBJ1-4", 
                   "TRBV7-2_TRBJ1-5", "TRBV7-2_TRBJ1-6", "TRBV7-2_TRBJ2-1",
                   "TRBV7-2_TRBJ2-2", "TRBV7-2_TRBJ2-3", "TRBV7-2_TRBJ2-4", 
                   "TRBV7-2_TRBJ2-5", "TRBV7-2_TRBJ2-6", "TRBV7-2_TRBJ2-7", 
                   "TRBV7-3_TRBJ1-1", "TRBV7-3_TRBJ1-2", "TRBV7-3_TRBJ1-3", 
                   "TRBV7-3_TRBJ1-4", "TRBV7-3_TRBJ1-5", "TRBV7-3_TRBJ1-6",
                   "TRBV7-3_TRBJ2-1", "TRBV7-3_TRBJ2-2", "TRBV7-3_TRBJ2-3", 
                   "TRBV7-3_TRBJ2-4", "TRBV7-3_TRBJ2-5", "TRBV7-3_TRBJ2-6", 
                   "TRBV7-3_TRBJ2-7", "TRBV7-4_TRBJ1-1", "TRBV7-4_TRBJ1-2", 
                   "TRBV7-4_TRBJ1-3", "TRBV7-4_TRBJ1-4", "TRBV7-4_TRBJ1-5",
                   "TRBV7-4_TRBJ1-6", "TRBV7-4_TRBJ2-1", "TRBV7-4_TRBJ2-2", 
                   "TRBV7-4_TRBJ2-3", "TRBV7-4_TRBJ2-4", "TRBV7-4_TRBJ2-5", 
                   "TRBV7-4_TRBJ2-6", "TRBV7-4_TRBJ2-7", "TRBV7-6_TRBJ1-1", 
                   "TRBV7-6_TRBJ1-2", "TRBV7-6_TRBJ1-3", "TRBV7-6_TRBJ1-4",
                   "TRBV7-6_TRBJ1-5", "TRBV7-6_TRBJ1-6", "TRBV7-6_TRBJ2-1", 
                   "TRBV7-6_TRBJ2-2", "TRBV7-6_TRBJ2-3", "TRBV7-6_TRBJ2-4", 
                   "TRBV7-6_TRBJ2-5", "TRBV7-6_TRBJ2-6", "TRBV7-6_TRBJ2-7", 
                   "TRBV7-7_TRBJ1-1", "TRBV7-7_TRBJ1-2", "TRBV7-7_TRBJ1-3",
                   "TRBV7-7_TRBJ1-4", "TRBV7-7_TRBJ1-5", "TRBV7-7_TRBJ1-6", 
                   "TRBV7-7_TRBJ2-1", "TRBV7-7_TRBJ2-2", "TRBV7-7_TRBJ2-3", 
                   "TRBV7-7_TRBJ2-4", "TRBV7-7_TRBJ2-5", "TRBV7-7_TRBJ2-6", 
                   "TRBV7-7_TRBJ2-7", "TRBV7-8_TRBJ1-1", "TRBV7-8_TRBJ1-2",
                   "TRBV7-8_TRBJ1-3", "TRBV7-8_TRBJ1-4", "TRBV7-8_TRBJ1-5", 
                   "TRBV7-8_TRBJ1-6", "TRBV7-8_TRBJ2-1", "TRBV7-8_TRBJ2-2", 
                   "TRBV7-8_TRBJ2-3", "TRBV7-8_TRBJ2-4", "TRBV7-8_TRBJ2-5", 
                   "TRBV7-8_TRBJ2-6", "TRBV7-8_TRBJ2-7", "TRBV7-9_TRBJ1-1",
                   "TRBV7-9_TRBJ1-2", "TRBV7-9_TRBJ1-3", "TRBV7-9_TRBJ1-4", 
                   "TRBV7-9_TRBJ1-5", "TRBV7-9_TRBJ1-6", "TRBV7-9_TRBJ2-1", 
                   "TRBV7-9_TRBJ2-2", "TRBV7-9_TRBJ2-3", "TRBV7-9_TRBJ2-4", 
                   "TRBV7-9_TRBJ2-5", "TRBV7-9_TRBJ2-6", "TRBV7-9_TRBJ2-7",
                   "TRBV9_TRBJ1-1", "TRBV9_TRBJ1-2", "TRBV9_TRBJ1-3", 
                   "TRBV9_TRBJ1-4", "TRBV9_TRBJ1-5", "TRBV9_TRBJ1-6", 
                   "TRBV9_TRBJ2-1", "TRBV9_TRBJ2-2", "TRBV9_TRBJ2-3", 
                   "TRBV9_TRBJ2-4", "TRBV9_TRBJ2-5", "TRBV9_TRBJ2-6", 
                   "TRBV9_TRBJ2-7")

rownames(preTrans) <- c


# 2nd dose
second <- lungene[, c(1, 63:123)]

second[is.na(second)] <- 1e-7 

colnames(second) <- c("vjGene", "1056201630", "1056201652", "1056201723", 
                      "1056201734", "1056201763", "1056201766", "1093501642",
                      "1093501649", "1093501690", "1322701679", "1351901585",
                      "1351901590", "1351901743", "1351901799", "1371101668",
                      "1371101712", "2000044446", "2000044739", "2000044755",
                      "2000044784", "2000134826", "2000199725", "2000206731",
                      "2000206742", "2000206980", "2000210806", "2000210942",
                      "2000211830", "10025011361", "10562011366", "10562011565",
                      "12455011536", "13227011150", "13519011344", 
                      "13519011345", "13519011353", "13711011381", 
                      "20000891275", "20001081074", "20001331759", 
                      "20001351017", "20001352321", "20001971574", 
                      "20001991312", "20002061360", "20002062054", 
                      "20002081760", "20002081893", "20002091874", 
                      "20002091885", "20002101827", "2000211132", "20002132350",
                      "20002181234", "20004391109", "20004391114", 
                      "20007341206", "20007341222", "20007341336", 
                      "20007341420", "20011092291")

