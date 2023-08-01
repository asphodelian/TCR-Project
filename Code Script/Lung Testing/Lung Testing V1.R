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




