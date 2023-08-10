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

pre <- read_excel("C:/Users/knigh/OneDrive/Desktop/Github/TCR-Project/Datasets/Lung Data/predose.xlsx")
dim(pre)

second <- read_excel("C:/Users/knigh/OneDrive/Desktop/Github/TCR-Project/Datasets/Lung Data/2ndose.xlsx")
dim(second)
