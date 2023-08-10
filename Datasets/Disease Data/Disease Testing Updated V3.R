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
# Data Prep #
#############

# combine the vj datasets
combine <- merge(C19vj, vj, by = "vjGene")

# replace NAs w/ extremely small number
combine[is.na(combine)] <- 1e-7

# rename rows as vjGene
row.names(combine) <- combine[,1]

# removed col
combine$vjGene <- NULL

# log
combine <- log(combine) 

# transpose
transComb <- t(combine)

# standardize
transComb.std <- scale(transComb)
