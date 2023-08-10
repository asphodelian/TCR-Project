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

# colnames

pt.names <- colnames(combine)
pt.length <- numeric(70)
newName <- character(pt.length)

for(i in 1:70) {
  a <- strsplit(pt.names[i], split = "-")[[1]]
  newName[i] <- paste(a[3], a[4], sep = "_")
}

rownames(transComb.std)[1:70] <- newName

healthy.pt <- c(paste("HD", 1:9, sep = "0"), paste("HD", 10:39, sep = ""))

rownames(transComb.std)[71:109] <- healthy.pt
