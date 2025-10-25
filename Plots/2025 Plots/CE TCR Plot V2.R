##########################
# Culminating Experience #
##########################

#############
# Libraries #
#############

library(corrplot)
library(dplyr)
library(factoextra)
library(ggfortify)
library(ggplot2)
library(gridExtra)
library(psych)
library(readr)
library(readxl)
library(SKAT)
library(tidyr)

library(caret)
library(stats)
library(MASS) # QDA & LDA
library(class) # KNN

###########
# Dataset #
###########

gene <- read_excel("D:/Coding/R Storage/Summer TCR Project/TCR Datasets/2025/fullgenes.xlsx")
dim(gene)
head(names(gene))
table(is.na(gene$Y))
table(is.na(gene$Y1))

is.na(gene$Y[22])
is.na(gene$Y1[22])
genedit <- gene[-22,]
dim(genedit)

genedit[is.na(genedit)] <- "healthy" 
table(is.na(genedit$Y))
table(is.na(genedit$Y1))