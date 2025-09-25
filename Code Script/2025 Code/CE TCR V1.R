##########################
# Culminating Experience #
##########################

#############
# Libraries #
#############

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
library(leaps) # best subset
library(glmnet) # ridge regress & LASSO

###########
# Dataset #
###########

gene <- read_excel("C:/Users/knigh/OneDrive/Desktop/Github/TCR-Project/Datasets/Disease Data/fullgenes.xlsx")
attach(gene)

dim(gene)
head(names(gene))
summary(gene)

View(gene)

# missing values, need to address