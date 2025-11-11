##########################
# Culminating Experience #
##########################

#############
# Libraries #
#############

# data manipulation
library(dplyr)
library(psych)
library(readr)
library(readxl)
library(tidyr)

# visualize
library(factoextra)
library(ggfortify)
library(ggplot2)
library(gridExtra)
library(scales)

# techniques
library(brglm2) # bias-reduced log regress
library(caret) # data partition, CV
library(class) # KNN 
library(gbm) # boosting
library(MASS) # QDA & LDA
library(randomForest) # bag, rand forest
library(rpart) # class tree
library(stats)
library(tree) # regression tree

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

##############
# Train/Test #
##############

set.seed(895)

train <- sample(1:nrow(genedit),0.8*nrow(genedit))
test <- -train
train.data <- genedit[train,]
test.data <- genedit[-train,]

# turn Y binary
train.data$Y <- as.numeric(ifelse(train.data$Y == "disease", 1, 0))
test.data$Y  <- as.numeric(ifelse(test.data$Y  == "disease", 1, 0))