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

# having onedrive issues, so reading from personal storage
gene <- read_excel("D:/Coding/R Storage/Summer TCR Project/TCR Datasets/2025/fullgenes.xlsx")
attach(gene)

dim(gene)
head(names(gene))
summary(gene)

View(gene)

# missing values in dataset
rows <- c(22,94:110)
genedit <- gene[-rows,]
dim(genedit)
View(genedit)

#training & test data
train <- sample(1:nrow(genedit),0.8*nrow(genedit))
test <- -train
train.data <- genedit[train,]
test.data <- genedit[-train,]

# dependent vars
y <- train.data$Y # binary
y[y == "disease"] <- 1 
y1 <- train.data$Y1 # 3 classes

#glm.fit
glm.fit <- glm(y~., train.data, family = "binomial")
summary(glm.fit)
