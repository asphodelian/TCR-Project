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

train <- sample(1:nrow(genedit),0.8*nrow(genedit))
test <- -train
train.data <- genedit[train,]
test.data <- genedit[-train,]

y <- train.data$Y # binary
y[y == "disease"] <- 1 
y[y == "healthy"] <- 0
train.data$Y <- as.numeric(y)

#####################
# Significant Genes #
#####################

col <- ncol(train.data)
ycol <- col-1
gene_idx <- 2:(col-2)
gene.name <- names(train.data)[2:ncol(train.data)-2]
pvalue <- rep(0,length(gene.name))

for (i in seq_along(gene_idx))
{
  gene_name <- gene.name[i]
  Xi <- train.data[, gene_idx[i], drop = FALSE]
  names(Xi) <- gene.name  # set column name to the gene
  
  dat <- data.frame(Y = train.data[[ycol]], Xi, check.names = FALSE)
  glm.fit <- glm(Y ~ ., data = dat, family = binomial())
  pvalue[i] <- coef(summary(glm.fit))[2, 4]
}

# Combine results into a nice table:
results <- data.frame(
  Gene = gene.name,
  P_value = pvalue
)

# Sort by significance
results <- results[order(results$P_value), ]

head(results)
underA <- results[c(1:38),]

##############
# GLM: Top 5 #
##############
glm.fit1 <- glm(Y ~ `TRBV9_TRBJ2-7`, data = train.data, family = binomial())
summary(glm.fit1) # both signif

glm.fit2 <- glm(Y ~ `TRBV23-1_TRBJ2-2`, data = train.data, family = binomial())
summary(glm.fit2)

glm.fit3 <- glm(Y ~ `TRBV7-6_TRBJ2-6`, data = train.data, family = binomial())
summary(glm.fit3)

glm.fit4 <- glm(Y ~ `TRBV4-1_TRBJ2-3`, data = train.data, family = binomial())
summary(glm.fit4) # both signif

glm.fit5 <- glm(Y ~ `TRBV2_TRBJ1-6`, data = train.data, family = binomial())
summary(glm.fit5)



