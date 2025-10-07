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

set.seed(895)

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

##############
# GLM: Top 5 #
##############

# group first 5 in glm

glm.fit1 <- glm(Y ~ `TRBV9_TRBJ2-7`, data = train.data, family = binomial())
summary(glm.fit1) # both signif

glm.fit2 <- glm(Y ~ `TRBV23-1_TRBJ2-2`, data = train.data, family = binomial())
summary(glm.fit2) # both signif

glm.fit3 <- glm(Y ~ `TRBV2_TRBJ1-6`, data = train.data, family = binomial())
summary(glm.fit3)

glm.fit4 <- glm(Y ~ `TRBV21-1_TRBJ2-3`, data = train.data, family = binomial())
summary(glm.fit4) # both signif

glm.fit5 <- glm(Y ~ `TRBV12-5_TRBJ2-6`, data = train.data, family = binomial())
summary(glm.fit5)

glm.fit6 <- glm(Y ~ `TRBV9_TRBJ2-7` + `TRBV23-1_TRBJ2-2` + `TRBV2_TRBJ1-6` + `TRBV21-1_TRBJ2-3` + `TRBV12-5_TRBJ2-6`, data = train.data, family = binomial())
summary(glm.fit6)

###################################
# Quadratic Discriminant Analysis #
###################################

test.data$Y[test.data$Y == "disease"] <- 1 
test.data$Y[test.data$Y == "healthy"] <- 0
test.data$Y <- as.numeric(test.data$Y)

qda.fit6 <- qda(Y ~ `TRBV9_TRBJ2-7` + `TRBV23-1_TRBJ2-2` + `TRBV2_TRBJ1-6` + `TRBV21-1_TRBJ2-3` + `TRBV12-5_TRBJ2-6`, data = train.data)
qda.fit6
qda.class6 <- predict(qda.fit6, test.data)$class
table(qda.class6, test.data$Y)
mean(qda.class6 == test.data$Y)

qda.fit1 <- qda(Y ~ `TRBV9_TRBJ2-7`, data = train.data)
qda.fit1
qda.class1 <- predict(qda.fit1, test.data)$class
table(qda.class1, test.data$Y)
mean(qda.class1 == test.data$Y)
#0.7727273

qda.fit2 <- qda(Y ~ `TRBV23-1_TRBJ2-2`, data = train.data)
qda.fit2
qda.class2 <- predict(qda.fit2, test.data)$class
table(qda.class2, test.data$Y)
mean(qda.class2 == test.data$Y)
#0.3636364

qda.fit3 <- qda(Y ~ `TRBV7-6_TRBJ2-6`, data = train.data)
qda.fit3
qda.class3 <- predict(qda.fit3, test.data)$class
table(qda.class3, test.data$Y)
mean(qda.class3 == test.data$Y)
#0.7727273

qda.fit4 <- qda(Y ~ `TRBV4-1_TRBJ2-3`, data = train.data)
qda.fit2
qda.class4 <- predict(qda.fit4, test.data)$class
table(qda.class4, test.data$Y)
mean(qda.class4 == test.data$Y)
# 0.6363636

qda.fit5 <- qda(Y ~ `TRBV2_TRBJ1-6`, data = train.data)
qda.fit5
qda.class5 <- predict(qda.fit5, test.data)$class
table(qda.class5, test.data$Y)
mean(qda.class5 == test.data$Y)
#0.7727273

################################
# Linear Discriminant Analysis #
################################

lda.fit6 = lda(Y ~ `TRBV9_TRBJ2-7` + `TRBV23-1_TRBJ2-2` + `TRBV2_TRBJ1-6` + `TRBV21-1_TRBJ2-3` + `TRBV12-5_TRBJ2-6`, data = train.data) 
lda.fit6
plot(lda.fit1) # margins too large
lda.class6 = predict(lda.fit6, test.data)$class
table(lda.class6, test.data$Y)
mean(lda.class6 == test.data$Y)

lda.fit1 = lda(Y ~ `TRBV9_TRBJ2-7`, data = train.data) 
lda.fit1
#plot(lda.fit1) # margins too large
lda.class1 = predict(lda.fit1, test.data)$class
table(lda.class1, test.data$Y)
mean(lda.class1 == test.data$Y)
# 0.7727273

lda.fit2 = lda(Y ~ `TRBV23-1_TRBJ2-2`, data = train.data) 
lda.fit2
lda.class2 = predict(lda.fit2, test.data)$class
table(lda.class2, test.data$Y)
mean(lda.class2 == test.data$Y)
# 0.7727273

lda.fit3 = lda(Y ~ `TRBV7-6_TRBJ2-6`, data = train.data) 
lda.fit3
lda.class3 = predict(lda.fit3, test.data)$class
table(lda.class3, test.data$Y)
mean(lda.class3 == test.data$Y)
# 0.7727273

lda.fit4 = lda(Y ~ `TRBV4-1_TRBJ2-3`, data = train.data) 
lda.fit4
lda.class4 = predict(lda.fit4, test.data)$class
table(lda.class4, test.data$Y)
mean(lda.class4 == test.data$Y)
# 0.6363636

lda.fit5 = lda(Y ~ `TRBV2_TRBJ1-6`, data = train.data) 
lda.fit5
lda.class5 = predict(lda.fit5, test.data)$class
table(lda.class5, test.data$Y)
mean(lda.class5 == test.data$Y)
# 0.7727273

########################
# K-Nearest Neighbors ##
########################

noY1 <- genedit[,-679]

train <- sample(1:nrow(noY1),0.8*nrow(noY1))
test <- -train

train.X = genedit[train,]
test.X = genedit[-train,]
y.train = train.data$Y

knn.pred = knn(train.data, test.data, y.train, k = 1)
table(knn.pred, test.data$Y)
mean(knn.pred == test.data$Y)