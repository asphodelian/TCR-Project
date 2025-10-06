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
gene_idx <- 2:(n-2)
gene.name <- names(train.data)[2:ncol(train.data)-2]
pvalue <- rep(0,length(gene.name))

for (i in seq_along(gene.name))
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

genedit$`TRBV23-1_TRBJ2-2`
genedit$`TRBV2_TRBJ1-6`
genedit$`TRBV21-1_TRBJ2-3`
genedit$`TRBV12-5_TRBJ2-6`
genedit$`TRBV7-6_TRBJ2-6`
genedit$`TRBV10-2_TRBJ2-2`
genedit$`TRBV9_TRBJ1-6`
genedit$`TRBV4-1_TRBJ2-3`
genedit$`TRBV9_TRBJ2-6`
genedit$`TRBV10-2_TRBJ1-6`
genedit$`TRBV28_TRBJ2-2`
genedit$`TRBV12-5_TRBJ1-5`
genedit$`TRBV21-1_TRBJ2-6`
genedit$`TRBV9_TRBJ2-2`
genedit$`TRBV2_TRBJ2-6`
genedit$`TRBV14_TRBJ2-3`
genedit$`TRBV7-3_TRBJ1-6`
genedit$`TRBV9_TRBJ1-3`
genedit$`TRBV6-4_TRBJ2-5`
genedit$`TRBV12-5_TRBJ1-6`
genedit$`TRBV20-1_TRBJ2-3`
genedit$`TRBV28_TRBJ1-6`
genedit$`TRBV7-9_TRBJ2-7`
genedit$`TRBV30_TRBJ1-5`
genedit$`TRBV7-3_TRBJ2-2`
genedit$`TRBV4-3_TRBJ2-4`
genedit$`TRBV4-1_TRBJ1-6`
genedit$`TRBV25-1_TRBJ2-2`
genedit$`TRBV30_TRBJ1-3`
genedit$`TRBV6-1_TRBJ1-1`
genedit$`TRBV30_TRBJ2-2`
genedit$`TRBV5-8_TRBJ2-7`
genedit$`TRBV5-1_TRBJ2-6`
genedit$`TRBV6-4_TRBJ2-6`
genedit$`TRBV2_TRBJ2-4`
genedit$`TRBV9_TRBJ1-2`
genedit$`TRBV30_TRBJ2-5`
genedit$`TRBV6-6_TRBJ2-4`

glm.fit <- glm(Y ~ genedit$, data = genedit, family = binomial())
summary(glm.fit)


