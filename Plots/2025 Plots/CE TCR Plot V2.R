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

#####################
# Significant Genes #
#####################

col <- ncol(train.data)
ycol <- match("Y", names(train.data))
gene_idx  <- 2:(col - 2)
gene.name <- names(train.data)[gene_idx]
pvalue <- numeric(length(gene_idx))

for (i in seq_along(gene_idx))
{
  gene_name <- gene.name[i]
  Xi <- train.data[, gene_idx[i], drop = FALSE]
  names(Xi) <- gene_name  # set column name to the gene
  
  dat <- data.frame(Y = train.data[[ycol]], Xi, check.names = FALSE)
  glm.fit <- glm(Y ~ ., data = dat, family = binomial())
  pvalue[i] <- coef(summary(glm.fit))[2, 4]
}

# Combine results into a nice table:
results <- data.frame(Gene = gene.name, P_value = pvalue)

# Sort by significance
results <- results[order(results$P_value), ]
head(results)

alpha <- results[results$P_value < 0.05,]
dim(alpha)

ranked <- alpha$Gene
ranked <- intersect(ranked, intersect(names(train.data), names(test.data)))

###########
# GLM Fit #
###########

steps <- seq(5, length(ranked), by = 5)

if (length(ranked) > 0 && tail(steps, 1) != length(ranked)) {
  steps <- c(steps, length(ranked))
}

glm_curve <- data.frame(TopGenes = integer(), Test_Acc = numeric())

for (k in steps) {
  genes_k <- ranked[1:k]
  
  Xi_tr <- as.data.frame(train.data[, genes_k, drop = FALSE])
  Xi_te <- as.data.frame(test.data[,  genes_k, drop = FALSE])
  
  # Impute NAs in test with TRAIN medians (per feature)
  for (nm in colnames(Xi_tr)) {
    med <- median(Xi_tr[[nm]], na.rm = TRUE)
    if (is.finite(med)) {
      Xi_tr[[nm]][is.na(Xi_tr[[nm]])] <- med
      Xi_te[[nm]][is.na(Xi_te[[nm]])] <- med
    }
  }
  
  # Drop zero-variance predictors
  nzv <- vapply(Xi_tr, function(x) length(unique(na.omit(x))) > 1, logical(1))
  Xi_tr <- Xi_tr[, nzv, drop = FALSE]
  Xi_te <- Xi_te[, nzv, drop = FALSE]
  if (ncol(Xi_tr) == 0L) {
    glm_curve <- rbind(glm_curve, data.frame(TopGenes = k, Test_Acc = NA))
    next
  }
  
  # build modeling frames
  dat_tr <- data.frame(Y = train.data$Y, Xi_tr, check.names = FALSE)
  dat_te <- data.frame(Y = test.data$Y,  Xi_te,  check.names = FALSE)
  
  # Make Y a 2-level factor (most robust for binomial)
  dat_tr$Y <- factor(dat_tr$Y, levels = c(0, 1))
  dat_te$Y <- factor(dat_te$Y, levels = levels(dat_tr$Y))
  
  
  # Separation-safe GLM (brglm2)
  
  fit <- tryCatch(glm(Y ~ ., data = dat_tr, 
                      family = binomial(link = "logit"), 
                      method = brglm2::brglmFit),
                  error = function(e) NULL)
  
  if (is.null(fit)) {
    glm_curve <- rbind(glm_curve, data.frame(TopGenes = k, 
                                             Test_Acc = NA))
    next
  }
  
  prob <- predict(fit, newdata = dat_te, type = "response")
  pred <- ifelse(prob > 0.5, 1, 0)
  acc  <- mean(pred == dat_te$Y)
  
  cat("Top", k, "→ Test Acc:", sprintf("%.4f", acc), "\n")
  glm_curve <- rbind(glm_curve, data.frame(TopGenes = k, 
                                           Test_Acc = acc))
}

#######
# QDA #
#######

# 2-lvl factor in both sets
train.data$Y <- factor(train.data$Y, levels = c(0,1))
test.data$Y  <- factor(test.data$Y,  levels = levels(train.data$Y))

# made sure to run through all the genes
max_k <- min(37L, length(ranked))
if (max_k < 5L) max_k <- length(ranked)  # if you have <5 genes, just do that many
steps <- seq(5L, max_k, by = 5L)
if (tail(steps, 1) != max_k) steps <- c(steps, max_k)

# impute NAs in both train/test using TRAIN medians 
impute_from_train <- function(Xtr, Xte) {
  for (nm in colnames(Xtr)) {
    med <- median(Xtr[[nm]], na.rm = TRUE)
    if (is.finite(med)) {
      Xtr[[nm]][is.na(Xtr[[nm]])] <- med
      if (nm %in% colnames(Xte)) Xte[[nm]][is.na(Xte[[nm]])] <- med
    }
  }
  list(Xtr = Xtr, Xte = Xte)
}

# drop 0-var cols
drop_nzv <- function(Xtr, Xte) {
  nzv <- vapply(Xtr, function(x) length(unique(na.omit(x))) > 1, logical(1))
  Xtr <- Xtr[, nzv, drop = FALSE]
  Xte <- Xte[, nzv, drop = FALSE]
  list(Xtr = Xtr, Xte = Xte)
}

# storage
qda_curve  <- data.frame(TopGenes = integer(), Test_Acc = numeric())
qda_models <- vector("list", length(steps))
names(qda_models) <- paste0("top_", steps)

for (s in seq_along(steps)) {
  k <- steps[s]
  genes_k <- ranked[1:k]
  
  Xtr <- as.data.frame(train.data[, genes_k, drop = FALSE])
  Xte <- as.data.frame(test.data[,  genes_k, drop = FALSE])
  
  tmp <- impute_from_train(Xtr, Xte); Xtr <- tmp$Xtr; Xte <- tmp$Xte
  tmp <- drop_nzv(Xtr, Xte);          Xtr <- tmp$Xtr; Xte <- tmp$Xte
  if (ncol(Xtr) == 0L) {
    qda_curve <- rbind(qda_curve, data.frame(TopGenes = k, Test_Acc = NA_real_))
    next
  }
  
  dat_tr <- data.frame(Y = train.data$Y, Xtr, check.names = FALSE)
  dat_te <- data.frame(Y = test.data$Y,  Xte, check.names = FALSE)
  
  qda.fit <- tryCatch(qda(Y ~ ., data = dat_tr),
                      error = function(e) { warning("top_", k, ": ", e$message); NULL })
  if (is.null(qda.fit)) {
    qda_curve <- rbind(qda_curve, data.frame(TopGenes = k, Test_Acc = NA_real_))
    next
  }
  
  qda_models[[s]] <- qda.fit
  pred_test <- predict(qda.fit, newdata = dat_te)$class
  acc <- mean(pred_test == dat_te$Y)
  cat("Top", k, "→ Test Acc:", sprintf("%.4f", acc), "\n")
  
  qda_curve <- rbind(qda_curve, data.frame(TopGenes = k, Test_Acc = acc))
}
