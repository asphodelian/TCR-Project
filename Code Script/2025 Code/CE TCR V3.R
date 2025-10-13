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
y <- train.data$Y 
y[y == "disease"] <- 1 
y[y == "healthy"] <- 0
train.data$Y <- as.numeric(y)

test.data$Y[test.data$Y == "disease"] <- 1
test.data$Y[test.data$Y == "healthy"] <- 0
test.data$Y <- as.numeric(test.data$Y)

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

alpha <- results[results$P_value < 0.05,]
dim(alpha)

###########
# GLM Fit #
###########

# make sure signif genes exist in train.data
allName <- names(train.data)
signif <- intersect(alpha$Gene, allName)

if (length(signif) == 0L) stop("No significant genes found in train.data.")

# split into batches of 5 (last is smaller)
batch_id <- ceiling(seq_along(signif) / 5)
batches  <- split(signif, batch_id)       # list: batch -> character vector of genes

# fit multivar log regress/batch
models <- vector("list", length(batches))
names(models) <- paste0("batch_", seq_along(batches))

coef_rows <- list()

for (b in seq_along(batches)) {
  genes_b <- batches[[b]]
  
  # predictors matrix for this batch
  Xi <- train.data[, genes_b, drop = FALSE]
  
  # (optional) drop zero-variance columns to avoid singular fits
  nzv <- vapply(Xi, function(x) length(unique(na.omit(x))) > 1, logical(1))
  if (!all(nzv)) Xi <- Xi[, nzv, drop = FALSE]
  
  if (ncol(Xi) == 0L) {
    warning("Batch ", b, " had no usable predictors; skipping.")
    next
  }
  
  dat <- data.frame(Y = train.data$Y, Xi, check.names = FALSE)
  
  fit <- glm(Y ~ ., data = dat, family = binomial())
  models[[b]] <- fit
  
  s  <- summary(fit)$coefficients
  cf <- as.data.frame(s)
  cf$term  <- rownames(cf)
  cf$batch <- names(models)[b]
  cf$AIC   <- AIC(fit)
  
  # keep only predictors (drop intercept)
  cf <- cf[cf$term != "(Intercept)",
           c("batch","term","Estimate","Std. Error","z value","Pr(>|z|)","AIC")]
  rownames(cf) <- NULL
  coef_rows[[b]] <- cf
}

# combined tidy table of all batches
batch_results <- if (length(coef_rows)) do.call(rbind, coef_rows) else
  data.frame(batch=character(), term=character(),
             Estimate=numeric(), `Std. Error`=numeric(),
             `z value`=numeric(), `Pr(>|z|)`=numeric(), AIC=numeric())

# (optional) add odds ratios and 95% CI
if (nrow(batch_results)) {
  batch_results$OR  <- NA_real_
  batch_results$LCL <- NA_real_
  batch_results$UCL <- NA_real_
  
  for (b in seq_along(batches)) {
    fit <- models[[b]]
    if (is.null(fit)) next
    beta <- coef(fit)
    ci   <- suppressMessages(confint(fit))   # profile CI
    
    idx <- batch_results$batch == names(models)[b]
    terms_b <- batch_results$term[idx]
    batch_results$OR[idx]  <- exp(beta[terms_b])
    batch_results$LCL[idx] <- exp(ci[terms_b, 1])
    batch_results$UCL[idx] <- exp(ci[terms_b, 2])
  }
}

# inspect
head(batch_results)

#######
# QDA #
#######

train.data$Y <- factor(train.data$Y, levels = c(0, 1))
test.data$Y  <- factor(test.data$Y,  levels = levels(train.data$Y))
# 1) Genes to use (e.g., alpha$Gene) and ensure they exist in BOTH sets
signif <- intersect(signif, intersect(names(train.data), names(test.data)))
stopifnot(length(signif) > 0)
# 2) Split into batches of 5
batches <- split(signif, ceiling(seq_along(signif) / 5))
# 3) Storage
qda_models <- vector("list", length(batches))
names(qda_models) <- paste0("batch_", seq_along(batches))
qda_perf <- data.frame(batch = character(), Test_Acc = numeric(), stringsAsFactors = FALSE)
# 4) Loop
for (b in seq_along(batches)) {
  genes_b <- batches[[b]]
  cat("\n===== QDA Batch", b, "=====\n"); print(genes_b)
  Xi_train <- train.data[, genes_b, drop = FALSE]
  Xi_test  <- test.data[,  genes_b, drop = FALSE]
  # optional: drop zero-variance columns (prevents singulars)
  nzv <- vapply(Xi_train, function(x) length(unique(na.omit(x))) > 1, logical(1))
  if (!all(nzv)) {
    Xi_train <- Xi_train[, nzv, drop = FALSE]
    Xi_test  <- Xi_test[,  nzv, drop = FALSE]
  }
  if (ncol(Xi_train) == 0L) { warning("Batch ", b, " has no usable predictors; skipping."); next }
  
  dat_train <- data.frame(Y = train.data$Y, Xi_train, check.names = FALSE)
  dat_test  <- data.frame(Y = test.data$Y,  Xi_test,  check.names = FALSE)
  
  # Fit + predict (test only)
  qda.fit <- tryCatch(qda(Y ~ ., data = dat_train),
                      error = function(e) { warning("Batch ", b, ": ", e$message); NULL })
  if (is.null(qda.fit)) next
  qda_models[[b]] <- qda.fit
  
  pred <- predict(qda.fit, newdata = dat_test)
  qda.class <- pred$class
  
  # Confusion matrix
  print(table(Pred = qda.class, Actual = dat_test$Y))
  
  # Accuracy (handle any NAs safely)
  acc <- mean(qda.class == dat_test$Y, na.rm = TRUE)
  
  cat("Test Accuracy:", sprintf("%.4f", acc), "\n")
  
  qda_perf <- rbind(qda_perf, data.frame(batch = names(qda_models)[b], Test_Acc = acc))
}
# Summary across batches
qda_perf
