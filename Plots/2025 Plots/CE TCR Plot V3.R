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

########
# Prep #
########

train.data$Y <- factor(train.data$Y, levels = c(0,1))
test.data$Y  <- factor(test.data$Y,  levels = levels(train.data$Y))

# steps
max_k <- min(37L, length(ranked))
if (max_k < 5L) max_k <- length(ranked)
steps <- seq(5L, max_k, by = 5L)
if (tail(steps, 1) != max_k) steps <- c(steps, max_k)

# helper
impute_from_train <- function(Xtr, Xte) {
  for (nm in colnames(Xtr)) {
    med <- suppressWarnings(median(Xtr[[nm]], na.rm = TRUE))
    if (is.finite(med)) {
      Xtr[[nm]][is.na(Xtr[[nm]])] <- med
      Xte[[nm]][is.na(Xte[[nm]])] <- med
    }
  }
  list(Xtr = Xtr, Xte = Xte)
}

#######################
# Classification Tree #
#######################

tree_curve <- data.frame(TopGenes = integer(), Test_Error = numeric())

for (k in steps) {
  genes_k <- ranked[1:k]
  genes_k <- intersect(genes_k, intersect(names(train.data), names(test.data)))
  if (!length(genes_k)) {
    tree_curve <- rbind(tree_curve, data.frame(TopGenes = k, Test_Acc = NA_real_))
    next
  }
  
  # 1) Extract predictors
  Xtr <- as.data.frame(train.data[, genes_k, drop = FALSE])
  Xte <- as.data.frame(test.data[,  genes_k, drop = FALSE])
  
  # 2) Impute TEST from TRAIN medians
  for (nm in colnames(Xtr)) {
    med <- suppressWarnings(median(Xtr[[nm]], na.rm = TRUE))
    if (is.finite(med)) {
      Xtr[[nm]][is.na(Xtr[[nm]])] <- med
      Xte[[nm]][is.na(Xte[[nm]])] <- med
    }
  }
  
  # 3) ONE canonical name map; apply to BOTH sets BEFORE building data frames
  safe_names <- make.names(genes_k, unique = TRUE)
  colnames(Xtr) <- safe_names
  colnames(Xte) <- safe_names
  
  # 4) Modeling frames (same columns & order)
  dat_tr <- data.frame(Y = factor(train.data$Y, levels = c(0,1)),
                       Xtr[, safe_names, drop = FALSE],
                       check.names = FALSE)
  dat_te <- data.frame(Y = factor(test.data$Y, levels = c(0,1)),
                       Xte[, safe_names, drop = FALSE],
                       check.names = FALSE)
  
  # 5) Formula from the SAME names (avoid paste() pitfalls)
  form_tree <- reformulate(termlabels = safe_names, response = "Y")
  
  # 6) Fit + prune
  fit <- rpart(form_tree, data = dat_tr, method = "class",
               control = rpart.control(cp = 0.001, minsplit = 10, xval = 10))
  
  cpt <- fit$cptable
  imin <- which.min(replace(cpt[, "xerror"], is.na(cpt[, "xerror"]), Inf))
  cp_opt <- cpt[imin, "CP"]
  fit_pruned <- prune(fit, cp = cp_opt)
  
  # 7) Align TEST to what the model expects (names & order)
  vars_needed <- attr(fit_pruned$terms, "term.labels")   # should equal safe_names
  # quick guard: must be empty
  if (length(setdiff(vars_needed, names(dat_te)))) {
    stop("Missing in test: ", paste(setdiff(vars_needed, names(dat_te)), collapse = ", "))
  }
  new_te <- dat_te[, vars_needed, drop = FALSE]
  # ensure identical name vector
  stopifnot(identical(names(new_te), vars_needed))
  
  # 8) Predict & TEST ACCURACY
  pred <- predict(fit_pruned, newdata = new_te, type = "class")
  test_acc <- mean(pred == dat_te$Y)
  
  cat("Top", k, "genes → Test Accuracy:", sprintf("%.4f", test_acc), "\n")
  tree_curve <- rbind(tree_curve, data.frame(TopGenes = k, Test_Acc = test_acc))
}
