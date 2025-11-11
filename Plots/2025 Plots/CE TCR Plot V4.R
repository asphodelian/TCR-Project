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

# drop 0-var cols
drop_nzv <- function(Xtr, Xte) {
  nzv <- vapply(Xtr, function(x) length(unique(na.omit(x))) > 1, logical(1))
  Xtr <- Xtr[, nzv, drop = FALSE]
  Xte <- Xte[, nzv, drop = FALSE]
  list(Xtr = Xtr, Xte = Xte)
}

###########
# GLM Fit #
###########

steps <- seq(5, length(ranked), by = 5)

if (length(ranked) > 0 && tail(steps, 1) != length(ranked)) {
  steps <- c(steps, length(ranked))
}

# storage
glm_curve <- data.frame(TopGenes = integer(), Test_Acc = numeric())

# for loop
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

# storage
qda_curve  <- data.frame(TopGenes = integer(), Test_Acc = numeric())
qda_models <- vector("list", length(steps))
names(qda_models) <- paste0("top_", steps)

# for loop
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

####### 
# LDA #
#######

# lda prep
lda_curve  <- data.frame(TopGenes = integer(), Test_Acc = numeric())
lda_models <- vector("list", length(steps))
names(lda_models) <- paste0("top_", steps)

# for loop
for (s in seq_along(steps)) {
  k <- steps[s]
  genes_k <- ranked[1:k]
  
  Xtr <- as.data.frame(train.data[, genes_k, drop = FALSE])
  Xte <- as.data.frame(test.data[,  genes_k, drop = FALSE])
  
  # impute NAs (from TRAIN medians) + drop zero-variance
  tmp <- impute_from_train(Xtr, Xte); Xtr <- tmp$Xtr; Xte <- tmp$Xte
  tmp <- drop_nzv(Xtr, Xte);          Xtr <- tmp$Xtr; Xte <- tmp$Xte
  if (ncol(Xtr) == 0L) {
    lda_curve <- rbind(lda_curve, data.frame(TopGenes = k, 
                                             Test_Acc = NA_real_))
    next
  }
  
  dat_tr <- data.frame(Y = train.data$Y, Xtr, check.names = FALSE)
  dat_te <- data.frame(Y = test.data$Y,  Xte, check.names = FALSE)
  
  lda.fit <- tryCatch(lda(Y ~ ., data = dat_tr),
                      error = function(e) 
                      { warning("top_", k, ": ", e$message); NULL })
  
  if (is.null(lda.fit)) {
    lda_curve <- rbind(lda_curve, data.frame(TopGenes = k, 
                                             Test_Acc = NA_real_))
    next
  }
  
  lda_models[[s]] <- lda.fit
  pred_test <- predict(lda.fit, newdata = dat_te)$class
  acc <- mean(pred_test == dat_te$Y)
  cat("Top", k, "→ LDA Test Acc:", sprintf("%.4f", acc), "\n")
  
  lda_curve <- rbind(lda_curve, data.frame(TopGenes = k, Test_Acc = acc))
}

#######
# KNN #
#######

# storage
knn_curve <- data.frame(TopGenes = integer(), k = integer(), 
                        Test_Acc = numeric())

# for loop
for (m in steps) {
  genes_m <- ranked[1:m]
  
  Xtr <- as.data.frame(train.data[, genes_m, drop = FALSE])
  Xte <- as.data.frame(test.data[,  genes_m, drop = FALSE])
  ytr <- train.data$Y
  yte <- test.data$Y
  
  # --- Impute NAs with TRAIN medians (per feature) ---
  for (nm in colnames(Xtr)) {
    med <- median(Xtr[[nm]], na.rm = TRUE)
    if (is.finite(med)) {
      Xtr[[nm]][is.na(Xtr[[nm]])] <- med
      Xte[[nm]][is.na(Xte[[nm]])] <- med
    }
  }
  
  # --- Drop zero-variance columns (after impute) ---
  nzv <- vapply(Xtr, function(x) length(unique(na.omit(x))) > 1, logical(1))
  if (!any(nzv)) {
    knn_curve <- rbind(knn_curve, data.frame(TopGenes = m, k = 7, 
                                             Test_Acc = NA_real_))
    next
  }
  Xtr <- Xtr[, nzv, drop = FALSE]
  Xte <- Xte[, nzv, drop = FALSE]
  
  # --- Standardize using TRAIN mean/sd (critical for KNN) ---
  mu  <- vapply(Xtr, mean, numeric(1), na.rm = TRUE)
  sdx <- vapply(Xtr, sd,   numeric(1), na.rm = TRUE); sdx[sdx == 0] <- 1
  Xtr_sc <- scale(Xtr, center = mu, scale = sdx)
  Xte_sc <- scale(Xte, center = mu, scale = sdx)
  
  # --- KNN prediction on TEST ONLY ---
  pred <- knn(train = Xtr_sc, test = Xte_sc, cl = ytr, k = k3)
  acc  <- mean(pred == yte)
  
  cat("Top", m, "genes  →  K = 7 Test Acc:", 
      sprintf("%.4f", acc), "\n")
  knn_curve <- rbind(knn_curve, data.frame(TopGenes = m, k = 7, 
                                           Test_Acc = acc))
}

#######################
# Classification Tree #
#######################

# storage
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

#################
# Random Forest #
#################

randFor <- data.frame(TopGenes = integer(),
                      mtry = integer(),
                      OOB_Error = numeric(),
                      Test_Acc = numeric())

# for loop
for (k in steps) {
  genes_k <- ranked[1:k]
  
  # Extract predictors
  Xtr <- as.data.frame(train.data[, genes_k, drop = FALSE])
  Xte <- as.data.frame(test.data[,  genes_k, drop = FALSE])
  
  # Impute NAs (train medians → apply to test)
  tmp <- impute_from_train(Xtr, Xte); Xtr <- tmp$Xtr; Xte <- tmp$Xte
  
  # Safe, consistent names
  safe_names <- make.names(genes_k, unique = TRUE)
  colnames(Xtr) <- safe_names
  colnames(Xte) <- safe_names
  
  dat_tr <- data.frame(Y = train.data$Y, Xtr, check.names = FALSE)
  dat_te <- data.frame(Y = test.data$Y,  Xte, check.names = FALSE)
  
  # ----- Auto-tune mtry via OOB error -----
  p <- length(safe_names)
  # candidate grid (clipped to [1, p], unique integers)
  grid_raw <- c(sqrt(p)/2, sqrt(p), 2*sqrt(p), p/3, p/2, p)
  mtry_grid <- sort(unique(pmax(1L, pmin(p, round(grid_raw)))))
  
  best_oob <- Inf
  best_fit <- NULL
  best_mtry <- NA_integer_
  
  form_rf <- reformulate(termlabels = safe_names, response = "Y")
  
  for (m in mtry_grid) {
    rf.fit <- randomForest(form_rf, data = dat_tr,
                           mtry = m, ntree = 500,
                           nodesize = 1, importance = FALSE)
    # OOB error from the running err.rate table
    oob_err <- tail(rf.fit$err.rate[, "OOB"], 1)
    if (!is.finite(oob_err)) {
      # rare fallback if OOB is NA
      oob_err <- mean(rf.fit$y != rf.fit$predicted, na.rm = TRUE)
    }
    if (oob_err < best_oob) {
      best_oob <- oob_err
      best_fit <- rf.fit
      best_mtry <- m
    }
  }
  
  # Predict on TEST with the best model and compute TEST ACCURACY
  pred <- predict(best_fit, newdata = dat_te, type = "class")
  test_acc <- mean(pred == dat_te$Y)
  
  # Print minimal info (as requested)
  cat("Top", k, "genes → Test Accuracy:", sprintf("%.4f", test_acc),
      " | best mtry =", best_mtry, " | OOB Err =", sprintf("%.4f", best_oob), "\n")
  
  # Store
  randFor <- rbind(rf_tuned_curve,
                   data.frame(TopGenes = k,
                              mtry = best_mtry,
                              OOB_Error = best_oob,
                              Test_Acc = test_acc))
}

############ 
# Boosting #
############

boost <- data.frame(TopGenes = integer(), shrinkage = numeric(),
                    depth = integer(), best_iter = integer(),
                    CV_Deviance = numeric(), Test_Acc  = numeric())

# for loop
for (k in steps) {
  
  genes_k <- ranked[1:k]
  # Extract & impute
  Xtr <- as.data.frame(train.data[, genes_k, drop = FALSE])
  Xte <- as.data.frame(test.data[,  genes_k, drop = FALSE])
  tmp <- impute_from_train(Xtr, Xte); Xtr <- tmp$Xtr; Xte <- tmp$Xte
  
  # Safe names
  safe_names <- make.names(genes_k, unique = TRUE)
  colnames(Xtr) <- safe_names
  colnames(Xte) <- safe_names
  
  # gbm wants 0/1 numeric Y
  dat_tr <- data.frame(Y = as.integer(train.data$Y) - 1L, Xtr, check.names = FALSE)
  dat_te <- data.frame(Y = as.integer(test.data$Y)  - 1L, Xte, check.names = FALSE)
  
  # Auto-tune shrinkage & depth by CV deviance
  best_fit <- NULL
  best_iter <- NA_integer_
  best_cv   <- Inf
  best_pars <- c(shrink = NA_real_, depth = NA_integer_)
  
  form_boost <- reformulate(termlabels = safe_names, response = "Y")
  
  for (sh in shrinkage_grid) {
    for (dp in depth_grid) {
      fit <- gbm(
        formula = form_boost, data = dat_tr, distribution = "bernoulli",
        n.trees = n_trees_max, interaction.depth = dp,
        shrinkage = sh, n.minobsinnode = 10, bag.fraction = 0.5,
        cv.folds = cv_folds, keep.data = FALSE, verbose = FALSE
      )
      bi <- gbm.perf(fit, method = "cv", plot.it = FALSE)
      cv_min <- suppressWarnings(min(fit$cv.error[is.finite(fit$cv.error)]))
      
      if (is.finite(cv_min) && cv_min < best_cv) {
        best_cv <- cv_min
        best_fit <- fit
        best_iter <- bi
        best_pars <- c(shrink = sh, depth = dp)
      }
    }
  }
  
  # Predict TEST with tuned model
  prob <- predict(best_fit, newdata = dat_te, n.trees = best_iter, type = "response")
  pred <- ifelse(prob > 0.5, 1L, 0L)
  test_acc <- mean(pred == dat_te$Y)
  
  cat("Top ", k, " genes → Test Accuracy:", sprintf("%.4f", test_acc),
      " | shrinkage = ", best_pars["shrink"], " depth = ", best_pars["depth"],
      " trees = ", best_iter, "\n", sep="")
  
  boost <- rbind(boost, data.frame(TopGenes = k, 
                                   shrinkage  = as.numeric(best_pars["shrink"]),
                                   depth = as.integer(best_pars["depth"]),
                                   best_iter = best_iter,
                                   CV_Deviance = best_cv, 
                                   Test_Acc = test_acc)
  )
}