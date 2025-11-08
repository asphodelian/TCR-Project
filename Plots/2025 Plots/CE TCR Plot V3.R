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

###########
# Bagging #
###########

bag_curve <- data.frame(TopGenes = integer(), ntree = integer(),
                        nodesize  = integer(), sampsize  = integer(),
                        OOB_Error = numeric(), Test_Acc  = numeric())

for (k in steps) {
  genes_k <- ranked[1:k]
  
  # Extract predictors and impute
  Xtr <- as.data.frame(train.data[, genes_k, drop = FALSE])
  Xte <- as.data.frame(test.data[,  genes_k, drop = FALSE])
  tmp <- impute_from_train(Xtr, Xte); Xtr <- tmp$Xtr; Xte <- tmp$Xte
  
  # Safe names
  safe_names <- make.names(genes_k, unique = TRUE)
  colnames(Xtr) <- safe_names
  colnames(Xte) <- safe_names
  
  dat_tr <- data.frame(Y = train.data$Y, Xtr, check.names = FALSE)
  dat_te <- data.frame(Y = test.data$Y,  Xte, check.names = FALSE)
  
  # ---- Auto grids derived from data (no manual params) ----
  n_tr <- nrow(dat_tr)
  p    <- length(safe_names)
  
  # nodesize: tiny → moderate, scaled by n (kept small for classification)
  nodesize_grid <- unique(pmax(1L, round(c(1, sqrt(n_tr)/4, sqrt(n_tr)/2, sqrt(n_tr)))))
  nodesize_grid <- nodesize_grid[nodesize_grid <= max(1L, round(n_tr/5))]
  
  # sampsize: bootstrap fraction set adaptively
  sampfrac_grid <- c(0.6, 0.75, 0.9, 1.0)
  sampsize_grid <- unique(pmax(1L, round(sampfrac_grid * n_tr)))
  
  # ntree: pick best by scanning OOB curve from one large forest
  NTREE_MAX <- 2000L
  
  form_bag <- reformulate(termlabels = safe_names, response = "Y")
  
  best <- list(oob = Inf, nt = NA_integer_, ns = NA_integer_, ss = NA_integer_, fit = NULL)
  
  for (ns in nodesize_grid) {
    for (ss in sampsize_grid) {
      
      # 1) Fit a large forest once; read its OOB error curve to pick ntree*
      rf_big <- randomForest(
        form_bag, data = dat_tr,
        mtry = p,                    # bagging = all predictors
        ntree = NTREE_MAX,
        nodesize = ns,
        sampsize = ss,
        importance = FALSE
      )
      oob_vec <- rf_big$err.rate[, "OOB"]
      oob_vec[!is.finite(oob_vec)] <- Inf
      nt_star <- which.min(oob_vec)
      oob_min <- oob_vec[nt_star]
      
      # 2) Refit exactly at ntree* for final model (predict uses all trees)
      rf_best <- randomForest(
        form_bag, data = dat_tr,
        mtry = p,
        ntree = nt_star,
        nodesize = ns,
        sampsize = ss,
        importance = FALSE
      )
      
      if (oob_min < best$oob) {
        best <- list(oob = oob_min, nt = nt_star, ns = ns, ss = ss, fit = rf_best)
      }
    }
  }
  
  # Predict on TEST for the best combo
  pred <- predict(best$fit, newdata = dat_te, type = "class")
  test_acc <- mean(pred == dat_te$Y)
  
  cat("Top ", k, " genes → Test Accuracy:", sprintf("%.4f", test_acc),
      " | best ntree = ", best$nt,
      " nodesize = ", best$ns,
      " sampsize = ", best$ss,
      " | OOB Err = ", sprintf("%.4f", best$oob), "\n", sep = "")
  
  bag_curve <- rbind(
    bag_curve,
    data.frame(
      TopGenes  = k,
      ntree     = best$nt,
      nodesize  = best$ns,
      sampsize  = best$ss,
      OOB_Error = best$oob,
      Test_Acc  = test_acc
    )
  )
}

################# 
# Random Forest #
#################

rf_tuned_curve <- data.frame(TopGenes = integer(), mtry = integer(),
                             OOB_Error = numeric(), Test_Acc = numeric())

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
  rf_tuned_curve <- rbind(rf_tuned_curve,
                          data.frame(TopGenes = k,
                                     mtry = best_mtry,
                                     OOB_Error = best_oob,
                                     Test_Acc = test_acc))
}

############
# Boosting #
############

# Tuning grids
shrinkage_grid <- c(0.1, 0.05, 0.01)
depth_grid     <- c(1, 2, 3, 4)
n_trees_max    <- 3000   # enough for small shrinkage
cv_folds       <- 5

# Storage
boost_tuned_curve <- data.frame(TopGenes  = integer(), shrinkage = numeric(), 
                                depth     = integer(), best_iter = integer(),
                                CV_Deviance = numeric(), Test_Acc  = numeric())

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
  
  boost_tuned_curve <- rbind(
    boost_tuned_curve,
    data.frame(
      TopGenes   = k,
      shrinkage  = as.numeric(best_pars["shrink"]),
      depth      = as.integer(best_pars["depth"]),
      best_iter  = best_iter,
      CV_Deviance = best_cv,
      Test_Acc   = test_acc
    )
  )
}

########
# Plot #
########

as_method_df <- function(df, method_name, acc_col = "Test_Acc") {
  stopifnot(is.data.frame(df))
  if (!("TopGenes" %in% names(df))) stop(method_name, ": missing column TopGenes")
  if (!(acc_col %in% names(df))) stop(method_name, ": missing column ", acc_col)
  df %>%
    transmute(TopGenes = TopGenes,
              Test_Acc = .data[[acc_col]],
              Method = method_name)
}

curves <- list()

# 1) Classification Tree
if (exists("tree_curve", inherits = FALSE)) {
  curves[["Classification Tree"]] <- as_method_df(tree_curve, "Classification Tree")
}

# 2) Bagging (prefer tuned if available)
if (exists("bag_tuned_curve", inherits = FALSE)) {
  curves[["Bagging (tuned)"]] <- as_method_df(bag_tuned_curve, "Bagging (tuned)")
} else if (exists("bag_curve", inherits = FALSE)) {
  curves[["Bagging"]] <- as_method_df(bag_curve, "Bagging")
}

# 3) Random Forest (prefer tuned if available)
if (exists("rf_tuned_curve", inherits = FALSE)) {
  curves[["Random Forest (tuned mtry)"]] <- as_method_df(rf_tuned_curve, "Random Forest (tuned mtry)")
} else if (exists("rf_curve", inherits = FALSE)) {
  curves[["Random Forest"]] <- as_method_df(rf_curve, "Random Forest")
}

# 4) Boosting (prefer tuned if available)
if (exists("boost_tuned_curve", inherits = FALSE)) {
  curves[["Boosting (tuned)"]] <- as_method_df(boost_tuned_curve, "Boosting (tuned)")
} else if (exists("boost_curve", inherits = FALSE)) {
  curves[["Boosting"]] <- as_method_df(boost_curve, "Boosting")
}

# Bind the available curves
if (length(curves) == 0) stop("No model curves found to plot.")
compare_df <- bind_rows(curves) %>% filter(is.finite(Test_Acc))

# Optional: fix legend order if you like
method_order <- intersect(
  c("Classification Tree",
    "Bagging", "Bagging (tuned)",
    "Random Forest", "Random Forest (tuned mtry)",
    "Boosting", "Boosting (tuned)"),
  unique(compare_df$Method)
)
compare_df$Method <- factor(compare_df$Method, levels = method_order)

# Plot: Test Accuracy vs Number of Top Genes
gg <- ggplot(compare_df, aes(x = TopGenes, y = Test_Acc, color = Method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Test Accuracy vs Number of Top Genes",
    x = "Number of Top Genes",
    y = "Test Accuracy (Higher = Better)",
    color = "Model"
  ) +
  # precise y-axis (tweak limits to your range)
  scale_y_continuous(
    limits = c(0.70, 1.00),
    breaks = seq(0.70, 1.00, by = 0.02),
    labels = number_format(accuracy = 0.001)
  ) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )
print(gg)

# Highlight best point per method
best_by_method <- compare_df %>%
  group_by(Method) %>%
  slice_max(Test_Acc, n = 1, with_ties = FALSE)

gg +
  geom_point(data = best_by_method,
             aes(x = TopGenes, y = Test_Acc, color = Method),
             size = 3) +
  geom_text(
    data = best_by_method,
    aes(label = paste0("max=", round(Test_Acc, 3), " @", TopGenes)),
    vjust = -1.0, size = 3.2, show.legend = FALSE
  )