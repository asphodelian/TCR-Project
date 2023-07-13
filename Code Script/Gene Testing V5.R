###########
# Library #
###########

library(dplyr)
library(ggfortify)
library(psych)
library(readr)
library(readxl)
library(SKAT)
library(tidyr)

###########
# Dataset #
###########

gene <- read_excel("fullgenes.xlsx")

################
# SKAT Attempt #
################

attach(gene)

# Partial string to match in column names

# v gene
stringv1 <- "TRBV10-1"
stringv2 <- "TRBV10-2"
stringv3 <- "TRBV10-3"
stringv4 <- "TRBV11-1"
stringv5 <- "TRBV11-2"
stringv6 <- "TRBV11-3"
stringv7 <- "TRBV12-1"
stringv8 <- "TRBV12-3"
stringv9 <- "TRBV12-4"
stringv10 <- "TRBV12-5"

stringv11 <- "TRBV13"
stringv12 <- "TRBV14"
stringv13 <- "TRBV15"
stringv14 <- "TRBV18"
stringv15 <- "TRBV19"
stringv16 <- "TRBV2"
stringv17 <- "TRBV20-1"
stringv18 <- "TRBV21-1"
stringv19 <- "TRBV23-1"
stringv20 <- "TRBV24-1"

stringv21 <- "TRBV25-1"
stringv22 <- "TRBV27"
stringv23 <- "TRBV28"
stringv24 <- "TRBV29-1"
stringv25 <- "TRBV3-2"
stringv26 <- "TRBV30"
stringv27 <- "TRBV4-1"
stringv28 <- "TRBV4-2"
stringv29 <- "TRBV4-3"
stringv30 <- "TRBV5-1"

stringv31 <- "TRBV5-3"
stringv32 <- "TRBV5-4"
stringv33 <- "TRBV5-5"
stringv34 <- "TRBV5-6"
stringv35 <- "TRBV5-7"
stringv36 <- "TRBV5-8"
stringv37 <- "TRBV6-1"
stringv38 <- "TRBV6-2"
stringv39 <- "TRBV6-3"
stringv40 <- "TRBV6-4"

stringv41 <- "TRBV6-5"
stringv42 <- "TRBV6-6"
stringv43 <- "TRBV6-7"
stringv44 <- "TRBV6-8"
stringv45 <- "TRBV6-9"
stringv46 <- "TRBV7-2"
stringv47 <- "TRBV7-3"
stringv48 <- "TRBV7-4"
stringv49 <- "TRBV7-5"
stringv50 <- "TRBV7-6"

# j gene
stringj1 <- "TRBJ1-1"
stringj2 <- "TRBJ1-2"
stringj3 <- "TRBJ1-3"
stringj4 <- "TRBJ1-4"
stringj5 <- "TRBJ1-5"
stringj6 <- "TRBJ1-6"
stringj7 <- "TRBJ2-1"
stringj8 <- "TRBJ2-2"
stringj9 <- "TRBJ2-3"
stringj10 <- "TRBJ2-4"

stringj11 <- "TRBJ2-5"
stringj12 <- "TRBJ2-6"
stringj13 <- "TRBJ2-7"

# Find columns that match the partial string

# v gene
colv1 <- grep(stringv1, names(gene), value = TRUE)
colv2 <- grep(stringv2, names(gene), value = TRUE)
colv3 <- grep(stringv3, names(gene), value = TRUE)
colv4 <- grep(stringv4, names(gene), value = TRUE)
colv5 <- grep(stringv5, names(gene), value = TRUE)
colv6 <- grep(stringv6, names(gene), value = TRUE)
colv7 <- grep(stringv7, names(gene), value = TRUE)
colv8 <- grep(stringv8, names(gene), value = TRUE)
colv9 <- grep(stringv9, names(gene), value = TRUE)
colv10 <- grep(stringv10, names(gene), value = TRUE)

colv11 <- grep(stringv11, names(gene), value = TRUE)
colv12 <- grep(stringv12, names(gene), value = TRUE)
colv13 <- grep(stringv13, names(gene), value = TRUE)
colv14 <- grep(stringv14, names(gene), value = TRUE)
colv15 <- grep(stringv15, names(gene), value = TRUE)
colv16 <- grep(stringv16, names(gene), value = TRUE)
colv17 <- grep(stringv17, names(gene), value = TRUE)
colv18 <- grep(stringv18, names(gene), value = TRUE)
colv19 <- grep(stringv19, names(gene), value = TRUE)
colv20 <- grep(stringv20, names(gene), value = TRUE)

colv21 <- grep(stringv21, names(gene), value = TRUE)
colv22 <- grep(stringv22, names(gene), value = TRUE)
colv23 <- grep(stringv23, names(gene), value = TRUE)
colv24 <- grep(stringv24, names(gene), value = TRUE)
colv25 <- grep(stringv25, names(gene), value = TRUE)
colv26 <- grep(stringv26, names(gene), value = TRUE)
colv27 <- grep(stringv27, names(gene), value = TRUE)
colv28 <- grep(stringv28, names(gene), value = TRUE)
colv29 <- grep(stringv29, names(gene), value = TRUE)
colv30 <- grep(stringv30, names(gene), value = TRUE)

colv31 <- grep(stringv31, names(gene), value = TRUE)
colv32 <- grep(stringv32, names(gene), value = TRUE)
colv33 <- grep(stringv33, names(gene), value = TRUE)
colv34 <- grep(stringv34, names(gene), value = TRUE)
colv35 <- grep(stringv35, names(gene), value = TRUE)
colv36 <- grep(stringv36, names(gene), value = TRUE)
colv37 <- grep(stringv37, names(gene), value = TRUE)
colv38 <- grep(stringv38, names(gene), value = TRUE)
colv39 <- grep(stringv39, names(gene), value = TRUE)
colv40 <- grep(stringv40, names(gene), value = TRUE)

colv41 <- grep(stringv41, names(gene), value = TRUE)
colv42 <- grep(stringv42, names(gene), value = TRUE)
colv43 <- grep(stringv43, names(gene), value = TRUE)
colv44 <- grep(stringv44, names(gene), value = TRUE)
colv45 <- grep(stringv45, names(gene), value = TRUE)
colv46 <- grep(stringv46, names(gene), value = TRUE)
colv47 <- grep(stringv47, names(gene), value = TRUE)
colv48 <- grep(stringv48, names(gene), value = TRUE)
colv49 <- grep(stringv49, names(gene), value = TRUE)
colv50 <- grep(stringv50, names(gene), value = TRUE)

# j gene
colj1 <- grep(stringj1, names(gene), value = TRUE)
colj2 <- grep(stringj2, names(gene), value = TRUE)
colj3 <- grep(stringj3, names(gene), value = TRUE)
colj4 <- grep(stringj4, names(gene), value = TRUE)
colj5 <- grep(stringj5, names(gene), value = TRUE)
colj6 <- grep(stringj6, names(gene), value = TRUE)
colj7 <- grep(stringj7, names(gene), value = TRUE)
colj8 <- grep(stringj8, names(gene), value = TRUE)
colj9 <- grep(stringj9, names(gene), value = TRUE)
colj10 <- grep(stringj10, names(gene), value = TRUE)

colj11 <- grep(stringj11, names(gene), value = TRUE)
colj12 <- grep(stringj12, names(gene), value = TRUE)
colj13 <- grep(stringj13, names(gene), value = TRUE)

# Subset the dataset with the matching columns

# v gene
subv1 <- as.matrix(gene[, colv1]) 
subv2 <- as.matrix(gene[, colv2]) 
subv3 <- as.matrix(gene[, colv3]) 
subv4 <- as.matrix(gene[, colv4]) 
subv5 <- as.matrix(gene[, colv5]) 
subv6 <- as.matrix(gene[, colv6])  
subv7 <- as.matrix(gene[, colv7]) 
subv8 <- as.matrix(gene[, colv8]) 
subv9 <- as.matrix(gene[, colv9]) 
subv10 <- as.matrix(gene[, colv10]) 

subv11 <- as.matrix(gene[, colv11]) 
subv12 <- as.matrix(gene[, colv12]) 
subv13 <- as.matrix(gene[, colv13]) 
subv14 <- as.matrix(gene[, colv14]) 
subv15 <- as.matrix(gene[, colv15]) 
subv16 <- as.matrix(gene[, colv16]) 
subv17 <- as.matrix(gene[, colv17]) 
subv18 <- as.matrix(gene[, colv18])  
subv19 <- as.matrix(gene[, colv19]) 
subv20 <- as.matrix(gene[, colv20]) 

subv21 <- as.matrix(gene[, colv21])  
subv22 <- as.matrix(gene[, colv22]) 
subv23 <- as.matrix(gene[, colv23])  
subv24 <- as.matrix(gene[, colv24]) 
subv25 <- as.matrix(gene[, colv25])  
subv26 <- as.matrix(gene[, colv26]) 
subv27 <- as.matrix(gene[, colv27])  
subv28 <- as.matrix(gene[, colv28])  
subv29 <- as.matrix(gene[, colv29]) 
subv30 <- as.matrix(gene[, colv30]) 

subv31 <- as.matrix(gene[, colv31])  
subv32 <- as.matrix(gene[, colv32])  
subv33 <- as.matrix(gene[, colv33])  
subv34 <- as.matrix(gene[, colv34])  
subv35 <- as.matrix(gene[, colv35])  
subv36 <- as.matrix(gene[, colv36]) 
subv37 <- as.matrix(gene[, colv37])  
subv38 <- as.matrix(gene[, colv38]) 
subv39 <- as.matrix(gene[, colv39]) 
subv40 <- as.matrix(gene[, colv40])  

subv41 <- as.matrix(gene[, colv41]) 
subv42 <- as.matrix(gene[, colv42]) 
subv43 <- as.matrix(gene[, colv43]) 
subv44 <- as.matrix(gene[, colv44]) 
subv45 <- as.matrix(gene[, colv45])  
subv46 <- as.matrix(gene[, colv46]) 
subv47 <- as.matrix(gene[, colv47])  
subv48 <- as.matrix(gene[, colv48])  
subv49 <- as.matrix(gene[, colv49])  
subv50 <- as.matrix(gene[, colv50])  

# j gene
subj1 <- as.matrix(gene[, colj1]) 
subj2 <- as.matrix(gene[, colj2]) 
subj3 <- as.matrix(gene[, colj3]) 
subj4 <- as.matrix(gene[, colj4]) 
subj5 <- as.matrix(gene[, colj5]) 
subj6 <- as.matrix(gene[, colj6])  
subj7 <- as.matrix(gene[, colj7]) 
subj8 <- as.matrix(gene[, colj8]) 
subj9 <- as.matrix(gene[, colj9]) 
subj10 <- as.matrix(gene[, colj10]) 

subj11 <- as.matrix(gene[, colj11])  
subj12 <- as.matrix(gene[, colj12])  
subj13 <- as.matrix(gene[, colj13]) 

### SKAT ###

# Note 6/29:try doing the pca plots with Y1
# fill up the NAs first
# Will need to rerun SKAT test
# do three pairs: active/recovered, active/healthy, recovered/healthy
# try running the signif v and j genes together for pca
# Write up a google doc/github repo about the work I've done

set.na1 <- c(22)
set.na2 <- c(94:109)
Y1 <- gene$Y1
Y1[set.na1] <- "active"
Y1[set.na2] <- "healthy"

# subsets
actRec <- subset(gene, Y1 == "active" | Y1 == "recovered")
Y.ar <- rep(0, length(actRec$Y1))
Y.ar[which(actRec$Y1 == "active")] = 1

actHea <- subset(gene, Y1 == "active" | Y1 == "healthy")
Y.ah <- rep(0, length(actHea$Y1))
Y.ah[which(actHea$Y1 == "active")] = 1

recHea <- subset(gene, Y1 == "recovered" | Y1 == "healthy")
Y.rh <- rep(0, length(recHea$Y1))
Y.rh[which(recHea$Y1 == "recovered")] = 1

# null models
obj.ar <- SKAT_Null_Model(Y.ar ~ 1, out_type = "D")
obj.ah <- SKAT_Null_Model(Y.ah ~ 1, out_type = "D")
obj.rh <- SKAT_Null_Model(Y.rh ~ 1, out_type = "D")

##################
# for loops EDIT #
##################

# actRec
pvec <- rep(0,50)
pval <- rep(0,13)
cat("Active/Recovered Pair \n")

# loops

# loop doesn't run, only does one p-value before an error pops up
# problems are revealed in double checking section

for (i in 1:50) {
  sub <- get(paste0("subv", i,sep=""))  
  out <- SKATBinary(sub, obj.ar, kernel = "linear.weighted")
  p <- out$p.value
  pvec[i] <- p
  cat("P-value of subv", i, "is:", p, "\n")
}
vres <- data.frame(cbind(c(1:50), pvec))
colnames(vres) <- c("vgene.idx","pvalue")
View(vres)

for (i in 1:13) {
  sub <- get(paste0("subj", i)) 
  out <- SKATBinary(sub, obj.ar, kernel = "linear.weighted")
  p <- out$p.value
  pval[i] <- p
  cat("P-value of subj", i, "is:", p, "\n")
}
jres <- data.frame(cbind(c(1:13),pval))
colnames(jres) <- c("jgene.idx","p-value")
View(jres)

# Double checking

# v gene
outv1 <- SKATBinary(subv1, obj.ar, kernel = "linear.weighted")
outv1$p.value
outv2 <- SKATBinary(subv2, obj.ar, kernel = "linear.weighted") # missing value where TRUE/FALSE needed
outv2$p.value
outv3 <- SKATBinary(subv3, obj.ar, kernel = "linear.weighted")
outv3$p.value
outv4 <- SKATBinary(subv4, obj.ar, kernel = "linear.weighted")
outv4$p.value
outv5 <- SKATBinary(subv5, obj.ar, kernel = "linear.weighted")
outv5$p.value
outv6 <- SKATBinary(subv6, obj.ar, kernel = "linear.weighted")
outv6$p.value
outv7 <- SKATBinary(subv7, obj.ar, kernel = "linear.weighted")
outv7$p.value
outv8 <- SKATBinary(subv8, obj.ar, kernel = "linear.weighted") # dimension problem
outv8$p.value
outv9 <- SKATBinary(subv9, obj.ar, kernel = "linear.weighted")
outv9$p.value
outv10 <- SKATBinary(subv10, obj.ar, kernel = "linear.weighted")
outv10$p.value

outv11 <- SKATBinary(subv11, obj.ar, kernel = "linear.weighted")
outv11$p.value
outv12 <- SKATBinary(subv12, obj.ar, kernel = "linear.weighted")
outv12$p.value
outv13 <- SKATBinary(subv13, obj.ar, kernel = "linear.weighted")
outv13$p.value
outv14 <- SKATBinary(subv14, obj.ar, kernel = "linear.weighted")
outv14$p.value
outv15 <- SKATBinary(subv15, obj.ar, kernel = "linear.weighted") # dimension problem
outv15$p.value
outv16 <- SKATBinary(subv16, obj.ar, kernel = "linear.weighted") # dimension problem
outv16$p.value
outv17 <- SKATBinary(subv17, obj.ar, kernel = "linear.weighted") # dimension problem
outv17$p.value
outv18 <- SKATBinary(subv18, obj.ar, kernel = "linear.weighted")
outv18$p.value
outv19 <- SKATBinary(subv19, obj.ar, kernel = "linear.weighted")
outv19$p.value
outv20 <- SKATBinary(subv20, obj.ar, kernel = "linear.weighted")
outv20$p.value

outv21 <- SKATBinary(subv21, obj.ar, kernel = "linear.weighted")
outv21$p.value
outv22 <- SKATBinary(subv22, obj.ar, kernel = "linear.weighted") # dimension problem
outv22$p.value
outv23 <- SKATBinary(subv23, obj.ar, kernel = "linear.weighted")
outv23$p.value
outv24 <- SKATBinary(subv24, obj.ar, kernel = "linear.weighted") # dimension problem
outv24$p.value
outv25 <- SKATBinary(subv25, obj.ar, kernel = "linear.weighted")
outv25$p.value
outv26 <- SKATBinary(subv26, obj.ar, kernel = "linear.weighted")
outv26$p.value
outv27 <- SKATBinary(subv27, obj.ar, kernel = "linear.weighted")
outv27$p.value
outv28 <- SKATBinary(subv28, obj.ar, kernel = "linear.weighted")
outv28$p.value
outv29 <- SKATBinary(subv29, obj.ar, kernel = "linear.weighted")
outv29$p.value
outv30 <- SKATBinary(subv30, obj.ar, kernel = "linear.weighted")
outv30$p.value

outv31 <- SKATBinary(subv31, obj.ar, kernel = "linear.weighted")
outv31$p.value
outv32 <- SKATBinary(subv32, obj.ar, kernel = "linear.weighted")
outv32$p.value
outv33 <- SKATBinary(subv33, obj.ar, kernel = "linear.weighted")
outv33$p.value
outv34 <- SKATBinary(subv34, obj.ar, kernel = "linear.weighted") # dimension problem
outv34$p.value
outv35 <- SKATBinary(subv35, obj.ar, kernel = "linear.weighted")
outv35$p.value
outv36 <- SKATBinary(subv36, obj.ar, kernel = "linear.weighted")
outv36$p.value
outv37 <- SKATBinary(subv37, obj.ar, kernel = "linear.weighted") # dimension problem
outv37$p.value
outv38 <- SKATBinary(subv38, obj.ar, kernel = "linear.weighted")
outv38$p.value
outv39 <- SKATBinary(subv39, obj.ar, kernel = "linear.weighted") # dimension problem
outv39$p.value
outv40 <- SKATBinary(subv40, obj.ar, kernel = "linear.weighted")
outv40$p.value

outv41 <- SKATBinary(subv41, obj.ar, kernel = "linear.weighted") # dimension problem
outv41$p.value
outv42 <- SKATBinary(subv42, obj.ar, kernel = "linear.weighted")
outv42$p.value
outv43 <- SKATBinary(subv43, obj.ar, kernel = "linear.weighted")
outv43$p.value
outv44 <- SKATBinary(subv44, obj.ar, kernel = "linear.weighted")
outv44$p.value
outv45 <- SKATBinary(subv45, obj.ar, kernel = "linear.weighted")
outv45$p.value
outv46 <- SKATBinary(subv46, obj.ar, kernel = "linear.weighted")
outv46$p.value
outv47 <- SKATBinary(subv47, obj.ar, kernel = "linear.weighted")
outv47$p.value
outv48 <- SKATBinary(subv48, obj.ar, kernel = "linear.weighted")
outv48$p.value
outv49 <- SKATBinary(subv49, obj.ar, kernel = "linear.weighted")
outv49$p.value
outv50 <- SKATBinary(subv50, obj.ar, kernel = "linear.weighted")
outv50$p.value

# j gene
outj1 <- SKATBinary(subj1, obj.ar, kernel = "linear.weighted") # dimension problem
outj1$p.value
outj2 <- SKATBinary(subj2, obj.ar, kernel = "linear.weighted")
outj2$p.value
outj3 <- SKATBinary(subj3, obj.ar, kernel = "linear.weighted")
outj3$p.value
outj4 <- SKATBinary(subj4, obj.ar, kernel = "linear.weighted")
outj4$p.value
outj5 <- SKATBinary(subj5, obj.ar, kernel = "linear.weighted")
outj5$p.value
outj6 <- SKATBinary(subj6, obj.ar, kernel = "linear.weighted")
outj6$p.value
outj7 <- SKATBinary(subj7, obj.ar, kernel = "linear.weighted") # dimension problem
outj7$p.value
outj8 <- SKATBinary(subj8, obj.ar, kernel = "linear.weighted")
outj8$p.value
outj9 <- SKATBinary(subj9, obj.ar, kernel = "linear.weighted") # dimension problem
outj9$p.value
outj10 <- SKATBinary(subj10, obj.ar, kernel = "linear.weighted")
outj10$p.value

outj11 <- SKATBinary(subj11, obj.ar, kernel = "linear.weighted") # dimension problem
outj11$p.value
outj12 <- SKATBinary(subj12, obj.ar, kernel = "linear.weighted")
outj12$p.value
outj13 <- SKATBinary(subj13, obj.ar, kernel = "linear.weighted") # dimension problem
outj13$p.value

# actHea

# Double checking

# v gene
outv1 <- SKATBinary(subv1, obj.ah, kernel = "linear.weighted")
outv1$p.value
outv2 <- SKATBinary(subv2, obj.ah, kernel = "linear.weighted") # missing value where TRUE/FALSE needed, prints pval
outv2$p.value
outv3 <- SKATBinary(subv3, obj.ah, kernel = "linear.weighted")
outv3$p.value
outv4 <- SKATBinary(subv4, obj.ah, kernel = "linear.weighted")
outv4$p.value
outv5 <- SKATBinary(subv5, obj.ah, kernel = "linear.weighted")
outv5$p.value
outv6 <- SKATBinary(subv6, obj.ah, kernel = "linear.weighted")
outv6$p.value
outv7 <- SKATBinary(subv7, obj.ah, kernel = "linear.weighted")
outv7$p.value
outv8 <- SKATBinary(subv8, obj.ah, kernel = "linear.weighted") # dimension problem
outv8$p.value
outv9 <- SKATBinary(subv9, obj.ah, kernel = "linear.weighted")
outv9$p.value
outv10 <- SKATBinary(subv10, obj.ah, kernel = "linear.weighted")
outv10$p.value

outv11 <- SKATBinary(subv11, obj.ah, kernel = "linear.weighted")
outv11$p.value
outv12 <- SKATBinary(subv12, obj.ah, kernel = "linear.weighted")
outv12$p.value
outv13 <- SKATBinary(subv13, obj.ah, kernel = "linear.weighted")
outv13$p.value
outv14 <- SKATBinary(subv14, obj.ah, kernel = "linear.weighted") # dimension problem. prints pval
outv14$p.value
outv15 <- SKATBinary(subv15, obj.ah, kernel = "linear.weighted") # dimension problem
outv15$p.value
outv16 <- SKATBinary(subv16, obj.ah, kernel = "linear.weighted") # dimension problem
outv16$p.value
outv17 <- SKATBinary(subv17, obj.ah, kernel = "linear.weighted") # dimension problem
outv17$p.value
outv18 <- SKATBinary(subv18, obj.ah, kernel = "linear.weighted")
outv18$p.value
outv19 <- SKATBinary(subv19, obj.ah, kernel = "linear.weighted")
outv19$p.value
outv20 <- SKATBinary(subv20, obj.ah, kernel = "linear.weighted")
outv20$p.value

outv21 <- SKATBinary(subv21, obj.ah, kernel = "linear.weighted")
outv21$p.value
outv22 <- SKATBinary(subv22, obj.ah, kernel = "linear.weighted") # dimension problem
outv22$p.value
outv23 <- SKATBinary(subv23, obj.ah, kernel = "linear.weighted")
outv23$p.value
outv24 <- SKATBinary(subv24, obj.ah, kernel = "linear.weighted") # dimension problem
outv24$p.value
outv25 <- SKATBinary(subv25, obj.ah, kernel = "linear.weighted")
outv25$p.value
outv26 <- SKATBinary(subv26, obj.ah, kernel = "linear.weighted")
outv26$p.value
outv27 <- SKATBinary(subv27, obj.ah, kernel = "linear.weighted")
outv27$p.value
outv28 <- SKATBinary(subv28, obj.ah, kernel = "linear.weighted")
outv28$p.value
outv29 <- SKATBinary(subv29, obj.ah, kernel = "linear.weighted")
outv29$p.value
outv30 <- SKATBinary(subv30, obj.ah, kernel = "linear.weighted") # dimension problem, prints pval
outv30$p.value

outv31 <- SKATBinary(subv31, obj.ah, kernel = "linear.weighted")
outv31$p.value
outv32 <- SKATBinary(subv32, obj.ah, kernel = "linear.weighted")
outv32$p.value
outv33 <- SKATBinary(subv33, obj.ah, kernel = "linear.weighted")
outv33$p.value
outv34 <- SKATBinary(subv34, obj.ah, kernel = "linear.weighted") # dimension problem, prints pval
outv34$p.value
outv35 <- SKATBinary(subv35, obj.ah, kernel = "linear.weighted")
outv35$p.value
outv36 <- SKATBinary(subv36, obj.ah, kernel = "linear.weighted")
outv36$p.value
outv37 <- SKATBinary(subv37, obj.ah, kernel = "linear.weighted") # dimension problem
outv37$p.value
outv38 <- SKATBinary(subv38, obj.ah, kernel = "linear.weighted")
outv38$p.value
outv39 <- SKATBinary(subv39, obj.ah, kernel = "linear.weighted") # dimension problem, prints pval
outv39$p.value
outv40 <- SKATBinary(subv40, obj.ah, kernel = "linear.weighted")
outv40$p.value

outv41 <- SKATBinary(subv41, obj.ah, kernel = "linear.weighted") # dimension problem
outv41$p.value
outv42 <- SKATBinary(subv42, obj.ah, kernel = "linear.weighted")
outv42$p.value
outv43 <- SKATBinary(subv43, obj.ah, kernel = "linear.weighted")
outv43$p.value
outv44 <- SKATBinary(subv44, obj.ah, kernel = "linear.weighted")
outv44$p.value
outv45 <- SKATBinary(subv45, obj.ah, kernel = "linear.weighted")
outv45$p.value
outv46 <- SKATBinary(subv46, obj.ah, kernel = "linear.weighted")
outv46$p.value
outv47 <- SKATBinary(subv47, obj.ah, kernel = "linear.weighted")
outv47$p.value
outv48 <- SKATBinary(subv48, obj.ah, kernel = "linear.weighted")
outv48$p.value
outv49 <- SKATBinary(subv49, obj.ah, kernel = "linear.weighted")
outv49$p.value
outv50 <- SKATBinary(subv50, obj.ah, kernel = "linear.weighted")
outv50$p.value

# j gene
outj1 <- SKATBinary(subj1, obj.ah, kernel = "linear.weighted") # dimension problem, prints pval
outj1$p.value
outj2 <- SKATBinary(subj2, obj.ah, kernel = "linear.weighted") # dimension problem, prints pval
outj2$p.value
outj3 <- SKATBinary(subj3, obj.ah, kernel = "linear.weighted")
outj3$p.value
outj4 <- SKATBinary(subj4, obj.ah, kernel = "linear.weighted")
outj4$p.value
outj5 <- SKATBinary(subj5, obj.ah, kernel = "linear.weighted")
outj5$p.value
outj6 <- SKATBinary(subj6, obj.ah, kernel = "linear.weighted")
outj6$p.value
outj7 <- SKATBinary(subj7, obj.ah, kernel = "linear.weighted") # dimension problem
outj7$p.value
outj8 <- SKATBinary(subj8, obj.ah, kernel = "linear.weighted") # dimension problem, prints pval
outj8$p.value
outj9 <- SKATBinary(subj9, obj.ah, kernel = "linear.weighted") # dimension problem
outj9$p.value
outj10 <- SKATBinary(subj10, obj.ah, kernel = "linear.weighted")
outj10$p.value

outj11 <- SKATBinary(subj11, obj.ah, kernel = "linear.weighted") # dimension problem
outj11$p.value
outj12 <- SKATBinary(subj12, obj.ah, kernel = "linear.weighted")
outj12$p.value
outj13 <- SKATBinary(subj13, obj.ah, kernel = "linear.weighted") # dimension problem
outj13$p.value

# recHea

# Double checking

# v gene
outv1 <- SKATBinary(subv1, obj.rh, kernel = "linear.weighted")
outv1$p.value
outv2 <- SKATBinary(subv2, obj.rh, kernel = "linear.weighted") 
outv2$p.value
outv3 <- SKATBinary(subv3, obj.rh, kernel = "linear.weighted")
outv3$p.value
outv4 <- SKATBinary(subv4, obj.rh, kernel = "linear.weighted")
outv4$p.value
outv5 <- SKATBinary(subv5, obj.rh, kernel = "linear.weighted") # dimension problem, still prints pval
outv5$p.value
outv6 <- SKATBinary(subv6, obj.rh, kernel = "linear.weighted")
outv6$p.value
outv7 <- SKATBinary(subv7, obj.rh, kernel = "linear.weighted")
outv7$p.value
outv8 <- SKATBinary(subv8, obj.rh, kernel = "linear.weighted") # dimension problem
outv8$p.value
outv9 <- SKATBinary(subv9, obj.rh, kernel = "linear.weighted")
outv9$p.value
outv10 <- SKATBinary(subv10, obj.rh, kernel = "linear.weighted")
outv10$p.value

outv11 <- SKATBinary(subv11, obj.rh, kernel = "linear.weighted")
outv11$p.value
outv12 <- SKATBinary(subv12, obj.rh, kernel = "linear.weighted")
outv12$p.value
outv13 <- SKATBinary(subv13, obj.rh, kernel = "linear.weighted")
outv13$p.value
outv14 <- SKATBinary(subv14, obj.rh, kernel = "linear.weighted")
outv14$p.value
outv15 <- SKATBinary(subv15, obj.rh, kernel = "linear.weighted") # dimension problem
outv15$p.value
outv16 <- SKATBinary(subv16, obj.rh, kernel = "linear.weighted") # dimension problem
outv16$p.value
outv17 <- SKATBinary(subv17, obj.rh, kernel = "linear.weighted") # dimension problem
outv17$p.value
outv18 <- SKATBinary(subv18, obj.rh, kernel = "linear.weighted")
outv18$p.value
outv19 <- SKATBinary(subv19, obj.rh, kernel = "linear.weighted")
outv19$p.value
outv20 <- SKATBinary(subv20, obj.rh, kernel = "linear.weighted")
outv20$p.value

outv21 <- SKATBinary(subv21, obj.rh, kernel = "linear.weighted")
outv21$p.value
outv22 <- SKATBinary(subv22, obj.rh, kernel = "linear.weighted") # dimension problem
outv22$p.value
outv23 <- SKATBinary(subv23, obj.rh, kernel = "linear.weighted")
outv23$p.value
outv24 <- SKATBinary(subv24, obj.rh, kernel = "linear.weighted") # dimension problem
outv24$p.value
outv25 <- SKATBinary(subv25, obj.rh, kernel = "linear.weighted")
outv25$p.value
outv26 <- SKATBinary(subv26, obj.rh, kernel = "linear.weighted")
outv26$p.value
outv27 <- SKATBinary(subv27, obj.rh, kernel = "linear.weighted")
outv27$p.value
outv28 <- SKATBinary(subv28, obj.rh, kernel = "linear.weighted")
outv28$p.value
outv29 <- SKATBinary(subv29, obj.rh, kernel = "linear.weighted")
outv29$p.value
outv30 <- SKATBinary(subv30, obj.rh, kernel = "linear.weighted")
outv30$p.value

outv31 <- SKATBinary(subv31, obj.rh, kernel = "linear.weighted")
outv31$p.value
outv32 <- SKATBinary(subv32, obj.rh, kernel = "linear.weighted")
outv32$p.value
outv33 <- SKATBinary(subv33, obj.rh, kernel = "linear.weighted")
outv33$p.value
outv34 <- SKATBinary(subv34, obj.rh, kernel = "linear.weighted") 
outv34$p.value
outv35 <- SKATBinary(subv35, obj.rh, kernel = "linear.weighted")
outv35$p.value
outv36 <- SKATBinary(subv36, obj.rh, kernel = "linear.weighted")
outv36$p.value
outv37 <- SKATBinary(subv37, obj.rh, kernel = "linear.weighted") # dimension problem
outv37$p.value
outv38 <- SKATBinary(subv38, obj.rh, kernel = "linear.weighted")
outv38$p.value
outv39 <- SKATBinary(subv39, obj.rh, kernel = "linear.weighted") 
outv39$p.value
outv40 <- SKATBinary(subv40, obj.rh, kernel = "linear.weighted")
outv40$p.value

outv41 <- SKATBinary(subv41, obj.rh, kernel = "linear.weighted") # dimension problem
outv41$p.value
outv42 <- SKATBinary(subv42, obj.rh, kernel = "linear.weighted")
outv42$p.value
outv43 <- SKATBinary(subv43, obj.rh, kernel = "linear.weighted")
outv43$p.value
outv44 <- SKATBinary(subv44, obj.rh, kernel = "linear.weighted")
outv44$p.value
outv45 <- SKATBinary(subv45, obj.ar, kernel = "linear.weighted")
outv45$p.value
outv46 <- SKATBinary(subv46, obj.rh, kernel = "linear.weighted")
outv46$p.value
outv47 <- SKATBinary(subv47, obj.rh, kernel = "linear.weighted")
outv47$p.value
outv48 <- SKATBinary(subv48, obj.rh, kernel = "linear.weighted")
outv48$p.value
outv49 <- SKATBinary(subv49, obj.rh, kernel = "linear.weighted")
outv49$p.value
outv50 <- SKATBinary(subv50, obj.rh, kernel = "linear.weighted")
outv50$p.value

# j gene
outj1 <- SKATBinary(subj1, obj.rh, kernel = "linear.weighted") 
outj1$p.value
outj2 <- SKATBinary(subj2, obj.rh, kernel = "linear.weighted")
outj2$p.value
outj3 <- SKATBinary(subj3, obj.rh, kernel = "linear.weighted")
outj3$p.value
outj4 <- SKATBinary(subj4, obj.rh, kernel = "linear.weighted")
outj4$p.value
outj5 <- SKATBinary(subj5, obj.rh, kernel = "linear.weighted")
outj5$p.value
outj6 <- SKATBinary(subj6, obj.rh, kernel = "linear.weighted")
outj6$p.value
outj7 <- SKATBinary(subj7, obj.rh, kernel = "linear.weighted") # dimension problem
outj7$p.value
outj8 <- SKATBinary(subj8, obj.rh, kernel = "linear.weighted") # dimension problem, print pval
outj8$p.value
outj9 <- SKATBinary(subj9, obj.rh, kernel = "linear.weighted") # dimension problem
outj9$p.value
outj10 <- SKATBinary(subj10, obj.rh, kernel = "linear.weighted")
outj10$p.value

outj11 <- SKATBinary(subj11, obj.rh, kernel = "linear.weighted") # dimension problem
outj11$p.value
outj12 <- SKATBinary(subj12, obj.rh, kernel = "linear.weighted")
outj12$p.value
outj13 <- SKATBinary(subj13, obj.rh, kernel = "linear.weighted") # dimension problem
outj13$p.value

############# 
# PCA Plots #
#############

# dataframe
gene$Y1 <- Y1

# v gene
dfv16 <- gene[, colv16]
dfv26 <- gene[, colv26]
dfv27 <- gene[, colv27]
dfv <- gene[, c(colv16, colv26, colv27)] 

# j gene
dfj7 <- gene[, colj7]
dfj9 <- gene[, colj9]
dfj10 <- gene[, colj10]
dfj <- gene[, c(colj7, colj9, colj10)]

# pca res

# v gene
pcav16 <- prcomp(dfv16, scale. = TRUE)
pcav26 <- prcomp(dfv26, scale. = TRUE)
pcav27 <- prcomp(dfv27, scale. = TRUE) 
pcav <- prcomp(dfv, scale. = TRUE)

# j gene
pcaj7 <- prcomp(dfj7, scale. = TRUE)
pcaj9 <- prcomp(dfj9, scale. = TRUE)
pcaj10 <- prcomp(dfj10, scale. = TRUE)
pcaj <- prcomp(dfj, scale. = TRUE)

# plot
autoplot(pcav16, data = gene, colour = 'Y1')
autoplot(pcav26, data = gene, colour = 'Y1')
autoplot(pcav27, data = gene, colour = 'Y1')
autoplot(pcav, data = gene, colour = 'Y1')
autoplot(pcaj7, data = gene, colour = 'Y1')
autoplot(pcaj9, data = gene, colour = 'Y1')
autoplot(pcaj10, data = gene, colour = 'Y1')
autoplot(pcaj, data = gene, colour = 'Y1')

# trying the whole dataset
dfull <- gene[3:630]
pcaFull <- prcomp(dfull, scale. = TRUE)
autoplot(pcaFull, data = gene, colour = 'Y1')
