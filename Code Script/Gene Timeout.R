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

############################

gene <- read_excel("~/TCR-Project/Datasets/fullgenes.xlsx")
attach(gene)

############################

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

############################

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

############################

set.na1 <- c(22)
set.na2 <- c(94:109)
Y1 <- gene$Y1
Y1[set.na1] <- "active"
Y1[set.na2] <- "healthy"

############################

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

############################

# dataframe
gene$Y1 <- Y1
dfull <- gene[3:630]

# v gene
ar.v <- actRec[, c(colv1, colv2, colv3, colv4, colv5, colv8, colv9, colv12, 
                   colv13, colv14, colv15, colv16, colv17, colv20, colv22, 
                   colv23, colv24, colv26, colv27, colv30, colv31, colv32, 
                   colv33, colv34, colv35, colv37, colv39, colv40, colv41, 
                   colv42, colv45, colv46, colv47, colv49, colv50)] 

ah.v <- actHea[, c(colv9, colv26, colv45)] 

rh.v <- recHea[, c(colv4, colv8, colv15, colv17, colv22, colv27, colv30, 
                   colv47, colv48, colv49)]

# j gene
ar.j <- actRec[, c(colj4, colj5, colj6, colj7, colj9, colj10, colj11, colj12, 
                   colj13)]
ar.j <- ar.j[, which(apply(ar.j, 2, var) != 0)]

ah.j <- actHea[, c(colj7, colj8)] 

rh.j <- recHea[, c(colj3, colj5, colj7, colj10, colj13)]
rh.j <- rh.j[, which(apply(rh.j, 2, var) != 0)]

# pca res
pcaFull <- prcomp(dfull, scale. = TRUE)
# v gene
pca.arV <- prcomp(ar.v, scale. = TRUE)
pca.ahV <- prcomp(ah.v, scale. = TRUE)
pca.rhV <- prcomp(rh.v, scale. = TRUE) 

# j gene
pca.arJ <- prcomp(ar.j, scale. = TRUE)
pca.ahJ <- prcomp(ah.j, scale. = TRUE)
pca.rhJ <- prcomp(rh.j, scale. = TRUE)

############################

# plot
ARplotV <- autoplot(pca.arV, data = actRec, colour = 'Y1')
AHplotV <- autoplot(pca.ahV, data = actHea, colour = 'Y1')
RHplotV <- autoplot(pca.rhV, data = recHea, colour = 'Y1')
ARplotJ <- autoplot(pca.arJ, data = actRec, colour = 'Y1')
AHplotJ <- autoplot(pca.ahJ, data = actHea, colour = 'Y1')
RHplotJ <- autoplot(pca.rhJ, data = recHea, colour = 'Y1')
autoplot(pcaFull, data = gene, colour = 'Y1')

# arrange
grid.arrange(ARplotV, ARplotJ, ncol = 2)
grid.arrange(AHplotV, AHplotJ, ncol = 2)
grid.arrange(RHplotV, RHplotJ, ncol = 2)

# 7/20/2023 Meeting Note:
# kernel = "linear", "linear.weighted", "IBS" (genetic) 
# kernel = "IBS.weighted", "quadratic" and "2wayIX" (genetic)

p.ar <- rep(0,50)
ar.val <- rep(0,13)

# loop
for (i in 1:50) {
  col.idx <- get(paste0("colv", i,sep=""))
  sub <- as.matrix(actRec[,col.idx])
  out <- SKATBinary(sub, obj.ar, kernel = "quadratic")
  p <- out$p.value
  p.ar[i] <- p
}
ar.v <- data.frame(cbind(c(1:50), p.ar))
colnames(ar.v) <- c("vgene.idx","pvalue")
ar.v

# do pvalue adj before doing the pca

ar.pv <- ar.v$pvalue
p.adjust(ar.pv, method = p.adjust.methods, n = length(ar.pv)) < 0.05

ar.v <- actRec[, c(colv1, colv2, colv3, colv4, colv5, colv8, colv9, colv13, 
                   colv14, colv15, colv16, colv17, colv20, colv22, colv23, 
                   colv24, colv26, colv29, colv30, colv31, colv32, colv33, 
                   colv34, colv35, colv37, colv39, colv40, colv41, colv42, 
                   colv45, colv46, colv47, colv50)] 
pca.arV <- prcomp(ar.v, scale. = TRUE)
autoplot(pca.arV, data = actRec, colour = 'Y1')
