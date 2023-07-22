###########
# Library #
###########

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

###########
# Dataset #
###########

gene <- read_excel("fullgenes.xlsx")
attach(gene)

################
# SKAT Attempt #
################

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

###########
# NA fill #
###########

set.na1 <- c(22)
set.na2 <- c(94:109)
Y1 <- gene$Y1
Y1[set.na1] <- "active"
Y1[set.na2] <- "healthy"



