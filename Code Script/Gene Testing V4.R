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

# Note 6/26:
# Read M448 Book Ch12 
# pca plots, color dots with Y, do with all 710 genes & significant v genes 
# p.adjust(p, method = p.adjust.methods, n = length(p)) for all p-values
# Try the SKATBin with the j gene, vjgene

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