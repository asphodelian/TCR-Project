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