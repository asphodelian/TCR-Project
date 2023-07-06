# p-value adjustment

# v gene
pv <- result$pvalue
p.adjust(pv, method = p.adjust.methods, n = length(pv))

# j gene
pj <- jres$`p-value`
p.adjust(pj, method = p.adjust.methods, n = length(pj))

# No pval adjust, most notable: stringv16/v26/v27, stringj7/j9/j10
# pval adjust, nothing matters

############# 
# PCA Plots #
#############

# dataframe
# v gene
dfv16 <- gene[, colv16]
dfv26 <- gene[, colv26]
dfv27 <- gene[, colv27] 
# j gene
dfj7 <- gene[, colj7]
dfj9 <- gene[, colj9]
dfj10 <- gene[, colj10]

# pca res
# v gene
pcav16 <- prcomp(dfv16, scale. = TRUE)
pcav26 <- prcomp(dfv26, scale. = TRUE)
pcav27 <- prcomp(dfv27, scale. = TRUE) 
# j gene
pcaj7 <- prcomp(dfj7, scale. = TRUE)
pcaj9 <- prcomp(dfj9, scale. = TRUE)
pcaj10 <- prcomp(dfj10, scale. = TRUE)

# plot
autoplot(pcav16, data = gene, colour = 'Y')
autoplot(pcav26, data = gene, colour = 'Y')
autoplot(pcav27, data = gene, colour = 'Y')
autoplot(pcaj7, data = gene, colour = 'Y')
autoplot(pcaj9, data = gene, colour = 'Y')
autoplot(pcaj10, data = gene, colour = 'Y')

# trying the whole dataset
dfull <- gene[3:630]
pcaFull <- prcomp(dfull, scale. = TRUE)
autoplot(pcaFull, data = gene, colour = 'Y')


