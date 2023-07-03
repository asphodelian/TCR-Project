# Library

library(SKAT)

# data
data(SKAT.example)
names(SKAT.example)
attach(SKAT.example)

# cont trait
obj1 <- SKAT_Null_Model(y.c ~ X, out_type = "C") # Y ~ V10 dataset
out1.c <- SKAT(Z, obj1)
out1.c$p.value

objv10.1 <- SKAT_Null_Model(genes$Y ~ 1, out_type = "D")
outv10.1 <- SKAT(genes[,1:13],objv10.1)

# dicho trait
obj2 <- SKAT_Null_Model(y.b ~ X, out_type = "C")
out2.b <- SKAT(Z, obj2)
out2.b$p.value

# other
out1.c$param
out1.c$test.snp.mac

#########################
# binary & small sample #
#########################

IDX <- -c(1:100, 1001:1100)

# with adjustment
obj.s <- SKAT_Null_Model(y.b[IDX] ~ X[IDX,], out_type = "D")
SKAT(Z[IDX,], obj.s, kernel = "linear.weighted")$p.value

# no adjust
obj.s <- SKAT_Null_Model(y.b[IDX] ~ X[IDX,], out_type = "D", Adjustment = FALSE)
SKAT(Z[IDX,], obj.s, kernel = "linear.weighted")$p.value

# USE SKAT binary ONLY for fullgenes

# default hybrid approach
out <- SKATBinary(Z[IDX,], obj.s, kernel = "linear.weighted")
out$p.value

# robust approach
rob <- SKATBinary_Robust(Z[IDX,], obj.s, kernel = "linear.weighted")
rob$p.value

#####################
# Weight Assignment #
#####################

SKAT(Z, obj1, kernel = "linear.weighted", weights.beta = c(0.5, 0.5))$p.value

# shape of log weight

MAF <- 1:1000/1000
W <- Get_Logistic_Weights_MAF(MAF, par1 = 0.07, par2 = 150)
par(mfrow = c(1,2))
plot(MAF, W, xlab = "MAF", ylab = "Weights", type = "l")
plot(MAF[1:100], W[1:100], xlab = "MAF", ylab = "Weights", type = "l")

# Use log weight

weights <- Get_Logistic_Weights(Z, par1 = 0.07, par2 = 150)
SKAT(Z, obj1, kernel = "linear.weighted", weights = weights)$p.value 
#PDF output = 0.3293643
#R output = 0.07264769