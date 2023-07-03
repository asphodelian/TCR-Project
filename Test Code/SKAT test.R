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