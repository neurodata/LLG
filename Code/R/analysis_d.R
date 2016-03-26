rm(list = ls())
# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

library("igraph")

par(mfrow=c(2, 2));

dataName = "CPAC200"
n = 200
# dataName = "desikan"
# n = 70
# dataName = "JHU"
# n = 48
# dataName = "slab907"
# n = 907
# dataName = "slab1068"
# n = 1068
# dataName = "Talairach"
# n = 1105

for (m in 1:4) {
  for (isSVD in 0:1) {
    if (isSVD) {
      fileName = paste("../../Result/result_", dataName, "_", n, "_", m, "_svd.RData", sep="")
    } else {
      fileName = paste("../../Result/result_", dataName, "_", n, "_", m, "_eig.RData", sep="")
    }
    
    load(fileName)
    print(c(mean(error_A_bar), mean(error_P_hat)))
    
#     plot(dVec, type="n", ylim = c(0.0005, 0.0015), xlab="embedding dimension", ylab="MSE")
#     points(rowMeans(error_A_bar), type="l", col="red")
#     points(rowMeans(error_P_hat), type="l", col="blue")
#     title(paste("m = ", m, sep=""))
  }
}

dHat = dVec[[1]]



