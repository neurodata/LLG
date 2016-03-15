rm(list = ls())
# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

library("igraph")

par(mfrow=c(2, 2));

dataName = "CPAC200"
n = 200;

for (m in 2:5) {
  load(paste("../../Result/", "result_", dataName, "_", n, "_brute_", m, ".RData", sep =""))
  
  plot(dVec, type="n", ylim = c(0.0005, 0.0015), xlab="embedding dimension", ylab="MSE")
  points(rowMeans(error_A_bar), type="l", col="red")
  points(rowMeans(error_P_hat), type="l", col="blue")
  title(paste("m = ", m, sep=""))
}

