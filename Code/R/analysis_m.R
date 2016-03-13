rm(list = ls())
# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

library("igraph")

par(mfrow=c(2,2));

dataName = "CPAC200"
n = 200;

for (m in c(2, 5, 10, 15)) {
  load(paste("../../Result/", dataName, "_", n, "_brute_", m, ".RData", sep =""))
  
  plot(2:99, type="n", ylim = c(0, 0.05), xlab="embedding dimension", ylab="MSE")
  points(rowMeans(err_A_Bar), type="l", col="red")
  points(rowMeans(err_P_Hat), type="l", col="blue")
  title(paste("m = ", m, sep=""))
}

