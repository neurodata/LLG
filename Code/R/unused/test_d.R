# Analysis of dimensions based on SWU4 data
rm(list = ls())
# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

# library(igraph)
# library("rARPACK")
# source("graphFunctions.R")
# source("USVT.R")

dataName = "CPAC200"
# dataName = "desikan"
# dataName = "JHU"
# dataName = "slab907"
# dataName = "slab1068"
# dataName = "Talairach"

################ Read M graphs ################
source("function_collection.R")
tmpList = read_data(dataName, DA=T)
A_all = tmpList[[1]]
n = tmpList[[2]]
M = tmpList[[3]]
rm(tmpList)

################ Test on difference between 2 scans ################
require(rARPACK)
require(Matrix)
# dVec = seq(5,n-20,10)
dVec = 2:15
# dVec = seq(2, 15, 3)
ndVec = length(dVec)
dMax = max(dVec)
normDiff = array(rep(NaN, M/2*ndVec), dim=c(M/2, ndVec))
for (i in 1:(M/2)) {
  print(i)
  A1 = as.matrix(A_all[[i*2-1]])
  ASE1 = eigs_sym(A1, dMax, which = "LM")
  A2 = as.matrix(A_all[[i*2]])
  ASE2 = eigs_sym(A2, dMax, which = "LM")
  
  for (iD in 1:ndVec) {
    d = dVec[iD]
    Phat1 = ASE1$vectors[,1:d] %*% Diagonal(d, x=ASE1$values[1:d]) %*% t(ASE1$vectors[,1:d])
    Phat2 = ASE2$vectors[,1:d] %*% Diagonal(d, x=ASE2$values[1:d]) %*% t(ASE2$vectors[,1:d])
    Phat1 = regularize(Phat1)
    Phat2 = regularize(Phat2)
    normDiff[i, iD] = norm(Phat1 - Phat2, "F")/n/(n-1)
  }
}

boxplot(normDiff, notch=TRUE, xlab = "embedded dimension", 
        ylab="||A_{i1}-A_{i2}||_F/n/(n-1)", names = dVec)

################ Plot elbows of all M graphs ################
source("getElbows.R")
# require(rARPACK)
require(irlba)
dMax = n-10
elbMat = matrix(0, M, 3)
evalMat = matrix(0, M, dMax)
eval3Mat = matrix(0, M, 3)
plot(1:dMax, type="n", ylim=c(0,50), xlab="embedding dimension", ylab="eigen value")
for (i in 1:M) {
  A = A_all[[i]]
  vecs = irlba(A, dMax, dMax)$d
  # A = as.matrix(A_all[[i]])
  # vecs = eigs_sym(A, dMax, which = "LM")$values
  evalMat[i,] = vecs
  elb = getElbows(vecs, plot=F)
  elbMat[i,] = elb
  eval3Mat[i,] = vecs[elb]
  points(vecs, type="l", col="grey")
  points(elb, vecs[elb], pch=19, col=2:4, cex=0.5)
}

# Box plot of elbows of all M graphs
boxplot(elbMat, notch=TRUE, xlab="elbow", ylab="embedding dimension")

# save.image("../../Result/evalues.RData")

# load("../../Result/evalues.RData")

################ Cluster M graphs into two groups ################

# Clustering using first 3 elbows
library(mclust)
GMM = Mclust(eval3Mat, 2)

# Pair plot
png("../../Result/pair_cluster_3elbow.png")
clcol <- rainbow(length(unique(GMM$classification)))[GMM$classification]
pairs(eval3Mat, col=clcol)
title("2 Clusters using the first 3 elbows")
dev.off ();

# Scree plot
png("../../Result/scree_cluster_3elbow.png")
plot(1:dMax, type="n", ylim=c(0,50), xlab="embedding dimension", ylab="eigen value")
for (i in 1:M) {
  if (GMM$classification[i] == 1) {
    # colStr = "grey72"
    colStr = "yellow"
  } else {
    # colStr = "grey52"
    colStr = "purple"
  }
  points(evalMat[i,], type="l", col=colStr)
  points(elbMat[i,], eval3Mat[i,], pch=19, col=2:4, cex=0.5)
  GMM$classification == 1
}
title("2 Clusters using the first 3 elbows")
dev.off ();

# Check 2 scans
sum(GMM$classification == 1)
sum(GMM$classification == 2)
nv = (GMM$classification == 2)
tmp = (1:M)[nv]
sum((GMM$classification[1:(M/2)*2 - 1] == 2) & (GMM$classification[1:(M/2)*2] == 2))
sum(GMM$classification[1:(M/2)*2 - 1] != GMM$classification[1:(M/2)*2])

# Using first 2 eigenvalues

# GMM1 = Mclust(evalMat[,1:30], 2)
# GMM2 = Mclust(evalMat[,1:2], 2)
# sum(GMM1$classification != GMM2$classification)

GMM = Mclust(evalMat[,1:2], 2)

plot(1:(n-10), type="n", ylim=c(0,50), xlab="embedding dimension", ylab="eigen value")
for (i in 1:M) {
  if (GMM$classification[i] == 1) {
    colStr = "grey72"
  } else {
    colStr = "grey52"
  }
  points(evalMat[i,], type="l", col=colStr)
  points(elbMat[i,], eval3Mat[i,], pch=19, col=2:4, cex=0.5)
  GMM$classification == 1
}
title("2 Clusters using the first 2 eigenvalues")

plot(GMM, what="classification", xlab="first eigenvalue", ylab="second eigenvalue", identify=F)
title("Clustering result using the first two eigenvalues")



normDiff = rep(0, 1, M/2)
for (i in 1:(M/2)) {
  normDiff[i] = norm(A_all[,,i] - A_all[,,i+1], "F")
}
boxplot(normDiff, notch=TRUE, ylab="||A_{i1}-A_{i2}||_F")
boxplot(normDiff/n/(n-1), notch=TRUE, ylab="||A_{i1}-A_{i2}||_F/n/(n-1)")

