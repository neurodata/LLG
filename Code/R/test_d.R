# Analysis of dimensions based on SWU4 data
rm(list = ls())
setwd("E:/Dropbox/GitHub/LLG/Code/R")
# setwd("/Users/Runze/Dropbox/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

library("igraph")
# library("rARPACK")
# source("graphFunctions.R")
# source("USVT.R")

subjectsID = readLines("../../Data/subnames.txt")
dataName = "CPAC200"
# dataName = "desikan"
# dataName = "JHU"
# dataName = "slab907"
# dataName = "slab1068"
# dataName = "Talairach"


################ Read M graphs ################
nSample = 50;
A = read_graph(paste("../../Data/", dataName, "/SWU4_", subjectsID[1], 
                     "_1_", dataName, "_sg.graphml", sep =""), format="graphml")
A = as_adj(A, type="both", sparse=FALSE)
n = dim(A)[1]

M = 227*2;
A_all <- array(rep(0, n*n*M), dim=c(n, n, M))
for (sub in 1:227) {
  for (session in 1:2) {
    A = read_graph(paste("../../Data/", dataName, "/SWU4_", subjectsID[sub], 
                         "_", session, "_", dataName, "_sg.graphml",sep=""), format = "graphml")
    A = as_adj(A, type="both", sparse=FALSE)
    A_all[,,(sub-1)*2 + session] <- A;
  }
}


################ Plot elbows of all M graphs ################
source("getElbows.R")
require("Matrix")
require(irlba)
elbMat <- matrix(0, 3, M)
plot(1:(n-10), type="n", ylim=c(0,50), xlab="embedding dimension", ylab="eigen value")
for (i in 1:M) {
  gi <- A_all[,,i]
  A <- gi + Diagonal(x=rowSums(gi))/(n-1)
  vecs <- irlba(A, nrow(gi)-10, ncol(gi)-10)$d
  elb <- getElbows(vecs, plot=F)
  elbMat[,i] <- elb
  points(vecs, type="l", col="grey")
  points(elb, vecs[elb], pch=19, col=2:4, cex=0.5)
}

#Box plot of elbows of all M graphs
boxplot(t(elbMat),notch=TRUE,xlab="elbow",ylab="embedding dimension")




