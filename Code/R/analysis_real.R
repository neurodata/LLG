rm(list = ls())
# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

# dataName = "CPAC200"
# dataName = "desikan"
dataName = "JHU"
# dataName = "slab907"
# dataName = "slab1068"
# dataName = "Talairach"

set.seed(12345)

m = 10
isSVD = 0

require(Matrix)
library(latex2exp)
source("function_collection.R")
tmpList = read_data(dataName, DA=F)
A_all = tmpList[[1]]
n = tmpList[[2]]
M = tmpList[[3]]
rm(tmpList)

add <- function(x) Reduce("+", x)
P = add(A_all)/M
image(Matrix(P),main=list(label=TeX('$P$ for JHU'),cex=2),sub="",xlab=list(cex=0),ylab=list(cex=0),
      scales=list(x=list(draw=FALSE),y=list(draw=FALSE)))

sampleVec = sample.int(M, m)
A_bar = add(A_all[sampleVec])/m
image(Matrix(A_bar),main=list(label=TeX('$\\bar{A}$ for JHU with M=10'),cex=2),sub="",
      xlab=list(cex=0),ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)))

####### Estimate dimensions ######
source("getElbows.R")
nElb = 3
dMax = ceiling(n*3/5)
evalVec = ase(A_bar, dMax, isSVD)[[1]]
dHat = getElbows(evalVec, n=nElb, plot=F)[[nElb]]

####### Calculate Phat ######
A.ase = ase(diag_aug(A_bar), dHat, isSVD)
if (dHat == 1) {
  Ahat = A.ase[[1]] * A.ase[[3]] %*% t(A.ase[[2]])
} else {
  Ahat <- A.ase[[3]][,1:dHat] %*% diag(A.ase[[1]][1:dHat]) %*% t(A.ase[[2]][,1:dHat])
}
P_hat = regularize(Ahat)

image(Matrix(P_hat),main=list(label=TeX('$\\hat{P}$ for JHU with M=10'),cex=2),sub="",
      xlab=list(cex=0),ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)))

####### Plot the difference ######
Diff = A_bar - P_hat
Diff_abs = abs(Diff)
library(lattice)


# levelplot(Diff[1:n,n:1])

# new.palette=colorRampPalette(c("black","red"),space="rgb") 
# levelplot(Diff[1:n,n:1],col.regions=new.palette(20))

valLow = sort(Diff_abs, decreasing=T)[0.01*n^2]
nv = (Diff_abs<valLow)
nv[upper.tri(nv,diag=T)] = FALSE
Diff_abs[nv] = 0
new.palette=colorRampPalette(c("white","black"),space="rgb") 
# levelplot(Diff_abs[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
#           ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
#           main=list(label="|Abar-Phat|",cex=2))
# levelplot(Diff_abs[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
#           ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
#           main=list(label=expression(bgroup("|",bar(A)-hat(P),"|"),"for ","CPAC200 with m=2"),cex=2))

levelplot(Diff_abs[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
          ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
          main=list(label=TeX('$|\\bar{A} - \\hat{P}|$ for JHU with M=10'),cex=2))


dHat