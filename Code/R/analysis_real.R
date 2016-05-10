rm(list = ls())
# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

# dataName = "CPAC200"
dataName = "desikan"
# dataName = "JHU"
# dataName = "slab907"
# dataName = "slab1068"
# dataName = "Talairach"

set.seed(12345)

m = 5
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
# image(Matrix(P),main=list(label=TeX('$P$ for desikan'),cex=2),sub="",xlab=list(cex=0),
#       ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),lwd=0)
print(image(Matrix(P),main=list(label=TeX('$P$ for desikan'),cex=2),sub="",xlab=list(cex=0),
      ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),lwd=0),
      split=c(x=1,y=1,nx=3,ny=1),more=TRUE)

sampleVec = sample.int(M, m)
A_bar = add(A_all[sampleVec])/m
# image(Matrix(A_bar),main=list(label=TeX('$\\bar{A}$ for desikan with M=5'),cex=2),sub="",
#       xlab=list(cex=0),ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
#       lwd=0)
print(image(Matrix(A_bar),main=list(label=TeX('$\\bar{A}$ for desikan with M=5'),cex=2),sub="",
      xlab=list(cex=0),ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
      lwd=0),split=c(x=2,y=1,nx=3,ny=1),more=TRUE)

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

# image(Matrix(P_hat),main=list(label=TeX('$\\hat{P}$ for desikan with M=5'),cex=2),sub="",
#       xlab=list(cex=0),ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
#       lwd=0)
print(image(Matrix(P_hat),main=list(label=TeX('$\\hat{P}$ for desikan with M=5'),cex=2),sub="",
      xlab=list(cex=0),ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
      lwd=0),split=c(x=3,y=1,nx=3,ny=1))

dHat

####### Plot the difference ######
Diff_between = abs(A_bar - P_hat)
Diff_A_bar = abs(A_bar - P)
Diff_P_hat = abs(P_hat - P)

library(lattice)

valLow = sort(Diff_between, decreasing=T)[0.01*n^2]
nv = (Diff_between<valLow)
nv[upper.tri(nv,diag=T)] = FALSE
Diff_between[nv] = 0
new.palette=colorRampPalette(c("white","black"),space="rgb") 
print(levelplot(Diff_between[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
                ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
                main=list(label=TeX('$|\\bar{A} - \\hat{P}|$ for desikan with M=5'),cex=2),
                colorkey=FALSE),split=c(x=1,y=1,nx=3,ny=1),more=TRUE)

valLow = sort(Diff_A_bar, decreasing=T)[0.01*n^2]
nv = (Diff_A_bar<valLow)
nv[upper.tri(nv,diag=T)] = FALSE
Diff_A_bar[nv] = 0
new.palette=colorRampPalette(c("white","black"),space="rgb") 
print(levelplot(Diff_A_bar[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
                ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
                main=list(label=TeX('$|\\bar{A} - P|$ for desikan with M=5'),cex=2),
                colorkey=FALSE),split=c(x=2,y=1,nx=3,ny=1),more=TRUE)

valLow = sort(Diff_P_hat, decreasing=T)[0.01*n^2]
nv = (Diff_P_hat<valLow)
nv[upper.tri(nv,diag=T)] = FALSE
Diff_P_hat[nv] = 0
new.palette=colorRampPalette(c("white","black"),space="rgb") 
print(levelplot(Diff_P_hat[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
                ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
                main=list(label=TeX('$|\\hat{P} - P|$ for desikan with M=5'),cex=2),
                colorkey=FALSE),split=c(x=3,y=1,nx=3,ny=1))




levelplot(Diff_between[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
          ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
          main=list(label=TeX('$|\\bar{A} - \\hat{P}|$ for desikan with M=5'),cex=2.5),
          colorkey=FALSE)

levelplot(Diff_A_bar[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
          ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
          main=list(label=TeX('$|\\bar{A} - P|$ for desikan with M=5'),cex=2.5),
          colorkey=FALSE)

levelplot(Diff_P_hat[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
          ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
          main=list(label=TeX('$|\\hat{P} - P|$ for desikan with M=5'),cex=2.5),
          colorkey=TRUE)
