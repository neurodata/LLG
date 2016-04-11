rm(list = ls())

setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("E:/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

require(Matrix)

B = array(c(0.9,0.27,0.05,0.1,0.3,
            0.27,0.67,0.02,0.26,0.14,
            0.05,0.02,0.44,0.25,0.33,
            0.1,0.26,0.25,0.7,0.18,
            0.3,0.14,0.33,0.18,0.58), dim=c(5,5))

# tmp = runif(5)
# tmp/sum(tmp)
rho = c(0.22,0.39,0.05,0.16,0.18)
n = 1000

tau = rep(1:5,round(n*rho))
P = B[tau,tau]
diag(P) = 0

par(mfrow=c(1,2))
image(Matrix(P), lwd=0, add="T", main="P", xlab="", ylab="", sub="")


g = sample_sbm(n, B, round(n*rho), directed=F, loops=F)
A = as_adj(g, type="both", sparse=FALSE)
image(Matrix(A), lwd=0, main="A", xlab="", ylab="", sub="")



