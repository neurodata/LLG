rm(list = ls())
# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

maxIter = 1000

###### Fix m ######
m = 100
# nVec = c(30, 50, 100, 250, 500, 1000, 2000)
nVec = c(30, 50, 100, 250, 500, 1000)
# nVec = c(30, 50, 100, 250)
isSVD = 0
iModel = 1
d = 2

library(ggplot2)
library(grid)
source("function_collection.R")

pp = list()

for (iN in 1:length(nVec)) {
  n = nVec[iN]
  
  if (isSVD) {
    fileName = paste("../../Result/result_sim_", iModel, "_d_", d,
                     "_n_", n, "_m_", m, "_svd.RData", sep="")
  } else {
    fileName = paste("../../Result/result_sim_", iModel, "_d_", d,
                     "_n_", n, "_m_", m, "_eig.RData", sep="")
  }
  
  load(fileName)
  
  if (iN == 1) {
    mse1M = array(NaN, dim=c(length(nVec), maxIter))
    mse2M = array(NaN, dim=c(length(nVec), maxIter))
    mse3M = array(NaN, dim=c(length(nVec), maxIter))
    
    re1M = array(NaN, dim=c(length(nVec), maxIter))
    re2M = array(NaN, dim=c(length(nVec), maxIter))
    re3M = array(NaN, dim=c(length(nVec), maxIter))
  }
  
  mse1M[iN,] = error_P_hat[1,1:maxIter]*n
  mse2M[iN,] = error_P_hat[2,1:maxIter]*n
  mse3M[iN,] = error_P_hat[3,1:maxIter]*n
  
  re1M[iN,] = mse1M[iN,]/error_A_bar[1,1:maxIter]
  re2M[iN,] = mse2M[iN,]/error_A_bar[2,1:maxIter]
  re3M[iN,] = mse3M[iN,]/error_A_bar[3,1:maxIter]
  
}

mse1ExpectM = B[1,1]*(1-B[1,1])*(1/rho[1]+1/rho[1])/m
mse2ExpectM = B[2,2]*(1-B[2,2])*(1/rho[2]+1/rho[2])/m
mse3ExpectM = B[1,2]*(1-B[1,2])*(1/rho[1]+1/rho[2])/m

reExpect = (1/rho[1]+1/rho[2])

mse1MeanM = rowMeans(mse1M)
mse1LowerM = mse1MeanM - 
  sqrt(apply(mse1M, 1, var))/sqrt(dim(mse1M)[2])*1.96
mse1UpperM = mse1MeanM + 
  sqrt(apply(mse1M, 1, var))/sqrt(dim(mse1M)[2])*1.96

mse2MeanM = rowMeans(mse2M)
mse2LowerM = mse2MeanM - 
  sqrt(apply(mse2M, 1, var))/sqrt(dim(mse2M)[2])*1.96
mse2UpperM = mse2MeanM + 
  sqrt(apply(mse2M, 1, var))/sqrt(dim(mse2M)[2])*1.96

mse3MeanM = rowMeans(mse3M)
mse3LowerM = mse3MeanM - 
  sqrt(apply(mse3M, 1, var))/sqrt(dim(mse3M)[2])*1.96
mse3UpperM = mse3MeanM + 
  sqrt(apply(mse3M, 1, var))/sqrt(dim(mse3M)[2])*1.96

df = data.frame(n=rep(nVec,3), errorMean=c(mse1MeanM,mse2MeanM,mse3MeanM),
                errorLower=c(mse1LowerM,mse2LowerM,mse3LowerM),
                errorUpper=c(mse1UpperM,mse2UpperM,mse3UpperM),
                flag=c(rep("B11 associated edges",length(nVec)),
                       rep("B22 associated edges",length(nVec)),
                       rep("B12 associated edges",length(nVec))))

p = ggplot(data=df, aes(x=n, y=errorMean, color=flag)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
p = p + geom_hline(aes(lty="Theoretical: B11 associated edges",
                       yintercept=mse1ExpectM), show.legend = TRUE)
p = p + geom_hline(aes(lty="Theoretical: B22 associated edges",
                       yintercept=mse2ExpectM), show.legend = TRUE)
p = p + geom_hline(aes(lty="Theoretical: B12, B21 associated edges",
                       yintercept=mse3ExpectM), show.legend = TRUE)
p = p + theme(legend.text=element_text(size = 14, face="bold"))
p = p + theme(legend.title=element_blank())
p = p + theme(plot.title=element_text(size = 16, face="bold"))
p = p + scale_x_continuous(name="N") + 
  scale_y_continuous(name=expression(N %.% MSE(hat(P))))
p = p + theme(axis.text=element_text(size=14,face="bold"),
              axis.title=element_text(size=14,face="bold"))
p = p + ggtitle(paste("M = ", m, sep=""))
p = p + scale_color_manual(values=c("red","green","blue"))
pp[[1]] = p



re1MeanM = rowMeans(re1M)
re1LowerM = re1MeanM - 
  sqrt(apply(re1M, 1, var))/sqrt(dim(re1M)[2])*1.96
re1UpperM = re1MeanM + 
  sqrt(apply(re1M, 1, var))/sqrt(dim(re1M)[2])*1.96

re2MeanM = rowMeans(re2M)
re2LowerM = re2MeanM - 
  sqrt(apply(re2M, 1, var))/sqrt(dim(re2M)[2])*1.96
re2UpperM = re2MeanM + 
  sqrt(apply(re2M, 1, var))/sqrt(dim(re2M)[2])*1.96

re3MeanM = rowMeans(re3M)
re3LowerM = re3MeanM - 
  sqrt(apply(re3M, 1, var))/sqrt(dim(re3M)[2])*1.96
re3UpperM = re3MeanM + 
  sqrt(apply(re3M, 1, var))/sqrt(dim(re3M)[2])*1.96

df = data.frame(n=rep(nVec,3), errorMean=c(re1MeanM,re2MeanM,re3MeanM),
                errorLower=c(re1LowerM,re2LowerM,re3LowerM),
                errorUpper=c(re1UpperM,re2UpperM,re3UpperM),
                flag=c(rep("B11 associated edges",length(nVec)),
                       rep("B22 associated edges",length(nVec)),
                       rep("B12 associated edges",length(nVec))))

p = ggplot(data=df, aes(x=n, y=errorMean, color=flag)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
p = p + geom_hline(aes(lty="Theoretical value",
                       yintercept=reExpect), show.legend = TRUE)
p = p + theme(legend.text=element_text(size = 14, face="bold"))
p = p + theme(legend.title=element_blank())
p = p + theme(plot.title=element_text(size = 16, face="bold"))
p = p + scale_x_continuous(name="N") + 
  scale_y_continuous(name=expression(N %.% RE(bar(A),hat(P))))
p = p + theme(axis.text=element_text(size=14,face="bold"),
              axis.title=element_text(size=14,face="bold"))
p = p + ggtitle(paste("M = ", m, sep=""))
p = p + scale_color_manual(values=c("red","green","blue"))
pp[[3]] = p


###### Fix n ######
n = 1000
mVec = c(30, 50, 100, 250, 500, 1000)
isSVD = 0
iModel = 1
d = 2

library(ggplot2)
library(grid)
source("function_collection.R")

for (iM in 1:length(mVec)) {
  m = mVec[iM]
  
  if (isSVD) {
    fileName = paste("../../Result/result_sim_", iModel, "_d_", d,
                     "_n_", n, "_m_", m, "_svd.RData", sep="")
  } else {
    fileName = paste("../../Result/result_sim_", iModel, "_d_", d,
                     "_n_", n, "_m_", m, "_eig.RData", sep="")
  }
  
  load(fileName)
  
  if (iM == 1) {
    mse1N = array(NaN, dim=c(length(mVec), maxIter))
    mse2N = array(NaN, dim=c(length(mVec), maxIter))
    mse3N = array(NaN, dim=c(length(mVec), maxIter))
    
    re1N = array(NaN, dim=c(length(mVec), maxIter))
    re2N = array(NaN, dim=c(length(mVec), maxIter))
    re3N = array(NaN, dim=c(length(mVec), maxIter))
  }
  
  mse1N[iM,] = error_P_hat[1,1:maxIter]*m
  mse2N[iM,] = error_P_hat[2,1:maxIter]*m
  mse3N[iM,] = error_P_hat[3,1:maxIter]*m
  
  re1N[iM,] = error_P_hat[1,1:maxIter]/error_A_bar[1,1:maxIter]
  re2N[iM,] = error_P_hat[2,1:maxIter]/error_A_bar[2,1:maxIter]
  re3N[iM,] = error_P_hat[3,1:maxIter]/error_A_bar[3,1:maxIter]
  
}

mse1ExpectN = B[1,1]*(1-B[1,1])*(1/rho[1]+1/rho[1])/n
mse2ExpectN = B[2,2]*(1-B[2,2])*(1/rho[2]+1/rho[2])/n
mse3ExpectN = B[1,2]*(1-B[1,2])*(1/rho[1]+1/rho[2])/n

mse1MeanN = rowMeans(mse1N)
mse1LowerN = mse1MeanN - 
  sqrt(apply(mse1N, 1, var))/sqrt(dim(mse1N)[2])*1.96
mse1UpperN = mse1MeanN + 
  sqrt(apply(mse1N, 1, var))/sqrt(dim(mse1N)[2])*1.96

mse2MeanN = rowMeans(mse2N)
mse2LowerN = mse2MeanN - 
  sqrt(apply(mse2N, 1, var))/sqrt(dim(mse2N)[2])*1.96
mse2UpperN = mse2MeanN + 
  sqrt(apply(mse2N, 1, var))/sqrt(dim(mse2N)[2])*1.96

mse3MeanN = rowMeans(mse3N)
mse3LowerN = mse3MeanN - 
  sqrt(apply(mse3N, 1, var))/sqrt(dim(mse3N)[2])*1.96
mse3UpperN = mse3MeanN + 
  sqrt(apply(mse3N, 1, var))/sqrt(dim(mse3N)[2])*1.96

df = data.frame(m=rep(mVec,3), errorMean=c(mse1MeanN,mse2MeanN,mse3MeanN),
                errorLower=c(mse1LowerN,mse2LowerN,mse3LowerN),
                errorUpper=c(mse1UpperN,mse2UpperN,mse3UpperN),
                flag=c(rep("B11 associated edges",length(mVec)),
                       rep("B22 associated edges",length(mVec)),
                       rep("B12 associated edges",length(mVec))))

p = ggplot(data=df, aes(x=m, y=errorMean, color=flag)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
p = p + geom_hline(aes(lty="Theoretical: B11 associated edges",
                       yintercept=mse1ExpectN), show.legend = TRUE)
p = p + geom_hline(aes(lty="Theoretical: B22 associated edges",
                       yintercept=mse2ExpectN), show.legend = TRUE)
p = p + geom_hline(aes(lty="Theoretical: B12, B21 associated edges",
                       yintercept=mse3ExpectN), show.legend = TRUE)
p = p + theme(legend.text=element_text(size = 14, face="bold"))
p = p + theme(legend.title=element_blank())
p = p + theme(plot.title=element_text(size = 16, face="bold"))
p = p + scale_x_continuous(name="M") + 
  scale_y_continuous(name=expression(M %.% MSE(hat(P))))
p = p + theme(axis.text=element_text(size=14,face="bold"),
              axis.title=element_text(size=14,face="bold"))
p = p + ggtitle(paste("N = ", n, sep=""))
p = p + scale_color_manual(values=c("red","green","blue"))
pp[[2]] = p


# re1MeanN = rowMeans(re1N)
# re1LowerN = re1MeanN - 
#   sqrt(apply(re1N, 1, var))/sqrt(dim(re1N)[2])*1.96
# re1UpperN = re1MeanN + 
#   sqrt(apply(re1N, 1, var))/sqrt(dim(re1N)[2])*1.96
# 
# re2MeanN = rowMeans(re2N)
# re2LowerN = re2MeanN - 
#   sqrt(apply(re2N, 1, var))/sqrt(dim(re2N)[2])*1.96
# re2UpperN = re2MeanN + 
#   sqrt(apply(re2N, 1, var))/sqrt(dim(re2N)[2])*1.96
# 
# re3MeanN = rowMeans(re3N)
# re3LowerN = re3MeanN - 
#   sqrt(apply(re3N, 1, var))/sqrt(dim(re3N)[2])*1.96
# re3UpperN = re3MeanN + 
#   sqrt(apply(re3N, 1, var))/sqrt(dim(re3N)[2])*1.96
# 
# df = data.frame(m=rep(mVec,3), errorMean=c(re1MeanN,re2MeanN,re3MeanN),
#                 errorLower=c(re1LowerN,re2LowerN,re3LowerN),
#                 errorUpper=c(re1UpperN,re2UpperN,re3UpperN),
#                 flag=c(rep("B11 associated edges",length(mVec)),
#                        rep("B22 associated edges",length(mVec)),
#                        rep("B12 associated edges",length(mVec))))
# 
# p = ggplot(data=df, aes(x=m, y=errorMean, color=flag)) + geom_point() + geom_line() +
#   geom_ribbon(aes(ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
# p = p + geom_hline(aes(lty="Theoretical value",
#                        yintercept=reExpect), show.legend = TRUE)
# p = p + theme(legend.text=element_text(size = 14, face="bold"))
# p = p + theme(legend.title=element_blank())
# p = p + theme(plot.title=element_text(size = 16, face="bold"))
# p = p + scale_x_continuous(name="M") + 
#   scale_y_continuous(name=expression(RE(bar(A),hat(P))))
# p = p + theme(axis.text=element_text(size=14,face="bold"),
#               axis.title=element_text(size=14,face="bold"))
# p = p + ggtitle(paste("N = ", n, sep=""))
# p = p + scale_color_manual(values=c("red","green","blue"))
# pp[[4]] = p






library(gridExtra)

grid_arrange_shared_legend2 <- function(plots, nrows = 1, ncols = 2) {
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  pl  <- lapply(plots, function(x) x + theme(legend.position="none"))
  tmp <- do.call(arrangeGrob, c(pl, list(ncol=ncols, nrow=nrows)))
  grid.arrange(tmp, legend, ncol=1, heights = unit.c(unit(1, "npc") - lheight, lheight))
}

grid_arrange_shared_legend2(list(pp[[1]], pp[[2]]), 1, 2)
# 1170x700

grid_arrange_shared_legend2(list(pp[[3]]), 1, 1)




###### New RE Plot ######
df = data.frame(n=nVec, errorMean=re1MeanM,
                errorLower=re1LowerM,
                errorUpper=re1UpperM,
                flag=rep("Simulated value for corresponding edges",length(nVec)))
p = ggplot(data=df, aes(x=n, y=errorMean, color=flag)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
p = p + geom_hline(aes(lty="Theoretical value",
                       yintercept=reExpect), show.legend = TRUE)
p = p + theme(legend.text=element_text(size = 14, face="bold"))
p = p + theme(legend.title=element_blank())
p = p + theme(plot.title=element_text(size = 16, face="bold"))
p = p + scale_x_continuous(name="N") + 
  scale_y_continuous(name=expression(N %.% RE(bar(A),hat(P))))
p = p + theme(axis.text=element_text(size=14,face="bold"),
              axis.title=element_text(size=14,face="bold"))
p = p + ggtitle(paste0("M = ", m, ", B11 associated edges"))
p = p + scale_color_manual(values=c("blue"))
pp[[7]] = p

df = data.frame(n=nVec, errorMean=re2MeanM,
                errorLower=re2LowerM,
                errorUpper=re2UpperM,
                flag=rep("B22 associated edges",length(nVec)))
p = ggplot(data=df, aes(x=n, y=errorMean, color=flag)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
p = p + geom_hline(aes(lty="Theoretical value",
                       yintercept=reExpect), show.legend = TRUE)
p = p + theme(legend.text=element_text(size = 14, face="bold"))
p = p + theme(legend.title=element_blank())
p = p + theme(plot.title=element_text(size = 16, face="bold"))
p = p + scale_x_continuous(name="N") + 
  scale_y_continuous(name=expression(N %.% RE(bar(A),hat(P))))
p = p + theme(axis.text=element_text(size=14,face="bold"),
              axis.title=element_text(size=14,face="bold"))
p = p + ggtitle(paste0("M = ", m, ", B22 associated edges"))
p = p + scale_color_manual(values=c("blue"))
pp[[8]] = p

df = data.frame(n=nVec, errorMean=re3MeanM,
                errorLower=re3LowerM,
                errorUpper=re3UpperM,
                flag=rep("B12 associated edges",length(nVec)))
p = ggplot(data=df, aes(x=n, y=errorMean, color=flag)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
p = p + geom_hline(aes(lty="Theoretical value",
                       yintercept=reExpect), show.legend = TRUE)
p = p + theme(legend.text=element_text(size = 14, face="bold"))
p = p + theme(legend.title=element_blank())
p = p + theme(plot.title=element_text(size = 16, face="bold"))
p = p + scale_x_continuous(name="N") + 
  scale_y_continuous(name=expression(N %.% RE(bar(A),hat(P))))
p = p + theme(axis.text=element_text(size=14,face="bold"),
              axis.title=element_text(size=14,face="bold"))
p = p + ggtitle(paste0("M = ", m, ", B12 associated edges"))
p = p + scale_color_manual(values=c("blue"))
pp[[9]] = p

grid_arrange_shared_legend2(list(pp[[7]],pp[[8]],pp[[9]]), 3, 1)



###### Change rho ######
m = 100
n = 500
isSVD = 0
iModel = 1
d = 2

    mse1Expectrho = array(NaN, dim=c(1,9))
    mse2Expectrho = array(NaN, dim=c(1,9))
    mse3Expectrho = array(NaN, dim=c(1,9))
    re1Expectrho = array(NaN, dim=c(1,9))
    re2Expectrho = array(NaN, dim=c(1,9))
    re3Expectrho = array(NaN, dim=c(1,9))
      
    for (iRho in 1:9) {
      rho1 = (iRho)/10
      rho = c(rho1, 1-rho1)
      iModel = 10*(rho1 + 1)
      
      if (isSVD) {
        fileName = paste("../../Result/result_sim_", iModel, "_d_", d,
                         "_n_", n, "_m_", m, "_rho1_", rho1, "_svd.RData", sep="")
      } else {
        fileName = paste("../../Result/result_sim_", iModel, "_d_", d,
                         "_n_", n, "_m_", m, "_rho1_", rho1, "_eig.RData", sep="")
      }
      
      load(fileName)
      
      if (iRho == 1) {
        mse1rho = array(NaN, dim=c(9, maxIter))
        mse2rho = array(NaN, dim=c(9, maxIter))
        mse3rho = array(NaN, dim=c(9, maxIter))
        
        re1rho = array(NaN, dim=c(9, maxIter))
        re2rho = array(NaN, dim=c(9, maxIter))
        re3rho = array(NaN, dim=c(9, maxIter))
      }
      
      mse1rho[iRho,] = error_P_hat[1,1:maxIter]
      mse2rho[iRho,] = error_P_hat[2,1:maxIter]
      mse3rho[iRho,] = error_P_hat[3,1:maxIter]
      
      re1rho[iRho,] = mse1rho[iRho,]/error_A_bar[1,1:maxIter]
      re2rho[iRho,] = mse2rho[iRho,]/error_A_bar[2,1:maxIter]
      re3rho[iRho,] = mse3rho[iRho,]/error_A_bar[3,1:maxIter]
      
      mse1Expectrho[iRho] = B[1,1]*(1-B[1,1])*(1/rho[1]+1/rho[1])/m/n
      mse2Expectrho[iRho] = B[2,2]*(1-B[2,2])*(1/rho[2]+1/rho[2])/m/n
      mse3Expectrho[iRho] = B[1,2]*(1-B[1,2])*(1/rho[1]+1/rho[2])/m/n
      
      re1Expectrho[iRho] = (1/rho[1]+1/rho[1])/n
      re2Expectrho[iRho] = (1/rho[2]+1/rho[2])/n
      re3Expectrho[iRho] = (1/rho[1]+1/rho[2])/n
      
    }

mse1Meanrho = rowMeans(mse1rho)
mse1Lowerrho = mse1Meanrho - 
  sqrt(apply(mse1rho, 1, var))/sqrt(dim(mse1rho)[2])*1.96
mse1Upperrho = mse1Meanrho + 
  sqrt(apply(mse1rho, 1, var))/sqrt(dim(mse1rho)[2])*1.96

mse2Meanrho = rowMeans(mse2rho)
mse2Lowerrho = mse2Meanrho - 
  sqrt(apply(mse2rho, 1, var))/sqrt(dim(mse2rho)[2])*1.96
mse2Upperrho = mse2Meanrho + 
  sqrt(apply(mse2rho, 1, var))/sqrt(dim(mse2rho)[2])*1.96

mse3Meanrho = rowMeans(mse3rho)
mse3Lowerrho = mse3Meanrho - 
  sqrt(apply(mse3rho, 1, var))/sqrt(dim(mse3rho)[2])*1.96
mse3Upperrho = mse3Meanrho + 
  sqrt(apply(mse3rho, 1, var))/sqrt(dim(mse3rho)[2])*1.96

df = data.frame(n=rep((1:9)/10,6), errorMean=c(mse1Meanrho,mse2Meanrho,mse3Meanrho,
                                               mse1Expectrho,mse2Expectrho,mse3Expectrho),
                errorLower=c(mse1Lowerrho,mse2Lowerrho,mse3Lowerrho,
                             mse1Expectrho,mse2Expectrho,mse3Expectrho),
                errorUpper=c(mse1Upperrho,mse2Upperrho,mse3Upperrho,
                             mse1Expectrho,mse2Expectrho,mse3Expectrho),
                flag=c(rep("B11 associated edges",9),
                       rep("B22 associated edges",9),
                       rep("B12 associated edges",9),
                       rep("Theoretical: B11 associated edges",9),
                       rep("Theoretical: B22 associated edges",9),
                       rep("Theoretical: B12 associated edges",9)))

p = ggplot(data=df, aes(x=n, y=errorMean, color=flag)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
p = p + theme(legend.text=element_text(size = 14, face="bold"))
p = p + theme(legend.title=element_blank())
p = p + theme(plot.title=element_text(size = 16, face="bold"))
p = p + scale_x_continuous(name=expression(rho[1])) + 
  scale_y_continuous(name=expression(MSE(hat(P))))
p = p + theme(axis.text=element_text(size=14,face="bold"),
              axis.title=element_text(size=14,face="bold"))
p = p + ggtitle(paste("N = ", n, ", M = ", m, sep=""))
pp[[5]] = p



re1Meanrho = rowMeans(re1rho)
re1Lowerrho = re1Meanrho - 
  sqrt(apply(re1rho, 1, var))/sqrt(dim(re1rho)[2])*1.96
re1Upperrho = re1Meanrho + 
  sqrt(apply(re1rho, 1, var))/sqrt(dim(re1rho)[2])*1.96

re2Meanrho = rowMeans(re2rho)
re2Lowerrho = re2Meanrho - 
  sqrt(apply(re2rho, 1, var))/sqrt(dim(re2rho)[2])*1.96
re2Upperrho = re2Meanrho + 
  sqrt(apply(re2rho, 1, var))/sqrt(dim(re2rho)[2])*1.96

re3Meanrho = rowMeans(re3rho)
re3Lowerrho = re3Meanrho - 
  sqrt(apply(re3rho, 1, var))/sqrt(dim(re3rho)[2])*1.96
re3Upperrho = re3Meanrho + 
  sqrt(apply(re3rho, 1, var))/sqrt(dim(re3rho)[2])*1.96

df = data.frame(n=rep((1:9)/10,6), errorMean=c(re1Meanrho,re2Meanrho,re3Meanrho,
                                           re1Expectrho,re2Expectrho,re3Expectrho),
                errorLower=c(re1Lowerrho,re2Lowerrho,re3Lowerrho,
                             re1Expectrho,re2Expectrho,re3Expectrho),
                errorUpper=c(re1Upperrho,re2Upperrho,re3Upperrho,
                             re1Expectrho,re2Expectrho,re3Expectrho),
                flag=c(rep("B11 associated edges",9),
                       rep("B22 associated edges",9),
                       rep("B12 associated edges",9),
                       rep("Theoretical: B11 associated edges",9),
                       rep("Theoretical: B22 associated edges",9),
                       rep("Theoretical: B12 associated edges",9)))

p = ggplot(data=df, aes(x=n, y=errorMean, color=flag)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
p = p + theme(legend.text=element_text(size = 14, face="bold"))
p = p + theme(legend.title=element_blank())
p = p + theme(plot.title=element_text(size = 16, face="bold"))
p = p + scale_x_continuous(name=expression(rho[1])) + 
  scale_y_continuous(name=expression(RE(bar(A),hat(P))))
p = p + theme(axis.text=element_text(size=14,face="bold"),
              axis.title=element_text(size=14,face="bold"))
p = p + ggtitle(paste("N = ", n, ", M = ", m, sep=""))
pp[[6]] = p


grid_arrange_shared_legend2(list(pp[[5]], pp[[6]]), 1, 2)
# 1170x700
