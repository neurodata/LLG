rm(list = ls())
# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

###### Fix m ######
m = 100
nVec = c(30, 50, 100, 250, 500, 1000)
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
    fileName = paste("../../Result/result_sim_", iModel, "_d_", d, "_n_", n, "_m_", m, "_svd.RData", sep="")
  } else {
    fileName = paste("../../Result/result_sim_", iModel, "_d_", d, "_n_", n, "_m_", m, "_eig.RData", sep="")
  }
  
  load(fileName)
  
  if (iN == 1) {
    error1 = array(NaN, dim=c(length(nVec), nIter))
    error2 = array(NaN, dim=c(length(nVec), nIter))
    error3 = array(NaN, dim=c(length(nVec), nIter))
  }
  
  error1[iN,] = error_P_hat[1,]*n
  error2[iN,] = error_P_hat[2,]*n
  error3[iN,] = error_P_hat[3,]*n
  
}

# rowMeans(error_P_hat)*n
# c(B[1,1]*(1-B[1,1])*(1/rho[1]+1/rho[2])/m,
#   B[2,2]*(1-B[2,2])*(1/rho[1]+1/rho[2])/m,
#   B[1,2]*(1-B[1,2])*(1/rho[1]+1/rho[2])/m)
error1Expect = B[1,1]*(1-B[1,1])*(1/rho[1]+1/rho[2])/m
error2Expect = B[2,2]*(1-B[2,2])*(1/rho[1]+1/rho[2])/m
error3Expect = B[1,2]*(1-B[1,2])*(1/rho[1]+1/rho[2])/m

error1Mean = rowMeans(error1)
error1Lower = error1Mean - 
  sqrt(apply(error1, 1, var))/sqrt(dim(error1)[2])*1.96
error1Upper = error1Mean + 
  sqrt(apply(error1, 1, var))/sqrt(dim(error1)[2])*1.96

error2Mean = rowMeans(error2)
error2Lower = error2Mean - 
  sqrt(apply(error2, 1, var))/sqrt(dim(error2)[2])*1.96
error2Upper = error2Mean + 
  sqrt(apply(error2, 1, var))/sqrt(dim(error2)[2])*1.96

error3Mean = rowMeans(error3)
error3Lower = error3Mean - 
  sqrt(apply(error3, 1, var))/sqrt(dim(error3)[2])*1.96
error3Upper = error3Mean + 
  sqrt(apply(error3, 1, var))/sqrt(dim(error3)[2])*1.96

df = data.frame(n=rep(nVec,3), errorMean=c(error1Mean,error2Mean,error3Mean),
                errorLower=c(error1Lower,error2Lower,error3Lower),
                errorUpper=c(error1Upper,error2Upper,error3Upper),
                flag=c(rep("B11 associated edges",length(nVec)),
                       rep("B22 associated edges",length(nVec)),
                       rep("B12 associated edges",length(nVec))))

p = ggplot(data=df, aes(x=n, y=errorMean, color=flag)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
p = p + geom_hline(aes(lty="Theoretical: B11 associated edges",yintercept=error1Expect),show_guide = TRUE)
p = p + geom_hline(aes(lty="Theoretical: B22 associated edges",yintercept=error2Expect),show_guide = TRUE)
p = p + geom_hline(aes(lty="Theoretical: B12 associated edges",yintercept=error3Expect),show_guide = TRUE)
p = p + theme(legend.text=element_text(size = 12, face="bold"))
p = p + theme(legend.title=element_blank())
p = p + theme(plot.title=element_text(face="bold"))
p = p + scale_x_continuous(name="n") + scale_y_continuous(name="n*MSE")
p = p + ggtitle(paste("m = ", m, sep=""))

p = p + scale_color_manual(values=c("red","green","blue"))

pp[[iM]] = p



# layout <- matrix(1:4, nrow = 2, byrow = TRUE)
# multiplot(plotlist = pp, layout = layout)

# grid.newpage()
# pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(1, 8, 8), "null"))))
# grid.text(paste(dataName, ", n=", n, ", M=", M, sep=""),
#           vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2),
#           gp=gpar(fontsize=30))
# for (iRow in 2:3) {
#   for (iCol in 1:2) {
#     print(pp[[(iRow-2)*2+iCol]], vp = viewport(layout.pos.row = iRow, layout.pos.col = iCol))
#   }
# }


library(gridExtra)

grid_arrange_shared_legend <- function(t, ...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  theight <- lheight*0.5
  grid.arrange(
    main=textGrob(t, gp=gpar(cex=1.5)),
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(theight, unit(1, "npc") - lheight - theight, lheight))
}

grid_arrange_shared_legend(paste("n=", n, sep=""), 
                           pp[[1]], pp[[2]], pp[[3]], pp[[4]])


