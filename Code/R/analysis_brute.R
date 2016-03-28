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

mVec = c(1, 2, 5, 10)

yMin = Inf
yMax = 0
for (m in mVec) {
  # EIG
  fileName = paste("../../Result/result_", dataName, "_brute_", "m_", m, "_eig.RData", sep="")
  load(fileName)
  yMin = min(yMin, rowMeans(error_P_hat), mean(error_A_bar))
  yMax = max(yMax, rowMeans(error_P_hat), mean(error_A_bar))
  
  # SVD
  fileName = paste("../../Result/result_", dataName, "_brute_", "m_", m, "_svd.RData", sep="")
  load(fileName)
  yMin = min(yMin, rowMeans(error_P_hat), mean(error_A_bar))
  yMax = max(yMax, rowMeans(error_P_hat), mean(error_A_bar))
}
yMax = yMax*1.1
yMin = yMin*0.9

library(ggplot2)
library(grid)
source("function_collection.R")

pp = list()

for (iM in 1:length(mVec)) {
  m = mVec[iM]
  
  # SVD
  fileName = paste("../../Result/result_", dataName, "_brute_", "m_", m, "_svd.RData", sep="")
  load(fileName)
  errorPhatSVDMean = rowMeans(error_P_hat)
  errorPhatSVDLower = errorPhatSVDMean - 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  errorPhatSVDUpper = errorPhatSVDMean + 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  
  # Eigen-decomposition
  fileName = paste("../../Result/result_", dataName, "_brute_", "m_", m, "_eig.RData", sep="")
  load(fileName)
  errorAbarMean = rep(mean(error_A_bar))
  errorAbarLower = errorAbarMean - sqrt(var(error_A_bar))/sqrt(length(error_A_bar))*1.96
  errorAbarUpper = errorAbarMean + sqrt(var(error_A_bar))/sqrt(length(error_A_bar))*1.96
  errorPhatEIGMean = rowMeans(error_P_hat)
  errorPhatEIGLower = errorPhatEIGMean - 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  errorPhatEIGUpper = errorPhatEIGMean + 
    sqrt(apply(error_P_hat, 1, var))/sqrt(dim(error_P_hat)[2])*1.96
  
  df = data.frame(d=rep(dVec,3), errorMean=c(rep(mean(error_A_bar), length(dVec)),
                                             errorPhatSVDMean,errorPhatEIGMean),
                  errorLower=c(rep(errorAbarLower, length(dVec)), errorPhatSVDLower,
                               errorPhatEIGLower),
                  errorUpper=c(rep(errorAbarUpper, length(dVec)), errorPhatSVDUpper,
                               errorPhatEIGUpper),
                  flag=c(rep("Abar",length(dVec)), rep("Phat_SVD",length(dVec)),
                         rep("Phat_EIG",length(dVec))))
  
  p = ggplot(data=df, aes(x=d, y=errorMean, color=flag)) + geom_point() + geom_line() +
    geom_ribbon(aes(ymin=errorLower, ymax=errorUpper), linetype=2, alpha=0.1)
  p = p + theme(legend.text=element_text(size = 12, face="bold"))
  p = p + theme(legend.title=element_blank())
  p = p + theme(plot.title = element_text(face="bold"))
  p = p + scale_x_continuous(name="Dimension") + scale_y_continuous(name="MSE")
  p = p + ggtitle(paste("m = ", m, sep=""))
  p = p + expand_limits(y=c(yMin, yMax))
  pp[[iM]] = p
}

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

grid_arrange_shared_legend(paste(dataName, ", n=", n, ", M=", M, sep=""), 
                           pp[[1]], pp[[2]], pp[[3]], pp[[4]])


