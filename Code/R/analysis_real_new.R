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
print(image(Matrix(P),main=list(label=TeX('$P$ for desikan'),cex=3),sub="",
            xlab=list(cex=0),ylab=list(cex=0),
            scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),lwd=0),
      split=c(x=1,y=1,nx=3,ny=1),more=TRUE)

sampleVec = sample.int(M, m)
A_bar = add(A_all[sampleVec])/m
print(image(Matrix(A_bar),main=list(label=TeX('$\\bar{A}$ for desikan with M=5'),cex=3),
            sub="",xlab=list(cex=0),ylab=list(cex=0),
            scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),lwd=0),
      split=c(x=2,y=1,nx=3,ny=1),more=TRUE)

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

print(image(Matrix(P_hat),main=list(label=TeX('$\\hat{P}$ for desikan with M=5'),cex=3),
            sub="",xlab=list(cex=0),ylab=list(cex=0),
            scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),lwd=0),
      split=c(x=3,y=1,nx=3,ny=1))

print(dHat)


###### Scatter Plot ######
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point()

plot(abs(A_bar-P),abs(P_hat-P))

err_Abar = abs(A_bar-P)[lower.tri(abs(A_bar-P))]
err_Phat = abs(P_hat-P)[lower.tri(abs(P_hat-P))]
nv = (err_Abar < err_Phat)
plot(err_Abar[nv],err_Phat[nv],pch=".",col="blue",cex=2,
     xlim=c(0,0.5), ylim=c(0,0.5),xlab="|Abar-P|",ylab="|Phat-P|")
points(err_Abar[!nv],err_Phat[!nv],pch=".",col="red",cex=2)

sum(err_Abar)
sum(err_Phat)

sum(err_Abar^2)
sum(err_Phat^2)


# scatter_df <- rbind(data.frame(err_Abar=err_Abar,
#                                err_Phat=err_Phat,which="desikan")) %>%
#   mutate(which=factor(paste0("desikan"),c("desikan")))
# 
# gg <- ggplot(scatter_df,aes(x=err_Abar,y=err_Phat,linetype=factor(which),shape=factor(which)))+
#   facet_wrap(~m)+
#   geom_point(data=scatter_df,size=3)+
#   scale_shape_manual(name="",values=c(-1,-1,15,17))+
#   xlab("")+ylab("MSE")+
#   theme(strip.text.x = element_text(size=20,face="bold"))+
#   theme(axis.text=element_text(size=15),
#         axis.title=element_text(size=20,face="bold"))+
#   theme(panel.grid.major = element_line(colour="grey95"),
#         panel.grid.minor = element_blank())+
#   theme(panel.background = element_rect(fill = 'white', colour = 'grey70'))+
#   theme(legend.text=element_text(size=20,face="bold"))+
#   theme(legend.position="bottom")+
#   ggtitle(paste0(dataName, ", N=", n, ", ", M, " graphs"))+
#   theme(legend.key.size=unit(legendSize,"line"))+
#   theme(plot.title=element_text(lineheight=.8,size=20,face="bold"))


####### Plot the difference ######
Diff_A_bar = abs(A_bar - P)
Diff_P_hat = abs(P_hat - P)

library(lattice)

# valLow = 0.35
valLow = 0.4

# nv = (Diff_A_bar<valLow)
# nv[upper.tri(nv,diag=T)] = FALSE
# Diff_A_bar[nv] = 0
# new.palette=colorRampPalette(c("white","black"),space="rgb") 
# print(levelplot(Diff_A_bar[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
#                 ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
#                 main=list(label=TeX('$|\\bar{A} - P|$ for desikan with M=5'),cex=2),
#                 colorkey=FALSE),split=c(x=2,y=1,nx=3,ny=1),more=TRUE)
# 
# nv = (Diff_P_hat<valLow)
# nv[upper.tri(nv,diag=T)] = FALSE
# Diff_P_hat[nv] = 0
# new.palette=colorRampPalette(c("white","black"),space="rgb") 
# print(levelplot(Diff_P_hat[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
#                 ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
#                 main=list(label=TeX('$|\\hat{P} - P|$ for desikan with M=5'),cex=2),
#                 colorkey=FALSE),split=c(x=3,y=1,nx=3,ny=1))

new.palette=colorRampPalette(c("white","black"),space="rgb")

nv = (Diff_A_bar<valLow)
nv[upper.tri(nv,diag=T)] = FALSE
Diff_A_bar[nv] = 0
levelplot(Diff_A_bar[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
          ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
          main=list(label=TeX('$|\\bar{A} - P|$ for desikan with M=5'),cex=2.5),
          colorkey=FALSE)

nv = (Diff_P_hat<valLow)
nv[upper.tri(nv,diag=T)] = FALSE
Diff_P_hat[nv] = 0
levelplot(Diff_P_hat[1:n,n:1],col.regions=new.palette(20),xlab=list(cex=0),
          ylab=list(cex=0),scales=list(x=list(draw=FALSE),y=list(draw=FALSE)),
          main=list(label=TeX('$|\\hat{P} - P|$ for desikan with M=5'),cex=2.5),
          colorkey=list(labels=list(cex=2)))

###### Write to files ######

Diff_between = abs(A_bar - P_hat)
Diff_A_bar = abs(A_bar - P)
Diff_P_hat = abs(P_hat - P)

# valLow = sort(Diff_P_hat, decreasing=T)[0.01*n^2]
# nv = (Diff_P_hat<valLow)
# s = ""
# for (i in 1:(n-1)) {
#   for (j in (i+1):n) {
#     if (nv[i,j]==F) {
#       s = paste0(s,",",i,",",j,",",P_hat[i,j]-P[i,j])
#     }
#   }
# }
# s = substr(s,2,nchar(s))
# write(s,file="../../Result/Edge_Diff_Phat_desikan.csv")

valLow = sort(Diff_A_bar-Diff_P_hat, decreasing=T)[50]
nv = ((Diff_A_bar-Diff_P_hat)<valLow)
s = ""
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    if (nv[i,j]==F) {
      s = paste0(s,",",i,",",j,",",P_hat[i,j]-P[i,j])
    }
  }
}
s = substr(s,2,nchar(s))
write(s,file="../../Result/Edge_Diff_between_desikan.csv")




rowSumDiffBetween = rowSums(Diff_A_bar-Diff_P_hat)

valLow = sort(rowSumDiffBetween, decreasing=T)[5]
nv = (rowSumDiffBetween<valLow)
s = ""
for (i in 1:n) {
  if (nv[i]==F) {
    s = paste0(s,",",i)
  }
}
s = substr(s,2,nchar(s))
write(s,file="../../Result/Vertex_Diff_Between_desikan.csv")

# rowSumDiffPhat = rowSums(Diff_P_hat)
# 
# valLow = sort(rowSumDiffPhat, decreasing=T)[5]
# nv = (rowSumDiffPhat<valLow)
# s = ""
# for (i in 1:n) {
#   if (nv[i]==F) {
#     s = paste0(s,",",i)
#   }
# }
# s = substr(s,2,nchar(s))
# write(s,file="../../Result/Vertex_Diff_Phat_desikan.csv")



