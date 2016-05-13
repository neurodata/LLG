rm(list = ls())
# setwd("E:/GitHub/LLG/Code/R")
setwd("/Users/Runze/Documents/GitHub/LLG/Code/R")
# setwd("/cis/home/rtang/LLG/Code/R")

library(igraph)

g <- read.graph('../../Data/desikan/SWU4_0025629_1_desikan_sg.graphml', format='graphml')

centroids <- get.vertex.attribute(g, 'centroid')

l = nchar(centroids[[1]])
s = substr(centroids[[1]],2,l-1)
for (i in 2:length(centroids)) {
  l = nchar(centroids[[i]])
  s = paste0(s,", ",substr(centroids[[i]],2,l-1))
}

write(s,file="../../Data/desikan/centroid.csv")
