

# Read M graphs
read_data <- function(dataName, DA=T) {
  if (DA) {
    fileName = paste("../../Data/data_", dataName, "_DA.RData", sep="")
  } else {
    fileName = paste("../../Data/data_", dataName, ".RData", sep="")
  }
  if (file.exists(fileName)) {
    load(fileName)
    return(list(A_all, n, M))
  } else {
    require(igraph)
    subjectsID = readLines("../../Data/subnames.txt")
    g = read_graph(paste("../../Data/", dataName, "/SWU4_", subjectsID[1], 
                         "_1_", dataName, "_sg.graphml", sep =""), format="graphml")
    n = vcount(g)
    
    M = 227*2;
    A_all = list()
    for (sub in 1:227) {
      for (session in 1:2) {
        g = read_graph(paste("../../Data/", dataName, "/SWU4_", subjectsID[sub], 
                             "_", session, "_", dataName, "_sg.graphml",sep=""), format = "graphml")
        A = as_adj(g, type="both", sparse=FALSE)
        if (DA) {
          A = diag_aug(A)
        }
        A_all[[(sub-1)*2 + session]] = A;
      }
    }
    
    save(A_all, n, M, file=fileName)
    return(list(A_all, n, M))
  }  
}




# Diagonal Augmentation
diag_aug <- function(A) {
  require(Matrix)
  n = dim(A)[1]
  return(A + Diagonal(n, x=rowSums(A))/(n-1))
}




# Regularize probability matrix
regularize <- function(A) {
  diag(A) = 0
  A[A > 1] = 1
  A[A < 0] = 0
  return(A)
}


add <- function(x) Reduce("+", x)

# Dimensions bruteforce exploring
dim_brute <- function(M, m, d, A_all, A_sum, isSVD=1) {
  
  sampleVec = sample.int(M, m)
  A_bar = add(A_all[sampleVec])
  P_bar = (A_sum - A_bar)/(M - m);
  A_bar = A_bar/m;
  
  P_hat = regularize(ase.Ahat(diag_aug(A_bar), d, isSVD))
  
  return(c(norm(P_bar - A_bar, "F")/n/(n-1), norm(P_bar - P_hat, "F")/n/(n-1)))
}

dim_brute1 <- function(M, m, dVec, A_all, A_sum, isSVD=1) {
  result = rep(NaN, nD+1)
  
  sampleVec = sample.int(M, m)
  A_bar = add(A_all[sampleVec])
  P_bar = (A_sum - A_bar)/(M - m)
  A_bar = A_bar/m
  result[1] = norm(P_bar - A_bar, "F")/n/(n-1)
  
  dMax = max(dVec)
  nD = length(dVec)
  
  A.ase = ase(diag_aug(A_bar), dMax, isSVD)
  for (iD in 1:nD) {
    d = dVec[iD]
    if (d == 1)
      Ahat = A.ase[[1]][1] * A.ase[[2]][,1:d] %*% t(A.ase[[2]][,1:d])
    else
      Ahat <- A.ase[[2]][,1:d] %*% diag(A.ase[[1]][1:d]) %*% t(A.ase[[2]][,1:d])
    P_hat = regularize(Ahat)
    result[iD+1] = norm(P_bar - P_hat, "F")/n/(n-1)
  }
  
  return(result)
}



# Using ZG to choose dimension
llg_ZG <- function(M, m, A_all, A_sum, isSVD=1) {
  
  sampleVec = sample.int(M, m)
  A_bar = add(A_all[sampleVec])
  P_bar = (A_sum - A_bar)/(M - m);
  A_bar = A_bar/m;
  
  P_hat = regularize(ase.Ahat(diag_aug(A_bar), d, isSVD))
  
  return(c(norm(P_bar - A_bar, "F")/n/(n-1), norm(P_bar - P_hat, "F")/n/(n-1)), d)
}




llg_d <- function(M, m, A_all, A_sum, d, isSVD=1) {
  
  sampleVec = sample.int(M, m)
  A_bar = add(A_all[sampleVec])
  P_bar = (A_sum - A_bar)/(M - m);
  A_bar = A_bar/m;
  
  P_hat = regularize(ase.Ahat(diag_aug(A_bar), d, isSVD))
  
  return(c(norm(P_bar - A_bar, "F")/n/(n-1), norm(P_bar - P_hat, "F")/n/(n-1), d))
}



# ASE using SVD or eigen-decomposition.
ase <- function(A, dim, isSVD=1){
  if (isSVD) {
    if(nrow(A) >= 400){
      require(irlba)
      A.svd = irlba(A, nu = dim, nv = dim)
      A.values = A.svd$d
      A.vectors = A.svd$v
    } else{
      A.svd = svd(A)
      A.values = A.svd$d[1:dim]
      A.vectors = A.svd$v[,1:dim]
    }
  } else {
    if(nrow(A) >= 400){
      require(rARPACK)
      A.eig = eigs(A, dim, which = "LM")
      A.values = A.eig$values
      A.vectors = A.eig$vectors
    } else{
      A.eig = eigen(A, symmetric = T)
      A.values = A.eig$values[1:dim]
      A.vectors = A.eig$vectors[,1:dim]
    }
  }
  return(list(A.values, A.vectors))
}


# ASE return xhat
ase.x <- function(A, dim, isSVD=1){
  A.ase = ase(A, dim, isSVD)
  if(dim == 1)
    A.x = sqrt(A.ase[[1]]) * A.ase[[2]]
  else
    A.x <- A.ase[[2]] %*% diag(sqrt(A.ase[[1]]))
  return(A.x)
}


# ASE return Ahat
ase.Ahat <- function(A, dim, isSVD=1){
  A.ase = ase(A, dim, isSVD)
  if(dim == 1)
    Ahat = A.ase[[1]] * A.ase[[2]] %*% t(A.ase[[2]])
  else
    Ahat <- A.ase[[2]] %*% diag(A.ase[[1]]) %*% t(A.ase[[2]])
  return(Ahat)
}



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}