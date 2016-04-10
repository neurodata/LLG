

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
diag_aug <- function(A, d=1) {
#   require(Matrix)
#   n = dim(A)[1]
#   return(A + Diagonal(n, x=rowSums(A))/(n-1))
  for (iIter in 1:1) {
    tmp = ase(A, d, 0)
    diag(A) = diag(tmp[[3]][,1:d] %*% diag(tmp[[1]][1:d]) %*% t(tmp[[2]][,1:d]))    
  }
  return(A)
}




# Regularize probability matrix
regularize <- function(A) {
  diag(A) = 0
  A[A > 1] = 1
  A[A < 0] = 0
  return(A)
}


add <- function(x) Reduce("+", x)

dim_brute <- function(m, n, rho, tau, B, dVec, isSVD=1) {
  result = rep(NaN, nD+1)
  
  require(igraph)
  A_all = list()
  for (i in 1:m) {
    g = sample_sbm(n, B, n*rho, directed=F, loops=F)
    A = as_adj(g, type="both", sparse=FALSE)
    A_all[[i]] = A
  }
  
  tau = rep(1:K,n*rho)
  P = B[tau,tau]
  diag(P) = 0
  
  A_bar = add(A_all)/m
  result[1] = norm(P - A_bar, "F")/n/(n-1)
  
  dMax = max(dVec)
  nD = length(dVec)
  
  A.ase = ase(diag_aug(A_bar), dMax, isSVD)
  for (iD in 1:nD) {
    d = dVec[iD]
    if (d == 1)
      Ahat = A.ase[[1]][1] * A.ase[[3]][,1:d] %*% t(A.ase[[2]][,1:d])
    else
      Ahat <- A.ase[[3]][,1:d] %*% diag(A.ase[[1]][1:d]) %*% t(A.ase[[2]][,1:d])
    P_hat = regularize(Ahat)
    result[iD+1] = norm(P - P_hat, "F")/n/(n-1)
  }
  
  return(result)
}


dim_brute1 <- function(M, m, dVec, A_all, A_sum, isSVD=1) {
  result = rep(NaN, nD+1)
  
  sampleVec = sample.int(M, m)
  A_bar = add(A_all[sampleVec])
  P_bar = (A_sum - A_bar)/(M - m)
  #   P_bar = A_sum/M
  A_bar = A_bar/m
  result[1] = norm(P_bar - A_bar, "F")/n/(n-1)
  
  dMax = max(dVec)
  nD = length(dVec)
  
  A.ase = ase(diag_aug(A_bar), dMax, isSVD)
  for (iD in 1:nD) {
    d = dVec[iD]
    if (d == 1)
      Ahat = A.ase[[1]][1] * A.ase[[3]][,1:d] %*% t(A.ase[[2]][,1:d])
    else
      Ahat <- A.ase[[3]][,1:d] %*% diag(A.ase[[1]][1:d]) %*% t(A.ase[[2]][,1:d])
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
      A.lvectors = A.svd$u
      A.rvectors = A.svd$v
    } else{
      A.svd = svd(A)
      A.values = A.svd$d[1:dim]
      A.lvectors = A.svd$u[,1:dim]
      A.rvectors = A.svd$v[,1:dim]
    }
  } else {
    if(nrow(A) >= 400){
      require(rARPACK)
      A.eig = eigs_sym(matrix(A, ncol=dim(A)[1]), dim, which = "LA")
      A.values = A.eig$values
      A.lvectors = A.eig$vectors
      A.rvectors = A.lvectors
    } else{
      A.eig = eigen(A, symmetric = T)
      A.values = A.eig$values[1:dim]
      A.lvectors = A.eig$vectors[,1:dim]
      A.rvectors = A.lvectors
    }
  }
  return(list(A.values, A.rvectors, A.lvectors))
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
    Ahat = A.ase[[1]] * A.ase[[3]] %*% t(A.ase[[2]])
  else
    Ahat <- A.ase[[3]] %*% diag(A.ase[[1]]) %*% t(A.ase[[2]])
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



sim_all <- function(m, n, tau, B, d, isSVD=1) {
  result = matrix(rep(NaN, 2*3), ncol=2)
  
  require(igraph)
  A_all = list()
  for (i in 1:m) {
    g = sample_sbm(n, B, round(n*rho), directed=F, loops=F)
    A = as_adj(g, type="both", sparse=FALSE)
    A_all[[i]] = A
  }
  
  P = B[tau,tau]
  diag(P) = 0
  
  nv1 = (tau == 1)
  nv2 = (tau == 2)
  n1 = sum(nv1)
  n2 = sum(nv2)
  
  A_bar = add(A_all)/m
  A_bar1 = A_bar[nv1, nv1]
  A_bar2 = A_bar[nv2, nv2]
  A_bar3 = A_bar[nv1, nv2]
  
  result[1, 1] = (norm(P[nv1, nv1] - A_bar1, "F"))^2/n1/(n1-1)
  result[2, 1] = (norm(P[nv2, nv2] - A_bar2, "F"))^2/n2/(n2-1)
  result[3, 1] = (norm(P[nv1, nv2] - A_bar3, "F"))^2/n1/n2
  
  A.ase = ase(diag_aug(A_bar, d), d, isSVD)
#   A.ase = ase(diag_aug(A_bar), d, isSVD)
  
  if (d == 1) {
    Ahat = A.ase[[1]][1] * A.ase[[3]][,1:d] %*% t(A.ase[[2]][,1:d])
  } else {
    Ahat = A.ase[[3]][,1:d] %*% diag(A.ase[[1]][1:d]) %*% t(A.ase[[2]][,1:d])
  }
  P_hat = regularize(Ahat)
  P_hat1 = P_hat[nv1, nv1]
  P_hat2 = P_hat[nv2, nv2]
  P_hat3 = P_hat[nv1, nv2]
  
  result[1, 2] = (norm(P[nv1, nv1] - P_hat1, "F"))^2/n1/(n1-1)
  result[2, 2] = (norm(P[nv2, nv2] - P_hat2, "F"))^2/n2/(n2-1)
  result[3, 2] = (norm(P[nv1, nv2] - P_hat3, "F"))^2/n1/n2
  
  return(result)
}


# sim_all1 <- function(m, n, tau, B, d, isSVD=1) {
#   result = matrix(rep(NaN, 2*3), ncol=2)
#   
#   require(igraph)
#   A_all = list()
#   for (i in 1:m) {
#     g = sample_sbm(n, B, n*rho, directed=F, loops=T)
#     A = as_adj(g, type="both", sparse=FALSE)
#     A_all[[i]] = A
#   }
#   
#   P = B[tau,tau]
#   
#   nv1 = (tau == 1)
#   nv2 = (tau == 2)
#   n1 = sum(nv1)
#   n2 = sum(nv2)
#   
#   A_bar = add(A_all)/m
#   A_bar1 = A_bar[nv1, nv1]
#   A_bar2 = A_bar[nv2, nv2]
#   A_bar3 = A_bar[nv1, nv2]
#   
#   result[1, 1] = (norm(P[nv1, nv1] - A_bar1, "F"))^2/n1/n1
#   result[2, 1] = (norm(P[nv2, nv2] - A_bar2, "F"))^2/n2/n2
#   result[3, 1] = (norm(P[nv1, nv2] - A_bar3, "F"))^2/n1/n2
#   
#   A.ase = ase(A_bar, d, isSVD)
#   
#   if (d == 1) {
#     Ahat = A.ase[[1]][1] * A.ase[[3]][,1:d] %*% t(A.ase[[2]][,1:d])
#   } else {
#     Ahat = A.ase[[3]][,1:d] %*% diag(A.ase[[1]][1:d]) %*% t(A.ase[[2]][,1:d])
#   }
#   P_hat = regularize(Ahat)
#   P_hat1 = P_hat[nv1, nv1]
#   P_hat2 = P_hat[nv2, nv2]
#   P_hat3 = P_hat[nv1, nv2]
#   
#   result[1, 2] = (norm(P[nv1, nv1] - P_hat1, "F"))^2/n1/n1
#   result[2, 2] = (norm(P[nv2, nv2] - P_hat2, "F"))^2/n2/n2
#   result[3, 2] = (norm(P[nv1, nv2] - P_hat3, "F"))^2/n1/n2
#   
#   return(result)
# }


