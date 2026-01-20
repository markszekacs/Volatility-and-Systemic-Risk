GFEVD <- function(resid.var, ma.coef){
  # Each column in ei and ej is a vector whose i/j-th element is 1 and all others are 0
  # This comes from equation (7) in Pesaran and Shin (1998)
  cov.error <- cov(resid.var)
  nobs <- dim(cov.error)[1]
  ei <- diag(nobs)
  ej <- diag(nobs)
  
  # Create a matrix object to store the results
  Dg <- matrix(NA, ncol = nobs, nrow = nobs)
  
  # Compute the GFEVD based on the second formula at the top of page 4 in Pesaran and Shin (1998)
  # (theta_{ij}^{g})
  for(j in 1:ncol(ej)){
    
    # Compute the denominator ('denom')
    ejj <- ej[, j]
    little.sigma <- 1 / sqrt(cov.error[j, j])
    
    denom.step1 <- llply(
      ma.coef,
      function(x) t(ejj) %*% x %*% cov.error %*% t(x) %*% ejj
    )
    denom <- Reduce('+', denom.step1)
    
    for(i in 1:ncol(ej)){
      
      # Numerator ('num'), corresponding to equation (10) in Pesaran and Shin (1998)
      eii <- ei[, i]
      num.step1 <- llply(
        ma.coef,
        function(x) t(ejj) %*% x %*% cov.error %*% eii
      )
      num.squared <- llply(num.step1, function(x) x^2)
      num.sum <- Reduce('+', num.squared)
      num <- little.sigma * num.sum
      
      # Final step: divide numerator by denominator
      Dg[j, i] <- num / denom
    }
  }
  Dg
}

ConnectednessIndices <- function(mat, names, date.col){
  
  temp.mat <- mat
  diag(temp.mat) <- 0
  
  to <- apply(temp.mat, 2, sum)
  from <- apply(temp.mat, 1, sum)
  
  mat.df <- AddMargins(mat, names, to, from)
  
  net <- to - from
  
  from <- as.data.frame(from, row.names = names)
  to <- as.data.frame(to, row.names = names)
  net <- as.data.frame(net, row.names = names)
  
  colnames(from) <- date.col
  colnames(to) <- date.col
  colnames(net) <- date.col
  
  list(
    table = mat.df,
    To = to,
    From = from,
    Net = net
  )
}

AddMargins <- function(mat, names, to, from){
  
  mat.df <- as.data.frame(mat)
  colnames(mat.df) <- names
  mat.df$From <- from
  
  to2 <- c(to, mean(to))
  
  mat.df <- rbind(mat.df, to2)
  rownames(mat.df) <- c(names, "To")
  
  mat.df
}

  
  mat.df <- rbind(mat.df,to2)
  rownames(mat.df) <- c(names,"To")
  mat.df
}

