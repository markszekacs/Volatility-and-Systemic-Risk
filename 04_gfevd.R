GFEVD <- function(resid.var,ma.coef){
  # minden oszlop ei-ben és ej-ben egy olyan vektor, aminek mindig az i/j-edik eleme 1, és 0 egyébként
  # Ez a Pesaran and Shin (1998) tanulmány (7) képletéből jön
  cov.error <- cov(resid.var)
  nobs <- dim(cov.error)[1]
  ei <- diag(nobs)
  ej <- diag(nobs)
  
  # Az eredményt tároló mátrixobjetum létrehozása
  Dg <- matrix(NA,ncol=nobs,nrow=nobs)
  
  # GFEVD számolás maga Pesaran and Shin (1998) 4. oldal tetején lévő második képlet (theta_{ij}^{g}) alapján
  for(j in 1:(ncol(ej))){
    # nevező ('denom') számolása 
    ejj <- ej[,j]
    little.sigma <- 1/sqrt(cov.error[j,j])
    
    denom.step1 <- llply(ma.coef,function(x) t(ejj)%*%x%*%cov.error%*%t(x)%*%ejj)
    denom = Reduce('+',denom.step1)
    for(i in 1:(ncol(ej))){
      # ez innentől kezdve a számláló ('num'), ami a (10) képlet Pesaran and Shin (1998)-ban
      eii <- ei[,i]
      num.step1 <- llply(ma.coef,function(x) t(ejj)%*%x%*%cov.error%*%eii)
      num.squared <- llply(num.step1,function(x) x^2)
      num.sum <- Reduce('+',num.squared)
      num <- little.sigma*num.sum
      
      # itt a vége: számlálót osztom nevezővel: num/denom
      Dg[j,i] <- (num/denom)
      
    }
  }
  Dg
}

ConnectednessIndexek <- function(mat,names,date.col){
  
  temp.mat <- mat
  
  diag(temp.mat) <- 0
  to <- apply(temp.mat,2,sum)
  from <- apply(temp.mat,1,sum)
  
  mat.df <- PeremOsszegek(mat,names,to,from)
  
  net <- to - from
  
  from <- as.data.frame(from,row.names = names)
  to <- as.data.frame(to,row.names = names)
  net <- as.data.frame(net,row.names = names)
  
  colnames(from) <- date.col
  colnames(to) <- date.col
  colnames(net) <- date.col
  
  list(table=mat.df,To=to,From=from,Net=net)
  
}

PeremOsszegek <- function(mat,names,to,from){
  mat.df <- as.data.frame(mat)
  colnames(mat.df) <- names
  mat.df$From <- from
  
  to2 <- c(to,mean(to))
  
  mat.df <- rbind(mat.df,to2)
  rownames(mat.df) <- c(names,"To")
  mat.df
}
