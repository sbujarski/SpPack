#Simulates bivariate correlation of specified n, means, sd, and rho

SimCor <- function (n, Xmean, Xsd, Ymean, Ysd, rho)
{
  Xnorm <- rnorm(n,0,1)
  Ynorm <- rnorm(n,0,1)
  a <- rho/(sqrt(1-rho^2))
  Ycor <- a*Xnorm+Ynorm
  
  X <- Xnorm*Xsd+Xmean
  Y <- Ycor*Ysd+Ymean
  
  return (data.frame(X,Y))
}

#Usage Examples
# SimCor(n=20, Xmean=10, Xsd=5, Ymean=1, Ysd=.5, rho=.5)