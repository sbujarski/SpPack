#Simulates correlated data with a given vector of x data

SimCorX <- function (x, ymean, ysd, rho)
{
  Xnorm <- (X-mean(X))/sd(X)
  n <- length(X)
  Ynorm <- rnorm(n,0,1)
  a <- rho/(sqrt(1-rho^2))
  Ycor <- a*Xnorm+Ynorm

  Y <- Ycor*Ysd+Ymean

  return (data.frame(X,Y))
}

#Usage Examples
# SimCorX(x, Ymean=1, Ysd=.5, rho=.5)
