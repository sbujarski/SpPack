#' Simulating Correlated Data
#'
#' Simulates bivariate correlation of specified n, means, sd, and rho
#' @name SimCor
#' @param n number of datapoints to simulate
#' @param n number fo datapoints to simulate
#' @param n number fo datapoints to simulate
#' @param n number fo datapoints to simulate

#' @return dataframe adding adjustment factors and inflation adjusted values
#' @keywords inflation
#' @export
#' @examples
#' InfAdj(yr=1990, adj.yr=2013, value=1)
#' Dollars <- data.frame(yr=seq(1990,2010), value=1)
#' InfAdj(yr=Dollars$yr, adj.yr=2013, value=Dollars$value)


#Simulates bivariate correlation of specified n, means, sd, and rho

SimCor <- function (n, xmean, xsd, ymean, ysd, rho)
{
  xnorm <- rnorm(n,0,1)
  ynorm <- rnorm(n,0,1)
  a <- rho/(sqrt(1-rho^2))
  ycor <- a*xnorm+ynorm

  x <- xnorm*xsd+xmean
  y <- ycor*ysd+ymean

  return (data.frame(x,y))
}

#Usage Examples
# SimCor(n=20, Xmean=10, Xsd=5, Ymean=1, Ysd=.5, rho=.5)
