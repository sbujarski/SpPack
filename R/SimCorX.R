#' Simulating Correlated Y for Given X
#'
#' Simulates bivariate correlation of specified n, means, sd, and rho
#' @name SimCor
#' @param n number of datapoints to simulate
#' @param xmean mean of x variable
#' @param xsd standard deviation of x variable
#' @param ymean mean of y variable
#' @param ysd standard deviation of y variable
#' @param rho intended correlation
#' @return dataframe of correlated x and y variables
#' @keywords simulation
#' @export
#' @examples
#' SimCor(n=20, xmean=10, xsd=5, ymean=1, ysd=.5, rho=.5)


SimCorX <- function (x, ymean, ysd, rho)
{
  xnorm <- (x-mean(x))/sd(x)
  n <- length(x)
  ynorm <- rnorm(n,0,1)
  a <- rho/(sqrt(1-rho^2))
  ycor <- a*xnorm+ynorm

  y <- ycor*ysd+ymean

  return (data.frame(x,x))
}

#Usage Examples
# SimCorX(x, Ymean=1, Ysd=.5, rho=.5)
