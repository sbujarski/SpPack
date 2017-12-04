#' Uncertainty Elipses Computations
#'
#' Computes the datapoints in order to plot uncertainty elipses \cr
#' @name CompEllipse
#' @param x vector of x values
#' @param xsd vector of xsd values (specifies the width of the elipse)
#' @param y vector of y values
#' @param ysd vector of ysd values (specifies the height of the elipse)
#' @return dataframe of Ellipse x-y points
#' @keywords plotting
#' @export
#' @examples
#' Data <- data.frame(x=seq(1:10), xsd=runif(10), y=rnorm(n=10), ysd=runif(10))
#' Ellipse <- CompEllipse(x=Data$x, xsd=Data$xsd, y=Data$y, ysd=Data$ysd)
#' p <- ggplot()+
#'     geom_point(data=Data, aes(x=x, y=y))+
#'     geom_polygon(data=Ellipse,aes(x=xEll,y=yEll, group=obs), alpha=.15)
#' print(p)


CompEllipse <- function (x, xsd, y, ysd)
{
  data<-data.frame(x=x, xsd=xsd, y=y, ysd=ysd)

  data$obs <- as.numeric(rownames(data))
  data.Ellipse <- data.frame(t=rep(seq(0,6.3,.063),dim(data)[1]),obs=rep(1:dim(data)[1],each=101))
  data.Ellipse <- merge(data,data.Ellipse, by = "obs")

  #compute ellipse points centered at origin
  data.Ellipse$xEll.c <- data.Ellipse$xsd*sin(data.Ellipse$t)
  data.Ellipse$yEll.c <- data.Ellipse$ysd*cos(data.Ellipse$t)

  #move ellipse points to correct location
  data.Ellipse$xEll <- data.Ellipse$xEll.c + data.Ellipse$x
  data.Ellipse$yEll <- data.Ellipse$yEll.c + data.Ellipse$y

  data.Ellipse <- subset(data.Ellipse,select=c(obs,x,xsd,y,ysd,xEll,yEll))

  return (data.Ellipse)
}
