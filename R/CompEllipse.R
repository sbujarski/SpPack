#' Uncertainty Elipses Computations
#'
#' Computes the datapoints in order to plot uncertainty elipses \cr
#' @name CompEllipse
#' @param x vector of x values
#' @param xsd vector of xsd values (specifies the width of the elipse)
#' @param y vector of y values
#' @param ysd vector of ysd values (specifies the height of the elipse)
#' @keywords plotting
#' @export data.Ellipse dataframe containing ellipse datapoints
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

#Usage Examples
# X <- c(7.042, 2.419, 3.371, 2.394, 3.633, 3.904, 2.915, 7.676, 3.338, 2.440, 3.073, 2.651, 4.593, 4.734, 6.614, 5.403, 6.529, 6.576, 8.415, 3.306)
# Xsd<-c(1.660, 1.296, 1.283, 0.403, 1.686, 0.793, 1.019, 0.376, 1.792, 1.215, 1.030, 0.706, 1.314, 0.246, 0.590, 1.697, 0.425, 0.708, 0.417, 0.957)
# Y <- c(5.393, 3.793, 5.055, 2.769, 9.481, 3.704, 0.408, 6.143, 3.754, 6.163, 1.012, 4.447, 4.465, 4.769, 5.623, 5.255, 9.506, 3.071, 5.640, 2.837)
# Ysd<-c(0.618, 1.523, 1.045, 0.208, 1.812, 1.774, 1.508, 1.599, 0.641, 0.723, 0.230, 0.774, 1.585, 0.369, 0.372, 1.087, 0.518, 1.122, 0.786, 1.184)
# XYData<-data.frame(X=X,Xsd=Xsd,Y=Y,Ysd=Ysd)
#
# XY.Ellipse <- CompEllipse(x=XYData$X, xsd=XYData$Xsd, y=XYData$Y, ysd=XYData$Ysd)
# p <- ggplot()+
#   geom_point(data=XYData, aes(x=X, y=Y))+
#   #geom_smooth(data=XYData, method="lm",aes(x=X, y=Y))+
#   geom_polygon(data=XY.Ellipse,aes(x=xEll,y=yEll, group=obs), alpha=.15)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
#         axis.text.x=element_text(colour = "black", size=12),
#         axis.text.y=element_text(colour = "black", size=12))
# print(p)
