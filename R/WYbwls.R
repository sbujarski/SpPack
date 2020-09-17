#' Williamson-York Bivariate Weighted Least Squared
#'
#' Runs the Williamson-York bivariate weighted least square regression algorith.
#' @name WYbwls
#' @param x vector of x values
#' @param xsd standard deviation of x values
#' @param y vector of y values
#' @param ysd standard deviation of y values
#' @param print boolean to print results when storing results object
#' @param plot boolean to produce plot with uncertainty ellipses OLS and WY regression lines
#' @param tol tolerance for algorithm (defaults to 1e-8)
#' @return list of WY.Summary, OLS regression, WY intercept, Intercept SE, WY slope, Slope SE, r anologue, R2 anologue, p-value, and plot
#' @keywords regression
#' @export
#' @examples
#' X <- c(7.042, 2.419, 3.371, 2.394, 3.633, 3.904, 2.915, 7.676, 3.338, 2.440, 3.073, 2.651, 4.593, 4.734, 6.614, 5.403, 6.529, 6.576, 8.415, 3.306)
#' Xsd<-c(1.660, 1.296, 1.283, 0.403, 1.686, 0.793, 1.019, 0.376, 1.792, 1.215, 1.030, 0.706, 1.314, 0.246, 0.590, 1.697, 0.425, 0.708, 0.417, 0.957)
#' Y <- c(5.393, 3.793, 5.055, 2.769, 9.481, 3.704, 0.408, 6.143, 3.754, 6.163, 1.012, 4.447, 4.465, 4.769, 5.623, 5.255, 9.506, 3.071, 5.640, 2.837)
#' Ysd<-c(0.618, 1.523, 1.045, 0.208, 1.812, 1.774, 1.508, 1.599, 0.641, 0.723, 0.230, 0.774, 1.585, 0.369, 0.372, 1.087, 0.518, 1.122, 0.786, 1.184)
#' XYData<-data.frame(X=X,Xsd=Xsd,Y=Y,Ysd=Ysd)
#'
#' WYtest <- WYbwls(x=XYData$X, xsd=XYData$Xsd, y=XYData$Y, ysd=XYData$Ysd, print=T, plot=T)

WYbwls <- function (x, xsd, y, ysd, print=T, plot=T, tol=1e-8, gof.adj = T, ols = T){
  #Error Checking
  {
    #make sure the data is of identical length
    if(!(length(x)==length(xsd)&&length(xsd)==length(y)&&length(y)==length(ysd))){
      print ("Error\nData columns are not of the same length")
      return(NA)
    }

    #check to make sure missing data is paired in x and y
    x.NA <- which(is.na(x))
    xsd.NA <- which(is.na(xsd))
    if(!(length(x.NA)==length(xsd.NA))){
      print("x values differ in amount of missingness")
      return(NA)
    }
    if(!(all.equal(x.NA,xsd.NA))){
      print ("Error\nMissingness in x is not paired correctly")
      return(NA)
    }

    y.NA <- which(is.na(y))
    ysd.NA <- which(is.na(ysd))
    if(!(length(y.NA)==length(ysd.NA))){
      print("y values differ in amount of missingness")
      return(NA)
    }
    if(!(all.equal(y.NA,ysd.NA))){
      print ("Error\nMissingness in y is not paired correctly")
      return(NA)
    }
  }

  #OLS correlation
  r<-cor(x,y)
  n<-length(x)
  
  OLSLM <- lm(y~x)
  if(ols){
    if(print){
      print("OLS Regression Results")
      print(summary(OLSLM))
    }
  }


  #Weighting Errors
  wx<-abs(1/(xsd^2))
  wy<-abs(1/(ysd^2))
  alpha <- sqrt(wx*wy)

  b <- OLSLM$coefficients["x"]
  d <- tol
  i <- 0
  ri <- 0

  #York et al. (2004) Algorithm
  if(print){print(noquote("Williamson-York Iterative Algorithm"))}
  while (d > tol || d == tol) #Tolerance check loop
  {
    i <- i+1
    b2 <- b
    W <- wx*wy/((wx) + ((b^2)*wy) - (2*b*alpha*ri))
    meanx <- sum(W*x)/sum(W)
    meany <-  sum(W*y)/sum(W)
    U <- x - meanx
    V <- y - meany
    Beta <- W*((U/wy)+((b*V)/wx) - (b*U + V)*(ri/alpha))
    meanBeta <- sum(W*Beta)/sum(W)
    b <- sum(W*Beta*V)/sum(W*Beta*U)
    dif <- b - b2
    d <- abs(dif)
    if(print){print(noquote(paste("iteration: ",i,"     d: ",d)))}
    if (i > 10000){
      return(list(WY.Summary=NA, OLSLM=NA,
                  WY.Int=NA,WY.Int.SE=NA,
                  WY.Slope=NA, WY.Slope.SE=NA,
                  r=NA, r2=NA, p=NA, plot=NA))
    }
  }

  U2 <- U^2
  V2 <- V^2
  a <- meany - b*meanx
  X <- meanx + Beta
  meanx <- sum(W*X)/sum(W)
  u <- X - meanx
  
  S <- sum(W*((y - b*x - a))^2)
  wr = sum(U*V)/sqrt((sum(U2)*sum(V2)))
  
  sigbsq <- 1/(sum(W*(u*u)))
  sigasq <- 1/(sum(W)) + meanx^2*(sigbsq)
  
  if(gof.adj){
    sigb <- sqrt(sigbsq*S/(n-2))
    siga <- sqrt(sigasq*S/(n-2))    
  } else {
    sigb <- sqrt(sigbsq)  
    siga <- sqrt(sigasq)
  }
  

  WYcoefficients <- data.frame(parameter = c("Intercept", "Slope"), coefficient = c(a, b), se = c(siga, sigb), t = c(a,b)/c(siga, sigb), p = 2*pt(-abs(c(a,b)/c(siga, sigb)),df=n-2))
  
  if(print){
    cat("\nWilliamson-York Algorithm for Bivariate Weighted Least Squared\n")
    if(gof.adj){cat("Goodness of Fit SE Adjusted\n\n")}
    print(WYcoefficients)    
    cat("\n")
    cat("WY correlation = ", wr, "\n\n")
  }


  #Generate Figure (if requested)
  Fig<-NULL
  if(plot){
    xydata <- data.frame(x=x, xsd=xsd, y=y, ysd=ysd)
    Data.Ellipse <- CompEllipse(x=xydata$x, xsd=xydata$xsd, y=xydata$y, ysd=xydata$ysd)
    xydata$meanSD <- (xydata$xsd+xydata$ysd)/2
    xydata$Wsize <- 1/(xydata$meanSD^2)
    WYline<-data.frame(x=c(min(Data.Ellipse$xEll),max(Data.Ellipse$xEll)))
    WYline$WY.y<-a+b*WYline$x
    
    
    if(ols){
      Fig <- ggplot()+
        geom_point(data=xydata, aes(x=x, y=y, size=Wsize), show.legend=F)+
        scale_size_continuous(range = c(2,7))+
        geom_polygon(data=Data.Ellipse,aes(x=xEll,y=yEll, group=obs), alpha=.15)+
        geom_smooth(data=xydata, method="lm", formula = 'y ~ x', aes(x=x, y=y, linetype="OLS"), se=F, size=1, colour="black")+
        geom_line(data=WYline, aes(x=x,y=WY.y, linetype="Williamson-York"), colour="black", size=1)+
        scale_linetype_manual("Analysis Type", values = c("dashed", "solid"))+
        SpTheme() + theme(legend.position = "right", legend.key.width=unit(2,"line"))
      print(Fig)
    } else {
      Fig <- ggplot()+
        geom_point(data=xydata, aes(x=x, y=y, size=Wsize), show.legend=F)+
        scale_size_continuous(range = c(2,7))+
        geom_polygon(data=Data.Ellipse,aes(x=xEll,y=yEll, group=obs), alpha=.15)+
        geom_line(data=WYline, aes(x=x,y=WY.y), colour="black", size=1)+
        SpTheme()
      print(Fig)  
    }
    
  }

  return(list(WYcoefficients=WYcoefficients, OLSLM=OLSLM,
              WY.Int=a,WY.Int.SE=siga,
              WY.Slope=b, WY.Slope.SE=sigb,
              r=wr, plot=Fig))
}




