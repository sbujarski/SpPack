#Generates Q-Q plot of data using ggplot2

library(ggplot2)

SpQQPlot<- function (data, varname=NA, save=F) # argument: datator of numbers
{
  if(is.na(varname)){
    varname<-deparse(substitute(data))  
  }
  
  y <- quantile(data[!is.na(data)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = data)
  
  qqplot <- ggplot(d, aes(sample = resids)) +
    stat_qq() + geom_abline(slope = slope, intercept = int) + 
    ggtitle(paste(varname, "Q-Q Plot")) + scale_x_continuous("Theoretical") + scale_y_continuous("Sample") + 
    SpTheme()
  
  if(save){
    ggsave(qqplot, filename=paste(paste(varname, " Q-Q Plot.png", sep="")), width = 5, height=4, dpi=500)
  }
  
  return(qqplot)
  
}

#Usage samples
# SpQQPlot(x, save=T)
# SpQQPlot(x, varname="varname #1", save=T)
# xqqplot <- SpQQPlot(XYData$X)
# xsdqqplot <- SpQQPlot(XYData$Xsd)
# yqqplot <- SpQQPlot(XYData$Y)
# ysdqqplot <- SpQQPlot(XYData$Ysd)