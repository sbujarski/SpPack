#' Histogram with ggplot
#'
#' Generate Histogram using ggplot2 with overlayed normal distribution. \cr
#' Also includes mean and SD in the title.
#' @name SpHist
#' @param data dataframe or vector of numeric values
#' @param variable string of the variable of interest
#' @param bins number of bins (defaults to 30)
#' @param save boolean of whether to save image
#'
#' @keywords descriptives plotting
#' @export
#' @examples
#' SpHist(data=rnorm(100), variable="X Variable", bins=30, save=F)


library(ggplot2)

SpHist <- function(data, variable=NA, bins=30, save=F)
{
  if("data.frame" %in% class(data)){
    data<-data[variable]
    data<-na.exclude(data)
    Mean<-mean(data[,1])
    SD<-sd(data[,1])

    Histogram <- ggplot(data, aes(data[1])) + geom_histogram(aes(y=..density..), colour="white", bins=bins) +
      stat_function(fun=dnorm, args=list(mean=Mean, sd=SD), size=3) +
      ggtitle(paste(variable, "Histogram", "\n", "Mean =", round(Mean,2), "SD =", round(SD,2))) + scale_x_continuous(variable) + SpTheme()
  } else {
    if(class(data)=="numeric"){
      variable<-deparse(substitute(data))
      data<-as.data.frame(data, col.names=T)
      data<-na.exclude(data)
      Mean<-mean(data[,1])
      SD<-sd(data[,1])

      Histogram <- ggplot(data, aes(data)) + geom_histogram(aes(y=..density..), colour="white", bins=bins) +
        stat_function(fun=dnorm, args=list(mean=Mean, sd=SD), size=3) +
        ggtitle(paste(variable, "Histogram", "\n", "Mean =", round(Mean,2), "SD =", round(SD,2))) + scale_x_continuous(variable) + SpTheme()
    } else {
      cat("-- ERROR --\n")
      cat("Please enter a numeric vector or a dataframe with numeric variable specified")
      }
    }

  if(save){
    ggsave(Histogram, filename=paste(paste(variable, " Histogram.png", sep="")), width = 8, height=6.5, dpi=500)
  }
  return (Histogram)
}

#Usage examples
# SpHist(data=XYData, variable="X")
# SpHist(data=x)
# SpHist(data=XYData$Y)
# SpHist(data=c("C","L","M")) -produces an error
