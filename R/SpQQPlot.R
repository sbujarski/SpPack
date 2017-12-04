#' Q-Q Plot with ggplot
#'
#' Generate Q-Q plot using ggplot2 with normal distribution reference. \cr
#' @name SpQQPlot
#' @param data vector of numeric values
#' @param variable string of the variable of interest for title
#' @param save boolean of whether to save image
#'
#' @keywords descriptives plotting
#' @export
#' @examples
#' SpQQPlot(rnorm(50), varname="Variable #1", save=T)

library(ggplot2)

SpQQPlot<- function (data, varname=NA, save=F)
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

