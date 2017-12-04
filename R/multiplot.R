#' Printing Multiple Plots in Grid
#'
#' Computes the datapoints in order to plot uncertainty elipses \cr
#' @name multiplot
#' @param ... Series of plots
#' @param cols number of columns (defaults to 1)
#' @param layout specifiable layout
#' @keywords plotting
#' @export
#' @examples
#' Data <- data.frame(x1=seq(1:10), y1=runif(10), x2=rnorm(n=10), y2=runif(10))
#' p1 <- ggplot(Data, aes(x=x1, y=y1)) + geom_point() + SpTheme()
#' p2 <- ggplot(Data, aes(x=x2, y=y2)) + geom_point() + SpTheme()
#' multiplot(p1, p2, cols=2)

library(ggplot)
library(grid)

multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

