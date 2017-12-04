#' Spencer's Default Theme
#'
#' Default ggplot theme for Spencer's publications
#' @name SpTheme
#' @param axis.text.size axis text size (defaults to 12)
#' @param axis.title.size axis title size (defaults to 12)
#' @param title.size title size (degaults to 16)
#' @param legend.position positions of the legend (defaults to none)
#' @keywords plotting
#' @export
#' @examples
#' Data <- data.frame(x=seq(1:10), y=rnorm(n=10))
#' p <- ggplot(Data, aes(x=x, y=y))+ geom_point() +
#'     SpTheme()
#' print(p)
#'
#' #Spencer's default ggplot2 theme

SpTheme <- function(axis.text.size=12, axis.title.size=12, title.size=16, legend.position="none")
{
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x=element_line(colour="black"), axis.line.y=element_line(colour="black"),
        axis.title.x=element_text(colour = "black", size=axis.title.size), axis.title.y=element_text(colour = "black", size=axis.title.size),
        axis.text.x=element_text(colour = "black", size=axis.text.size), axis.text.y=element_text(colour = "black", size=axis.text.size),
        plot.title=element_text(colour = "black",size=title.size, face="bold", hjust=.5),
        axis.ticks=element_line(colour="black"), legend.position = legend.position, legend.key=element_blank())
}
