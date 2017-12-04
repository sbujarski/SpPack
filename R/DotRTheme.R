#' Data on the Rocks Theme
#'
#' Default ggplot theme for Data on the Rocks blog posts
#' @name DotRTheme
#' @param axis.text.size axis text size (defaults to 16)
#' @param axis.title.size axis title size (defaults to 16)
#' @param title.size title size (degaults to 20)
#' @param legend.position positions of the legend (defaults to none)
#' @keywords plotting
#' @export
#' @examples
#' Data <- data.frame(x=seq(1:10), y=rnorm(n=10))
#' p <- ggplot(Data, aes(x=x, y=y))+ geom_point() +
#'     DotRTheme()
#' print(p)

DotRTheme <- function(axis.text.size=16, axis.title.size=16, title.size=20, legend.position="none")
{
  theme(panel.grid.major = element_line(colour="grey90"), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x=element_line(colour="black"), axis.line.y=element_line(colour="black"),
        axis.title.x=element_text(colour = "black", size=axis.title.size), axis.title.y=element_text(colour = "black", size=axis.title.size),
        axis.text.x=element_text(colour = "black", size=axis.text.size), axis.text.y=element_text(colour = "black", size=axis.text.size),
        plot.title=element_text(colour = "black",size=title.size, face="bold", hjust=.5),
        axis.ticks=element_line(colour="black"), legend.position = legend.position, legend.key=element_blank())
}
