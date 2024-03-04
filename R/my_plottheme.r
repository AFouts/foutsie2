#' A function that customizes my plots using my favourite gaury theme
#'
#' @param plot_in The plot to customize.
#'
#' @return A customized plot.
#'
#' @import ggplot2
#' @import ggthemes
#' 
#' @export
#' 
my_plottheme <- function(plot_in) {
    #usethis::use_package("ggplot2")
    #usethis::use_package("ggthemes")
    plot_out <- plot_in + theme_economist() + theme(axis.line.x.bottom=element_line(color="blue"), plot.title = element_text(size = rel(2)), plot.background = element_rect(fill = "pink"))
    return(plot_out)
}