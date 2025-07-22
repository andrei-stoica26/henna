#'@importFrom ggplot2 aes after_stat element_blank element_text geom_text ggplot ggtitle scale_fill_viridis_d margin theme theme_void
#'
NULL

#' Add a centered title to a plot
#'
#' This function adds a centered title to a ggplot object
#'
#' @param p A ggplot object.
#' @param title Plot title.
#' @param ... Other arguments passed to element_text.
#'
#' @return A ggplot object.
#'
#' @export
#'
titlePlot <- function(p, title, ...)
    return(p + ggtitle(title) + theme(plot.title=element_text(hjust=0.5, ...)))
