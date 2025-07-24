#'@importFrom ggplot2 aes after_stat coord_fixed element_blank element_text geom_bar geom_point geom_text ggplot ggproto ggtitle labs scale_color_discrete scale_fill_viridis_d margin theme theme_classic theme_void xlab
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
centerTitle <- function(p, title, ...)
    return(p + ggtitle(title) + theme(plot.title=element_text(hjust=0.5, ...)))
