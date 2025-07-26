#'@importFrom ggplot2 aes after_stat coord_fixed element_blank element_text geom_bar geom_point geom_polygon geom_segment geom_text ggplot ggproto ggtitle labs scale_color_discrete scale_color_manual scale_fill_manual scale_fill_viridis_d margin theme theme_classic theme_void xlab
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


#' Label points in a ggplot object
#'
#' This function labels points in a ggplot object
#'
#' @inheritParams centerTitle
#' @param labelDF Label data frame.
#' @param labelSize Label size.
#' @param labelColor Label colors
#' @param maxOverlaps Maximum overlaps.
#'
#' @return A ggplot object.
#'
#' @export
#'
labelPoints <- function(p, labelDF, labelSize = 2, labelColor = 'black',
                        maxOverlaps = 10){
    p <- p + geom_text_repel(data=labelDF,
                             aes(x=.data[[colnames(labelDF)[1]]],
                                 y=.data[[colnames(labelDF)[2]]],
                                 label=rownames(labelDF)),
                             size = labelSize,
                             color = labelColor,
                             max.overlaps = maxOverlaps)
    return(p)
}
