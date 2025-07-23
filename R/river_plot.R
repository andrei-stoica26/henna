#'@importFrom ggalluvial geom_alluvium geom_stratum StatStratum
#'
NULL

#' Create an alluvial plot
#'
#' This function creates an alluvial plot
#'
#' @param df A data frame with three columns.
#' @param title Plot title.
#' @param fillColIndex Index of column used for coloring the alluvia.
#' @param curveType Curve type.
#' @param alpha Opaqueness level for the colors of the alluvia.
#' @param strataFill Color used for the strata.
#' @param labelSize Size of labels of strata elements.
#' @param viridisPal Viridis palette.
#' @param legendPos Legend position.
#' @param margins Plot margins. Must be a vector of size 4 listing the desired
#' top, right, bottom and left margin, in that order.
#' @param ... Other arguments passed to titlePlot.
#'
#' @return A ggplot object.
#'
#' @export
#'
riverPlot <- function(df,
                      title = 'River plot',
                      fillColIndex = 2,
                      curveType = 'sigmoid',
                      alpha = 0.8,
                      strataFill = 'lightgoldenrod1',
                      labelSize = 3,
                      viridisPal = 'turbo',
                      legendPos = 'none',
                      margins = margin(0, -10, -10, -10),
                      ...){
    aesNames <- colnames(df)
    stratum <- StatStratum
    p <- ggplot(data=df, aes(axis1=.data[[aesNames[1]]],
                             axis2=.data[[aesNames[2]]],
                             y=.data[[aesNames[3]]])) +
        geom_alluvium(aes(fill=.data[[aesNames[fillColIndex]]]),
                      curve_type=curveType,
                      alpha=alpha) +
        geom_stratum(fill=strataFill) +
        geom_text(stat=stratum, aes(label=after_stat(stratum)),
                  size=labelSize) +
        scale_fill_viridis_d(option=viridisPal) +
        theme_void() +
        theme(legend.position=legendPos) +
        theme(plot.margin=margins)
    p <- titlePlot(p, title, ...)
    return(p)
}
