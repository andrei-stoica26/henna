#' Plot item bars grouped by class
#'
#' This function plots bars for each item while grouping them by class and
#' ordering them.
#'
#' @inheritParams networkPlot
#' @inheritParams hullPlot
#' @inheritParams radialPlot
#' @param df A data frame with at least three columns, with class, item and
#' value as the first three columns. The latter must be numeric.
#' @param legendLab Legend label.
#' @param labelColor Label color.
#' @param decreasing Whether to display the bars in decreasing order of length.
#'
#' @return A class plot.
#'
#' @examples
#'  df <- data.frame(Class = sample(paste0('C', seq(13)), 25, replace=TRUE),
#'  Item = paste0('I', seq(25)),
#'  Value = runif(25, 0.5, 1))
#'  classPlot(df)
#'
#'  df <- data.frame(Class = sample(paste0('C', seq(13)), 25, replace=TRUE),
#'  Item = sample(paste0('I', seq(21)), 25, replace=TRUE),
#'  Value = runif(25, 0.5, 1))
#'  classPlot(df)
#'
#' @export
#'
classPlot <- function(df,
                      title = 'Class plot',
                      xLab = 'Value',
                      yLab = 'Item',
                      legendLab ='Class',
                      palette = 'Spectral',
                      labelSize = 2.5,
                      labelColor ='black',
                      decreasing = TRUE,
                      ...){
    nClasses <- length(unique(df[, 1]))
    df <- df[order(df[, 3], decreasing=decreasing), ]
    df[, 1] <- factor(df[, 1], levels=unique(df[, 1]))
    df[, 4] <- make.unique(as.character(df[, 2]))
    df[, 4] <- factor(df[, 4], levels=rev(df[, 4]))
    p <- ggplot(data=df, aes(fill=df[, 1], x=df[, 3], y=df[, 4])) +
        geom_bar(position='stack', stat='identity') +
        theme_classic() +
        theme(axis.ticks.y=element_blank(),
              axis.text.y=element_blank()) +
        labs(x=xLab, y=yLab, fill=legendLab) +
        scale_fill_manual(values=hcl.colors(nClasses, palette)) +
        geom_text(aes(x=df[, 3] / 2, y=df[, 4], label=df[, 2]),
                  size=labelSize,
                  color=labelColor)
    p <- centerTitle(p, title, ...)
    return(p)
}
