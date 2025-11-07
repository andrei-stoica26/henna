#' Label points in a ggplot object
#'
#' This function labels points in a ggplot object.
#'
#' @inheritParams centerTitle
#' @inheritParams documentFun
#' @param ... Additional arguments passed to \code{geom_text_repel}
#' (if \code{labelType} is 'free') or \code{geom_text_label}
#' (if \code{labelType} is 'boxed').
#'
#' @return A ggplot object.
#'
#' @examples
#' filePath <- system.file('extdata', 'hullPlot.qs', package='henna')
#' sharedDF <- qs::qread(filePath)
#' name1 <- 'alpha'
#' name2 <- 'delta'
#' legendLabs <- as.factor(c('Non-top',
#' 'Shared',
#' paste0('Top only for ', name2),
#' paste0('Top only for ', name1)))
#' p <- hullPlot(sharedDF, 'Shared markers plot', xInt=1.5, yInt=1.3,
#' xLab=paste0('avg_log2FC (', name1, ')'),
#' yLab=paste0('avg_log2FC (', name2, ')'),
#' legendLabs=legendLabs)
#' labelDF <- sharedDF[sharedDF[, 'avg_log2FC_1'] > 1.5 &
#' sharedDF[, 'avg_log2FC_2'] > 1.3, ]
#' p <- labelPoints(p, labelDF, labelType='boxed', nudge_x=0.1, nudge_y=0.1)
#'
#' @export
#'
labelPoints <- function(p,
                        labelDF,
                        labelType = c('free', 'boxed'),
                        labelSize = 2.5,
                        labelColor = 'black',
                        labelRepulsion = 1,
                        labelPull = 1,
                        maxOverlaps = 50,
                        boxPadding = 0.2,
                        labelPadding = 0.1,
                        ...){

    labelType <- match.arg(labelType, c('free', 'boxed'))
    plotFuns <- setNames(list(geom_text_repel, geom_label_repel),
                         c('free', 'boxed'))
    basicArgs <- list(mapping=aes(x=labelDF[, 1],
                                  y=labelDF[, 2],
                                  label=rownames(labelDF)),
                      data=labelDF,
                      size=labelSize,
                      color=labelColor,
                      force=labelRepulsion,
                      force_pull=labelPull,
                      max.overlaps=maxOverlaps,
                      box.padding=boxPadding)
    extraArgs <- list(...)
    if(labelType == 'boxed')
        extraArgs <- c(label.padding=labelPadding, extraArgs)
    allArgs <- c(basicArgs, extraArgs)
    p <- p + do.call(plotFuns[[labelType]], allArgs)
    return(p)
}
