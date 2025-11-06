#' @importFrom EnhancedVolcano EnhancedVolcano
#'
NULL

#' Create a volcano plot
#'
#' This function creates a volcano plot.
#'
#' @inheritParams documentFun
#' @param df A data frame with rownames as genes, one log column and one
#' p-value column.
#' @param logCol Log column.
#' @param pvalCol P-value column.
#' @param logFCThr Threshold used to separate significant log values.
#' @param pvalThr Threshold used to separate significant p-values.
#' @param labeledGenes Gene labels to be displayed on the plot. Default is
#' \code{NULL} (gene labels will be displayed on the basis
#' of \code{labLogFCThr }) and \code{labPvalThr}).
#' @param labLogFCThr Threshold used to plot gene labels based on log values.
#' Ignored if \code{labeledGenes} is not \code{NULL}.
#' @param labPvalThr Threshold used to plot gene labels based on p-values.
#' Ignored if \code{labeledGenes} is not \code{NULL}.
#' @param alpha Opaqueness level of point color.
#' @param theme Plot theme. Choose between 'linedraw', 'bw', 'classic' and
#' 'minimal'. Default is 'linedraw'.
#'
#' @return An object of class \code{gg}.
#'
#' @export
#'
volcanoPlot <- function(df,
                        title = NULL,
                        logCol = 'avg_log2FC',
                        pvalCol = 'p_val_adj',
                        xLab = expression(log[2] ~ fold ~ change),
                        yLab = expression(-log[10] ~ `p-value`),
                        legendTitle = 'Significance',
                        legendLabs = c('Not significant',
                                       expression(log[2] ~ FC),
                                       'p-value',
                                       expression(`p-value` ~ and
                                                  ~ log[2] ~ FC)),
                        legendPos = c('right', 'top', 'left', 'bottom'),
                        logFCThr = 1,
                        pvalThr = 1e-05,
                        labeledGenes = NULL,
                        labLogFCThr = 1.8,
                        labPvalThr = 1e-12,
                        labelType = c('boxed', 'free'),
                        labelSize = 2.5,
                        labelColor = 'black',
                        labelRepulsion = 1,
                        labelPull = 0,
                        maxOverlaps = 100,
                        pointSize = 1,
                        alpha = 0.7,
                        palette = c("dodgerblue4", "goldenrod2", "slateblue3",
                                    "red2"),
                        legendTextSize = 10,
                        legendTitleSize = 10,
                        axisTextSize = 12,
                        axisTitleSize = 12,
                        theme = c('linedraw', 'bw', 'classic', 'minimal'),
                        ...){

    legendPos <- match.arg(legendPos, c('right', 'top', 'left', 'bottom'))
    theme <- match.arg(theme, c('linedraw', 'bw', 'classic', 'minimal'))
    labelType <- match.arg(labelType, c('boxed', 'free'))

    noGenes <- paste0(rep('#', max(vapply(rownames(df), nchar, integer(1)))
                          + 1), collapse='')

    p <- EnhancedVolcano(df,
                         lab=rownames(df),
                         x=logCol,
                         y=pvalCol,
                         title=NULL,
                         xlab=xLab,
                         ylab=yLab,
                         subtitle=NULL,
                         caption=NULL,
                         pCutoff=pvalThr,
                         FCcutoff=logFCThr,
                         selectLab=noGenes,
                         labSize=labelSize,
                         pointSize=pointSize,
                         legendLabels=legendLabs,
                         col=palette,
                         colAlpha=alpha,
                         boxedLabels=TRUE,
                         ...) + labs(color=legendTitle)

    if (is.null(labeledGenes)){
        labelDF <- df[abs(df[, logCol]) >= labLogFCThr &
                          df[, pvalCol] < labPvalThr, ]
        labeledGenes <- rownames(labelDF)
    } else
        labelDF <- df[labeledGenes, ]

    labelDF <- data.frame(log = labelDF[, logCol],
                          nlogPadj = -log(labelDF[, pvalCol], 10))
    rownames(labelDF) <- labeledGenes

    p <- labelPoints(p, labelDF, labelType, labelSize, labelColor,
                     labelRepulsion, labelPull, maxOverlaps)
    p <- p + eval(as.name(paste0('theme_', theme)))()
    p <- p + theme(legend.position=legendPos,
                   legend.text=element_text(size=legendTextSize),
                   legend.title=element_text(size=legendTitleSize),
                   axis.text=element_text(size=axisTextSize),
                   axis.title=element_text(size=axisTitleSize))
    p <- centerTitle(p, title)
    return(p)
}


