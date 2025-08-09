#' @importFrom wesanderson wes_palette
#'
NULL

#' Plot a numeric matrix or data frame
#'
#' This function plots a numeric matrix or data frame.
#'
#' @param mat A numeric matrix or data frame.
#' @inheritParams classPlot
#' @param isCor Whether the matrix is a correlation matrix, in which case the
#' limits of the color scale will be set to [-1, 1].
#' @param tileBoundaryColor Tile boundary color.
#' @param tileBoundaryWidth Tile boundary width.
#' @param wesPalette wesanderson palette to be used for the color scale.
#'
#' @return A tile plot.
#'
#' @examples
#' mat <- matrix(round(runif(100, 0, 1), 2), nrow=10)
#' rownames(mat) <- paste0('R', seq(10))
#' colnames(mat) <- paste0('C', seq(10))
#' tilePlot(mat)
#'
#' @export
#'
tilePlot <- function(mat,
                     title = 'Tile plot',
                     xLab = 'x',
                     yLab = 'y',
                     legendLab = 'Value',
                     isCor = FALSE,
                     labelSize = 3,
                     labelColor = 'black',
                     tileBoundaryColor = 'white',
                     tileBoundaryWidth = 0.2,
                     wesPalette = 'Zissou1'){
    if (is(mat)[1] != 'matrix')
        mat <- as.matrix(mat)

    df <- reshape2::melt(mat)

    if(isCor){
        limits <- c(-1, 1)
        df <- df[order(df[, 2], decreasing=TRUE), ]
        df <- df[order(df[, 1]), ]
        df[, 2] <- factor(df[, 2], levels=unique(df[, 2]))
    }else
        limits <- c(min(df[, 3]), max(df[, 3]))

    p <- ggplot(df, aes(x=df[, 1], y=df[, 2], fill=df[, 3])) +
        geom_tile(color=tileBoundaryColor, lwd=tileBoundaryWidth) +
        theme_classic() +
        geom_text(aes(label=df[, 3]), color=labelColor, size=labelSize) +
        scale_fill_gradientn(colors=wes_palette(wesPalette, 50,
                                             type='continuous'),
                             limits=limits) +
        labs(x=xLab, y=yLab, fill=legendLab)

    p <- centerTitle(p, title)
    return(p)
}

#' Plot a correlation matrix
#'
#' This function plots a correlation matrix.
#'
#' @details A thin wrapper around \code{tilePlot}.
#'
#' @inheritParams tilePlot
#' @param ... Additional parameters passed to tilePlot
#'
#' @return A tile plot.
#'
#' @examples
#' mat <- matrix(runif(100, -1, 1), nrow=10)
#' colnames(mat) <- paste0('I', seq(10))
#' mat <- round(cor(mat), 2)
#' correlationPlot(mat)
#'
#' @export
#'
correlationPlot <- function(mat, title='Correlation plot', ...)
    return(tilePlot(mat, title, isCor=TRUE, ...))
