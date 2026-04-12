#' Sort a data frame by the first column and convert the second to a factor
#'
#' This function sort a data frame by the first column and convert the second
#' to a factor.
#'
#' @param df A data frame.
#'
#' @return An object of class \code{gg}.
#'
#' @examples
#' df <- data.frame(a = c(2, 4, 1, 3, 6),
#' b = c(2, 8, 3, 19, 3))
#' reorderDF(df)
#'
#' @export
#'
reorderDF <- function(df){
    df <- df[order(df[, 2], decreasing=TRUE), ]
    df <- df[order(df[, 1]), ]
    df[, 2] <- factor(df[, 2], levels=unique(df[, 2]))
    return(df)
}

#' Plot a numeric matrix or data frame
#'
#' This function plots a numeric matrix or data frame.
#'
#' @inheritParams documentFun
#' @param mat A numeric matrix or data frame.
#' @param doMelt Whether the input needs to be melted using
#' \code{reshape2::melt}.
#' @param showNumbers Whether to show a numeric value for each matrix element.
#' @param sigDigits Number of significant digits to be displayed for each
#' matrix element.
#' @param limits Limits of the color scale. If \code{NULL}, they will be taken
#' as the minimum and maximum of the values in the input object.
#' @param tileBoundaryColor Tile boundary color.
#' @param tileBoundaryWidth Tile boundary width.
#' @param reverseColors Whether to reverse the order of colors in the palette.
#'
#' @return An object of class \code{gg}.
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
                     title = NULL,
                     xLab = NULL,
                     yLab = NULL,
                     doMelt = TRUE,
                     legendTitle = 'Value',
                     palette = 'Spectral',
                     reverseColors = TRUE,
                     showNumbers = TRUE,
                     sigDigits = 2,
                     limits = NULL,
                     labelSize = 3,
                     labelColor = 'black',
                     legendTextSize = 10,
                     legendTitleSize = 10,
                     axisTextSize = 12,
                     axisTitleSize = 12,
                     tileBoundaryColor = 'white',
                     tileBoundaryWidth = 0.2,
                     xAngle = 45,
                     vJust = 1,
                     hJust = 1,
                     ...){

    if(doMelt){
        if (is(mat)[1] != 'matrix')
            mat <- as.matrix(mat)

        mat <- round(mat, sigDigits)
        df <- reshape2::melt(mat)
    } else
        df <- mat

    palColors <- hcl.colors(50, palette)
    if (reverseColors)
         palColors <- rev(palColors)

    if(is.null(limits))
        limits <- c(min(df[, 3]), max(df[, 3]))

    df <- reorderDF(df)

    p <- ggplot(df, aes(x=.data[[names(df)[1]]],
                        y=.data[[names(df)[2]]],
                        fill=.data[[names(df)[3]]])) +
        geom_tile(color=tileBoundaryColor, lwd=tileBoundaryWidth) +
        theme_classic() +
        theme(legend.text=element_text(size=legendTextSize),
              legend.title=element_text(size=legendTitleSize),
              axis.text.x=element_text(angle=xAngle, vjust=vJust, hjust=hJust),
              axis.text=element_text(size=axisTextSize),
              axis.title=element_text(size=axisTitleSize)) +
        scale_fill_gradientn(colors=palColors, limits=limits) +
        labs(x=xLab, y=yLab, fill=legendTitle)

    if (showNumbers)
        p <- p + geom_text(aes(label=.data[[names(df)[3]]]),
                          color=labelColor,
                          size=labelSize)

    p <- centerTitle(p, title, ...)
    return(p)
}

#' Plot a correlation matrix
#'
#' This function plots a correlation matrix.
#'
#' @details A thin wrapper around \code{tilePlot}.
#'
#' @inheritParams tilePlot
#' @param ... Additional parameters passed to tilePlot.
#'
#' @return An object of class \code{gg}.
#'
#' @examples
#' mat <- matrix(runif(100, -1, 1), nrow=10)
#' colnames(mat) <- paste0('I', seq(10))
#' mat <- round(cor(mat), 2)
#' correlationPlot(mat)
#'
#' @export
#'
correlationPlot <- function(mat, title = NULL,
                            legendTitle = 'Correlation', ...)
    return(tilePlot(mat, title, legendTitle=legendTitle, limits=c(-1, 1), ...))
