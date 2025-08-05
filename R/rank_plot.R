#'@importFrom reshape2 melt
#'
NULL

#' Create a rank summary
#'
#' This function creates a summary of multiple ranks provided for input items.
#'
#' @param df A data frame with ranks as columns, items as rows.
#'
#' @return A rank summary dataframe.
#'
#' @export
#'
#'
rankSummary <- function(df){
    minPos <- min(df)
    maxPos <- max(df)
    positions <- seq(minPos, maxPos)
    posDF <- do.call(rbind, lapply(seq(minPos, maxPos), function(pos)
        apply(df, 1, function(x) sum(x == pos))))
    rownames(posDF) <- positions
    smr <- reshape2::melt(posDF)
    colnames(smr) <- c('Rank', 'Item', 'Count')
    smr <- smr[order(smr$Rank, -smr$Count), ]

    smr$Rank <- factor(smr$Rank)

    ranks <- sort(apply(df, 1, mean))
    smr$Item <- factor(smr$Item, levels=names(ranks))

    return(smr)
}

#' Create a rank plot
#'
#' This function creates a rank plot
#'
#' @param df A data frame with ranks as columns and items as rows, or a
#' summary data frame generated with \code{rankSummary}. If the latter,
#' \code{summarize} must be set to \code{FALSE}.
#' @inheritParams riverPlot
#' @param summarize Whether to summarize the ranks with \code{rankSummary}.
#' Must be set to \code{FALSE} if the input data frame has been generated with
#' \code{rankSummary}.
#' @param xLab Label of x axis.
#'
#' @return A ggplot object.
#'
#' @examples
#' df <- do.call(cbind, lapply(seq(30), function(i) sample(10, 10)))
#' rownames(df) <- paste('M', seq(10))
#' colnames(df) <- paste('R', seq(30))
#' rankPlot(df)
#'
#' @export
#'
rankPlot <- function(df, title = 'Rank plot', summarize = TRUE,
                     viridisPal = 'turbo',
                     xLab = 'Item', ...){
    if(summarize)
        df <- rankSummary(df)
    p <- ggplot(df, aes(x=Item, y=Count, fill=Rank)) +
        geom_bar(stat='identity') + theme_classic() +
        scale_fill_viridis_d(option=viridisPal) + xlab(xLab)
    p <- centerTitle(p, title, ...)
    return(p)
}
