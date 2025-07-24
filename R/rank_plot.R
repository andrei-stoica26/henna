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
#' @keywords internal
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
#' @inheritParams rankSummary
#' @inheritParams riverPlot
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
rankPlot <- function(df, title = 'Rank plot',
                     viridisPal = 'turbo',
                     xLab = 'Item', ...){
    smr <- rankSummary(df)
    p <- ggplot(smr, aes(x=Item, y=Count, fill=Rank)) +
        geom_bar(stat='identity') + theme_classic() +
        scale_fill_viridis_d(option=viridisPal) + xlab(xLab)
    p <- centerTitle(p, title, ...)
    return(p)
}
