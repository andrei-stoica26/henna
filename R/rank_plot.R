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
#' @examples
#' df <- do.call(cbind, lapply(seq(30), function(i) sample(10, 10)))
#' rownames(df) <- paste0('M', seq(10))
#' colnames(df) <- paste0('R', seq(30))
#' rankSummary(df)
#'
#' @export
#'
#'
rankSummary <- function(df){
    minPos <- min(df)
    maxPos <- max(df)
    positions <- sort(unique(as.numeric(df)))
    posDF <- do.call(rbind, lapply(positions, function(pos)
        apply(df, 1, function(x) sum(x == pos))))
    rownames(posDF) <- positions
    smr <- reshape2::melt(posDF)
    colnames(smr) <- c('Rank', 'Item', 'Count')
    smr <- smr[order(smr$Rank, -smr$Count), ]
    smr$Rank <- factor(smr$Rank)
    return(smr)
}

#' Compute the average rank of each iterm
#'
#' This function computes the average rank of each item.
#'
#' @param rankDF Rank data frame created with \code{rankSummary}.
#'
#' @return A single-column data frame of average ranks.
#'
#' @noRd
#'
computeMeanRanks <- function(rankDF){
    nMetrics <- sum(rankDF[rankDF[, 1] == unique(rankDF[, 1])[1], 3])
    meanRanks <- sort(vapply(as.character(unique(rankDF[, 2])), function(x){
        subRankDF <- rankDF[rankDF[, 2] == x, ]
        return(round(sum(as.numeric(subRankDF[, 1]) * subRankDF[, 3]) /
                   nMetrics, 2))},
        numeric(1)))
    return(data.frame(Item = names(meanRanks), MeanRank = meanRanks))
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
#' @param labelSize Size of label marking average rank for each item.
#' @param labelColor Color of label marking average rank for each item.
#'
#' @return A ggplot object.
#'
#' @examples
#' df <- do.call(cbind, lapply(seq(30), function(i) sample(10, 10)))
#' rownames(df) <- paste0('M', seq(10))
#' colnames(df) <- paste0('R', seq(30))
#' rankPlot(df)
#'
#' @export
#'
rankPlot <- function(df,
                     title = 'Rank plot',
                     summarize = TRUE,
                     viridisPal = 'turbo',
                     xLab = 'Item',
                     labelSize = 3.1,
                     labelColor = 'snow',
                     ...){


    if(summarize)
        df <- rankSummary(df)

    meanRanks <- computeMeanRanks(df)
    itemOrder <- rownames(meanRanks)
    df[, 2] <- factor(df[, 2], levels=itemOrder)

    p <- ggplot() +
        geom_bar(aes(x=Item, y=Count, fill=Rank), df, stat='identity') +
        theme_classic() +
        scale_fill_viridis_d(option=viridisPal) + xlab(xLab) +
        geom_text(data=meanRanks, aes(x=Item, y=MeanRank, label=MeanRank),
                   size=labelSize, color=labelColor)
    p <- centerTitle(p, title, ...)
    return(p)
}
