#' Find the connected components of a graph data frame
#'
#' This function finds the connected components of a graph data frame
#'
#' @param df A data frame with two categorical columns.
#' @param colName Name of the connected components column to be added.
#' @return A data frame with a column indicated the number of the
#' connected component.
#'
#' @examples
#' df <- data.frame(
#' gene1 = paste('G', c(1, 2, 6, 7, 8, 9,
#' 11, 25, 32, 17, 18)),
#' gene2 = paste('G', c(2, 8, 8, 8, 1, 25,
#' 32, 24, 24, 26, 26))
#' )
#' connectedComponents(df)
#'
#' @export
#'
connectedComponents <- function(df, colName = 'component'){
    if(!nrow(df))
        stop('The dataframe has no rows.')
    df[[colName]] <- -1
    rownames(df) <- seq(dim(df)[1])
    vertices <- unique(c(df[, 1], df[, 2]))
    seen <- c()
    nextComp <- 1
    for (v in vertices){
        if (v %in% seen)
            next
        currVertices <- c(v)
        while (length(currVertices)){
            v <- currVertices[1]
            leftdf <- subset(df, df[, 1] == v)
            rightdf <- subset(df, df[, 2] == v)
            seen <- c(seen, v)
            newEdges <- as.integer(c(rownames(leftdf), rownames(rightdf)))
            df[newEdges, colName] <- nextComp
            neighbors <- setdiff(c(leftdf[, 2], rightdf[, 1]),
                                 c(currVertices, seen))
            currVertices <- c(currVertices, neighbors)
            currVertices <- currVertices[-1]
        }
        nextComp <- nextComp + 1
    }
    df[, colName] <- factor(df[, colName])
    return(df)
}

#' Construct the convex hull of a set of points
#'
#' This function constructs the convex hull of a set points.
#'
#' @details The points must be provided as a data frame with two columns.
#'
#' @param pointsDF A data frame with the x and y coordinates of the points.
#'
#' @return The points on the convex hull of the original set of points.
#'
#' @examples
#' pointsDF <- data.frame(a = c(1, 2, 2, 3, 3, 4, 5, 6, 8, 6, 7, 8, 6, 8, 10, 3, 1),
#' b = c(2, 3, 4, 8, 5, 6, 5, 4, 8, 11, 13, 14, 2, 1, 2, 14, 9))
#' hull <- convexHull(pointsDF)
#'
#' @export
#'
convexHull <- function(pointsDF){
    hull <- pointsDF[chull(pointsDF[, 1], pointsDF[, 2]), c(1, 2)]
    colnames(hull) <- c('x', 'y')
    return(hull)
}

#' Construct a data frame of segments from a data frame of points
#'
#' This function constructs a data frame of segments from a data frame of
#' points.
#'
#' @param pointsDF A data frame with the x and y coordinates of the points.
#' Each point must appear only once.
#' @param joinEnds Whether to join the last point with the first one.
#'
#' @return A data frame of segments.
#'
#' @noRd
#'
pointsToSegments <- function(pointsDF,
                             joinEnds = TRUE){
    df <- data.frame(x = pointsDF[seq_len(nrow(pointsDF) - 1), 1],
                     y = pointsDF[seq_len(nrow(pointsDF) - 1), 2],
                     xEnd = pointsDF[seq(2, nrow(pointsDF)), 1],
                     yEnd = pointsDF[seq(2, nrow(pointsDF)), 2])
    if(joinEnds)
        df <- rbind(df, c(df$xEnd[nrow(df)],
                          df$yEnd[nrow(df)],
                          df$x[1],
                          df$y[1]))
    return(df)
}
