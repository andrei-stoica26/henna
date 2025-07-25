#' Find the coordinates where vertical or horizontal line intersects the hull
#'
#' This function finds the coordinates where vertical or horizontal line
#' intersects the hull.
#'
#'
#' @param df A four-column data frame representing segments.
#' @param axis An integer representing the axis intersected by the vertical or
#' horizontal line, x (1) or y (2).
#' @param axisIntersect The coordinate where the vertical or horizontal line
#' intersects the relevant axis.
#'
#' @return A vector of size two representing the coordinates of the two
#' intersection points between the vertical or horizontal line and the convex
#' hull on the axis different from the input axis.
#'
#' @keywords internal
#'
borderCoords <- function(df, axis, axisIntersect){

    if(is.null(axisIntersect))
        return(c('None', 'None'))

    otherAxis <- axis %% 2 + 1
    axisEnd <- axis + 2
    otherAxisEnd <- otherAxis + 2
    axisVals <- df[, axis]
    axisEndVals <- df[, axisEnd]

    coords <- c(df[axisVals == axisIntersect, ][, otherAxis])

    if (length(coords) < 2){
        df <- df[axisVals < axisIntersect & axisEndVals > axisIntersect |
                     axisVals > axisIntersect & axisEndVals < axisIntersect, ]

        axisVals <- df[, axis]
        axisEndVals <- df[, axisEnd]
        otherAxisVals <- df[, otherAxis]
        otherAxisEndVals <- df[, otherAxisEnd]

        df$diffRatio <- (otherAxisEndVals - otherAxisVals) /
            (axisEndVals  - axisVals)
        df$newCoord <- df$diffRatio * (axisIntersect - axisVals) +
            otherAxisVals

        coords <- c(coords, df$newCoord)
    }

    return(sort(coords))
}

#' Split the convex hull in two along an input line
#'
#' This function splits the convex hull in two along an input vertical or
#' horizontal line.
#'
#'
#' @param p A ggplot object representing the hull.
#' @inheritParams convexHull
#' @inheritParams borderCoords
#' @param pointCoords The coordinates of the input points on the axis
#' perpendicular to the input border line.
#' @param borderPoints The points where the border line intersects the
#' convex hull.
#' @param legendLabs Legend labels.
#' @param alpha Opaqueness level.
#'
#' @return A ggplot object showing the hull split in two parts along the input
#' line.
#'
#' @keywords internal
#'
splitInTwo <- function(p,
                       pointsDF,
                       axisIntersect,
                       pointCoords,
                       borderPoints,
                       legendLabs = paste0('Group ', seq(2)),
                       alpha = 0.5){
    df1 <- rbind(pointsDF[pointCoords < axisIntersect, ], borderPoints)
    df2 <- rbind(pointsDF[pointCoords > axisIntersect, ], borderPoints)

    hullSegments1 <- pointsToSegments(convexHull(df1))
    hullSegments2 <- pointsToSegments(convexHull(df2))

    p <- p + geom_polygon(data=hullSegments1, aes(x, y, fill=legendLabs[1]),
                          alpha=alpha)
    p <- p + geom_polygon(data=hullSegments2, aes(x, y, fill=legendLabs[2]),
                          alpha=alpha)
    return(p)
}

#' Find the coordinates of the points establishing the four divisions of
#' the hull
#'
#' This function finds the coordinates of the points establishing the
#' four divisions of the hull
#'
#' @inheritParams convexHull
#' @param xInt The coordinate where the vertical line intersects the x axis.
#' @param yInt The coordinate where the horizontal line intersects the y axis.
#' @param vCoords The y coordinates of the two points where the vertical line
#' intersects the convex hull.
#' @param hCoords The x coordinates of the two points where the horizontal line
#' intersects the convex hull.
#'
#' @return A data frame with 2 columns representing the 12 points (not unique)
#' determining the boundaries of the hull divisions.
#'
#' @keywords internal
#'
quadBorders <- function(pointsDF, xInt, yInt, vCoords, hCoords){
    a <- c(xInt, hCoords[1], xInt,
           xInt, hCoords[2], xInt,
           xInt, hCoords[1], xInt,
           xInt, hCoords[2], xInt)
    b <- c(yInt, yInt, vCoords[1],
           yInt, yInt, vCoords[2],
           yInt, yInt, vCoords[2],
           yInt, yInt, vCoords[1])
    df <- data.frame(a, b)
    colnames(df) <- colnames(pointsDF)
    return(df)
}

#' Split the convex hull in four parts along two input lines
#'
#' This function splits the convex hull in two parts along a vertical and a
#' horizontal line.
#'
#'
#' @param p A ggplot object representing the hull.
#' @inheritParams splitInTwo
#' @inheritParams quadBorders
#' @param borderPoints The points where the border lines intersects the
#' convex hull.
#'
#' @return A ggplot object showing the hull split in four parts along the input
#' axes.
#'
#' @noRd
#'
splitInFour <- function(p,
                        pointsDF,
                        xInt,
                        yInt,
                        borderPoints,
                        legendLabs = paste0('Group ', seq(4)),
                        alpha = 0.5){
    df1 <- rbind(pointsDF[pointsDF[, 1] < xInt & pointsDF[, 2] < yInt, ],
                 borderPoints[c(1, 2, 3), ])
    df2 <- rbind(pointsDF[pointsDF[, 1] > xInt & pointsDF[, 2] > yInt, ],
                 borderPoints[c(4, 5, 6), ])
    df3 <- rbind(pointsDF[pointsDF[, 1] < xInt & pointsDF[, 2] > yInt, ],
                 borderPoints[c(7, 8, 9), ])
    df4 <- rbind(pointsDF[pointsDF[, 1] > xInt & pointsDF[, 2] < yInt, ],
                 borderPoints[c(10, 11, 12), ])

    hullSegments1 <- pointsToSegments(convexHull(df1))
    hullSegments2 <- pointsToSegments(convexHull(df2))
    hullSegments3 <- pointsToSegments(convexHull(df3))
    hullSegments4 <- pointsToSegments(convexHull(df4))

    p <- p + geom_polygon(data=hullSegments1, aes(x, y, fill=legendLabs[1]),
                          alpha=alpha)
    p <- p + geom_polygon(data=hullSegments2, aes(x, y, fill=legendLabs[2]),
                          alpha=alpha)
    p <- p + geom_polygon(data=hullSegments3, aes(x, y, fill=legendLabs[3]),
                          alpha=alpha)
    p <- p + geom_polygon(data=hullSegments4, aes(x, y, fill=legendLabs[4]),
                          alpha=alpha)
    return(p)
}


#' Split the convex hull in four parts along two input lines
#'
#' This function splits the convex hull in two parts along a vertical and a
#' horizontal line.
#'
#'
#' @inheritParams splitInTwo
#' @param hullSegments Data frame of segments that define the convex hull.
#' @inheritParams quadBorders
#' @param borderColor The color of the horizontal and vertical dividing lines,
#' if provided. If \code{NULL}, no dividing lines will be drawn, though the
#' hull will still be split along these lines (if \code{xInt}
#' and/or \code{yInt}are not \code{NULL}).
#'
#' @return A ggplot object showing the hull split in four parts along the input
#' axes.
#'
#' @keywords internal
#'
splitHull <- function(p,
                      pointsDF,
                      hullSegments,
                      xInt = NULL,
                      yInt = NULL,
                      borderColor = 'navy',
                      legendLabs = paste0('Group ', seq(4)),
                      alpha = 0.5){

    vCoords <- borderCoords(hullSegments, 1, xInt)
    hCoords <- borderCoords(hullSegments, 2, yInt)

    if(!is.null(borderColor)){
        if (!is.null(xInt))
            p <- p + geom_segment(aes(x=xInt, y=vCoords[1],
                                      xend=xInt, yend=vCoords[2]),
                                  color='navy', linewidth=0.3,
                                  linetype='dashed')
        if (!is.null(yInt))
            p <- p + geom_segment(aes(x=hCoords[1], y=yInt,
                                      xend=hCoords[2], yend=yInt),
                                  color='navy', linewidth=0.3,
                                  linetype='dashed')

    }

    if(is.null(xInt) & is.null(yInt))
        return(p + geom_polygon(data=hullSegments, aes(x, y, fill=legendLabs[1]),
                                alpha=alpha))
    if(is.null(xInt))
        return(splitInTwo(p, pointsDF, yInt, pointsDF[, 2],
                          list(hCoords, c(yInt, yInt)),
                          legendLabs))

    if(is.null(yInt))
        return(splitInTwo(p, pointsDF, xInt, pointsDF[, 1],
                          list(c(xInt, xInt), vCoords),
                          legendLabs, alpha))


    return(splitInFour(p, pointsDF, xInt, yInt,
                       quadBorders(pointsDF, xInt, yInt, vCoords, hCoords),
                       legendLabs, alpha))
}

#' Plot the convex hull of a set of points
#'
#' This function plots the convex hull of a set of points and optionally draws
#' a vertical and a horizontal line, dividing the hull into areas of different
#' colors.
#'
#' @inheritParams convexHull
#' @inheritParams riverPlot
#' @inheritParams splitHull
#' @param palette Color palette.
#' @param showHull Whether to display the segments on the convex hull.
#' @param xLab x axis label.
#' @param yLab y axis label.
#' @param pointShape Point shape.
#' @inheritParams labelPoints
#' @param labelSize Label size. Ignored if \code{labelDF} is \code{NULL}.
#' @param labelColor Label color. Ignored if \code{labelDF} is \code{NULL}.
#' @param maxOverlaps Maximum overlaps. Ignored if \code{labelDF}
#' is \code{NULL}.
#'
#' @return A ggplot object.
#'
#' @examples
#' pointsDF <- data.frame(rank = c(1, 2, 4, 7, 10,
#' 12, 13, 15, 16),
#' n = c(1, 1, 2, 3, 3, 2,
#' 1, 2, 1))
#' hullPlot(pointsDF, 8.5)
#'
#' @export
#'
hullPlot <- function(pointsDF,
                     title = 'Hull plot',
                     xInt = NULL,
                     yInt = NULL,
                     borderColor = 'navy',
                     palette = hpColors(),
                     showHull = FALSE,
                     xLab = 'x',
                     yLab = 'y',
                     legendLabs = paste0('Group ', seq(4)),
                     legendPos = 'bottom',
                     pointShape = 20,
                     alpha = 0.5,
                     labelDF = NULL,
                     labelSize = 2,
                     labelColor = 'black',
                     maxOverlaps = 10,
                     ...){

    if (nrow(pointsDF) < 2)
        stop('The hull plot requires at least two points.')

    hull <- convexHull(pointsDF)
    hullSegments <- pointsToSegments(hull)

    p <- ggplot() + theme_classic() +
        labs(x=xLab, y=yLab) +
        theme(legend.title=element_blank(),
              legend.position=legendPos)


    p <- splitHull(p, pointsDF, hullSegments, xInt, yInt, borderColor,
                   legendLabs, alpha)
    p <- p + scale_fill_manual(values=palette, labels=legendLabs)

    p <- p + geom_point(data=pointsDF, aes(.data[[colnames(pointsDF)[1]]],
                                      .data[[colnames(pointsDF)[2]]]),
                   size=1, shape=pointShape)

    if(showHull)
        p <- p + geom_segment(data=hullSegments,
                              aes(x, y, xend=xEnd, yend=yEnd),
                              linewidth=0.8)

    if(!is.null(labelDF))
        p <- labelPoints(p, labelDF, labelSize, labelColor, maxOverlaps)

    p <- centerTitle(p, title, ...)
    return(p)
}
