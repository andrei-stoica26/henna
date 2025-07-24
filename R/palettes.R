#' Create a palette designed to represent dots over a \code{viridis} background.
#'
#' This function returns a 10-color palette used as the default
#' of \code{radialPlot}.
#'
#' @param nColors Number of colors
#'
#' @return Number of colors
#'
#' @export
#'
rpColors <- function(nColors = 10){
    colors <- c('red', 'purple1', 'olivedrab1','darkorange1',
                'lavender', 'thistle1','green1','violetred4',
                'goldenrod1', 'firebrick4')
    return(colors[seq(nColors)])
}

