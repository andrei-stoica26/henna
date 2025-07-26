#' Create the default \code{hullPlot} palette
#'
#' This function returns the default palette used by \code{hullPlot}.
#'
#' @return A character vector of colors.
#'
#' @export
#'
hpColors <- function()
    return(c('gold','purple', 'blue', 'red'))

#' Create a palette designed to represent dots over a \code{viridis} background
#'
#' This function returns a 10-color palette used as the default
#' of \code{radialPlot}.
#'
#' @param nColors Number of colors
#'
#' @return A character vector of colors.
#'
#' @export
#'
rpColors <- function(nColors = 10){
    colors <- c('red', 'purple1', 'olivedrab1','darkorange1',
                'lavender', 'thistle1','green1','violetred4',
                'goldenrod1', 'firebrick4')
    return(colors[seq(nColors)])
}

