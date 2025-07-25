#' @importFrom dplyr mutate
#' @importFrom grDevices chull hcl.colors
#' @importFrom ggraph geom_edge_link geom_node_point geom_node_text ggraph scale_edge_width
#' @importFrom tidygraph activate as_tbl_graph
#'
NULL

#' Prepare data frame for network plot
#'
#' This function prepares a data frame for network plot.
#'
#' @param df Data frame.
#' @param rankCol Name of the rank column.
#' @param edgeScale Scaling factor used in generating
#' edge weights.
#'
#' @return A data frame ready to serve as input to
#' networkPlot.
#'
#' @keywords internal
#'
networkPlotDF <- function(df,
                          rankCol = 'rank',
                          edgeScale = 2){
    preWeight <- log(max(df[[rankCol]]) /
                         df[[rankCol]] + 0.01)
    df$weight <- edgeScale * preWeight /
        max(preWeight)
    df <- df[, c('gene1', 'gene2', 'weight')]
    return(df)
}

#' Return the connected components of vertices
#'
#' This function returns the connected components of vertices from a graph
#' data frame in which edges have been assigned connected components.
#'
#' @param df A data frame with a connected components column.
#' @param colName Name of the connected components column.
#'
#' @return A data frame ready to serve as input to
#' networkPlot.
#'
#' @noRd
#'
vertexComponents <- function(df, colName = 'component'){
    vertices <- unique(c(df[, 1], df[, 2]))
    boxes <- lapply(unique(df[, colName]), function(x) {
        compDF <- df[df[, colName] == x, ]
        return(unique(c(compDF[, 1], compDF[, 2])))
    })
    nBoxes <- length(boxes)
    allocations <- unlist(lapply(seq(nBoxes), function(i)
        setNames(rep(i, length(boxes[[i]])),boxes[[i]])))
    vertexComp <- as.factor(allocations[vertices])
    return(vertexComp)
}

#' Plot graph with different colors for connected components
#'
#' This function plots the graph of the data frame, marking nodes corresponding
#' to connected components with different colors.
#'
#' @inheritParams networkPlotDF
#' @inheritParams riverPlot
#' @param nodePointSize Point size of graph nodes.
#' @param nodeTextSize Text size of graph nodes.
#' @param palette grDevices palette
#' @param ... Additional parameters passed to titlePlot.
#'
#' @return A network plot.
#'
#' @examples
#' df <- data.frame(gene1 = paste0('G', c(1, 2, 5, 6, 7, 17)),
#' gene2 = paste0('G', c(2, 5, 8, 11, 11, 11)),
#' rank = c(1, 1, 3, 3, 3, 3))
#' networkPlot(df)
#'
#' @export
#'
networkPlot <- function(df, title = 'Network plot',
                        rankCol = 'rank',
                        edgeScale = 2, nodePointSize = 10, nodeTextSize = 2.3,
                        palette = ('Pastel 1'), ...){
    df <- networkPlotDF(df, rankCol, edgeScale)
    tblGraph <- as_tbl_graph(df, directed=FALSE)
    df <- connectedComponents(df)
    vertexComp <- vertexComponents(df)
    nComp <- length(unique(vertexComp))
    tblGraph <- mutate(activate(tblGraph, "vertices"),"component" = vertexComp)
    p <- ggraph(tblGraph, layout="nicely") +
        geom_edge_link(aes(width=weight)) +
        scale_edge_width(range=c(0.1, 0.3)) +
        geom_node_point(aes(color=component), size=nodePointSize) +
        geom_node_text(aes(label=name), size=nodeTextSize) +
        theme_void() +
        theme(legend.position='none') +
        scale_color_manual(values = hcl.colors(nComp, palette))
    p <- centerTitle(p, title, ...)
    return(p)
}
