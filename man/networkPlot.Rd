% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network_plot.R
\name{networkPlot}
\alias{networkPlot}
\title{Plot graph with different colors for connected components}
\usage{
networkPlot(
  df,
  title = "Network plot",
  rankCol = "rank",
  edgeScale = 2,
  nodePointSize = 10,
  nodeTextSize = 2.3,
  palette = ("Pastel 1"),
  ...
)
}
\arguments{
\item{df}{Data frame.}

\item{title}{Plot title.}

\item{rankCol}{Name of the rank column.}

\item{edgeScale}{Scaling factor used in generating
edge weights.}

\item{nodePointSize}{Point size of graph nodes.}

\item{nodeTextSize}{Text size of graph nodes.}

\item{palette}{grDevices palette}

\item{...}{Additional parameters passed to titlePlot.}
}
\value{
A network plot.
}
\description{
This function plots the graph of the data frame, marking nodes corresponding
to connected components with different colors.
}
\examples{
df <- data.frame(gene1 = paste0('G', c(1, 2, 5, 6, 7, 17)),
gene2 = paste0('G', c(2, 5, 8, 11, 11, 11)),
rank = c(1, 1, 3, 3, 3, 3))
networkPlot(df)

}
