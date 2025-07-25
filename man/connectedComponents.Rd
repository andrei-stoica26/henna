% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geometry.R
\name{connectedComponents}
\alias{connectedComponents}
\title{Find the connected components of a graph data frame}
\usage{
connectedComponents(df, colName = "component")
}
\arguments{
\item{df}{A data frame with two categorical columns.}

\item{colName}{Name of the connected components column to be added.}
}
\value{
A data frame with a column indicated the number of the
connected component.
}
\description{
This function finds the connected components of a graph data frame
}
\examples{
df <- data.frame(
gene1 = paste('G', c(1, 2, 6, 7, 8, 9,
11, 25, 32, 17, 18)),
gene2 = paste('G', c(2, 8, 8, 8, 1, 25,
32, 24, 24, 26, 26))
)
connectedComponents(df)

}
