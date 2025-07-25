% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hull_plot.R
\name{splitHull}
\alias{splitHull}
\title{Split the convex hull in four parts along two input lines}
\usage{
splitHull(
  p,
  pointsDF,
  hullSegments,
  xInt = NULL,
  yInt = NULL,
  borderColor = "navy",
  legendLabs = paste0("Group ", seq(4)),
  alpha = 0.5
)
}
\arguments{
\item{p}{A ggplot object representing the hull.}

\item{pointsDF}{A data frame with the x and y coordinates of the points.}

\item{hullSegments}{Data frame of segments that define the convex hull.}

\item{xInt}{The coordinate where the vertical line intersects the x axis.}

\item{yInt}{The coordinate where the horizontal line intersects the y axis.}

\item{borderColor}{The color of the horizontal and vertical dividing lines,
if provided. If \code{NULL}, no dividing lines will be drawn, though the
hull will still be split along these lines (if \code{xInt}
and/or \code{yInt}are not \code{NULL}).}

\item{legendLabs}{Legend labels.}

\item{alpha}{Opaqueness level.}
}
\value{
A ggplot object showing the hull split in four parts along the input
axes.
}
\description{
This function splits the convex hull in two parts along a vertical and a
horizontal line.
}
\keyword{internal}
