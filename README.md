# henna
`henna` provides nine versatile visualization utilities. They are designed with 
scRNA-seq data analysis in mind, but can be used for a variety of other 
applications as well.

## Installation

To install `henna`, run the following R code:

```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
    
BiocManager::install("andrei-stoica26/henna")
```
## Visualization tools

- `classPlot`: Create a bar plot for a set of items grouped by class and orders
the items and classes.

![](man/figures/class_plot.png)

- `correlationPlot`: Plot a correlation matrix.

![](man/figures/correlation_plot.png)

- `densityPlot`: Create a density plot for a set of points, optionally displaying
segements between each point and its nearest neighbor.

![](man/figures/density_plot.png)

- `hullPlot`: Plot the convex hull of a set of points. Optionally, divide the
points into 2 or 4 classes of different colors by drawing a vertical and/or
a horizontal line that intersects the hull.

![](man/figures/hull_plot.png)

- `networkPlot`: Plot a graph using different colors for nodes that are part of 
different connected components.

![](man/figures/network_plot.png)

- `radialPlot`: Plot integer-valued points over concentric circles, with points 
located more centrally representing higher values.

![](man/figures/radial_plot.png)

- `rankPlot`: Aggregate multiple ranks available for a group of items in a single
plot.

![](man/figures/rank_plot.png)

- `riverPlot`: Generate an alluvial plot for a data frame with two categorical 
columns and a weights column.

![](man/figures/river_plot.png)

- `tilePlot`: Plot a numeric matrix.

![](man/figures/tile_plot.png)
