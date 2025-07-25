# henna
`henna` is an evolving collection of visualization utilities for scRNA-seq data analysis.

## Installation

To install `henna`, run the following R code:

```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
    
BiocManager::install("andrei-stoica26/henna")
```
## Functions implemented so far

`networkPlot`: Plot a graph using different colors for nodes that are part of 
different connected components.

`radialPlot`: Plot integer-valued points over concentric circles, with points 
located more centrally representing higher values.

`rankPlot`: Aggregate multiple ranks available for a group of items in a single
plot.

`riverPlot`: Generate an alluvial plot for a data frame with two categorical 
columns and a weights column. 
Uses [ggalluvial](https://cran.r-project.org/web/packages/ggalluvial/index.html).

