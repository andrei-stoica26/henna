# hammers
`henna` is an evolving collection of visualization utilities for scRNA-seq data analysis.

## Installation

To install `henna`, run the following R code:

```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
    
BiocManager::install("andrei-stoica26/henna")
```
## Functions implemented so far

`riverPlot`: Generates an alluvial plot for a data frame with two categorical 
columns and a weights column. 
Uses [ggalluvial](https://cran.r-project.org/web/packages/ggalluvial/index.html).

