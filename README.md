# arghqtl
Tools for plotting and downstream analysis of R/QTL objects in R.
It is intended in particular for cases where there are many QTL models to compare, and one's first though about how to plot them is 'aaaaaargh!'.

Additional tools are provided for grouping QTL if their credible intervals overlap.

## Installation
Using devtools from within R:

```
install.packages('devtools')
devtools::install_github("ellisztamas/arghqtl", build=TRUE)
```
### Dependencies
All input data is output data from functions in `qtl`. In addition, `rmarkdown` and `knitr` may be needed to knit the vignette.

## Tutorial
A vignette is provided covering basic usage. In principle this can be viewed with `vignette("arghqtl")`. However, this can be difficult to read, so I have also uploaded the (PDF version)[https://github.com/ellisztamas/arghqtl/blob/master/vignettes/Using-arghqtl.pdf] directly.

## Authors and license information

Tom Ellis (thomas[dot]ellis[at]ebc[dot]uu[dot]se)

Available under the MIT license. See LICENSE for more information.
