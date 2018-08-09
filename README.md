# arghqtl
Tools for plotting and downstream analysis of R/QTL objects in R.
It is intended in particular for cases where there are many QTL models to compare, and one's first though about how to plot them is 'aaaaaargh!'.

Additional tools are provided for grouping QTL if their credible intervals overlap.

## Installation
Using devtools from within R:

```
install.packages('devtools')
devtools::install_github("ellisztamas/arghqtl")
```

### Dependencies
All input data is output data from functions in `qtl`. In addition, `rmarkdown` and `knitr` may be needed to knit the vignette.

## Tutorial
A vignette is provided covering basic usage. I was not able to figure out how to bundle this in the build on the upload to github, so I have instead pushed the [PDF](https://github.com/ellisztamas/arghqtl/blob/master/vignettes/Using%20arghqtl.pdf) directly.

## Authors and license information

Tom Ellis (thomas[dot]ellis[at]ebc[dot]uu[dot]se)

Available under the MIT license. See LICENSE.txt for more information.
