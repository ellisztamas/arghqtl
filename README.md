# arghqtl
Tools for plotting and downstream analysis of R/QTL objects in R.
It is intended in particular for cases where there are many QTL models to compare, and one's first thought about how to plot them is 'AAAAARGH!'.

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
A vignette is provided covering basic usage. In principle this can be viewed with `vignette("arghqtl")`. However, this can be difficult to read, so I have also uploaded the [PDF version](https://github.com/ellisztamas/arghqtl/blob/master/vignettes/Using-arghqtl.pdf) directly.

## Citing `arghqtl`
Text reference:

> Ellis TJ (2018). arghqtl: Tools for plotting and downstream analysis of multiple R/QTL objects in R, available from https://github.com/ellisztamas/arghqtl

Bibtex:

```
@Manual{ellis2018,
	title = {arghqtl: Tools for plotting and downstream analysis of multiple {R/QTL} objects in R},
    author = {Thomas Ellis},
    year = {2018},
    url = {https://github.com/ellisztamas/arghqtl},
}
```

## Authors and license information

Tom Ellis (thomas.ellis@gmi.oeaw.ac.at)

Available under the MIT license. See LICENSE for more information.
