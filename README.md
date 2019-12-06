# metaplot

R package for creating rank probability plots and SUCRA plots.

- Extract rank probabilities from model objects into simple matrices
- Plot rank probabilities or SUCRA values

### Installation

For users of Windoze, first install [rtools](https://cran.r-project.org/bin/windows/Rtools/) suited for
your R version. Also install the `remotes` package (ony once). Next, in R type:

```r
remotes::install_github("markvanderloo/metaplot", subdir="pkg")
```


### Example

```r
library(pcnetmeta)
library(metaplot)
nma <- pcnetmeta::nma.ab(...)

sucra_values(nma)
sucra_plot(nma)
rank_probabilities(nma)
rank_probability_plot(nma)
```

### Supported models and R objects

Currenlty the package can extract and plot objects of the following type:

- `nma.ab` From the [pcnetmeta](https://cran.r-project.org/package=pcnetmeta) package.


### Note

This package is experimental and interfaces, function names and so on may change. You know the drill.





