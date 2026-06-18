# drcanalyser

A small R package to analyse cell-viability / dose-response (DRC) assays. It reads
SparkControl plate-reader exports, normalises intensities (IC/GI/GR), fits
dose-response curves with the `drc` package, and produces ggplot figures plus
IC50/GI50/GR50 statistics.

## Installation

Install from GitHub with [`remotes`](https://cran.r-project.org/package=remotes):

```r
# install.packages("remotes")
remotes::install_github("bk1n/drc-analyser")
```

To also build the vignette during installation:

```r
remotes::install_github("bk1n/drc-analyser", build_vignettes = TRUE)
```

## Getting started

The vignette walks through a complete analysis (describe plates → fit curves →
plot → save):

```r
browseVignettes("drcanalyser")
# or
vignette("arid1ako", package = "drcanalyser")
```

See the function help pages for details, e.g. `?process_plates` and `?plot_drc`.
