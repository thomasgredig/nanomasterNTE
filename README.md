# nanomasterNTE

<!-- badges: start -->
<!-- badges: end -->

Load and graph data from the Nanomaster Thermal Evaporator (NTE).

## Installation

You can install the released version of nanomasterNTE from [Github](https://github.com/thomasgredig/nanomasterNTE) with:

``` r
devtools::install_github("thomasgredig/nanomasterNTE")
library(nanomasterNTE)
```

## Example

Sample code to store a quad graph with the results from the depostion:

``` r
library(nanomasterNTE)
r = data.frame()
d = NTE.load(filename)
d2 = NTE.depStartEnd(d)
if (!(is.na(d2))) {
    r1 = NTE.getParams(d)
    r = rbind(r, r1)
    g3 = NTE.plotDeposition(d)
    ggsave2(plot = g3, file = 'NTE.png')
}
```

