
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

# networkedwebsitesdetector

The goal of networkedwebsitesdetector is to offer a structured approach
for finding websites which have clear signs of common ownership or are
otherwise related.

The package is mostly functional and should serve its purpose. However,
it has not yet been fully tested.

Basic documentation describing typical workflow is available on
[`networkedwebsitesdetector`’s
website](https://giocomai.github.io/networkedwebsitesdetector).
Additional details, use cases, and examples of outputs will be added.

## Installation

You can install the development version of `networkedwebsitesdetector`
from [CRAN](https://CRAN.R-project.org) with:

``` r
remotes::install_github("giocomai/networkedwebsitesdetector")
```

As of this writing (2019-09-06), the package works with the development
versions of both `tidyr` and `googledrive` and will result in errors
with the current cran versions. This notice will be removed as soon as
the current development version of both will make it to CRAN.

``` r
remotes::install_github("tidyverse/tidyr")
remotes::install_github("tidyverse/googledrive")
```
