
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyinftheo
===========

[![Travis-CI Build Status](https://travis-ci.org/pohlio/tidyinftheo.svg?branch=master)](https://travis-ci.org/pohlio/tidyinftheo) [![codecov](https://codecov.io/gh/pohlio/tidyinftheo/branch/master/graph/badge.svg)](https://codecov.io/gh/pohlio/tidyinftheo)

You can install tidyinftheo from github with:

``` r
devtools::install_github("pohlio/tidyinftheo")
```

Example
-------

``` r
library(tidyinftheo)
starwars %>% shannon_entropy(hair_color)
```
