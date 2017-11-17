
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyinftheo
===========

[![Travis-CI Build Status](https://travis-ci.org/pohlio/tidyinftheo.svg?branch=master)](https://travis-ci.org/pohlio/tidyinftheo) [![codecov](https://codecov.io/gh/pohlio/tidyinftheo/branch/master/graph/badge.svg)](https://codecov.io/gh/pohlio/tidyinftheo)

You can install tidyinftheo from github with:

``` r
devtools::install_github("pohlio/tidyinftheo")
```

Examples
--------

Calculate (in bits) the Shannon Entropy of the eye color variable in the `starwars` dataset:

``` r
library(tidyinftheo)
starwars %>% shannon_entropy(eye_color)
#> [1] 3.117176
```

With the classic `mtcars` dataset, choose some columns to make a matrix of mutual information pairwise comparisons. In particular, the *cyl*, *vs*, *am*, *gear*, and *carb* columns are all whole numbers indicating they belong to a category. The other columns are continuous and are better suited to correlation comparisons, unless they're discretized. Here are the first few rows of **mtcars**:

``` r
mtcars %>% select(cyl, vs, am, gear, carb) %>% head()
```

|                   |  cyl|   vs|   am|  gear|  carb|
|-------------------|----:|----:|----:|-----:|-----:|
| Mazda RX4         |    6|    0|    1|     4|     4|
| Mazda RX4 Wag     |    6|    0|    1|     4|     4|
| Datsun 710        |    4|    1|    1|     4|     1|
| Hornet 4 Drive    |    6|    1|    0|     3|     1|
| Hornet Sportabout |    8|    0|    0|     3|     2|
| Valiant           |    6|    1|    0|     3|     1|

And here is our comparison table. There should be *5-choose-2 = 10* different combinations. NMI stands for *Normalized Mutual Information*, so the mutual information, normally given in bits, is scaled between 0 and 1:

``` r
mi_matr <- as_tibble(mtcars) %>% 
    mutate_if(is_double, as.character) %>%
    mutual_info_matrix(cyl, vs, am, gear, carb, normalized=TRUE)
mi_matr
```

| V1   | V2   |        NMI|
|:-----|:-----|----------:|
| cyl  | vs   |  0.4937932|
| cyl  | am   |  0.1672528|
| cyl  | gear |  0.3504372|
| cyl  | carb |  0.3983338|
| vs   | am   |  0.0208314|
| vs   | gear |  0.2397666|
| vs   | carb |  0.2861119|
| am   | gear |  0.5173527|
| am   | carb |  0.1149038|
| gear | carb |  0.1905054|

The matrix is already in a convenient format to plot:

``` r
axis_names <- mtcars %>% select(cyl, vs, am, gear, carb) %>% names()
p <- mi_matr %>%
    ggplot(aes(V1, V2)) +
    geom_tile(aes(fill=NMI), color="white") +
    scale_x_discrete(limits=axis_names[1:(length(axis_names)-1)]) +
    scale_y_discrete(limits=rev(axis_names)[1:(length(axis_names)-1)]) +
    theme(axis.text.x=element_text(angle=90, hjust=1, size=12),
          axis.text.y=element_text(size=12)) +
    xlab(NULL) + ylab(NULL) + 
    ggtitle("mtcars mutual information comparisons")
print(p)
```

![](README-unnamed-chunk-5-1.png)
