# tidyinftheo 0.2.1

* Edits for CRAN compliance.
* References added.

# tidyinftheo 0.2.0

* Heatmap function added.

# tidyinftheo 0.1.2

* Unbelievably stupid bug fixed in the first README example producing an error.  Was:
```R
starwars %>% shannon_entropy(eye_color)
#> Warning in data.matrix(X): NAs introduced by coercion
#> [1] NaN
```
should be:
```R
starwars %>% shannon_entropy(eye_color)
#> [1] 3.117176
```
* Should accept zero-argument syntax like:
```R
starwars %>% select(eye_color) %>% shannon_entropy() 
```
now.
* data entering all `infotheo` functions should be purely either tibbles or vectors now.

# tidyinftheo 0.1.1

* Using "infotheo" package for calculations.

# tidyinftheo 0.1.0

* Basically functioning.
