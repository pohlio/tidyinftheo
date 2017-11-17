# abbreviated, from tidyverse/R/{attach,zzz}.R:

core <- c("ggplot2", "tibble", "tidyr", "readr", "purrr", "dplyr", "stringr", "forcats")

core_loaded <- function() {
    search <- paste0("package:", core)
    core[search %in% search()]
}
core_unloaded <- function() {
    search <- paste0("package:", core)
    core[!search %in% search()]
}

tidyverse_attach <- function() {
    to_load <- core_unloaded()
    if (length(to_load) == 0)
        return(invisible())
    lapply(to_load, library, character.only = TRUE, warn.conflicts = FALSE)
    invisible()
}

is_attached <- function(x) {
    paste0("package:", x) %in% search()
}

.onAttach <- function(...) {
    needed <- core[!is_attached(core)]
    if (length(needed) == 0)
        return()
    tidyverse_attach()
}
