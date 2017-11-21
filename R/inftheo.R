#' tidyinftheo: tidy-style information theoretic routines
#'
#' tidyinftheo provides an easy way to calculate shannon entropy,
#' mutual information, etc. from fields in a 'tibble'
#'
#' To learn more about tidyinftheo, start with the vignettes:
#' `browseVignettes(package = "tidyinftheo")`
#'
#' @import dplyr
#' @importFrom magrittr %>%
"_PACKAGE"

## Helper function that (a) removes columns not involved in the
## computatution, and [if desired] (b) removes rows that have an NA
reduce_data <- function(.data, ..., numvars=2, na.rm=FALSE) {
    reduced_tab <- as_tibble(.data)
    # capture and check empty arguments
    args <- quos(...)
    if ((length(args) == 0) && (ncol(reduced_tab)==numvars)) {
        vars <- names(reduced_tab)
    }
    else if (length(args) ==0) {
        stop(paste0("no variables specifically selected for data sizd ", nrow(reduced_tab)," x ", ncol(reduced_tab)))
    }
    else {
        vars <- tidyselect::vars_select(names(reduced_tab), !!!quos(...))
    }
    # we should have only 1 or 2 (in which case numvars is set the same)
    # or numvars is 0 and we just need two or more args
    stopifnot((length(vars) == numvars) || ((numvars == 0) && (length(vars) > 1)))
    reduced_tab <- reduced_tab %>% select(vars)
    # trouble occurs in infotheo package if single-column dataframes/tibbles given
    if (ncol(reduced_tab) == 1) {
        reduced_tab <- reduced_tab %>% pull()
        if (na.rm) {
            reduced_tab <- reduced_tab[is.na(reduced_tab)==FALSE]
        }
    }
    else if (na.rm) {
        reduced_tab <- stats::na.omit(reduced_tab)
    }
    reduced_tab
}

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c(".", ".data", "V1", "V2", "MI"))

#' Shannon Entropy H(X)
#'
#' This calculates shannon entropy of a variable in a tibble.
#' It's assumed these columns are character typed with no NAs.
#'
#' @param .data A tibble with the column of interest
#' @param X Name of the column
#' @param na.rm remove all rows with NA values in at least one of the columns
#' @return a double with the calculated value
#' @seealso [shannon_cond_entropy]
#'
#' @export
#' @importFrom rlang .data
#' @importFrom infotheo entropy
#' @importFrom infotheo natstobits
#' @examples
#' shannon_entropy(iris, Species)
#' iris %>% as_tibble() %>% shannon_entropy(Species)
#' shannon_entropy(iris, 'Species')
#' shannon_entropy(iris, 5)
shannon_entropy <- function(.data, X, na.rm=FALSE)
{
    reduced_tab <- reduce_data(.data, !!enquo(X), numvars=1, na.rm=na.rm)
    val <- entropy(reduced_tab)
    natstobits(val)
}

#' Conditional Shannon Entropy H(X|Y) i.e. "H(X given Y)"
#'
#' This calculates conditional shannon entropy of two columns in a tibble.
#' It's assumed these columns are character typed with no NAs.
#'
#' @param .data A tibble with the columns of interest
#' @param ... two columns (variables) selected (order is important)
#' @param na.rm remove all rows with NA values in at least one of the columns
#' @return a double with the calculated value
#' @seealso [shannon_entropy]
#' @importFrom rlang .data
#' @importFrom infotheo condentropy
#' @importFrom infotheo natstobits
#' @export
#' @examples
#' # make an all-character version of mtchars
#' mt_tib <- as_tibble(mtcars) %>% mutate_all(as.character)
#' shannon_cond_entropy(mt_tib, vs, am)
#' shannon_cond_entropy(mt_tib, 'vs', 'am')
#' shannon_cond_entropy(mt_tib, starts_with('c'))
#' shannon_cond_entropy(mt_tib, 9:8)
shannon_cond_entropy <- function(.data, ..., na.rm=FALSE)
{
    reduced_tab <- reduce_data(.data, !!!quos(...), numvars=2, na.rm=na.rm)
    val <- condentropy(reduced_tab[,1], reduced_tab[,2])
    natstobits(val)
}

#' Mutual information MI(X;Y) = H(X) - H(X|Y) = H(Y) - H(Y|X)
#'
#' This calculates the mutual information between two variables in a tibble.
#' (if normalized).  It's assumed these columns are character typed with no NAs.
#'
#' @param .data A tibble with the column of interest
#' @param ... two columns (variables) selected
#' @param normalized if TRUE, scale from 0 to 1
#' @param na.rm remove all rows with NA values in at least one of the columns
#' @return a double with the calculated value
#' @importFrom infotheo entropy
#' @importFrom infotheo condentropy
#' @importFrom infotheo natstobits
#' @export
#' @examples
#' # make an all-character version of mtchars
#' mt_tib <- as_tibble(mtcars) %>% mutate_all(as.character)
#' mutual_info(mt_tib, vs, am)
#' mutual_info(mt_tib, 'am', 'vs')
#' mutual_info(mt_tib, vs, am, normalized=TRUE)
#' mutual_info(mt_tib, starts_with('c'))
mutual_info <- function(.data, ..., normalized=FALSE, na.rm=FALSE)
{
    reduced_tab <- reduce_data(.data, !!!quos(...), numvars=2, na.rm=na.rm)
    X <- reduced_tab[,1]
    Y <- reduced_tab[,2]
    ent_X <- natstobits(entropy(X))
    ent_X_g_Y <- natstobits(condentropy(X, Y))
    mi <- ent_X - ent_X_g_Y
    if (normalized) {
        ent_Y <- natstobits(entropy(Y))
        mi <- 2 * mi/(ent_X + ent_Y)
    }
    mi
}

#' Mutual information Matrix
#'
#' To simplify the task of comparing variables, this calculates a matrix of
#' mutual information values from each pairwise combination of the variables
#' selected. If 6 variables are selected, that would yield a table with
#' 15 rows (choose(6,2)), and 3 columns.
#'
#' @param .data A tibble with the columns of interest
#' @param ... a selection of columns, selected in the same way as [select]
#' @param normalized if TRUE, scale from 0 to 1
#' @param na.rm remove all rows with NA values in at least one of the columns
#' @return a 3 column tibble with each pairwise combination and its calculated mutual information
#' @export
#' @examples
#' # make an all-character version of mtchars
#' mt_tib <- as_tibble(mtcars) %>% mutate_all(as.character)
#' mutual_info_matrix(mt_tib, 8:11)
mutual_info_matrix <- function(.data, ..., normalized=FALSE, na.rm=FALSE)
{
    reduced_tab <- reduce_data(.data, !!!quos(...), numvars=0, na.rm=na.rm)
    combs <- reduced_tab %>% names()
    combs <- utils::combn(combs, 2) %>%
        t() %>%
        as_tibble() %>%
        rowwise() %>%
        mutate(MI=mutual_info(reduced_tab, V1, V2, normalized=normalized, na.rm=na.rm))
    combs
}
