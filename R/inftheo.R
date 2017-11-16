#' tidyinfotheo: tidy-style information theoretic routines
#'
#' tidyinfotheo provides an easy way to calculate shannon entropy,
#' mutual information, etc. from fields in a 'tibble'
#'
#' To learn more about tidyinfotheo, start with the vignettes:
#' `browseVignettes(package = "tidyinfotheo")`
#'
#' @import dplyr
#' @importFrom magrittr %>%
"_PACKAGE"

#' Internal function to check the variable's type.  Information theory
#' functions may not work as intended on doubles, so make a warning.
#' Sometimes doubles are the unexpected column type though.
#' @return the table with the column possibly coerced as a character
#' @keywords internal
check_type <- function(tab, varname)
{
    vec <- tab %>% pull(!!varname)
    modified <- tab
    stopifnot(purrr::is_atomic(vec) && (purrr::is_character(vec) ||
                                        purrr::is_integer(vec) ||
                                        purrr::is_logical(vec) ||
                                        purrr::is_double(vec)))
    if (purrr::is_double(vec)) {
        warning(call.=FALSE, paste0('converting \'',as.character(varname),
                                    '\' from double to character'))
        modified <- mutate(modified, varname = as.character(vec))
    }
    else if (is.factor(vec)) {
        # factors are kind of funny sometimes with NAs.  this might not
        # be necessary but...
        modified <- mutate(modified, varname = as.character(vec))
    }
    modified
}

#' Shannon Entropy H(X)
#'
#' This calculates shannon entropy of a variable in a tibble.
#' It's assumed these columns are character typed with no NAs.
#'
#' @param tab A tibble with the column of interest
#' @param X Name of the column
#' @return a double with the calculated value
#' @seealso [shannon_cond_entropy]
#'
#' @export
## @examples (see the Entropy notebook for now)
shannon_entropy <- function(tab, X, na.rm=FALSE)
{
    X_sym <- rlang::sym(tidyselect::vars_pull(names(tab), !! enquo(X)))
    modified_tab <- check_type(tab, X_sym)
    if (na.rm) {
        modified_tab <- modified_tab %>% filter(!is.na(!!X_sym))
    }
    modified_tab %>% group_by(!! X_sym) %>% summarize(N_X=n()) %>% ungroup() %>%
        mutate(P_X=N_X/sum(N_X), Log_Term=-1 * P_X * ifelse(P_X>0, log2(P_X), 0)) %>%
        summarize(H=sum(Log_Term)) %>% pull(H)
}

#' Conditional Shannon Entropy H(X|Y) i.e. "H(X given Y)"
#'
#' This calculates conditional shannon entropy of two columns in a tibble.
#' It's assumed these columns are character typed with no NAs.
#'
#' @param tab A tibble with the columns of interest
#' @param ... two columns (variables) selected
#' @return a double with the calculated value
#' @seealso [shannon_entropy]
#' @export
shannon_cond_entropy <- function(tab, ..., na.rm=FALSE)
{
    vars <- tidyselect::vars_select(names(tab), !!! quos(...))
    stopifnot(length(vars)==2)
    X_sym <- rlang::sym(vars[1])
    modified_tab <- check_type(tab, X_sym)
    Y_sym <- rlang::sym(vars[2])
    modified_tab <- check_type(modified_tab, Y_sym)
    if (na.rm) {
        modified_tab <- filter(modified_tab, !is.na(!!X_sym) & !is.na(!!Y_sym))
    }
    modified_tab %>% group_by(!! X_sym, !! Y_sym) %>%
        summarize(Count=n()) %>% ungroup() %>%
        group_by(!! X_sym) %>% mutate(Sum_X = sum(Count)) %>% ungroup() %>%
        group_by(!! Y_sym) %>% mutate(Sum_Y = sum(Count)) %>% ungroup() %>%
        mutate(P_X=Sum_X/sum(Count), P_Y=Sum_Y/sum(Count),
               P_X_g_Y=Count/Sum_Y, P_Y_g_X=Count/Sum_X,
               Log_Term=-1 * P_Y * P_X_g_Y * ifelse(P_X_g_Y>0, log2(P_X_g_Y), 0)) %>%
        group_by(!! Y_sym) %>% summarize(H=sum(Log_Term)) %>% pull(H) %>% sum()
}

#' Mutual information MI(X;Y) = H(X) - H(X|Y) = H(Y) - H(Y|X)
#'
#' This calculates the mutual information between two variables in a tibble.
#' (if normalized).  It's assumed these columns are character typed with no NAs.
#'
#' @param tab A tibble with the column of interest
#' @param ... two columns (variables) selected
#' @param normalized if TRUE, scale to [0,1]
#' @return a double with the calculated value
#' @export
## @examples (see the Entropy notebook for now)
mutual_info <- function(tab, ..., normalized=FALSE, na.rm=FALSE)
{
    # half of this setup code is the same as shannon_cond_entropy()
    # maybe another function is desirable for modularity
    vars <- tidyselect::vars_select(names(tab), !!! quos(...))
    stopifnot(length(vars)==2)
    X_sym <- rlang::sym(vars[1])
    modified_tab <- check_type(tab, X_sym)
    Y_sym <- rlang::sym(vars[2])
    modified_tab <- check_type(modified_tab, Y_sym)
    if (na.rm) {
        modified_tab <- filter(modified_tab, !is.na(!!X_sym) & !is.na(!!Y_sym))
    }
    ent_X <- shannon_entropy(modified_tab, !!X_sym)
    ent_X_g_Y <- shannon_cond_entropy(modified_tab, !!X_sym, !!Y_sym)
    mi_X_Y <- ent_X - ent_X_g_Y
    if (normalized) {
        ent_Y <- shannon_entropy(modified_tab, !!Y_sym)
        mi_X_Y <- 2 * mi_X_Y/(ent_X + ent_Y)
    }
    mi_X_Y
}