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

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c(".", ".data"))

#' Internal function to check the variable's type.  Information theory
#' functions may not work as intended on doubles, so make a warning.
#' Sometimes doubles are the unexpected column type though.
#' @return the table with the column possibly coerced as a character
#' @keywords internal
check_type <- function(.data, varname)
{
    vec <- .data %>% pull(!!varname)
    modified <- .data
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
#' @param .data A tibble with the column of interest
#' @param X Name of the column
#' @param na.rm remove all rows with NA values in at least one of the columns
#' @return a double with the calculated value
#' @seealso [shannon_cond_entropy]
#'
#' @export
#' @importFrom rlang .data
## @examples (see the Entropy notebook for now)
shannon_entropy <- function(.data, X, na.rm=FALSE)
{
    X_sym <- rlang::sym(tidyselect::vars_pull(names(.data), !! enquo(X)))
    modified_tab <- check_type(.data, X_sym)
    if (na.rm) {
        modified_tab <- modified_tab %>% filter(!is.na(!!X_sym))
    }
    modified_tab %>% group_by(!! X_sym) %>% summarize(N_X=n()) %>% ungroup() %>%
        mutate(P_X=.data$N_X/sum(.data$N_X),
               Log_Term=-1 * .data$P_X * ifelse(.data$P_X>0, log2(.data$P_X), 0)) %>%
        summarize(H=sum(.data$Log_Term)) %>% pull(.data$H)
}

#' Conditional Shannon Entropy H(X|Y) i.e. "H(X given Y)"
#'
#' This calculates conditional shannon entropy of two columns in a tibble.
#' It's assumed these columns are character typed with no NAs.
#'
#' @param .data A tibble with the columns of interest
#' @param ... two columns (variables) selected
#' @param na.rm remove all rows with NA values in at least one of the columns
#' @return a double with the calculated value
#' @seealso [shannon_entropy]
#' @importFrom rlang .data
#' @export
shannon_cond_entropy <- function(.data, ..., na.rm=FALSE)
{
    vars <- tidyselect::vars_select(names(.data), !!!quos(...))
    stopifnot(length(vars) == 2)
    X_sym <- rlang::sym(vars[1])
    modified_tab <- check_type(.data, X_sym)
    Y_sym <- rlang::sym(vars[2])
    modified_tab <- check_type(modified_tab, Y_sym)
    if (na.rm) {
        modified_tab <-
            filter(modified_tab,!is.na(!!X_sym) & !is.na(!!Y_sym))
    }
    modified_tab %>% group_by(!!X_sym,!!Y_sym) %>%
        summarize(Count = n()) %>% ungroup() %>%
        group_by(!!X_sym) %>% mutate(Sum_X = sum(.data$Count)) %>% ungroup() %>%
        group_by(!!Y_sym) %>% mutate(Sum_Y = sum(.data$Count)) %>% ungroup() %>%
        mutate(
            P_X = .data$Sum_X / sum(.data$Count),
            P_Y = .data$Sum_Y / sum(.data$Count),
            P_X_g_Y = .data$Count / .data$Sum_Y,
            P_Y_g_X = .data$Count / .data$Sum_X,
            Log_Term = -1 * .data$P_Y * .data$P_X_g_Y * ifelse(.data$P_X_g_Y > 0, log2(.data$P_X_g_Y), 0)
        ) %>%
        group_by(!!Y_sym) %>% summarize(H = sum(.data$Log_Term)) %>% pull(.data$H) %>% sum()
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
#' @export
## @examples (see the Entropy notebook for now)
mutual_info <- function(.data, ..., normalized=FALSE, na.rm=FALSE)
{
    # half of this setup code is the same as shannon_cond_entropy()
    # maybe another function is desirable for modularity
    vars <- tidyselect::vars_select(names(.data), !!!quos(...))
    stopifnot(length(vars)==2)
    X_sym <- rlang::sym(vars[1])
    modified_tab <- check_type(.data, X_sym)
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
## @examples (see the Entropy notebook for now)
mutual_info_matrix <- function(.data, ..., normalized=FALSE, na.rm=FALSE)
{
    # set up vars again.  this really should be generalzied
    vars <- tidyselect::vars_select(names(.data), !!! quos(...))
    var_syms <- rep(NULL, length(vars))
    modified_tab <- .data
    for (v in vars) {
        var_sym <- rlang::sym(v)
        modified_tab <- check_type(modified_tab, var_sym)
        var_syms <- c(var_syms, var_sym)
    }
    var_chars <- as.character(var_syms)
    combs <- utils::combn(var_chars, 2) %>% t() %>% as_tibble()
#    print(combs)
    combs <- bind_cols(combs, 'MI'=rep(NA, nrow(combs)))
    if (nrow(combs) > 0) {
        # maybe there's a nicer way without the loop, oh well
        for (ix in 1:nrow(combs)) {
            # umm, sometimes it feels like I'm bending over
            # backwards for this quasiquotation stuff.
            # I know there is probably a more elegant way.
            v1_sym <- rlang::sym(combs[ix,1] %>% pull(1))
            v2_sym <- rlang::sym(combs[ix,2] %>% pull(1))
            combs[ix,3] <- mutual_info(modified_tab,
                                       !!v1_sym,   # V1
                                       !!v2_sym,   # V2
                                       normalized=normalized,
                                       na.rm=na.rm)
        }
    }
    if (normalized) {
        combs <- rename(combs, 'NMI'=.data$MI)
    }
    combs
}
