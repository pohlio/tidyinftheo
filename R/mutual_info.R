#' Shannon Entropy H(X)
#'
#' This calculates shannon entropy of a variable in a tibble,
#' according to:
#' $$H(X) = -\sum_{x\in{X}}p(x)f(x)$$
#' It's assumed these columns are character typed with no NAs.
#'
#' @param tab A tibble with the column of interest
#' @param X Name of the column
#' @return a double with the calculated value
#' @seealso [shannon_cond_entropy()]
#' @export
## @examples (see the Entropy notebook for now)
shannon_entropy <- function(tab, X)
{
    X_sym <- as.name(tidyselect::vars_pull(names(tab), !! enquo(X)))
    tab %>% group_by(!! X_sym) %>% summarize(N_X=n()) %>% ungroup() %>%
        mutate(P_X=N_X/sum(N_X), Log_Term=-1 * P_X * ifelse(P_X>0, log2(P_X), 0)) %>%
        summarize(H=sum(Log_Term)) %>% pull(H)
}

#' Conditional Shannon Entropy H(X|Y) i.e. "H(X given Y)"
#'
#' This calculates conditional shannon entropy of two columns in a tibble,
#' according to:
#' $$H(X|Y) = -\sum_{y\in{Y}}\sum_{x\in{X}}p(x|y)f(p(x|y))$$
#' It's assumed these columns are character typed with no NAs.
#'
#' @param tab A tibble with the columns of interest
#' @param ... two columns (variables) selected
#' @return a double with the calculated value
#' @seealso [shannon_entropy()]
#' @export
## @examples (see the Entropy notebook for now)
shannon_cond_entropy <- function(tab, ...)
{
  vars <- tidyselect::vars_select(names(tab), !!! quos(...))
  stopifnot(length(vars)==2)
  X_sym <- sym(vars[1])
  Y_sym <- sym(vars[2])
  tab %>% group_by(!! X_sym, !! Y_sym) %>%
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
#' This calculates the mutual information between two variables in a tibble,
#' according to:
#' $$\mathit{MI}(X;Y) = H(X) - H(X|Y) = H(Y) - H(Y|X)$$
#' or:
#' $$\mathit{NMI}(X;Y) = \frac{2 \times \mathit{MI}(X;Y)}{H(X) + H(Y)}$$
#' (if normalized).  It's assumed these columns are character typed with no NAs.
#'
#' @param tab A tibble with the column of interest
#' @param ... two columns (variables) selected
#' @param normalized if TRUE, scale to [0,1]
#' @return a double with the calculated value
#' @export
## @examples (see the Entropy notebook for now)
mutual_info <- function(tab, ..., normalized=FALSE)
{
  vars <- tidyselect::vars_select(names(tab), !!! quos(...))
  stopifnot(length(vars)==2)
  X_sym <- sym(vars[1])
  Y_sym <- sym(vars[2])
  ent_X <- shannon_entropy(tab, !!X_sym)
  ent_X_g_Y <- shannon_cond_entropy(tab, !!X_sym, !!Y_sym)
  mi_X_Y <- ent_X - ent_X_g_Y
  if (normalized) {
    ent_Y <- shannon_entropy(tab, !!Y_sym)
    mi_X_Y <- 2 * mi_X_Y/(ent_X + ent_Y)
  }
  mi_X_Y
}
