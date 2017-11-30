#' Plot heatmap of mutual infos
#'
#' Given a matrix produced from mutual_info_matrix(), plot a heatmap with ggplot2
#'
#' @param mi_matrix a table produced from [mutual_info_matrix]
#' @param title title of plot
#' @param font_sizes A length-2 vector of x-axis and y-axis variable font sizes
#' @return a double with the calculated value
#' @seealso [mutual_info_matrix]
#' @export
#' @import ggplot2
#' @examples
#' p <- mtcars %>%
#'    mutual_info_matrix(cyl, vs, am, gear, carb, normalized=TRUE) %>%
#'    mutual_info_heatmap()
#' p
mutual_info_heatmap <- function(mi_matrix, title=NULL, font_sizes=c(12,12))
{
    if (ncol(mi_matrix) != 3) {
        stop("mi_matrix doesn't have 3 columns")
    }
    if (!all(colnames(mi_matrix)==c('V1','V2','MI')) ||
        !is.character(mi_matrix$V1) ||
        !is.character(mi_matrix$V2) ||
        !purrr::is_double(mi_matrix$MI)) {
        stop("mi_matrix has unexpected columns")
    }
    axis_names <- unique(c(mi_matrix$V1, mi_matrix$V2))
    recomb <- utils::combn(axis_names, 2) %>% t() %>% as_tibble()
    if (nrow(recomb) != nrow(mi_matrix)) {
        stop(paste0('mi_matrix should have ',nrow(recomb), 'rows'))
    }
    bigset1 <- c(paste0(recomb$V1,'|',recomb$V2),paste0(recomb$V2,'|',recomb$V1))
    bigset2 <- c(paste0(mi_matrix$V1,'|',mi_matrix$V2),paste0(mi_matrix$V2,'|',mi_matrix$V1))
    # they can be in different orders
    if (!(setequal(bigset1,bigset2))) {
        stop(paste0('combinations expected in mi_matrix not seen'))
    }
    p <- ggplot(data=mi_matrix, aes(V1, V2)) +
        geom_tile(aes(fill=MI), color="white") +
        scale_x_discrete(limits=axis_names[1:(length(axis_names)-1)]) +
        scale_y_discrete(limits=rev(axis_names)[1:(length(axis_names)-1)]) +
        scale_fill_continuous(limits=c(0,0.6)) +
        theme(axis.text.x=element_text(angle=90, hjust=1, size=font_sizes[1]),
              axis.text.y=element_text(size=font_sizes[2])) +
        xlab(NULL) + ylab(NULL) +
        ggtitle(title)
    p
}
