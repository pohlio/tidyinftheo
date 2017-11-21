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
    axis_names <- unique(c(mi_matrix$V1, mi_matrix$V2))
    ggplot(data=mi_matrix, aes(V1, V2)) +
        geom_tile(aes(fill=MI), color="white") +
        scale_x_discrete(limits=axis_names[1:(length(axis_names)-1)]) +
        scale_y_discrete(limits=rev(axis_names)[1:(length(axis_names)-1)]) +
        scale_fill_continuous(limits=c(0,0.6)) +
        theme(axis.text.x=element_text(angle=90, hjust=1, size=font_sizes[1]),
              axis.text.y=element_text(size=font_sizes[2])) +
        xlab(NULL) + ylab(NULL) +
        ggtitle(title)
}
