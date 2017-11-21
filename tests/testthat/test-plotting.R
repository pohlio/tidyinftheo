context("plotting")

test_that("plotting function handles bad input", {
    tab <- tibble::as_tibble(mtcars) %>% mutate_if(purrr::is_double, as.character)
    cols <- 8:11
    mi_mat <- mutual_info_matrix(tab, cols)
    # bad column names
    expect_error(mutual_info_heatmap(mi_matr %>% rename(V23=V1)))
    expect_error(mutual_info_heatmap(mi_matr %>% rename(V4=V2)))
    expect_error(mutual_info_heatmap(mi_matr %>% rename(NMI=MI)))
    # bad number of rows/cols
    expect_error(mutual_info_heatmap(mi_matr[1:7,]))
    expect_error(mutual_info_heatmap(mi_matr[,c(1,3)]))
    bad_mat <- mi_mat
    # unexpected variable
    expect_error(mutual_into_heatmap(bad_mat))
    bad_mat[3,1] <- "cyl"
    # break combinations
    bad_mat[3,1] <- "gear"
    expect_error(mutual_into_heatmap(bad_mat))
    # rearranging rows shouldn't matter
    expect_true(is.ggplot(mutual_info_heatmap(mi_mat %>% arrange(MI))))
})
