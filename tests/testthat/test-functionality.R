context("functionality")

test_that("various column selecting works with entropy",{
    tab <- mutate(as_tibble(mtcars), am=as.character(am))
    answer <- -1 * (13/32*log2(13/32) + 19/32*log2(19/32))
    answer_style_1 <- shannon_entropy(tab, am)
    answer_style_2 <- shannon_entropy(tab, 'am')
    answer_style_3 <- shannon_entropy(tab, 9)
    expect_equal(answer_style_1, answer, tolerance=0.0000001)
    expect_equal(answer_style_2, answer, tolerance=0.0000001)
    expect_equal(answer_style_3, answer, tolerance=0.0000001)
})

test_that("shannon entropy works, removing NAs", {
    tab <- mutate(as_tibble(mtcars), am=as.character(am))
    tab$am[13:22] <- NA
    my_answer_with_NA <-    -1 * (12/32*log2(12/32) + 10/32*log2(10/32) + 10/32*log2(10/32))
    my_answer_without_NA <- -1 * (12/22*log2(12/22) + 10/22*log2(10/22))
    answer_with_NA <- shannon_entropy(tab, am)
    answer_without_NA <- shannon_entropy(tab, am, na.rm=TRUE)
    expect_equal(answer_with_NA, answer_with_NA, tolerance=0.0000001)
    expect_equal(answer_without_NA, answer_without_NA, tolerance=0.0000001)
})

test_that("conditional shannon entropy works", {
    tab <- as_tibble(mtcars) %>% mutate(am=as.character(am), vs=as.character(vs))
    my_answer <- 7/32*log2((13/32)/(7/32)) +  6/32*log2((13/32)/(6/32)) +
              7/32*log2((19/32)/(7/32)) + 12/32*log2((19/32)/(12/32))
    my_answer_rev <- 2 * (7/32*log2((14/32)/(7/32))) +
                       6/32*log2((18/32)/(6/32)) + 12/32*log2((18/32)/(12/32))
    answer <- shannon_cond_entropy(tab, vs, am)
    answer_rev <- shannon_cond_entropy(tab, am, vs)
    expect_equal(my_answer, answer, tolerance=0.0000001)
    expect_equal(my_answer_rev, answer_rev, tolerance=0.0000001)
})

test_that("mutual information works", {
    tab <- as_tibble(mtcars) %>% mutate(am=as.character(am), vs=as.character(vs))
    my_answer <- shannon_entropy(tab, am) - shannon_cond_entropy(tab, am, vs)
    answer <- mutual_info(tab, am, vs)
    expect_equal(my_answer, answer, tolerance=0.0000001)
    expect_equal(mutual_info(tab, am, vs), mutual_info(tab, vs, am))
})

test_that("shannon conditional entropy works, NAs removed", {
    tab <- as_tibble(mtcars) %>% mutate(am=as.character(am), vs=as.character(vs))
    tab$vs[23:25] <- NA
    tab$am[14:17] <- NA
    my_answer <- 2 * 7/25*log2((14/25)/(7/25)) +
              6/25*log2((11/25)/(6/25)) + 5/25*log2((11/25)/(5/25))
    answer <- shannon_cond_entropy(tab, am, vs, na.rm=TRUE)
    expect_equal(my_answer, answer, tolerance=0.0000001)
})

test_that("mutual information works, NAs removed", {
    tab <- as_tibble(mtcars) %>% mutate(am=as.character(am), vs=as.character(vs))
    tab$vs[23:25] <- NA
    tab$am[14:17] <- NA
    answer <- shannon_entropy(tab, am, na.rm=TRUE) - shannon_cond_entropy(tab, am, vs, na.rm=TRUE)
    # shannon_entropy() and shannon_cond_entropy() remove different rows, so this will fail
    expect_failure(expect_equal(mutual_info(tab, am, vs, na.rm=TRUE), answer))
    # rig it to remove the same rows:
    tab$am[23:25] <- NA
    tab$vs[14:17] <- NA
    answer <- shannon_entropy(tab, am, na.rm=TRUE) - shannon_cond_entropy(tab, am, vs, na.rm=TRUE)
    expect_equal(mutual_info(tab, am, vs, na.rm=TRUE), answer)
    # same backwards and forwards
    expect_equal(mutual_info(tab, am, vs, na.rm=TRUE), mutual_info(tab, vs, am, na.rm=TRUE))
})

test_that("mutual info table works", {
    tab <- as_tibble(mtcars) %>% mutate_if(purrr::is_double, as.character)
    tab$vs[23:25] <- NA
    tab$am[14:17] <- NA
    # one comparison.  we'll spot-check the matrix and check the size.
    vs_gear <- mutual_info(tab, vs, gear)
    cols <- 8:11
    mi_mat <- mutual_info_matrix(tab, cols)
    expect_equal(dim(mi_mat), c(choose(length(cols), 2), 3))
    expect_equal(mi_mat %>%
                filter((V1=='vs' & V2=='gear') | (V2=='vs' & (V1=='gear'))) %>%
                select(MI) %>% pull(1), vs_gear)
})
