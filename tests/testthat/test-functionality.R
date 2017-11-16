context("functionality")

test_that("various column selecting works with entropy",{
    tab <- mutate(as_tibble(mtcars), am=as.character(am))
    answer <- -1 * (13/32*log2(13/32) + 19/32*log2(19/32))
    expect_equal(shannon_entropy(tab, am), answer)
    expect_equal(shannon_entropy(tab, 'am'), answer)
    expect_equal(shannon_entropy(tab, 9), answer)
    expect_equal(shannon_entropy(tab, -3), answer)
})

test_that("shannon entropy works, removing NAs", {
    tab <- mutate(as_tibble(mtcars), am=as.character(am))
    tab$am[13:22] <- NA
    answer_with_NA <-    -1 * (12/32*log2(12/32) + 10/32*log2(10/32) + 10/32*log2(10/32))
    answer_without_NA <- -1 * (12/22*log2(12/22) + 10/22*log2(10/22))
    expect_equal(shannon_entropy(tab, am), answer_with_NA)
    expect_equal(shannon_entropy(tab, am, na.rm=TRUE), answer_without_NA)
})

test_that("conditional shannon entropy works", {
    tab <- as_tibble(mtcars) %>% mutate(am=as.character(am), vs=as.character(vs))
    answer <- 7/32*log2((13/32)/(7/32)) +  6/32*log2((13/32)/(6/32)) +
              7/32*log2((19/32)/(7/32)) + 12/32*log2((19/32)/(12/32))
    answer_rev <- 2 * (7/32*log2((14/32)/(7/32))) +
                       6/32*log2((18/32)/(6/32)) + 12/32*log2((18/32)/(12/32))
    expect_equal(shannon_cond_entropy(tab, vs, am), answer)
    expect_equal(shannon_cond_entropy(tab, am, vs), answer_rev)
})

test_that("mutual information works", {
    tab <- as_tibble(mtcars) %>% mutate(am=as.character(am), vs=as.character(vs))
    answer <- shannon_entropy(tab, am) - shannon_cond_entropy(tab, am, vs)
    expect_equal(mutual_info(tab, am, vs), answer)
    expect_equal(mutual_info(tab, am, vs), mutual_info(tab, vs, am))
})

test_that("shannon conditional entropy works, NAs removed", {
    tab <- as_tibble(mtcars) %>% mutate(am=as.character(am), vs=as.character(vs))
    tab$vs[23:25] <- NA
    tab$am[14:17] <- NA
    answer <- 2 * 7/25*log2((14/25)/(7/25)) +
              6/25*log2((11/25)/(6/25)) + 5/25*log2((11/25)/(5/25))
    expect_equal(shannon_cond_entropy(tab, am, vs, na.rm=TRUE), answer)
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

