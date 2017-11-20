context("errors")

test_that("right number of columns given", {
    tab <- as_tibble(mtcars)
    # just a single number should be returned
    expect_length(shannon_entropy(tab, vs), 1)
    expect_length(shannon_cond_entropy(tab, vs, am), 1)
    expect_length(mutual_info(tab, vs, am), 1)
    # check empty argument style i.e. tables implicitly right size
    expect_length(tab %>% select(vs) %>% shannon_entropy(), 1)
    expect_length(tab %>% select(vs, am) %>% shannon_cond_entropy(), 1)
})

test_that("wrong number of columns given", {
    tab <- as_tibble(mtcars)
    expect_error(shannon_entropy(tab))
    expect_error(expect_warning(shannon_entropy(tab, am, vs)))
    expect_error(shannon_cond_entropy(tab))
    expect_error(tab %>% select(vs) %>% shannon_cond_entropy())
    expect_error(shannon_cond_entropy(tab, vs))
    expect_error(shannon_cond_entropy(tab, vs, am, carb))
    expect_error(mutual_info(tab))
    expect_error(mutual_info(tab, vs))
    expect_error(mutual_info(tab, vs, am, carb))
})

test_that("doubles give an error even if coerced to character", {
    tab <- as_tibble(mtcars)
    expect_error(shannon_entropy(tab, mpg))
})

test_that("complex-typed columns are an error", {
    tab <- tibble(x=0i ^ (-3:3))
    expect_warning(shannon_entropy(tab, x))
})
