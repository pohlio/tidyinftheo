context("errors")

test_that("wrong number of columns given", {
    tab <- as_tibble(mtcars)
    expect_error(shannon_entropy(tab))
    expect_error(expect_warning(shannon_entropy(tab, am, vs)))
    expect_error(shannon_cond_entropy(tab))
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
