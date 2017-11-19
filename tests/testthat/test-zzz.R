context("zzz")

test_that("stuff in zzz.R is accurate", {
    expect_true(all(is_attached(core)))
    for (pack in core) {
        pos <- which(paste0('package:', pack) == search())
        detach(pos=pos, character.only = TRUE, unload = TRUE)
    }
    expect_silent(tidyverse_attach())
})
