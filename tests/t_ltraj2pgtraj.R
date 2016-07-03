#libtrary(testthat)
#relocations <- c("x", "y")
#relocations <- c(NA, NA)
#relocations <- c("x", NA)
#relocations <- c("x", "c", "y")
#relocations <- c(1, 2)
#
#pgtrajs <- "pgt"
#animals <- "bla"
#pgtrajs <- "pgt"
#animals <- 1

test_input <- function(pgtrajs, animals, relocations, bursts) {
    test_pa <- function(x) {
        test_that("arguments 'pgtrajs' and 'animals' have correct inputs", {
                    expect_that(length(x) == 1, is_true())
                    expect_that(all(!is.na(x)), is_true())
                    expect_that(all(is.character(x)), is_true())
                })
    }
    
    test_pa(pgtrajs)
    test_pa(animals)
    
    test_that("argument 'bursts' has a correct input", {
                expect_that(length(bursts) == 1, is_true())
            })
    
    test_that("argument 'relocations' has a correct input", {
                expect_that(all(!is.na(relocations)), is_true())
                expect_that(length(relocations) >= 1, is_true())
                expect_that(length(relocations) <= 2, is_true())
                expect_that(all(is.character(relocations)), is_true())
            })
}