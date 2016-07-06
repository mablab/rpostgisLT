# Test inputs for the functions DB2reloc_temp(), as_pgtraj()

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

test_input(epsg = "abd")

test_input <- function(pgtrajs = NULL, animals = NULL, relocations = NULL,
        bursts = NULL, rids = NULL, epsg = NULL) {
    
    test_pa <- function(x) {
        test_that("arguments 'pgtrajs', 'animals' and 'rids' have correct inputs", {
                    expect_that(length(x) == 1, is_true())
                    expect_that(all(!is.na(x)), is_true())
                    expect_that(all(is.character(x)), is_true())
                })
    }
    
    if (!is.null(pgtrajs)) {
        test_pa(pgtrajs)
    }
    
    if (!is.null(animals)) {
        test_pa(animals)
    }
    
    if (!is.null(rids)) {
        test_pa(rids)
    }
    
    if (!is.null(bursts)) {
        test_that("argument 'bursts' has a correct input", {
                    expect_that(length(bursts) == 1, is_true())
                })
    }
    
    if (!is.null(relocations)) {
        test_that("argument 'relocations' has a correct input", {
                    expect_that(all(!is.na(relocations)), is_true())
                    expect_that(length(relocations) >= 1, is_true())
                    expect_that(length(relocations) <= 2, is_true())
                    expect_that(all(is.character(relocations)), is_true())
                })
    }
    
    if (!is.null(epsg)) {
        test_that("argument 'epsg' has a correct input", {
                    expect_that(is.numeric(epsg), is_true())
                    expect_that(epsg %% 1 == 0, is_true())
                })
    }
}

