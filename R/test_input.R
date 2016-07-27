#' Test inputs for the functions DB2reloc_temp(), as_pgtraj()
#' 
#' @param pgtrajs String. Name of the pgtraj or name of the field that stores the pgtraj names.
#' @param animals String. Name of the animal or name of the field that stores the animal names.
#' @param bursts String. Name of the burst or name of the field that stores the burst names.
#' @param relocations String. Name of the field that contains the relocations in relocation_data.
#' @param rids String. Name of the field in relocation_data that contains the numeric IDs of relocations.
#' @param epsg Numeric. The EPSG code of the Coordinate Reference System of the 
#' relocation coordinates in the ltraj. Defaults to 0.
#' 
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
################################################################################
test_input <- function(pgtrajs = NULL, animals = NULL, relocations = NULL,
        bursts = NULL, rids = NULL, epsg = NULL) {
    
    test_pa <- function(x) {
        testthat::test_that("arguments 'pgtrajs', 'animals' and 'rids' have correct inputs", {
                    testthat::expect_that(length(x) == 1, testthat::is_true())
                    testthat::expect_that(all(!is.na(x)), testthat::is_true())
                    testthat::expect_that(all(is.character(x)), testthat::is_true())
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
        testthat::test_that("argument 'bursts' has a correct input", {
                    testthat::expect_that(length(bursts) == 1, testthat::is_true())
                })
    }
    
    if (!is.null(relocations)) {
        testthat::test_that("argument 'relocations' has a correct input", {
                    testthat::expect_that(all(!is.na(relocations)), testthat::is_true())
                    testthat::expect_that(length(relocations) >= 1, testthat::is_true())
                    testthat::expect_that(length(relocations) <= 2, testthat::is_true())
                    testthat::expect_that(all(is.character(relocations)), testthat::is_true())
                })
    }
    
    if (!is.null(epsg)) {
        testthat::test_that("argument 'epsg' has a correct input", {
                    testthat::expect_that(is.numeric(epsg), testthat::is_true())
                    testthat::expect_that(epsg %% 1 == 0, testthat::is_true())
                })
    }
}

