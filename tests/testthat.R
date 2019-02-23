if(requireNamespace("testthat", quietly = TRUE)){
    library(testthat)
    library(dt.inflix)
    test_check(package = "dt.inflix",
               wrap = FALSE,
               stop_on_failure = FALSE)
}

