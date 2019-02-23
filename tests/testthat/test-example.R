testthat::test_that("name_of_test", {

  # load test data

  # create input data without calling external files
  # and without requiring connectivity

  dt <- data.table::data.table(expected_col = LETTERS)

  # run test

  # in this example
  # just copy expected result as result
  dto <- dt

  # optionally print visual output for test lof
  dto %>% print

  # run set of tests to ensure output is correct

  testthat::expect_equivalent(
    dto$expected_col,
    LETTERS
    )

    })
