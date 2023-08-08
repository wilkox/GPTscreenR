test_that("alpaca_sources is structured correctly", {

  # Alpaca_sources should have three columns that are named correctly
  expect_true(ncol(alpaca_sources) == 3)
  expect_true(all(names(alpaca_sources) == c("title", "abstract", "include")))

  # The elements should be typed correctly
  expect_true(is.character(alpaca_sources$title))
  expect_true(is.character(alpaca_sources$abstract))
  expect_true(is.logical(alpaca_sources$include))

})
