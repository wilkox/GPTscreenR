test_that("alpaca_sources is structured correctly", {

  # Each entry in alpaca_sources should have three elements
  expect_true(all(vapply(alpaca_sources, length, numeric(1)) == 3))

  # The elements should be named correctly
  expect_true(all(vapply(alpaca_sources, names, character(3)) == c("title", "abstract", "include")))

  # The elements should be typed correctly
  expect_true(all(vapply(alpaca_sources, function(s) { is.character(s$title) }, logical(1))))
  expect_true(all(vapply(alpaca_sources, function(s) { is.character(s$abstract) }, logical(1))))
  expect_true(all(vapply(alpaca_sources, function(s) { is.logical(s$include) }, logical(1))))

})
