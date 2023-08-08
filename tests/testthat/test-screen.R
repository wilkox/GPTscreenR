# Prepare example study description
objective <- alpaca_review_description$objective
population <- alpaca_review_description$population
context <- alpaca_review_description$context
concept <- alpaca_review_description$concept
review_description <- review_description(objective, population, context, concept)

# Prepare example sources
title <- alpaca_sources$title[1]
abstract <- alpaca_sources$abstract[1]
sources <- data.frame(title = title, abstract = abstract)

test_that("review_description() works", {

  expect_no_error(review_description(objective, population, context, concept))
  review_description <- review_description(objective, population, context, concept)
  expect_type(review_description, "character")
  expect_length(review_description, 1)

})

test_that("screen_source() inputs are correctly validated", {

  expect_no_error(screen_source(
    review_description = review_description,
    title = title,
    abstract = abstract,
    .verbose = FALSE,
    .dry_run = TRUE
  ))

  expect_no_warning(screen_source(
    review_description = review_description,
    title = title,
    abstract = abstract,
    .verbose = FALSE,
    .dry_run = TRUE
  ))

  expect_error(screen_source(
    review_description = FALSE,
    title = title,
    abstract = abstract,
    .verbose = FALSE,
    .dry_run = TRUE
  ))

  expect_error(screen_source(
    title = title,
    abstract = abstract,
    .verbose = FALSE,
    .dry_run = TRUE
  ))

  expect_warning(screen_source(
    review_description = "",
    title = title,
    abstract = abstract,
    .verbose = FALSE,
    .dry_run = TRUE
  ))
})

test_that("Screening works", {

  expect_no_error(screen_source(review_description = review_description, title = title, 
                                abstract = abstract, .verbose = FALSE, .dry_run = TRUE))

  expect_no_error(
    screen_sources(sources = sources, review_description = review_description, .dry_run = TRUE, 
                   .verbose = FALSE)
  ) 

})
