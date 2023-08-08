# Prepare example study description
objective <- alpaca_study_description$objective
population <- alpaca_study_description$population
context <- alpaca_study_description$context
concept <- alpaca_study_description$concept
study_description <- study_description(objective, population, context, concept)

# Prepare example sources
title <- alpaca_sources$title[1]
abstract <- alpaca_sources$abstract[1]
sources <- data.frame(title = title, abstract = abstract)

test_that("study_description() works", {

  expect_no_error(study_description(objective, population, context, concept))
  study_description <- study_description(objective, population, context, concept)
  expect_type(study_description, "character")
  expect_length(study_description, 1)

})

test_that("screen_source() inputs are correctly validated", {

  expect_no_error(screen_source(
    study_description = study_description,
    title = title,
    abstract = abstract,
    .verbose = FALSE,
    .dry_run = TRUE
  ))

  expect_no_warning(screen_source(
    study_description = study_description,
    title = title,
    abstract = abstract,
    .verbose = FALSE,
    .dry_run = TRUE
  ))

  expect_error(screen_source(
    study_description = FALSE,
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
    study_description = "",
    title = title,
    abstract = abstract,
    .verbose = FALSE,
    .dry_run = TRUE
  ))
})

test_that("Screening works", {

  expect_no_error(screen_source(study_description = study_description, title = title, 
                                abstract = abstract, .verbose = FALSE, .dry_run = TRUE))

  expect_no_error(
    screen_sources(sources = sources, study_description = study_description, .dry_run = TRUE, 
                   .verbose = FALSE)
  ) 

})
