# Prepare example study description
objective <- alpaca_inclusion_criteria$objective
population <- alpaca_inclusion_criteria$population
context <- alpaca_inclusion_criteria$context
concept <- alpaca_inclusion_criteria$concept
study_description <- study_description(objective, population, context, concept)

# Prepare example source
title <- alpaca_sources[[1]]$title
abstract <- alpaca_sources[[1]]$abstract

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

  expect_no_error(screen_source(
    study_description = study_description,
    title = title,
    abstract = abstract,
    .verbose = FALSE
  ))

})
