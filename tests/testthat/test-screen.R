# Prepare example inclusion criteria
study_objective <- alpaca_inclusion_criteria$study_objective
population <- alpaca_inclusion_criteria$population
context <- alpaca_inclusion_criteria$context
concept <- alpaca_inclusion_criteria$concept

# Prepare example source
title <- alpaca_sources[[1]]$title
abstract <- alpaca_sources[[1]]$abstract

test_that("inputs are correctly validated", {

  expect_no_error(screen_source(
    study_objective = study_objective,
    population = population,
    context = context,
    concept = concept,
    title = title,
    abstract = abstract
  ))

  expect_no_warning(screen_source(
    study_objective = study_objective,
    population = population,
    context = context,
    concept = concept,
    title = title,
    abstract = abstract
  ))

  expect_error(screen_source(
    study_objective = FALSE,
    population = population,
    context = context,
    concept = concept,
    title = title,
    abstract = abstract
  ))

  expect_error(screen_source(
    population = population,
    context = context,
    concept = concept,
    title = title,
    abstract = abstract
  ))

  expect_warning(screen_source(
    study_objective = "",
    population = population,
    context = context,
    concept = concept,
    title = title,
    abstract = abstract
  ))
})
