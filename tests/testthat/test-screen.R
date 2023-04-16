# Prepare example inclusion criteria
study_objective <- alpaca_inclusion_criteria$study_objective
population <- alpaca_inclusion_criteria$population
context <- alpaca_inclusion_criteria$context
concept <- alpaca_inclusion_criteria$concept

# Prepare example source
title <- "Therapy alpaca visits reduce depression, anxiety, and stress in Australian nursing home residents"
abstract <- "Nursing home residents typically have rates of depression, anxiety, and stress (DAS) significantly higher than those of equivalent populations living independently. As well as reducing quality of life and increasing morbidity and mortality, DAS significantly increases the burden on both nursing homes and health systems in caring for the elderly. Recent pilot trials of the use of therapy alpacas have shown promising initial results in reducing DAS among nursing home residents. We conducted a randomised control trial involving 8,124 residents across 17 nursing homes in urban and rural Australia in which residents were randomised to either a weekly 2-hour therapy alpaca visit or equivalent time in a television viewing lounge. Residents receiving the alpaca intervention had significantly (p < 0.01) reduced levels of depression (mean DASS-21 score for depression reduction 8.2 points), anxiety (mean reduction 11 points) and stress (mean reduction 13 points) compared to the control arm. This study represents the first large-scale randomised trial of alpaca therapy for nursing home residents, and provides strong support for its effectiveness in reducing DAS."

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
