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

vcr::use_cassette("ollama", { 

  test_that("ollama service works", {
    expect_no_error(screen_source(
      review_description = review_description,
      title = title,
      abstract = abstract,
      service = "ollama",
      model = "llama3.2",
      .verbose = FALSE
    ))
  } ) 
} )

vcr::use_cassette("openai", { 

  test_that("screen_source() inputs are correctly validated", {

    expect_no_error(screen_source(
      review_description = review_description,
      title = title,
      abstract = abstract,
      .verbose = FALSE
    ))

    expect_no_warning(screen_source(
      review_description = review_description,
      title = title,
      abstract = abstract,
      .verbose = FALSE
    ))

    expect_error(screen_source(
      review_description = FALSE,
      title = title,
      abstract = abstract,
      .verbose = FALSE
    ))

    expect_error(screen_source(
      title = title,
      abstract = abstract,
      .verbose = FALSE
    ))

    expect_warning(screen_source(
      review_description = "",
      title = title,
      abstract = abstract,
      .verbose = FALSE
    ))
  })  

  test_that("Screening works", {

    expect_no_error(screen_source(review_description = review_description, title = title, 
                                  abstract = abstract, .verbose = FALSE))

    expect_no_error(
      screen_sources(sources = sources, review_description = review_description,
                     .verbose = FALSE)
    ) 
  } ) 
} )
