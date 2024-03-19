#' Screen a source against review inclusion criteria
#'
#' @param review_description A description of the review, including objective
#' and inclusion criteria, a character vector of length 1
#' @param title The title of the article to be screened, a character vector of
#' length 1
#' @param abstract The abstract of the article to be screened, a character
#' vector of length 1
#' @param .verbose If FALSE, progress messages will be suppressed
#'
#' @export
screen_source <- function(review_description, title, abstract, .verbose = TRUE) {

  # Validate arguments
  validate_screening_param <- function(param, name) {
    if (missing(param)) { cli::cli_abort(name, " is missing") }
    if (! is.character(param) | ! length(param) == 1) { cli::cli_abort("{name} must be a character vector of length 1") }
    if (is.na(param) | stringr::str_length(param) == 0 ) { cli::cli_warn("{name} appears to have no content") }
  }
  validate_screening_param(review_description, "review_description")
  validate_screening_param(title, "title")
  validate_screening_param(abstract, "abstract")
  if (.verbose) { cli::cli_h2("Screening: {title}") }

  # Initialise conversation with the system message providing instructions on
  # how to perform the screening
  if (.verbose) { cli::cli_progress_step("Initiating conversation with GPT") }
  conversation <- chat(
    role = "system",
    content = "You are helping academic researchers perform a scoping review. Your task is to screen a single source against the review criteria. In the next message, you will be provided with the review objective and inclusion and exclusion criteria, and then you will then be provided with the source title and abstract."
  )

  # Provide the review description and source
  conversation <- add_message(
    conversation,
    role = "user",
    content = stringr::str_c(
      review_description, "\n",
      "TITLE: ", title, "\n",
      "ABSTRACT: ", abstract
    )
  )

  # Instruct GPT to generate criteria
  if (.verbose) { cli::cli_progress_step("GPT generating criteria") }
  conversation <- add_message(
    conversation,
    role = "system",
    content = "You must work step by step. FIRST, generate a numbered list of criteria that must be met for a source to be included."
  )
  conversation <- complete_GPT_tryCatch(conversation)

  # Instruct GPT to compare source against criteria
  if (.verbose) { cli::cli_progress_step("GPT comparing source against criteria") }
  conversation <- add_message(
    conversation,
    role = "system",
    content = "NEXT, for each numbered criterion, decide whether the criterion is TRUE or FALSE for the source. It is normal for the title and abstract to not have enough information to make a clear decision for every statement. For these situations, give your best guess. After giving your response of TRUE or FALSE, give a one sentence explanation for your response."
  )
  conversation <- complete_GPT_tryCatch(conversation)

  # Instruct GPT to give its final response
  if (.verbose) { cli::cli_progress_step("GPT giving its final recommendation") }
  conversation <- add_message(
    conversation,
    role = "system",
    content = "FINALLY, consider your decisions on whether the source meets the conclusion criteria. Respond with a single word, either INCLUDE or EXCLUDE, representing your recommendation on whether the source meets the inclusion criteria. Do not write anything other than INCLUDE or EXCLUDE."
  )
  conversation <- complete_GPT_tryCatch(conversation)

  # Check recommendation is in required format
  if (! stringr::str_detect(last_message(conversation), 
                              "(INCLUDE|EXCLUDE)")) {
    if (.verbose) cli::cli_alert_warning(c("GPT's recommendation could not be parsed"))
    GPT_includes <- NA
  } else {
    GPT_includes <- stringr::str_extract(
      last_message(conversation),
      "(INCLUDE|EXCLUDE)"
    ) == "INCLUDE"
  }

  # Return result
  return(list(conversation = conversation, GPT_includes = GPT_includes))
}

#' Combine the review objective and inclusion criteria into a formatted string
#'
#' @param objective A brief description of the overall review objective, a
#' character vector of length 1
#' @param population A brief description of the overall review objective, a
#' character vector of length 1
#' @param concept A brief description of the overall review objective, a
#' character vector of length 1
#' @param context A brief description of the overall review objective, a
#' character vector of length 1
#'
#' @export
review_description <- function(objective = NULL, population = NULL, concept = NULL, context = NULL) {

  if (missing(objective) & missing(population) & missing(concept) & missing(context)) {
    cli::cli_abort("No review information provided")
  }

  review_description <- ""
  if (! missing(objective)) {
    review_description <- stringr::str_c(review_description, "STUDY OBJECTIVE: ", objective, "\n\n")
  }
  if (! missing(population)) {
    review_description <- stringr::str_c(review_description, "POPULATION:", population, "\n\n")
  }
  if (! missing(concept)) {
    review_description <- stringr::str_c(review_description, "CONCEPT:", concept, "\n\n")
  }
  if (! missing(context)) {
    review_description <- stringr::str_c(review_description, "CONTEXT:", context, "\n\n")
  }

  review_description
}

#' Screen multiple sources against review inclusion criteria
#'
#' To support multiple screening sessions, a cache of the sources list is
#' written to a file.
#'
#' @param sources A data frame of sources to screen, containing at least
#' 'title' and 'abstract' columns
#' @param review_description A description of the review including objective and
#' inclusion criteria, a character vector of length 1
#' @param n Optional, a maximum number of sources to screen
#' @param random A logical value indicating whether to randomise the order in
#' which sources are screened, defaults to TRUE
#' @param cache_file A file in which the sources list cache is kept (as RDS),
#' either fs::path() or character vector of length 1
#' @param .verbose If FALSE, progress messages will be suppressed
#'
#' @export
screen_sources <- function(sources, review_description, n = NULL, random = TRUE, cache_file = fs::path("sources_cache.rds"), .verbose = TRUE) {

  if (.verbose) cli::cli_h1("Preparing to screen sources")

  # Report the model being used
  if (.verbose) {
    model <- Sys.getenv("OPENAI_MODEL")
    cli::cli_div(theme = list(span.model = list(color = "blue")))
    cli::cli_alert_info("The model is set to {.model {model}}")
  }

  # Validate arguments
  if (missing(sources) | missing(review_description)) {
    cli::cli_abort("sources and review_description are required arguments")
  }
  if (! is.data.frame(sources)) {
    cli::cli_abort("sources must be a data frame")
  }

  if (! is.character(review_description) | ! length(review_description) == 1) {
    cli::cli_abort("review_description must be a character vector of length 1")
  }

  if (! missing(n)) {
    if (! is.numeric(n)) {
      cli::cli_abort("n must be numeric")
    }
    n <- as.integer(n)
    if (n > nrow(sources)) {
      cli::cli_warn("n ({n}) is greater than the number of rows in sources ({nrow(sources)})")
    }
  } else {
    n <- nrow(sources) 
  }

  if (! is.logical(random) | ! length(random) == 1) {
    cli::cli_abort("random must be a logical vector of length 1")
  }

  if (! (is.character(cache_file) | "fs_path" %in% class(cache_file))) {
    cli::cli_abort("cache_file must be a path")
  }

  # If cache file already exists, load and use it
  cache_file <- fs::path(cache_file)
  if (fs::file_exists(cache_file)) {
    if (.verbose) cli::cli_alert_info("Loading cached results from {cache_file}")
    cached_sources <- readRDS(cache_file)
    sources <- dplyr::left_join(sources, cached_sources)

  # If cache file does not already exist, create it
  } else {

    # Remove redundant sources, if any
    distinct_sources <- dplyr::distinct(sources)
    if (! nrow(distinct_sources) == nrow(sources)) {
      cli::cli_alert_warning("Removing {nrow(sources) - nrow(distinct_sources)} redundant source{?s}")
      sources <- distinct_sources
    }

    if (! "conversation" %in% names(sources)) {
      sources$conversation <- as.list(rep(NA_character_, nrow(sources)))
    }

    if (! "GPT_includes" %in% names(sources)) {
      sources$GPT_includes <- rep(NA, nrow(sources))
    }

    if (.verbose) cli::cli_alert_info("Creating cache file {cache_file}")
    saveRDS(sources, cache_file)

  }

  # Main loop
  while (TRUE) {

    # Gather unscreened sources
    unscreened_i <- which(is.na(sources$conversation) | is.na(sources$GPT_includes)) 
    n_unscreened <- length(unscreened_i)
    n_sources <- nrow(sources)
    n_screened <- n_sources - n_unscreened
    if (n_screened >= n | n_unscreened == 0) break
    if (.verbose) {
      if (n == n_sources) {
        cli::cli_alert_info("{n_screened} of {n_sources} screened")
      } else {
        cli::cli_alert_info("{n_screened} of {n} screened (random subset of {n_sources})")
      }
    }

    # Select a source to screen
    if (n_unscreened == 1) {
      next_i <- unscreened_i
    } else {
      next_i <- ifelse(random, sample(unscreened_i, 1), unscreened_i[1])
    }

    # Screen the source and parse the response
    response <- screen_source(review_description, 
                              title = sources$title[next_i], abstract = sources$abstract[next_i], 
                              .verbose = .verbose)
    if (is.na(response$GPT_includes) & .verbose) {
      cli::cli_alert_warning(c("GPT's recommendation could not be parsed. Discarding this response."))
      next
    }
    sources$conversation[[next_i]] <- response$conversation
    sources$GPT_includes[next_i] <- response$GPT_includes
    if (.verbose) cli::cli_alert_info(stringr::str_c(
      "GPT recommends ",
      ifelse(response$GPT_includes, "inclusion", "exclusion")
    ))

    # Sync the sources list to the cache file
    saveRDS(sources, cache_file)
  }

  # Report on current state
  unscreened <- sources[which(is.na(sources$conversation) & is.na(sources$GPT_includes)), ]
  n_unscreened <- nrow(unscreened)
  n_sources <- nrow(sources)
  if (.verbose) cli::cli_h1("Please consider contributing your scoping review data")
  if (.verbose) cli::cli_alert_info("If you've used GPTscreenR to help screen sources for a scoping review, please consider contributing some data to help monitor GPTscreenR's performance in real-world screening and guide future improvements to the package. All you need to contribute are:

- The review description 
- The list of sources that were screened at the title and abstract level, including at least:
  - The title
  - The abstract
  - The consensus decision by human reviewers to include or exclude the source

In recognition of your contribution, a citation to your review will be added to the GPTscreenR README. Of course, you are not expected to release data publicly until your review is published.

If you have any questions, or to contribute data now, send an email to {.email gptscreenr@fastmail.fm} or open a new issue at {.url https://github.com/wilkox/GPTscreenR/issues/new}.")
  if (.verbose) cli::cli_h1("Screening complete")
  if (.verbose) cli::cli_alert_success("{n_sources - n_unscreened} of {n_sources} screened")

  return(sources)
}
