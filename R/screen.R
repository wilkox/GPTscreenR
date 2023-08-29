#' Screen a source against review inclusion criteria
#'
#' @param review_description A description of the review, including objective
#' and inclusion criteria, a character vector of length 1
#' @param title The title of the article to be screened, a character vector of
#' length 1
#' @param abstract The abstract of the article to be screened, a character
#' vector of length 1
#' @param .verbose If FALSE, progress messages will be suppressed
#' @param .dry_run If TRUE, calls to the GPT API will be skipped
#'
#' @export
screen_source <- function(review_description, title, abstract, .verbose = TRUE, .dry_run = FALSE) {

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
  conversation <- GPT_messages(
    role = "system",
    content = 
"You are being used to help researchers perform a scoping review. You are not interacting directly with a user.

A scoping review is a type of systematic review used to map the published scholarship on a topic. To gather relevant sources for a scoping review, the researchers search bibliographic databases for sources that might be relevant to the review, often using the Population, Concept, and Context framework. The titles and abstracts of sources that are found in this search are then screened against the review inclusion and exclusion criteria.

Your task is to screen a single source against the study criteria. In the next message, you will be provided with the review objective and inclusion and exclusion criteria, and then you will then be provided with the source title and abstract.

To screen the source, you must work step by step. First, generate a numbered list of statements that summarise the inclusion and exclusion criteria for the scoping review, including the Population, Concept, and Context if they are provided. The statements should be clear, comprehensive and complete. Any source for which all the statements are true is a source that meets the inclusion criteria. As a template, here are some example statements (these are a generic set of examples that are not related to the current scoping review):

1. The source reports the results of a randomised control trial
2. The source reports the results of a study in which:
  2a. The participants were all male; AND
  2b. The participants were all aged between 18 and 74 inclusive
3. The source reports the results of a study conducted in the European Union.

Aspects of the inclusion criteria with multiple elements should be broken down into separate points where possible. For example, instead of:

1. The source reports on a study of men who live in the European Union.

You should instead say:

1. The source reports on a study of people who are:
  1a. Male; and
  1b: Living in the European Union.

Then, continue to work step by step. Refer back to the set of statements you developed summarising the inclusion criteria. For each statement, decide whether or not the statement is true for the source described by the title and abstract. You must select from the following permitted responses: TRUE, FALSE, LIKELY TRUE, LIKELY FALSE, or NOT APPLICABLE. No other response is permitted. It is normal for the title and abstract to not have enough information to make a clear decision for every statement. There is a natural and normal amount of ambiguity in this process. For these situations, give your best guess, making use of your general knowledge, and deciding LIKELY TRUE or LIKELY FALSE. Responses like UNCLEAR or NOT ENOUGH INFORMATION are not permitted. After giving your response, give a one sentence explanation for your response. For example:

1. TRUE. The abstract describes the study design as a randomised control trial.
2a. TRUE. The abstract mentions that all the participants were male.
2b. TRUE. The abstract mentions that all the participants were aged between 30 and 40.
3. LIKELY TRUE. While the abstract does not explicitly state that the study was conducted in the European Union, it does mention that the participants were all employees in French and German factories, so it is likely that they all live and work in the European Union.

Finally, consider your decisions on whether the title and abstract meet the conclusion criteria. Overall, is it likely true that the source meets the inclusion criteria? End your response with a single word on a new line, either INCLUDE or EXCLUDE, representing your recommendation on whether the source is likely to meet the inclusion criteria. The response must end with a line containing only one of these two words; any other reply will cause the automatic parsing of your response to fail, which will be troublesome for the user. The response must not end with a blank line."
  )

  # Provide the review description and source
  if (.verbose) { cli::cli_progress_step("Initiating conversation with GPT") }
  conversation <- add_message(
    conversation,
    role = "user",
    content = stringr::str_c(
      review_description, "\n",
      "TITLE: ", title, "\n",
      "ABSTRACT: ", abstract
    )
  )

  # Ask GPT for its response
  if (.verbose) { cli::cli_progress_step("Waiting for GPT response") }
  if (! .dry_run) conversation <- complete_GPT_tryCatch(
    conversation,
    .dry_run = .dry_run
  )

  # Check recommendation is in required format
  if (.dry_run) {
    GPT_includes <- NA
  } else if (! stringr::str_detect(last_message(conversation), 
                              "(INCLUDE|EXCLUDE)$")) {
    if (.verbose) cli::cli_alert_warning(c("GPT's recommendation could not be parsed"))
    GPT_includes <- NA
  } else {
    GPT_includes <- stringr::str_extract(
      last_message(conversation),
      "(INCLUDE|EXCLUDE)$"
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
#' @param .dry_run If TRUE, calls to the GPT API will be skipped
#'
#' @export
screen_sources <- function(sources, review_description, n = NULL, random = TRUE, cache_file = fs::path("sources_cache.rds"), .verbose = TRUE, .dry_run = FALSE) {

  # Report the model being used
  if (.verbose) {
    model <- Sys.getenv("OPENAI_MODEL")
    msg <- paste0(
      cli::col_green("{cli::symbol$info}"),
      " The model is set to ",
      cli::col_blue(model)
    )
    rlang::inform(cli::format_inline(msg))
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
  if (.verbose) cli::cli_h1("Preparing sources list")
  cache_file <- fs::path(cache_file)
  if (fs::file_exists(cache_file) & ! .dry_run) {
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
    if (! .dry_run) saveRDS(sources, cache_file)

  }

  # Main loop
  while (TRUE) {

    # Gather unscreened sources
    unscreened_i <- which(is.na(sources$conversation) | is.na(sources$GPT_includes)) 
    n_unscreened <- length(unscreened_i)
    n_sources <- nrow(sources)
    if (n_sources - n_unscreened >= n | n_unscreened == 0) break
    if (.verbose) {
      if (missing(n)) {
        cli::cli_alert_info("{n_sources - n_unscreened} of {n_sources} screened")
      } else {
        cli::cli_alert_info("{n_sources - n_unscreened} of {n} screened (random subset of {n_sources})")
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
                              .verbose = .verbose, .dry_run = .dry_run)
    if (is.na(response$GPT_includes) & ! .dry_run & .verbose) {
      cli::cli_alert_warning(c("GPT's recommendation could not be parsed. Discarding this response."))
      next
    }
    sources$conversation[[next_i]] <- response$conversation
    if (.dry_run) response$GPT_includes <- TRUE
    sources$GPT_includes[next_i] <- response$GPT_includes
    if (.verbose) cli::cli_alert_info(stringr::str_c(
      "GPT recommends ",
      ifelse(response$GPT_includes, "inclusion", "exclusion")
    ))

    # Sync the sources list to the cache file
    if (! .dry_run) saveRDS(sources, cache_file)
  }

  # Report on current state and exit
  unscreened <- sources[which(is.na(sources$conversation) & is.na(sources$GPT_includes)), ]
  n_unscreened <- nrow(unscreened)
  n_sources <- nrow(sources)
  if (.verbose) cli::cli_alert_success("{n_sources - n_unscreened} of {n_sources} screened")

  return(sources)
}
