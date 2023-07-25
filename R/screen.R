#' Screen a source against study inclusion criteria
#'
#' @param study_description A description of the study including objective and
#' inclusion criteria, a character vector of length 1
#' @param title The title of the article to be screened, a character vector of
#' length 1
#' @param abstract The abstract of the article to be screened, a character
#' vector of length 1
#' @param .verbose If FALSE, progress messages will be suppressed
#' @param .dry_run If TRUE, calls to the GPT API will be skipped
#' @export
screen_source <- function(study_description, title, abstract, .verbose = TRUE, .dry_run = FALSE) {

  # Validate arguments
  validate_screening_param <- function(param, name) {
    if (missing(param)) { cli::cli_abort(name, " is missing") }
    if (! is.character(param) | ! length(param) == 1) { cli::cli_abort("{name} must be a character vector of length 1") }
    if (is.na(param) | stringr::str_length(param) == 0 ) { cli::cli_warn("{name} appears to have no content") }
  }
  validate_screening_param(study_description, "study_description")
  validate_screening_param(title, "title")
  validate_screening_param(abstract, "abstract")
  if (.verbose) { cli::cli_h2("Screening: {title}") }

  # Initialise conversation with a system message
  conversation <- GPT_messages(
    role = "system",
    content = "You are being used to help researchers perform a scoping review. You are not interacting directly with a user.\n\nA scoping review is a type of systematic review used to map the published scholarship on a topic. To gather relevant sources for a scoping review, the researchers search bibliographic databases for sources that match a selected Population, Concept, and Context (the inclusion criteria). The titles and abstracts of sources that are found in this search search are then screened against the inclusion criteria.\n\nYour task is to screen a single source against the inclusion criteria. You will be provided with the study objective and inclusion criteria, and then you will then be provided with the study title and abstract. You will then be instructed to work step by step through the process of comparing the source against the inclusion criteria. Finally, you will instructed to make a recommendation on whether the source should be included.\n\nThe next message will be from the user, and will contain the scoping review objective and inclusion criteria."
  )

  # Provide the study description
  conversation <- add_message(conversation, role = "user", content = study_description)

  # Instruct GPT to summarise the inclusion criteria as dot point statements
  if (.verbose) { cli::cli_progress_step("Asking GPT to summarise inclusion criteria") }
  conversation <- add_message(
    conversation,
    role = "system",
    content = "Let's work step by step. First, generate a numbered list of statements that summarise the inclusion criteria for the scoping review, including the Population, Concept, and Context. The statements should be clear, comprehensive and complete. Any source for which all the statements are true is a source that meets the inclusion criteria. As a template, here are some example statements (these are a generic set of examples that are not related to the current scoping review):\n\n1. The source reports the results of a randomised control trial\n2. The source reports the results of a study in which:\n  2a. The participants were all male; AND\n  2b. The participants were all aged between 18 and 74 inclusive\n3. The source reports the results of a study conducted in the European Union.\n\nAspects of the inclusion criteria with multiple elements should be broken down into separate points where possible. For example, instead of:\n\n1. The source reports on a study of men who live in the European Union.\n\nYou should instead say:\n\n1. The source reports on a study of people who are:\n1a. Male; and\n1b: Living in the European Union."
  )
  if (! .dry_run) conversation <- complete_GPT_tryCatch(conversation, .dry_run = .dry_run)

  # Provide the source
  conversation <- add_message(conversation, role = "system", "The next message will be from the user, and will contain the title and abstract of a study to be compared against the inclusion criteria.")
 source <- stringr::str_c(
    "TITLE: ", title, "\n",
    "ABSTRACT: ", abstract
  )
  conversation <- add_message(conversation, role = "user", content = source)

  # Ask GPT to compare the study title and abstract against the summarised inclusion criteria
  if (.verbose) { cli::cli_progress_step("Asking GPT to compare the source to the inclusion criteria") }
  conversation <- add_message(
    conversation,
    role = "system",
    content = "Let's continue to work step by step. Refer back to the set of statements you developed summarising the inclusion criteria. For each statement, decide whether or not the statement is true for the study described by the title and abstract. You must select from the following permitted responses: TRUE, FALSE, LIKELY TRUE, LIKELY FALSE, or NOT APPLICABLE. No other response is permitted. It is normal for the title and abstract to not have enough information to make a clear decision for every statement. There is a natural and normal amount of ambiguity in this process. For these situations, give your best guess, making use of your general knowledge, and deciding LIKELY TRUE or LIKELY FALSE. Responses like UNCLEAR or NOT ENOUGH INFORMATION are not permitted. After giving your response, give a one sentence explanation for your response."
  )
  if (! .dry_run) conversation <- complete_GPT_tryCatch(conversation, .dry_run = .dry_run)

  # Ask GPT for its final recommendation
  if (.verbose) { cli::cli_progress_step("Asking GPT to provide a final recommendation") }
  conversation <- add_message(
    conversation,
    role = "system",
    content = "Let's continue to work step by step. Consider your decisions on whether the title and abstract meet the conclusion criteria. Overall, is it likely true that the source meets the inclusion criteria? Reply with a single word, either INCLUDE or EXCLUDE, representing your recommendation on whether the source is likely to meet the inclusion criteria. You must reply with a single word only and it must be one of these two words; any other reply will cause the automatic parsing of your response to fail, which will be troublesome for the user."
  )
  if (! .dry_run) conversation <- complete_GPT_tryCatch(conversation, .dry_run = .dry_run)

  # Check recommendation is in required format
  if (.dry_run) {
    recommendation <- NA
  } else {
    recommendation <- last_message(conversation)
    if (! stringr::str_detect(recommendation, "^(INCLUDE|EXCLUDE)$")) {
      cli::cli_warn(c("GPT's recommendation could not be parsed:", recommendation))
    }
  }

  # Return result
  return(list(conversation = conversation, recommendation = recommendation))
}

#' Combine study objective and inclusion criteria into a formatted string
#'
#' @param objective A brief description of the overall study objective, a
#' character vector of length 1
#' @param population A brief description of the overall study objective, a
#' character vector of length 1
#' @param concept A brief description of the overall study objective, a
#' character vector of length 1
#' @param context A brief description of the overall study objective, a
#' character vector of length 1
study_description <- function(objective = NULL, population = NULL, concept = NULL, context = NULL) {

  if (missing(objective) & missing(population) & missing(concept) & missing(context)) {
    cli::cli_abort("No study information provided")
  }

  study_description <- ""
  if (! missing(objective)) {
    study_description <- stringr::str_c(study_description, "STUDY OBJECTIVE: ", objective, "\n\n")
  }
  if (! missing(population)) {
    study_description <- stringr::str_c(study_description, "POPULATION:", population, "\n\n")
  }
  if (! missing(concept)) {
    study_description <- stringr::str_c(study_description, "CONCEPT:", concept, "\n\n")
  }
  if (! missing(context)) {
    study_description <- stringr::str_c(study_description, "CONTEXT:", context, "\n\n")
  }

  study_description
}

#' Screen multiple sources against study inclusion criteria
#'
#' To support multiple screening sessions, a cache of the sources list is
#' written to a file.
#'
#' @param sources A data frame of sources to screen, containing at least
#' 'title' and 'abstract' columns
#' @param study_description A description of the study including objective and
#' inclusion criteria, a character vector of length 1
#' @param n Optional, a maximum number of sources to screen
#' @param random A logical value indicating whether to randomise the order in
#' which sources are screened, defaults to TRUE
#' @param cache_file A file in which the sources list cache is kept (as RDS),
#' either fs::path() or character vector of length 1
#' @param .verbose If FALSE, progress messages will be suppressed
#' @param .dry_run If TRUE, calls to the GPT API will be skipped
screen_sources <- function(sources, study_description, n = NULL, random = TRUE, cache_file = fs::path("sources_cache.rds"), .verbose = TRUE, .dry_run = FALSE) {

  # Validate arguments
  if (missing(sources) | missing(study_description)) {
    cli::cli_abort("sources and study_description are required arguments")
  }
  if (! is.data.frame(sources)) {
    cli::cli_abort("sources must be a data frame")
  }

  if (! is.character(study_description) | ! length(study_description) == 1) {
    cli::cli_abort("study_description must be a character vector of length 1")
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
    if (.verbose) cli::cli_alert_info("Loading sources from cache in {cache_file} ({.arg sources} will be ignored)")
    sources <- readRDS(cache_file)

  # If cache file does not already exist, create it
  } else {

    # Remove redundant sources, if any
    distinct_sources <- dplyr::distinct(sources)
    if (! nrow(distinct_sources) == nrow(sources)) {
      cli::cli_alert_warning("Removing {nrow(sources) - nrow(distinct_sources)} redundant source{?s}")
      sources <- distinct_sources
    }

    if (! "GPT_conversation" %in% names(sources)) {
      sources$GPT_conversation <- as.list(rep(NA_character_, nrow(sources)))
    }

    if (! "GPT_recommendation" %in% names(sources)) {
      sources$GPT_recommendation <- rep(NA_character_, nrow(sources))
    }

    if (.verbose) cli::cli_alert_info("Creating cache file {cache_file}")
    if (! .dry_run) saveRDS(sources, cache_file)

  }

  # Main loop
  while (TRUE) {

    # Gather unscreened sources
    unscreened_i <- which(is.na(sources$GPT_conversation) | is.na(sources$GPT_recommendation)) 
    n_unscreened <- length(unscreened_i)
    n_sources <- nrow(sources)
    if (n_sources - n_unscreened >= n | n_unscreened == 0) break
    if (.verbose) cli::cli_alert_info("{n_sources - n_unscreened} of {n_sources} screened")

    # Select a source to screen
    if (n_unscreened == 1) {
      next_i <- unscreened_i
    } else {
      next_i <- ifelse(random, sample(unscreened_i, 1), unscreened_i[1])
    }

    # Screen the source and parse the response
    response <- screen_source(study_description, 
                              title = sources$title[next_i], abstract = sources$abstract[next_i], 
                              .verbose = .verbose, .dry_run = .dry_run)
    sources$GPT_conversation[[next_i]] <- response$conversation
    if (.dry_run) response$recommendation <- "INCLUDE"
    sources$GPT_recommendation[next_i] <- response$recommendation
    if (.verbose) cli::cli_alert_info("Recommendation: {response$recommendation}")

    # Sync the sources list to the cache file
    if (! .dry_run) saveRDS(sources, cache_file)
  }

  # Report on current state and exit
  unscreened <- sources[which(is.na(sources$GPT_conversation) & is.na(sources$GPT_recommendation)), ]
  n_unscreened <- nrow(unscreened)
  n_sources <- nrow(sources)
  if (.verbose) cli::cli_alert_success("{n_sources - n_unscreened} of {n_sources} screened")

  return(sources)
}
