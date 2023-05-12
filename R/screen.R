#' Screen a source against study inclusion criteria
#'
#' @param study_objective A brief description of the overall study objective, a
#' character vector of length 1
#' @param population A brief description of the overall study objective, a
#' character vector of length 1
#' @param concept A brief description of the overall study objective, a
#' character vector of length 1
#' @param context A brief description of the overall study objective, a
#' character vector of length 1
#' @param title The title of the article to be screened, a character vector of
#' length 1
#' @param abstract The abstract of the article to be screened, a character
#' vector of length 1
#' @export
screen_source <- function(study_objective, population, concept, context, title, abstract, .verbose = TRUE) {

  # Validate arguments
  validate_screening_param <- function(param, name) {
    if (missing(param)) { stop(name, " is missing", call. = FALSE) }
    if (! is.character(param) | ! length(param) == 1) { stop(name, " must be a character vector of length 1", call. = FALSE) }
    if (is.na(param) | stringr::str_length(param) == 0 ) { warning(name, " appears to have no content", call. = FALSE) }
  }
  validate_screening_param(study_objective, "study_objective")
  validate_screening_param(population, "population")
  validate_screening_param(concept, "concept")
  validate_screening_param(context, "context")
  validate_screening_param(title, "title")
  validate_screening_param(abstract, "abstract")
  if (.verbose) { cli::cli_h1("Screening source"); cli::cli_text("{.strong Title}: {title}") }

  # Initialise conversation with a system message
  if (.verbose) { cli::cli_progress_step("Initiating conversation with GPT") }
  conversation <- GPT_messages(
    role = "system",
    content = "You are being used to help researchers perform a scoping review. You are not interacting directly with a user.\n\nA scoping review is a type of systematic review used to map the published scholarship on a topic. To gather relevant sources for a scoping review, the researchers search bibliographic databases for sources that match a selected Population, Concept, and Context (the inclusion criteria). The titles and abstracts of sources that are found in this search search are then screened against the inclusion criteria.\n\nYour task is to screen a single source against the inclusion criteria. You will be provided with the study objective and inclusion criteria, and then you will then be provided with the study title and abstract. You will then be instructed to work step by step through the process of comparing the source against the inclusion criteria. Finally, you will instructed to make a recommendation on whether the source should be included.\n\nThe next message will be from the user, and will contain the scoping review objective and inclusion criteria."
  )

  # Provide the study description
  study_description <- stringr::str_c(
    "STUDY OBJECTIVE: ", study_objective, "\n\n",
    "POPULATION: ", population, "\n\n",
    "CONCEPT: ", concept, "\n\n",
    "CONTEXT: ", context
  )
  conversation <- add_message(conversation, role = "user", content = study_description)

  # Instruct GPT to summarise the inclusion criteria as dot point statements
  if (.verbose) { cli::cli_progress_step("Asking GPT to summarise inclusion criteria") }
  conversation <- say_GPT(
    conversation,
    role = "system",
    content = "Let's work step by step. First, generate a numbered list of statements that summarise the inclusion criteria for the scoping review, including the Population, Concept, and Context. The statements should be clear, comprehensive and complete. Any source for which all the statements are true is a source that meets the inclusion criteria. As a template, here are some example statements (these are a generic set of examples that are not related to the current scoping review):\n\n1. The source reports the results of a randomised control trial\n2. The source reports the results of a study in which:\n  2a. The participants were all male; AND\n  2b. The participants were all aged between 18 and 74 inclusive\n3. The source reports the results of a study conducted in the European Union.\n\nAspects of the inclusion criteria with multiple elements should be broken down into separate points where possible. For example, instead of:\n\n1. The source reports on a study of men who live in the European Union.\n\nYou should instead say:\n\n1. The source reports on a study of people who are:\n1a. Male; and\n1b: Living in the European Union."
  )

  # Provide the source
  conversation <- add_message(conversation, role = "system", "The next message will be from the user, and will contain the title and abstract of a study to be compared against the inclusion criteria.")
 source <- stringr::str_c(
    "TITLE: ", title, "\n",
    "ABSTRACT: ", abstract
  )
  conversation <- add_message(conversation, role = "user", content = source)

  # Ask GPT to compare the study title and abstract against the summarised inclusion criteria
  if (.verbose) { cli::cli_progress_step("Asking GPT to compare the source to the inclusion criteria") }
  conversation <- say_GPT(
    conversation,
    role = "system",
    content = "Let's continue to work step by step. Refer back to the set of statements you developed summarising the inclusion criteria. For each statement, decide whether or not the statement is true for the study described by the title and abstract. You must select from the following permitted responses: TRUE, FALSE, LIKELY TRUE, LIKELY FALSE, or NOT APPLICABLE. No other response is permitted. It is normal for the title and abstract to not have enough information to make a clear decision for every statement. There is a natural and normal amount of ambiguity in this process. For these situations, give your best guess, making use of your general knowledge, and deciding LIKELY TRUE or LIKELY FALSE. Responses like UNCLEAR or NOT ENOUGH INFORMATION are not permitted. After giving your response, give a one sentence explanation for your response."
  )

  # Ask GPT for its final recommendation
  if (.verbose) { cli::cli_progress_step("Asking GPT to provide a final recommendation") }
  conversation <- say_GPT(
    conversation,
    role = "system",
    content = "Let's continue to work step by step. Consider your decisions on whether the title and abstract meet the conclusion criteria. Overall, is it likely true that the source meets the inclusion criteria? Reply with a single word, either INCLUDE or EXCLUDE, representing your recommendation on whether the source is likely to meet the inclusion criteria. You must reply with a single word only and it must be one of these two words; any other reply will cause the automatic parsing of your response to fail, which will be troublesome for the user."
  )

  # Check recommendation is in required format
  recommendation <- last_message(conversation)
  if (! stringr::str_detect(recommendation, "^(INCLUDE|EXCLUDE)$")) {
    warning(str_c("GPT's recommendation could not be parsed: ", recommendation, call. = FALSE))
  }

  # Return result
  return(list(
    conversation = list(conversation),
    recommendation = recommendation
  ))
}
