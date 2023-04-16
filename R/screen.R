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
screen_source <- function(study_objective, population, concept, context, title, abstract) {

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

  # The prompt for ChatGPT
  prompt <- "You are being used to help researchers automate the process of performing a scoping review. A scoping review is a type of scholarly review used to map the published scholarship on a particular topic. In a scoping review, a search of bibliographic databases is performed looking for sources that match a selected Population, Concept, and Context (the inclusion criteria). The titles and abstracts of sources that are identified from this search are then screened for whether they match the inclusion criteria. Your task now is to screen a single source for whether it matches the inclusion criteria. You will be provided with the inclusion criteria soon. You are instructed to respond with two concise paragraphs. The first paragraph should begin with 'DECISION:' followed by either 'INCLUDE' or 'EXCLUDE'. The second paragraph should begin with 'EXPLANATION:' followed by a BRIEF (1-2 sentence MAXIMUM) explanation for your decision. You are instructed to err on the side of inclusion if uncertain, as this screening stage will be followed by human appraisal of sources. You will be performing this screening in parallel with at least one human researcher and any source that is selected for inclusion by your or at least one human researcher will be included."

  # Construct the request
  request <- stringr::str_c(
    prompt, "\\n",
    "STUDY OBJECTIVE: ", study_objective, "\\n",
    "POPULATION: ", population, "\\n",
    "CONCEPT: ", concept, "\\n",
    "CONTEXT: ", context, "\\n",
    "TITLE: ", title, "\\n",
    "ABSTRACT: ", abstract
  )

  # Send request
  invisible(utils::capture.output(response <- chatgpt::ask_chatgpt(request)))

  # Check and parse response
  response_failed <- FALSE
  decision <- stringr::str_match(response, "^DECISION: (INCLUDE|EXCLUDE)")[1, 2]
  if (is.na(decision) | ! decision %in% c("INCLUDE", "EXCLUDE")) {
    response_failed <- "No clear include/exclude decision in response"
  }
  explanation <- stringr::str_match(response, "EXPLANATION: (.+)")[1, 2]
  if (is.na(explanation)) {
    response_failed <- "No clear explanation in response"
  }

  if (! response_failed == FALSE) {
    decision <- NA
    explanation <- stringr::str_c("ChatGPT response failed for following reason: ", response_failed)
  }

  # Return result
  return(list(decision = decision, explanation = explanation))
}

#' 
