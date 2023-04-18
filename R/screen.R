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

  # Initialise conversation with a system message
  GPT_instructions <- "You are being used to help researchers automate the process of performing a scoping review. You are not interacting directly with a user.\n\nA scoping review is a type of scholarly review used to map the published scholarship on a particular topic. In a scoping review, a search of bibliographic databases is performed looking for sources that match a selected Population, Concept, and Context (the inclusion criteria). The titles and abstracts of sources that are identified from this search are then screened for whether they match the inclusion criteria.\n\nYour task will be to screen a single source for whether it matches the inclusion criteria. You will be provided with the study objective and inclusion criteria, and then you will then be provided with the study title and abstract. You will then be instructed to work step by step through the process of comparing the source against the inclusion criteria, then to make a final recommendation on whether the source should be included."
  messages <- data.frame(role = "system", content = GPT_instructions)

  # Provide the study description
  study_description <- str_c(
    "STUDY OBJECTIVE: ", study_objective, "\n",
    "POPULATION: ", population, "\n",
    "CONCEPT: ", concept, "\n",
    "CONTEXT: ", context
  )
  messages <- rbind(messages, data.frame(role = "user", content = study_description))

  # Provide the source
  source <- str_c(
    "TITLE: ", title, "\n",
    "ABSTRACT: ", abstract
  )
  messages <- rbind(messages, data.frame(role = "user", content = source))

  # Ask GPT to consider the source population, then compare it against the
  # inclusion criteria
  population_instructions <- "Reply with a paragraph describing the population included in the source, to the extent possible from the available information. It should end by comparing the population against the population from the inclusion criteria. If uncertain, you must always err on the side of inclusion, bearing in mind that you will be conducting your screening in parallel with human reviewers."
  messages <- rbind(messages, data.frame(role = "system", content = population_instructions))
  population_reply <- create_chat_completion(messages)
  messages <- rbind(messages, data.frame(role = "assistant", content = population_reply))

  # Ask GPT to consider the source context, then compare it against the
  # inclusion criteria
  context_instructions <- "Reply with a paragraph describing the context included in the source, to the extent possible from the available information. It should end by comparing the context against the context from the inclusion criteria. If uncertain, you must always err on the side of inclusion, bearing in mind that you will be conducting your screening in parallel with human reviewers."
  messages <- rbind(messages, data.frame(role = "system", content = context_instructions))
  context_reply <- create_chat_completion(messages)
  messages <- rbind(messages, data.frame(role = "assistant", content = context_reply))

  # Ask GPT to consider the source concept, then compare it against the
  # inclusion criteria
  concept_instructions <- "Reply with a paragraph describing the concept included in the source, to the extent possible from the available information. It should end by comparing the concept against the concept from the inclusion criteria. If uncertain, you must always err on the side of inclusion, bearing in mind that you will be conducting your screening in parallel with human reviewers."
  messages <- rbind(messages, data.frame(role = "system", content = concept_instructions))
  concept_reply <- create_chat_completion(messages)
  messages <- rbind(messages, data.frame(role = "assistant", content = concept_reply))

  # Ask GPT to provide an explanation for whether the study should or should
  # not be included, without yet providing a final decision
  explanation_instructions <- "Reply with a paragraph summarising how the source population, context and concept compare to the inclusion criteria. Do not provide a final recommendation on inclusion, just reasons for or against inclusion. If uncertain, you must always err on the side of inclusion, bearing in mind that you will be conducting your screening in parallel with human reviewers."
  messages <- rbind(messages, data.frame(role = "system", content = explanation_instructions))
  explanation_reply <- create_chat_completion(messages)
  messages <- rbind(messages, data.frame(role = "assistant", content = concept_reply))

  # Ask GPT for its final recommendation
  recommendation_instructions <- "Reply with a single word, either 'INCLUDE' or 'EXCLUDE', representing your recommendation on whether the source meets the inclusion criteria. You must reply with a single word only and it must be one of these two words; any other reply will cause the automatic parsing of your response to fail, which will be troublesome for the user. If uncertain, you must always err on the side of inclusion, bearing in mind that you will be conducting your screening in parallel with human reviewers."
  messages <- rbind(messages, data.frame(role = "system", content = recommendation_instructions))
  recommendation_reply <- create_chat_completion(messages)
  messages <- rbind(messages, data.frame(role = "assistant", content = recommendation_reply))

  # Check recommendation is in required format
  if (! stringr::str_detect(recommendation_reply, "^(INCLUDE|EXCLUDE)$")) {
    warning(str_c("GPT's recommendation could not be parsed: ", recommendation_reply, call. = FALSE))
  }

  # Return result
  return(list(
    messages = list(messages),
    recommendation = recommendation_reply
  ))
}
