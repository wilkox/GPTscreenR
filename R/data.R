#' Example review description for source screening
#'
#' Description of a hypothetical scoping review.
#' 
#' @format
#' A list with four elements
#' \describe{
#'   \item{objective}{The objective of the review, a character vector of length 1}
#'   \item{population}{The review population, a character vector of length 1}
#'   \item{context}{The review context, a character vector of length 1}
#'   \item{concept}{The review concept, a character vector of length 1}
#' }
#'
#' @seealso alpaca_sources alpaca_results
"alpaca_review_description"

#' Example sources for screening
#'
#' A data frame of sources to be screened for a hypothetical scoping review.
#' 
#' @format
#' A data frame with ten rows and three columns:
#' \describe{
#'   \item{title}{The source title}
#'   \item{abstract}{The source abstract}
#'   \item{include}{Whether the source meets the description given in
#'   `alpaca_review_description`}
#' }
#'
#' @seealso alpaca_review_description alpaca_results
"alpaca_sources"

#' Example results of source screening
#'
#' A data frame of example results from screening sources with the `screen_sources()` function.
#'
#' @format
#' A data frame with ten rows and five columns:
#' \describe{
#'   \item{title}{The source title}
#'   \item{abstract}{The source abstract}
#'   \item{include}{Whether the source meets the description given in `alpaca_review_description`}
#'   \item{GPT_conversation}{The conversation with GPT in which it is lead
#'   through considering the source and making a recommendation to include or
#'   exclude, a list-column of `GPT_mssg` objects}
#'   \item{GPT_recommendation}{GPT's recommendation, either the word 'INCLUDE'
#'   or 'EXCLUDE'}
#' }
#'
#' @seealso alpaca_review_description alpaca_sources
"alpaca_results"
