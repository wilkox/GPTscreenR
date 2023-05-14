#' Example inclusion criteria for source screening
#'
#' Inclusion criteria for a hypothetical scoping review
#' 
#' @format
#' A list with four elements
#' \describe{
#'   \item{study_objective}{The objective of the review, a character vector of length 1}
#'   \item{population}{The review population, a character vector of length 1}
#'   \item{context}{The review context, a character vector of length 1}
#'   \item{concept}{The review concept, a character vector of length 1}
#' }
#'
#' @seealso alpaca_sources
"alpaca_inclusion_criteria"

#' Example sources for screening
#'
#' Sources to be screened for a hypothetical scoping review
#' 
#' @format
#' A list with ten elements, each contaning:
#' \describe{
#'   \item{title}{The source title, a character vector of length 1}
#'   \item{abstract}{The source abstract, a character vector of length 1}
#'   \item{include}{Whether the source meets the inclusion criteria given in `alpaca_inclusion_criteria`, a logical vector of length 1}
#' }
#'
#' @seealso alpaca_inclusion_criteria
"alpaca_sources"
