% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/screen.R
\name{screen_source}
\alias{screen_source}
\title{Screen a source against review inclusion criteria}
\usage{
screen_source(
  review_description,
  title,
  abstract,
  service = "openaichat",
  model = "gpt-4o",
  .verbose = TRUE
)
}
\arguments{
\item{review_description}{A description of the review, including objective
and inclusion criteria, a character vector of length 1}

\item{title}{The title of the article to be screened, a character vector of
length 1}

\item{abstract}{The abstract of the article to be screened, a character
vector of length 1}

\item{service}{The LLM service to use. Defaults to 'openaichat'. See the
lemur package for full details.}

\item{model}{The LLM model to use. Defaults to 'gpt-4o'. See the lemur
package for full details.}

\item{.verbose}{If FALSE, progress messages will be suppressed}
}
\description{
Screen a source against review inclusion criteria
}
