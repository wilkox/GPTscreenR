#' A print method for GPT_messages
#'
#' Coloured output based on
#' \url{https://github.com/r-lib/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r}
#'
#' @export
print.GPT_messages <- function(x, ...) {

  # Check if the terminal supports coloured output
  term <- Sys.getenv()["TERM"]
  can_colour <- term %in% c("xterm-color","xterm-256color", "screen", "screen-256color")

  for (i in seq_len(length(x$role))) {
    if (can_colour) {
      cat(paste0("\033[0;31m", stringr::str_to_upper(x$role[i]), ": \033[0m"))
    } else {
      cat(paste0((stringr::str_to_upperx$role), ": "))
    }
    if (can_colour) {
      cat(paste0("\033[0;34m", x$content[i], "\033[0m"))
    } else {
      cat(x$content)
    }
    cat("\n")
  }

}

#' A validator function for GPT_messages
#'
validate_GPT_messages <- function(x) {

  values <- unclass(x)

  # Must be a list
  if (! is.list(x)) { stop("Object of class GPT_messages not a list", call. = FALSE) }

  # Must contain exactly two character vectors, named "role" and "content", of
  # equal length
  if (! length(values) == 2 | ! all(vapply(values, is.character, logical(1))) | 
      ! all(names(values) %in% c("role", "content")) | 
      ! all(vapply(values, length, integer(1)) == length(values[[1]]))) { 
    stop("Object of class GPT_messages must contain exactly two character vectors, named 'role' and 'content', of equal length", call. = FALSE)
  }

  # All 'role's must be one of 'system', 'user', or 'assistant'
  if (! all(values$role %in% c("system", "user", "assistant"))) {
    stop("Object of class GPT_messages must have all values for 'role' one of 'system', 'user', or 'assistant'", call. = FALSE)
  }

  x
}

#' A constructor function for GPT_messages
#'
new_GPT_messages <- function(x = list(role = character(), content = character())) {
  stopifnot(is.list(x))
  stopifnot(is.character(x$role))
  stopifnot(is.character(x$content))
  structure(x, class = "GPT_messages")
}

#' Helper to create GPT_messages objects
#'
GPT_messages <- function(role = character(), content = character()) {
  x <- list(role = role, content = content)
  x <- new_GPT_messages(x)
  validate_GPT_messages(x)
}

#' Convert data frame to GPT_messages object
#'
#' @export
as_GPT_messages.data.frame <- function(df = data.frame()) {
  x <- new_GPT_messages(unclass(df))
  validate_GPT_messages(x)
}

as_GPT_messages <- function(df) {
  UseMethod("as_GPT_messages")
}

#' Convert GPT_messages object to data frame
#'
#' @export
as.data.frame.GPT_messages <- function(x, ...) {
  data.frame(role = x$role, content = x$content)
}

#' Add a message to a GPT_messages object
#'
#' @export
add_message.GPT_messages <- function(x, content, role = "user") {

  # Validate inputs
  if (! is.character(role) | ! length(role) == 1 | ! role %in% c("system", "user", "assistant")) {
    stop("'role' must be one of 'system', 'user' or 'assistant'", call. = FALSE)
  }
  if (! is.character(content) | ! length(content) == 1) {
    stop("'content' must be a character vector of length 1", call. = FALSE)
  }

  # Add message
  x$role <- c(x$role, role)
  x$content <- c(x$content, content)

  # Return
  return(x)

}

add_message <- function(x, role, content) {
  UseMethod("add_message")
}

#' Remove the last message from a GPT_messages object
#'
#' @export
remove_message.GPT_messages <- function(x) {

  # Remove the last message
  x$role <- x$role[-length(x$role)]
  x$content <- x$content[-length(x$content)]

  # Return
  return(x)

}

remove_message <- function(x, role, content) {
  UseMethod("remove_message")
}

#' Return the last message from a GPT_messages object
#'
#' @export
last_message.GPT_messages <- function(x) {

  # Return the last message
  return(x$content[length(x$content)])
}

last_message <- function(x, role, content) {
  UseMethod("last_message")
}

#' Convert GPT_messages object to a human-readable vector
#'
#' @export
as.vector.GPT_messages <- function(x, mode = "any") {
  vapply(
    seq_len(length(x$role)),
    function(i) {paste0(stringr::str_to_upper(x$role[i]), ": ", x$content[i])},
    character(1)
  )
}

#' A convenience function to send a message to GPT and capture the response
#'
#' @export
say_GPT.GPT_messages <- function(x, content, role = "user") {
  x <- add_message(x, role = role, content = content)
  x <- complete_GPT(x)
  x
}

say_GPT <- function(x, content, role = "user") {
  UseMethod("say_GPT")
}
