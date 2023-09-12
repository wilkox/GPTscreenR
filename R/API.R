#' Create chat completions with the OpenAI API
#'
#' API endpoint is documented at
#' \url{https://platform.openai.com/docs/api-reference/chat/create}
#'
#' @param x A GPT_messages object
#' @param .dry_run If TRUE, will not make an actual call to the GPT API
complete_GPT.GPT_messages <- function(x, .dry_run = FALSE) {

  # Must be a valid GPT_messages object
  x <- validate_GPT_messages(x)

  # Retrieve and set API key
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  if (stringr::str_length(openai_api_key) == 0 | is.na(openai_api_key)) {
    cli::cli_abort("Cannot find environmental variable {.envvar OPENAI_API_KEY}")
  }

  # Retrieve and set model
  model <- Sys.getenv("OPENAI_MODEL")
  if (stringr::str_length(model) == 0 | is.na(model)) {
    cli::cli_abort("Cannot find environmental variable {.envvar OPENAI_MODEL}")
  }

  # Set up params
  params <- list(
    model = model,
    messages = as.data.frame(x)
  )

  # POST to chat completion endpoint
  if (.dry_run) {
    response <- data.frame(status_code = 200, content = "This is a placeholder response.")

  } else {
    response <- httr::POST(
      "https://api.openai.com/v1/chat/completions",
      httr::add_headers("Authorization" = paste("Bearer", openai_api_key)),
      httr::content_type_json(),
      body = jsonlite::toJSON(params, auto_unbox = TRUE)
    )
  }

  # Check status code of response
  if (! response$status_code %in% 200:299) {
    cli::cli_abort(c(
        "The OpenAI API returned an error:",
        "!" = "  Code: {response$status_code}",
        "!" = "  Type: {httr::content(response)$error$type}",
        "!" = "  Message: {httr::content(response)$error$message}"
    ))
  }

  # Extract and return message content
  if (.dry_run) {
    content <- response$content
  } else {
    content <- httr::content(response)$choices[[1]]$message$content
  }
  x <- add_message(x, role = "assistant", content = content)
  return(x)
}

complete_GPT <- function(x, .dry_run) {
  UseMethod("complete_GPT")
}

#' tryCatch-buffered complete_GPT
#'
#' @param x A GPT_messages object
#' @param tries How many times to try before giving up
#' @param .dry_run If TRUE, will not make an actual call to the GPT API
#'
complete_GPT_tryCatch <- function(x, tries = 3, .dry_run = FALSE) {

  while (tries > 0) {
    r <- tryCatch(
      list(result = complete_GPT(x, .dry_run = .dry_run), error = NULL),
      error = function(e) list(result = NULL, error = e)
    )
    if (is.null(r$result)) {
      cli::cli_alert_warning("GPT API call failed:")
      cli::cli_alert_warning(r$error$message)
      for (b in r$error$body) cli::cli_alert_warning(b)
      tries <- tries - 1
      if (tries > 0) cli::cli_alert_warning("Retrying ({tries} tr{?y/ies} remaining)")

    } else {
      return(r$result)
    }
  }
  cli::cli_abort("Too many failed calls to GPT API")
}

#' Set the GPT model to use
#'
set_model <- function() {
  model <- "gpt-4"
  Sys.setenv(OPENAI_MODEL = model)
  msg <- paste0(
    cli::col_blue("{cli::symbol$info}"),
    " The model is set to ",
    cli::col_blue(model)
  )
  rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
}

#' Check that the OPENAI_API_KEY environmental variable is set
#'
#' @param show_key If TRUE, and if the environmental variable is set, the key
#' will be printed to the console. Defaults to FALSE.
#'
#' @export
check_API_key <- function(show_key = FALSE) {

  key <- Sys.getenv("OPENAI_API_KEY")

  if (key == "") {
    msg <- paste0(
      cli::col_red("{cli::symbol$cross}"),
      " The {.envvar OPENAI_API_KEY} environmental variable is not set\n",
      cli::col_blue("{cli::symbol$info}"),
      " This is required for GPTscreenR to access the OpenAI API\n",
      cli::col_blue("{cli::symbol$info}"),
      " Run {.code vignette(\"api-key\")} for instructions"
    )
    rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
    return()
  }

  msg <- paste0(
    cli::col_green("{cli::symbol$tick}"),
    " The {.envvar OPENAI_API_KEY} environmental variable is set"
  )
  if (show_key) msg <- paste0(
    msg,
    "\n",
    cli::col_blue("{cli::symbol$info}"),
    " Key: ",
    "{.val {key}}"
  )
  rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
}
