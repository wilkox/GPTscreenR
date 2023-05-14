#' Create chat completions with the OpenAI API
#'
#' API endpoint is documented at
#' \url{https://platform.openai.com/docs/api-reference/chat/create}
#'
#' @param x A GPT_messages object
#' @export
complete_GPT.GPT_messages <- function(x) {

  # Must be a valid GPT_messages object
  x <- validate_GPT_messages(x)

  # Retrieve and set API key
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  if (stringr::str_length(openai_api_key) == 0 | is.na(openai_api_key)) {
    stop("Cannot find environmental variable OPENAI_API_KEY", call. = FALSE)
  }

  # Set model
  model <- "gpt-4"

  # Set up params
  params <- list(
    model = model,
    messages = as.data.frame(x)
  )

  # POST to chat completion endpoint
  response <- httr::POST(
    "https://api.openai.com/v1/chat/completions",
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key)),
    httr::content_type_json(),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  if (! response$status_code %in% 200:299) {
    stop(
      stringr::str_c(
        "The OpenAI API returned an error code ",
        response$status_code,
        ": ",
        httr::content(response)$error$type,
        ": ",
        httr::content(response)$error$message
      ),
      call. = FALSE
    )
  }

  # Extract and return message content
  content <- httr::content(response)$choices[[1]]$message$content
  x <- add_message(x, role = "assistant", content = content)
  return(x)
}

complete_GPT <- function(x) {
  UseMethod("complete_GPT")
}
