library(tidyverse)
load_all()

sources <- readRDS("./COVID_misinfo_sources.rds")

convo_to_convo <- function(convo) {
  if (is.na(convo)) return(NA)
  convo <- str_split(convo, "\\\n\\\n\\* \\* \\*\\\n\\\n")[[1]]
  GPT_convo <- GPT_messages()
  for (m in convo) {
    role <- str_extract(m, "^(SYSTEM|USER|ASSISTANT)")
    if (is.na(role)) cli::cli_abort("Bad role in {m}")
    role <- str_to_lower(role)
    message <- str_remove(m, "^(SYSTEM|USER|ASSISTANT): ")
    GPT_convo <- add_message(GPT_convo, role = role, content = message)
  }
  return(GPT_convo)
}

sources$GPT_conversation <- map(sources$GPT_conversation, convo_to_convo)
saveRDS(sources, "./COVID_misinfo_sources.rds")
