# Libraries
library(tidyverse)
load_all()

# Load sources from list already validated with COT method
sources <- readRDS("solastalgia_sources.rds") %>%
  filter(! is.na(GPT_includes)) %>%
  select(title, abstract, human_recommendation)

# Set review description
review_description <- str_c(
  sep = "\n",
  "Definitions:",
  "- Solastalgia is a portmanteau of the words: solace (i.e., comfort), algos (i.e., pain or suffering) and nostalgia. Thus, solastalgia refers to the pain and distress caused by the loss or inability to derive solace when there is the lived experience of physical desolation in a home environment.",
  "",
  "Research questions:",
  "1. How is solastalgia defined and differentiated from other eco-psychological terms in Australian literature?",
  "2. What are the risk factors and protective factors for experiencing solastalgia in Australia?",
  "3. How is solastalgia experienced by Aboriginal and Torres Strait Islander peoples?",
  "4. How can solastalgia be addressed in Australia?",
  "",
  "Inclusion criteria:",
  "- Peer-reviewed",
  "- Published in English",
  "- Specific to the Australian context",
  "- Addresses 1 or more of the research questions",
  "",
  "Exclusion criteria:",
  "- Not specific to the Australian context",
  "- Secondary literature (i.e., reviews)"
)

# Run validation with zeroshot
screen_sources(sources, review_description, cache_file = "zeroshot_cache.rds")
