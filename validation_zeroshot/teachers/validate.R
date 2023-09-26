# Libraries
library(tidyverse)
load_all()

# Load sources from list already validated with COT method
sources <- readRDS("./teachers_sources.rds") %>%
  filter(! is.na(GPT_includes)) %>%
  select(title, abstract, human_recommendation)

# Set review description
review_description <- str_c(
  sep = "\n",
  "Study objective: to identify quantitative literature focused on teachers' soft skills, and synthesize the definitions, examples, methods and instruments used to approach them, as well as their main results",
  "Population:",
  "- Inclusion criteria: teachers or pre-service teachers (PST)",
  "- Exclusion criteria: other populations (students, engineers, nurses, etc.)",
  "Design:",
  "- Inclusion criteria: quantitative or mixed studies",
  "- Exclusion criteria: qualitative studies; theoretical articles",
  "Focus:",
  "- Inclusion criteria: focused on teachers' soft skills",
  "- Exclusion criteria: focus on other constructs or only laterally focused on soft skills"
)

# Run validation with zeroshot
screen_sources(sources, review_description, cache_file = "zeroshot_cache.rds")
