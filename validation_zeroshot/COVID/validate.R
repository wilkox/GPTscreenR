# Libraries
library(tidyverse)
load_all()

# Load sources from list already validated with COT method
sources <- readRDS("COVID_sources.rds") %>%
  filter(! is.na(GPT_includes)) %>%
  select(title, abstract, human_recommendation)

# Set review description
review_description <- str_c(
  sep = "\n",
  "RESEARCH QUESTION: With respect to studies exploring COVID-19 misinformation on social media, how many of them make mention of complementary, alternative, and integrative medicine (CAIM) and what do they report about these therapies?",
  "INCLUSION CRITERIA:",
  str_c(
    sep = "\n",
    "- The study specifically explored COVID-19 misinformation on social media",
    "- In defining social media, we followed a formalized definition to remove ambiguity when determining study eligibility. Obar and Wildman, define social media by the following four characteristics: '1) social media services are (currently) applications that are Web 2.0 Internet-based; 2) the lifeblood of social media is user-generated content; 3) for a site or app designed and maintained by a social media service, individuals and groups create user-specific profiles, and; 4) the development of social networks online by connecting a profile with those of other individuals and/or groups is facilitated by social media services.'",
    "- During title and abstract screening, studies were only selected for inclusion if the information provided in the title or abstract indicated that the study specifically explored COVID-19 misinformation on social media. This meant that a study was considered eligible regardless of whether CAIM was discussed in the article’s full text. This decision was made to avoid excluding relevant articles that include CAIM discussion in the full text, but not within the title or abstract.",
    "- Only primary research articles are (e.g., cross-sectional studies, descriptive analyses, questionnaire based studies, etc.) were considered and included in this scoping review.",
    "- Review articles were not eligible for inclusion in this review.",
    "- Research protocols, abstracts, editorials, opinion pieces, commentaries, and any non-English texts were not eligible for inclusion in this scoping review."
  )
)

# Run validation with zeroshot
screen_sources(sources, review_description, cache_file = "zeroshot_cache.rds")
