# Libraries
library(tidyverse)
load_all()

# Load sources from list already validated with COT method
sources <- readRDS("smartphones_sources.rds") %>%
  filter(! is.na(GPT_includes)) %>%
  select(title, abstract, human_includes)

# Set review description
review_description <- "AIMS: The aims of this scoping review are fourfold: (1) to categorise what kinds of behavior are measured in the smartphone addiction literature, (2) identify how frequently behavior is measured, (3) how behavior is measured (self-report vs logged), and (4) whether these have changed over time.\n\nINCLUSION CRITERIA:\n- Was the paper focused on smartphone addiction? (i.e. did it include an addiction measure, and if not was the paper substantially focused on the topic e.g. for qualitative studies).\n- Did the paper report the use of primary data? i.e. not a literature review, theoretical paper, systematic review or meta-analysis, and not reporting data previously reported in the literature (e.g. reuse of data, secondary analysis of an existing data set). Secondary data were removed because reporting of data will be limited to key variables of interest and overlap. For longitudinal or intervention studies that were included, measurements were taken at baseline or the first wave that smartphone addiction was included as a variable.\n- Was the paper written in English?"

# Run validation with zeroshot
screen_sources(sources, review_description, cache_file = "zeroshot_cache.rds")
