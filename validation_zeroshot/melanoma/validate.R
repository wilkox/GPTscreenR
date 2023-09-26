# Libraries
library(tidyverse)
load_all()

# Load sources from list already validated with COT method
sources <- readRDS("melanoma_sources.rds") %>%
  filter(! is.na(GPT_includes)) %>%
  select(title, abstract, human_includes)

# Set review description
review_description <-
"STUDY DESCRIPTION:

Our study aims to identify the current gaps in literature surrounding dermatologic health inequities with the goal of providing insight for future research to better optimize patient care.

To what degree are health inequities researched within the field of dermatology? The objective of this scoping review is to identify and map the different strengths and gaps in what is known about inequities in the topic of dermatology.

DEFINITIONS:

Health inequities can be defined as broad inequities in healthcare access, quality, and cost related to patient characteristics.

INCLUSION AND EXCLUSION CRITERIA:

The population of this review will include literature of the following study designs: clinical trials, retrospective database reviews, systematic reviews, meta-analysis, scoping reviews, literature reviews, cross-sectional analyses, cohort studies, and case-control studies. Commentaries and correspondences will be excluded, as they do not routinely report original research. To address the concept of the review, only studies pertaining to health inequities within dermatology will be included. To increase feasibility and quality of data extraction, only literature published in the English language will be included. Literature from all countries will be included. Finally, to address the context of this review, we will limit the inequities examined to: race and ethnicity, sex or gender, LGBTQ+ identity, underserved rural populations, education level, income, and occupation status. 

Exclusion criteria will include any study: (1) that was published before 2017 or after 2021, (2) that was written in a language other than English, (3) that was conducted on a topic unrelated to 
dermatology, (4) that failed to analyze one of the health inequities, and (5) that was written as a commentary, correspondence, or letter to the editor.
"

# Run validation with zeroshot
screen_sources(sources, review_description, cache_file = "zeroshot_cache.rds")
