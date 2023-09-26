# Libraries
library(tidyverse)
load_all()

# Load sources from list already validated with COT method
sources <- readRDS("suicide_sources.rds") %>%
  filter(! is.na(GPT_includes)) %>%
  select(title, abstract, human_includes)

# Set review description
review_description <- 
"STUDY DESCRIPTION:

Given that suicide can be conceptualised as a form of health-behaviour based on the formation of intentions, and considering the role of perceived social norms in determining other health-related behaviours and behavioural intentions, there remains a lack of reviews of the extent of the published research literature on the role of perceived social norms in suicidality and self-harm related outcomes. The present systematic scoping review aimed to identify the forms of perceived social norms which have been associated with self-harm and suicidality (i.e., suicidal thoughts, feelings, attempts, and help-seeking) and the role of perceived social norms in these outcomes.

DEFINITIONS:

Social norms are a key influence on personal behaviours and are broadly defined as 'rules and standards that are understood by members of a group, and that guide and/or constrain social behaviour without the force of law'. Social norms influence behavioural intention formation, the engagement in various behaviours, and are featured in a number of key models of health-related behaviour, most notably the Theory of Planned Behavior.

INCLUSION CRITERIA:

- Investigated the role of perceived social norms in the individual experience of suicide-related thoughts, feelings and/or behaviours including self-harm or non-suicidal self-injury (e.g. as an outcome variable in quantitative studies or part of a research question or an identified theme for qualitative studies)
- Original empirical research
- Published in the English language

EXCLUSION CRITERIA:

- Review articles or commentaries
- Studies on suicide attacks (e.g., suicide bombings, suicide terrorism), murder-suicides, or assisted suicide (i.e., euthanasia)
"

# Run validation with zeroshot
screen_sources(sources, review_description, cache_file = "zeroshot_cache.rds")
