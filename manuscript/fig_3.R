# Libraries
library(tidyverse)

# Load validation results
results <- readRDS("../validation/summary/pooled_validation_results.rds")

# Sensitivity and specificity
sensitivity <- function(standard, test) { sum(standard[test]) / sum(test) }
specificity <- function(standard, test) { sum(!standard[!test]) / sum(!test) }

results %>%
  group_by(study) %>%
  summarise(sensitivity = sensitivity(human_includes, GPT_includes),
            specificity = specificity(human_includes, GPT_includes)) %>%
  pivot_longer(cols = c(sensitivity, specificity), names_to = "statistic", values_to = "value") %>%
  mutate_at(c("study", "statistic"), str_to_title) %>%
  mutate(study = ifelse(study == "Covid", "COVID", study)) %>%
  ggplot(aes(x = statistic, y = value)) +
    geom_boxplot() +
    geom_point(aes(colour = study)) +
    theme_classic() +
    scale_colour_brewer(palette = "Set2") +
    labs(colour = "Study", x = NULL, y = NULL)

# Kappa
