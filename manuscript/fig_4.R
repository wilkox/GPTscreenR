# Libraries
library(tidyverse)

# Load validation results
results <- readRDS("../validation/summary/pooled_validation_results.rds")

# Calculate sensitivity, specificity, and accuracy, per study
sensitivity <- function(standard, test) {
  sum(standard & test) / sum(standard)
}

specificity <- function(standard, test) {
  sum(!standard & !test) / sum(!standard)
}

accuracy <- function(standard, test) {
  sum(standard == test) / length(standard)
}

perstudy <- results %>%
  group_by(study) %>%
  summarise(sensitivity = sensitivity(human_includes, GPT_includes),
            specificity = specificity(human_includes, GPT_includes),
            accuracy = accuracy(human_includes, GPT_includes),
            n = n()) %>%
  pivot_longer(
    cols = c(sensitivity, specificity, accuracy),
    names_to = "statistic",
    values_to = "value"
  )

# Plot results
perstudy %>%
  mutate_at(c("statistic", "study"), str_to_title) %>%
  mutate(study = ifelse(study == "Covid", "COVID", study)) %>%
  mutate(value = 100 * value) %>%
  ggplot(aes(x = statistic, y = value)) +
    geom_boxplot() +
    geom_point(aes(colour = study)) +
    labs(x = "Statistic", y = "Value (%)", colour = "Study") +
    theme_classic() +
    scale_colour_brewer(palette = "Set2")
ggsave("fig_4.pdf", width = 4, height = 4)
