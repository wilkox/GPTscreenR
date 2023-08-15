# Libraries
library(tidyverse)
library(readxl)
library(psych)
library(broom)

# Load the screening results
read_excel("./TITLE ABSTRACT FULL TEXT SCREENING DATA_CAIM COVID-19 SM Misinfo_Mar2223.xlsx", 
           sheet = "Title & Abstract Screening", range = "A2:G673") %>%
  select(human1 = `Screener 1 Decision`, human2 = `Screener 2 Decision`) %>%
  mutate_at(c("human1", "human2"), str_to_lower) %>%
  as.matrix() %>%
  cohen.kappa() %>%
  tidy()
