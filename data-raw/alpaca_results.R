# Prepare the `alpaca_results` data frame
alpaca_results <- screen_sources(alpaca_sources, alpaca_review_description)

usethis::use_data(alpaca_results, overwrite = TRUE)
