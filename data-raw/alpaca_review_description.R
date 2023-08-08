# Prepare the `alpaca_review_description` dataset
alpaca_review_description <- list(
  objective = "This scoping review will examine the existing primary research on the role of therapy alpacas in enhancing the mental health and emotional well-being of elderly residential aged care facility residents",
  population = "Elderly people living in residential aged care facilities. 'Elderly' is defined as 65 years of age or older.",
  context = "Residential aged care facilities that have used therapy alpacas as part of their programme of care for residents. 'Residential aged care facility' is defined as a residential setting intended as a long-term place of residence for elderly people which includes provision of support for activities of daily living (e.g. meal preparation, bathing, housekeeping) and nursing support (e.g. medication management). Such facilities will also typically offer other structured programmes and facilities to provide entertainment, diversion, and wellbeing. It excludes other residential settings intended for elderly people that do no provide daily living or nursing supports (e.g. independent living villages) or that are not long-term (e.g. hospitals or hospices).",
  concept = "The impact of therapy alpaca programmes on stress reduction, emotional well-being, mental health, overall life satisfaction, or similar outcomes for residents."
)
usethis::use_data(alpaca_review_description, overwrite = TRUE)
