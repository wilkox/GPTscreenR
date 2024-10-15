library("vcr") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  filter_sensitive_data = list("<<<my_api_key>>>" = Sys.getenv("OPENAI_API_KEY"))
))
vcr::check_cassette_names()
