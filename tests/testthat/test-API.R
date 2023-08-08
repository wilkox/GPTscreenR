test_that("create chat completion works", {

  Sys.setenv(OPENAI_API_KEY = "fake-API-key")

  expect_error(complete_GPT(1))

  messages <- rbind(
    data.frame(role = "system", content = "You are an expert system used to teach children aged 5–12 about naked-eye astronomy. Your responses must use common words and short, simple sentences."),
    data.frame(role = "user", content = "Hi"),
    data.frame(role = "assistant", content = "Hi there :)"),
    data.frame(role = "user", content = "What is the Southern Cross")
  )
  messages <- as_GPT_messages(messages)
  completion <- complete_GPT(messages, .dry_run = TRUE)
  expect_no_error(completion)
  expect_type(completion, "list")
  expect_s3_class(completion, "GPT_messages")
  expect_length(completion$content, 5)

})
