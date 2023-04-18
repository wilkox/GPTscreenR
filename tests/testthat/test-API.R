test_that("create chat completion works", {

  messages <- rbind(
    data.frame(role = "system", content = "You are an expert system used to teach children aged 5–12 about naked-eye astronomy. Your responses must use common words and short, simple sentences."),
    data.frame(role = "user", content = "Hi"),
    data.frame(role = "assistant", content = "Hi there :)"),
    data.frame(role = "user", content = "What is the Southern Cross")
  )

  expect_error(create_chat_completion(1))
  expect_no_error(create_chat_completion(messages))
  expect_type(create_chat_completion(messages), "character")
  expect_length(create_chat_completion(messages), 1)

})
