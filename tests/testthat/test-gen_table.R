test_that("gen_table works with basic data", {
  # Skip if heavy dependencies are missing
  skip_if_not_installed("sjlabelled")
  skip_if_not_installed("sjmisc")
  skip_if_not_installed("dplyr")
  
  # Create simple test data
  test_data <- data.frame(
    response = factor(c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree",
                       "Strongly Agree", "Agree", "Neutral")),
    Weight = c(1, 1, 1, 1, 1, 1, 1, 1)
  )
  
  # Add labels (simplified approach)
  attr(test_data$response, "label") <- "Test Question"
  
  # Test basic functionality
  expect_no_error({
    result <- gen_table(test_data, "response", weight = FALSE)
  })
  
  # Test that result is a data frame
  result <- gen_table(test_data, "response", weight = FALSE)
  expect_s3_class(result, "data.frame")
  
  # Test that result has expected columns
  expect_true(all(c("Response", "N", "Percent") %in% names(result)))
  
  # Test that sorting options work
  expect_no_error({
    gen_table(test_data, "response", sort_by = "A-Z", weight = FALSE)
  })
  
  expect_no_error({
    gen_table(test_data, "response", sort_by = "N", weight = FALSE)
  })
})

test_that("gen_table handles empty data gracefully", {
  skip_if_not_installed("sjlabelled")
  skip_if_not_installed("sjmisc")
  skip_if_not_installed("dplyr")
  
  # Create empty test data
  empty_data <- data.frame(
    response = factor(character(0)),
    Weight = numeric(0)
  )
  
  # Should handle empty data without error
  expect_no_error({
    result <- gen_table(empty_data, "response", weight = FALSE)
  })
})

test_that("gen_table parameter validation", {
  test_data <- data.frame(
    response = factor(c("Yes", "No", "Yes")),
    Weight = c(1, 1, 1)
  )
  
  # Test invalid sort_by parameter
  expect_no_error({
    gen_table(test_data, "response", sort_by = "Invalid", weight = FALSE)
  })
  
  # Test boolean parameters
  expect_no_error({
    gen_table(test_data, "response", add_total = FALSE, add_mean = FALSE, weight = FALSE)
  })
}) 