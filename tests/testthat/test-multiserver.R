library(testthat)
#TEST 1
test_that("Multiserver works correctly", {
  arrival_time <- c(10, 20, 30)
  service_time <- c(5, 10, 15)
  result <- Multiserver(arrival_time, service_time, 2)
  
  # Check that the number of rows is equal to the number of arrival times
  expect_equal(nrow(result), length(arrival_time))
  
  # Ensure service end times are greater than or equal to service begin times
  expect_true(all(result$ServiceEnds >= result$ServiceBegins))
})

# TEST 2
test_that("Multiserver works correctly", {
  arrival_time <- c(10, 20, 30)
  service_time <- c(5, 10, 15)
  result <- Multiserver(arrival_time, service_time, 2)
  expect_equal(nrow(result), length(arrival_time))
  expect_true(all(result$ServiceEnds >= result$ServiceBegins))
})

