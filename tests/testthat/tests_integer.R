
# Libs --------------------------------------------------------------------

library(testthat)
source("lib/integers.R", encoding = "UTF-8")

# Unit Tests --------------------------------------------------------------

testthat::test_that("Test that ndigits works as intended", {

  # Dummy data
  x1 <- 1
  x2 <- 12
  x3 <- 123456789
  x4 <- -123
  x5 <- 6.1
  x6 <- 0
  
  # Expected results
  exp_x1 <- 1
  exp_x2 <- 2
  exp_x3 <- 9
  # x4 should throw error
  exp_x5 <- 1
  exp_x6 <- -Inf # Fits in here:https://math.stackexchange.com/questions/1182535/how-many-digits-does-the-integer-zero-have
  
  # Tests
  expect_equal(ndigits(x = x1), exp_x1)
  expect_equal(ndigits(x = x2), exp_x2)
  expect_equal(ndigits(x = x3), exp_x3)
  expect_error(ndigits(x = x4))
  expect_equal(ndigits(x = x5), exp_x5)
  expect_equal(ndigits(x = x6), exp_x6)
})

testthat::test_that("Test that ndigits works as intended", {
  
  # Dummy data
  x1 <- 0
  x2 <- 1
  x3 <- 6
  x4 <- -1
  
  # Expected results
  exp_x1 <- 1
  exp_x2 <- c(1)
  exp_x3 <- c(1,10,100,1000,10000,100000,1000000)
  # x4 should throw error
  exp_x5 <- 1
  
  # Tests
  expect_equal(get_powers_of_ten(ndigits = x1), exp_x1)
  expect_equal(get_powers_of_ten(ndigits = x2), exp_x2)
  expect_equal(get_powers_of_ten(ndigits = x3), exp_x3)
  expect_error(get_powers_of_ten(ndigits = x4))
})



# TODO: Continue with get_powers_of_ten