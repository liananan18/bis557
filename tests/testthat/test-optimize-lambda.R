library(testthat)

context("Test the 'optimize_lambda' output of homework 2.")

test_that("You optimize_lambda() function works.", {
  
  data(iris)
  
  fit_optimize_lambda <- optimize_lambda(Sepal.Length  ~ ., iris[,-5])
  
  # found an optimal lambda which is greater than zero
  expect_equivalent(TRUE, fit_optimize_lambda$lambda > 0)
})