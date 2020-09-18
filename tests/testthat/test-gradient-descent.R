library(testthat)

context("Test the 'gradient_descent' output of homework 1.")

test_that("You gradient_descent() function works in an easy case.", {

  data(iris)

  fit_gradient_descent <- gradient_descent(iris[,1], iris[,2:4])

  fit_lm <- lm(Sepal.Length  ~ ., iris[,-5])

  expect_equivalent(fit_lm$coefficients, fit_gradient_descent$coefficients,
                    tolerance = 1e-2)
})


