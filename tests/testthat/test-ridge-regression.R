library(testthat)

context("Test the 'ridge_regression' output of homework 2.")

test_that("You ridge_regression() function works in an easy case.", {
  
  data(iris)
  
  fit_ridge_regression <- ridge_regression(Sepal.Length  ~ ., iris[,-5])
  
  fit_lm <- lm(Sepal.Length  ~ ., iris[,-5])
  
  expect_equivalent(fit_lm$coefficients, fit_ridge_regression$coefficients,
                    tolerance = 1e-3)
})

test_that("You ridge_regression() function works when there is colinear variables.", {
  
  data("iris")
  
  # create a colinear variable
  iris$Petal.Length2 <- iris$Petal.Length * 2 + 4
  
  fit_ridge_regression <- ridge_regression(Sepal.Length  ~ ., iris[,-5])
  
  fit_lm <- lm(Sepal.Length  ~ ., iris[,-5])
  
  expect_equivalent(fit_lm$coefficients[c(2,4)], fit_ridge_regression$coefficients[c(2,4)],
                    tolerance = 1e-3)
  expect_equivalent(FALSE, is.na(fit_ridge_regression$coefficients[5]))
})