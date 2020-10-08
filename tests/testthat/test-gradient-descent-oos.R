library(testthat)

context("Test the output of homework 2.")

test_that("You gradient_descent_oos() function works in an easy case.", {
  
  data(iris)

  fit_gradient_descent_oos <- gradient_descent_oos(iris[,1], iris[,2:4])
  
  fit_lm <- lm(Sepal.Length  ~ ., iris[,-5])
  
  expect_equivalent(fit_lm$coefficients, fit_gradient_descent_oos$coefficients,
                    tolerance = 1e-1)
  
  expect_equivalent(fit_gradient_descent_oos$Out_Of_Sample_MSE, 0.1130751,
                    tolerance = 1e-5)
  
})
