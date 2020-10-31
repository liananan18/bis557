library(testthat)
library(palmerpenguins)

context("Test the output of homework 3.")

test_that("You gradient_descent_mmt() function works.", {
  
  set.seed(10)
  # Momentum Algorithm
  X <- cbind(rep(1,100),matrix(rnorm(1000),200))
  ##poisson simulation
  beta <- c(1, 0.1, 0.2, 0.1, 0.3, -1)
  y <- rpois(nrow(X), exp(X%*%beta))
  data <- as.data.frame(cbind(y,X[,-1]))
  
  
  # with adaptive step size update
  res_adp<- gradient_descent_mmt(y,X,family = poisson(link = "log"),update = T)
  # constant step size
  res_cons<- gradient_descent_mmt(y,X,family = poisson(link = "log"),update = F)
  
  expect_equivalent(res_adp$coefficients, res_cons$coefficients,
                    tolerance = 1e-1)

})

test_that("You logit_multiclass() function works.", {
  
  data("penguins")
  X <- penguins[-which(is.na(penguins[,c(3,4,5,6)])),c(3,4,5,6)]
  X <- cbind(1,scale(X))
  y <- (unlist(penguins[-which(is.na(penguins[,c(3,4,5,6)])),1]))
  data = cbind(X,y)
  res_logit<- logit_multiclass(X,y)
  
  expect_equivalent(dim(res_logit$coefficients), c(length(unique(y)),ncol(X)),
                    tolerance = 1e-8)
  
  expect_equivalent(res_logit$error, 0,tolerance = 1e-1)
  
})