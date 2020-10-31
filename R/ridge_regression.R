#' @title Ridge regression function 
#' @description This ridge Refression function will take into account colinear (or nearly colinear) 
#' regression variables and return the coefficients
#' @param form       ridge regression formula
#' @param dataframe  data frame
#' @param lambda     optional; default as 0
#' @examples
#' data(iris)
#' ridge_regression(Sepal.Length ~ ., iris[,-5], 0.01)
#' @export
ridge_regression <- function(form,dataframe,lambda=0){
  
  # according to class notes
  data_na_free <- model.frame(form,dataframe)#only drop rows of na in the chosen variables
  X <- model.matrix(form, data_na_free)
  y_name <- as.character(form)[2]
  y <- matrix(data_na_free[, y_name], ncol = 1)
  
  # taking into account colinear (or nearly colinear) regression variables
  svd_X <- svd(X)
  num <- svd_X$d / svd_X$d[1]
  need <- seq(1,max(which(1e-5<num)),1)
  svd_X$d <- svd_X$d[need]
  svd_X$u <- svd_X$u[,need]
  svd_X$v <- svd_X$v[,need]
  
  # calculate the ridge regression coefficients
  Sigma <- diag(svd_X$d)
  lambda_I <- diag(rep(lambda, length(svd_X$d)))
  
  beta <- svd_X$v %*% solve(Sigma^2 + lambda_I) %*% Sigma %*% t(svd_X$u) %*% y
  
  result <- as.vector(beta)
  names(result) <- colnames(X)
  
  return(list(coefficients=result))
}
