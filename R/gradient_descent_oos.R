#' @title Use gradient descent that calculates the loss 
#' based on the out-of-sample accuracy
#' @description This function will apply gradient descent 
#' to a dataset and return the coefficients and out-of-sample MSE. 
#' This method will devide the imput dataset into two parts; 
#' 80% as training and 20% as test dataset. It uses the 20% to calcualte
#' out of sample prediction error.
#' @param y         vector of the dependent variable 
#' @param X         data frame or matrix of all the independent variables (all the elements should be numetric)
#' @param epsilon   optional; default as 0.0001, difference to determine the convergence
#' @param lrate     learning rate
#' @param iters     maximum number of iterations 
#' @examples
#' data(iris)
#' gradient_descent_oos(iris[,1], iris[,c(2,3,4)])
#' @export
gradient_descent_oos <- function(y,X,epsilon=0.0001,lrate=2, iters=1e5){
  set.seed(12345)
  # split X into training and test dataset by a 8-2 split
  sp <- sort(sample(nrow(X), floor(nrow(X)*.8)))
  trainX <- X[sp, ]
  trainy <- y[sp]
  testX <- X[-sp, ]
  testy <- y[-sp]

  # use the function previously built to calculate the gradient descent coefficients
  coeff <- gradient_descent(trainy,trainX,epsilon,lrate,iters)$coefficients
  
  # Calculate the out of sample MSE
  intercept<-rep(1,length(testy))
  testX <- as.matrix(data.frame(intercept,testX))
  predy <- testX %*% coeff
  MSE <- mean((testy - predy)^2)
  
  return(list(coefficients=coeff, Out_Of_Sample_MSE = MSE))
  
}