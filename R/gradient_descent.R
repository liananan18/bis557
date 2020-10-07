#' @title Use gradient descent to run a linear model
#' @description This function will apply gradient descent to a dataset and return the coefficients
#' @param y         vector of the dependent variable 
#' @param X         data frame or matrix of all the independent variables (all the elements should be numetric)
#' @param epsilon   optional; default as 0.0001, difference to determine the convergence
#' @param eta       learning rate
#' @param iters     maximum number of iterations 
#' @examples
#' data(iris)
#' gradient_descent(iris[,1], iris[,c(2,3,4)])
#' @export
gradient_descent <- function(y,X,epsilon=0.0001,lrate=2, iters=1e5){
  #referance: https://www.ocf.berkeley.edu/~janastas/stochastic-gradient-descent-in-r.html
  intercept<-rep(1,length(y))
  X = as.matrix(data.frame(intercept,X))
  N= dim(X)[1]
  theta0 = t(as.matrix(rnorm(n=dim(X)[2], mean=0,sd = 1))) # Initialize theta
  grad0 = -(2/N)%*%(t(y) - theta0%*%t(X))%*%X
  theta = theta0 - lrate*(1/N)*grad0
  for(i in 1:iters){
    grad = -(2/N)%*%(t(y) - theta%*%t(X))%*%X
    theta = theta - lrate*(2/N)*grad
    if(sqrt(sum(grad^2)) <= epsilon){
      break
    }
  }
  ret<-list("coefficients" = as.vector(t(theta)))
  names(ret$coefficients)<-colnames(X)
  #class(ret)<-"gradient_descent"
  return(ret)
  
}

#print.gradient_descent<-function(x, ...){
  #cat("\nCoefficients:\n")
  #print.default(x$coefficients)
  
#}



