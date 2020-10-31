#' @title Use gradient descent for GLM with constant or adaptive step size
#' @description This function implements a first-order solution for the GLM maximum likelihood problem using only gradient information. 
#'              Include both a constant step size along with an adaptive one (using momentum algorithm). 
#' @param y         vector of the dependent variable 
#' @param X         data frame or matrix of all the independent variables (all the elements should be numetric)
#' @param family    define the y distribution
#' @param update    optional; default as TRUE (use momentum algorithm to update the step size)
#'                  set FALSE then use constant step size
#' @param lrate     learning rate
#' @param iters     maximum number of iterations 
#' @param epsilon   error tolerence
#' @param change    rate of change of momentum algorithem
#' @examples
#' data("penguins")
#' X = penguins[-which(is.na(penguins[,c(3,4,5,6)])),c(3,4,5,6)]
#' X = cbind(1,scale(X))
#' y = (unlist(penguins[-which(is.na(penguins[,c(3,4,5,6)])),1]))
#' data = cbind(X,y)
#' logit_multiclass(X,y)
#' @export

gradient_descent_mmt <-function(y,X,family,update=TRUE,lrate = 0.001,iters = 1e5,epsilon = 1e-8, change =0.9){
  
  beta <- rep(0, ncol(X)) #initialize the parameters
  for(i in seq_len(iters)){
    beta_old <- beta
    eta <- X %*% beta
    mu <- family$linkinv(eta)
    grad <- t(X) %*% (y - mu) #the first order gradient
    
    if(!update){
      beta <- beta + lrate * grad
    }
    if(update){
      # https://towardsdatascience.com/stochastic-gradient-descent-with-momentum-a84097641a5d
      # apply a moving average
      grad2 <- change*beta + (1-change)* grad
      beta <- beta+lrate*grad2
    }
    
    if(sqrt(crossprod(beta - beta_old)) <= epsilon) break
  }
  
  ret<-list("coefficients" = as.vector(beta))
  names(ret$coefficients)<-colnames(X)
  return(ret)
}



