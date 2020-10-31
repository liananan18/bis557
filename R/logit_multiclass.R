#' @title A logistic model works for more than 2 classes
#' @description This function implement a logistic model which can make prediction of more than 2 classes
#' @param y         vector of the dependent variable 
#' @param X         data frame or matrix of all the independent variables (all the elements should be numetric)
#' @param iters     maximum number of iterations 
#' @param epsilon   error tolerence
#' @examples
#' # Momentum Algorithm
#' X <- cbind(rep(1,100),matrix(rnorm(1000),200))
#' ##poisson silulation
#' beta <- c(1, 0.1, 0.2, 0.1, 0.3, -1)
#' y <- rpois(nrow(X), exp(X%*%beta))
#' data <- as.data.frame(cbind(y,X[,-1]))
#' # with adaptive step size update
#' gradient_descent_mmt(y,X,family = poisson(link = "log"),update = T, lrate)
#' @export

logit_multiclass <-
  function(X, y, iters=25, epsilon=1e-10){
    # define the classes
    class <- unique(y)
    # number of classes
    num <- length(class)
    family = binomial(link = "logit")
    
    # change y into rows of binary (3 classes = 3 rows) classes and 
    # apply a regular logistic model to each line of data
    # reference textbook 5.3
    beta <- matrix(0,num,ncol(X))
    beta_old <- matrix(0,num,ncol(X))
    for(i in seq_len(iters)){
      for (j in 1:num){
      beta_old[j,] <- beta[j,]
      y_new <- as.numeric(y == class[j]) # turn in to binary
      
      eta <- X %*% beta[j,]
      mu <- family$linkinv(eta)
      mu_p <- family$mu.eta(eta)
      z <- eta + (y_new - mu) / mu_p
      W <- as.numeric(mu_p^2 / family$variance(mu))
      XtX <- crossprod(X, diag(W) %*% X)
      Xtz <- crossprod(X, W * z)
      beta[j,] <- solve(XtX, Xtz)
      if(sqrt(crossprod(beta[j,] - beta_old[j,])) < epsilon) break
      }
    }
    
    # calculate the probability of belonging to a class
    prob <- matrix(0,nrow(X),num)
    for (i in 1:num){
      prob[,i] <- exp(X%*%beta[i,])
    }
    prob <- prob/sum(prob)
    
    
    # make prediction by assigning to the class with the highest probability.
    y_pre <- class[apply(prob, 1, which.max)]
    
    # error rate
    err = mean(y_pre!=y)
    
    ret<-list("coefficients" = beta, "predict" = y_pre, "error" = err)
    colnames(ret$coefficients)<-colnames(X)
    rownames(ret$coefficients)<-paste("is",class)
    
    
    return(ret)
    
  }


