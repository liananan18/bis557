#' @title Optimize ridge parameter lambda
#' @description This function optimizes the ridge parameter lambda and output 
#' the coefficients and chosen lambda
#' @param form       ridge regression formula
#' @param dataframe  data frame
#' @examples
#' data(iris)
#' optimize_lambda(Sepal.Length ~ ., iris[,-5])
#' @import foreach rsample doParallel purrr tibble ggplot2
#' @export
optimize_lambda <- function(form,dataframe){
  
  set.seed(12345)
  
  # according to class notes
  folds = vfold_cv(dataframe)
  
  lambdas <- seq(0, 1, by=0.0005)
  
  # use 10-fold to calculate the out-of-sample MSE
  # and find the optimized lambda
  #system.time({
  resids <- foreach(i = seq_along(lambdas)) %dopar% {
    foreach(fold = folds$splits, .combine = c) %do% {
      fit <- ridge_regression(form, analysis(fold), lambdas[i])
      y_name <- as.character(form)[2]
      y <- matrix(assessment(fold)[, y_name], ncol = 1)
      predict_y <- suppressWarnings(model.matrix(form, assessment(fold)) %*% fit$coefficients)
      as.vector(mean((y - predict_y)^2))
  }}
  #})
  
  rd <- tibble(lambda = lambdas,
               median = map_dbl(resids, median),
               mean = map_dbl(resids, mean),
               sd = map_dbl(resids, sd))
  
  # the best data is when the mean error is the smallest
  opt_lambda<- lambdas[which(rd$mean == min(rd$mean))]
  final <- ridge_regression(form, dataframe, opt_lambda)
  rd1 <- rd[1:(opt_lambda/0.0005*3),]
  plot <- ggplot(rd1, aes(x=lambda, y=mean)) + geom_line()
  
  return(list(coefficients = final$coefficients, lambda = opt_lambda, plot = plot))
  
}
