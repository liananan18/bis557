#' @title Run a linear model
#' @description This function will run a linear model and return the coefficients
#' @param
#' formula    linear regression formula
#' dataframe  data frame
#' contrasts  optional; list of constrasts for factor variables
#' @examples
#' data(iris)
#' linear_model(Sepal.Length ~ ., iris,contrasts = list(Species = "contr.sum"))
#' @import stats
#' @export
linear_model <- function(form,dataframe,contrasts=NULL){
  data_na_free<-model.frame(form,dataframe)#only drop rows of na in the chosen variables

  if(is.null(contrasts)){
    IND<-model.matrix(form,data_na_free)#independent variable
    DEP<-matrix(data_na_free[,as.character(form)[2]],ncol=1)

  } else{
    st<-paste("~",names(contrasts))
    IND<-model.matrix(as.formula(st),model.frame(as.formula(st),dataframe),contrasts.arg = contrasts)
    IND<-as.matrix(cbind(model.matrix(form,data_na_free[,-ncol(data_na_free)]),IND[,-1]))
    DEP<-as.matrix(data_na_free[,as.character(form)[2]],ncol=1)
  }

  #Deal with extreme situations
  if (kappa(IND) < 1e+07) {
    beta<-as.numeric(solve(t(IND)%*%IND)%*%t(IND)%*%DEP) #calculate the batas
    names(beta)<-row.names(beta)
    ret<-list('coefficients'=beta)
  } else {
    ret<-list("coefficients" =as.vector(qr.coef(qr(IND), DEP)))
  }

  #Give
  names(ret$coefficients)<-colnames(IND)
  #class(ret)<-"linear_model"
  return(ret)

}


#print.linear_model<-function(x, ...){
  #cat("\nCoefficients:\n")
  #print.default(x$coefficients)

#}

