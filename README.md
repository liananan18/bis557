# bis557
<!-- badges: start -->
[![Travis build status](https://travis-ci.com/github/liananan18/bis557.svg?branch=master](https://travis-ci.com/brian-d1018/bis557)
[![Coveralls test coverage](https://coveralls.io/repos/github/brian-d1018/bis557/badge.svg)](https://coveralls.io/r/brian-d1018/bis557?branch=master)
[![Travis build status](https://travis-ci.com/liananan18/bis557.svg?branch=master)](https://travis-ci.com/liananan18/bis557)
<!-- badges: end -->

Lian's BIS557 class assignment answers, write-ups and other materials

---
title: "HW1-LinearModel_GradientDescent"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{An Example Vignette}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

<br><br>

## Dataset
load the 'lm_patho' dataset from the {bis557} package, which can be used as a tough case of linear regression. 
```{r setup}
library(bis557)
data(lm_patho)
```


## Funtions

### Linear Model
You can used a formula and a dataframe to create a linear model.The output is the estimated coefficients of the regression. I used linear algebra with matrix multiplication to build this model.
A simple example as below:
```{r}
library(bis557)
data(iris)
linear_model(Sepal.Length ~ ., iris)
```
To deal with a dataset with extreme values, I used function QR factorization with 'qr.coef()' function to solve the issue.
Here is an example with extreme case:
```{r}
library(bis557)
data(lm_patho)
linear_model(y ~., lm_patho)
```
When there are factors in the dataset, you can also use the "contrasts" parameter to customize how to create the contrasts and build a linear model accordingly:
```{r}
library(bis557)
data(iris)
linear_model(Sepal.Length ~ ., iris, contrasts = list(Species = "contr.sum"))
```

### Gradient Descent
Use the gradient descent method to find the estimated coefficient.
```{r}
library(bis557)
data(iris)
gradient_descent(iris[,1], iris[,2:4])
```
