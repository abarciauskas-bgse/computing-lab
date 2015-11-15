# `lin.reg`
# 
# The function requires the arguments:
#  1) `y`: the dependent variable vector y,
#  2) `X`: the eXplanatory variables matriX X
#
# The function returns the least squares estimator beta
#
lin.reg <- function(y, X) {
  solve(t(X)%*%X) %*% t(X) %*% y
}

# To test see test-Problemset-5.R compared results to Chistian's code.

# `ridge.reg`
#
# The function requires the arguments:
#  1) `y`: the dependent variable vector y,
#  2) `X`: the eXplanatory variables matriX X
#  3) `lambda`: the ridge tuning parameter
#
# The function returns the ridge estimator beta
#
ridge.reg <- function(y, X, lambda) {
  solve(t(X) %*% X + lambda*diag(ncol(X))) %*% t(X)%*%y
}

# To test I used the code from ls-vs-ridge.R and checked to make sure I got
# similar results

# `lasso.reg`
#
# The function requires the arguments:
#  1) the dependent variable vector `y`,
#  2) the explanatory variables matrix `X`,
#  3) the LASSO tuning parameter `lambda`
#
# The function returns the lasso estimator beta
#
# (Use as initial value of the shooting algorithm the least squares estimator.
# (Compute the lasso estimator using the shooting algorithm described in the
# (handout.)
#
lasso.reg <- function(y, X, lambda) {
  max.iter <- 100
  P <- ncol(X)

  beta <- solve(t(X)%*%X, t(X)%*%y)
  beta.prev <- beta

  # Do until convergence
  for (iter in 1:max.iter) {
    for (k in 1:P)
      # c
      y.minus_k <- y - X[,setdiff(1:P,k)] %*% beta[setdiff(1:P,k)]
      x.k <- X[,k]
      cov <- sum(y.minus_k * x.k)
      var <- sum(x.k * x.k)

      beta.k.ls <- cov/var
      beta[k] <- sign(beta.k.ls)*max( c( abs(beta.k.ls) - lambda/(2*var) , 0 ) )
      # cov/var ~ least squares kth coefficient
      # beta[i] <- soft.threshold(cov/var , lambda/(2*var))
      if (sum((beta - beta.prev)**2) < 1e-6) { return(beta) }

      beta.prev <- beta
  }

  beta
}


# `cross.validation` performs 5-fold cross validation.
#
# The routine works as follows:
#  1) Divide the sample into 5 sub-samples of equal size.
#  2) Use 4 sub-samples to estimate the beta parameter (by LASSO or RIDGE)
#  3) Use the remaining one to compute the residual sum of squares.
#  4) Repeat the operation 5 times leaving out each time a diﬀerent sub-sample.
#  5) Return the average pseudo–RSS.
#
# The function requires the arguments:
#  1) the dependent variable vector y,
#  2) the explanatory variables matrix X,
#  3) the tuning parameter lambda,
#  4) the penalized estimation routine pen.reg (which can either be lasso.reg or ridge.reg)
#
# The function returns the cross.validation RSS
# 1:400,        #pointer is nobs - test_size (train_size)
# 1:300,400:500 #pointer is train_size - test_size
# 1:200,300:500 # pointer is train_size - 2*test_size
# 1:100,200:500
# ,400:500
cross.validation <- function(y, X, lambda, pen.reg) {
  nsubsamples <- 5
  nobs <- length(y)
  test_size <- floor(nobs/nsubsamples)
  list_of_rss <- c()
  
  # FIXME this offsetting is off
  for (i in 1:nsubsamples) {
    pointer <- nobs-i*test_size
    if (pointer == 0) pointer <- 1
   
    y_train <- c(y[1:(pointer-1)], y[(test_size + pointer):nobs])
    # although unused
    X_train <- rbind(X[1:(pointer-1),], X[(test_size + pointer):nobs,])

    y_test <- y[pointer:(test_size + pointer - 1)]
    X_test <- X[pointer:(test_size + pointer - 1),]

    # Estimate beta from 4 subsamples
    beta.pen <- pen.reg(y_train, X_train, lambda)
    fitted <- X_test %*% beta.pen
    list_of_rss <- append(list_of_rss, sum((y_test - fitted)^2))
  }
  mean(list_of_rss)
}
