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
soft.threshold <- function(x,lambda){
  sign(x)*max( c( abs(x) - lambda , 0 ) )
}

lasso.reg <- function(y, X, lambda) {
  iterations <- 10
  # Number of explanatory variables
  P <- ncol(X)

  beta <- solve(t(X)%*%X,t(X)%*%y)
  beta.prev <- beta

  # Do until convergence
  for (i in 1:iterations) {
    for (p in 1:P)
      # For evey coefficient, define y_i(-k) = y_i - sum(beta[,setdiff(beta, i)]%*%)
      y.k <- y[i] - X[,setdiff(1:P,p)] %*% beta[setdiff(1:P,p)]
      x.k <- X[,p]
      cov <- sum(y.k * x.k)
      var <- sum(x.k * x.k)

      # cov/var is just the least squares kth coefficient
      beta[i] <- soft.threshold(cov/var , lambda/(2*var))
      if (sum((beta - beta.prev)**2) < 1e-6 ) { return(beta) }

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
#
cross.validation <- function(y, X, lambda, pen.reg) {
  y_subsamples <- split(y, ceiling(seq_along(y)/5))

  # Estimate beta from 4 subsamples
  beta.pen <- pen.reg(y, X, lambda)
  
}
