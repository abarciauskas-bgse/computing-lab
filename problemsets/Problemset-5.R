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
  iterations <- 10
  # Number of explanatory variables
  P <- ncol(X)

  beta <- lin.reg(y, X)
  beta.prev <- beta

  # Do until convergence
  for (i in 1:iterations) {
    for (p in 1:P)
      p <- 2
      # For evey coefficient, define y_i(-k) = y_i - sum(beta[,setdiff(beta, i)]%*%)
      y.k <- y - X[,setdiff(1:P,p)] %*% beta[setdiff(1:P,p)]
      x.k <- X[,p]
      cov <- sum(y.k * x.k)
      var <- sum(x.k * x.k)

      # cov/var is just the least squares kth coefficient
      beta[i] <- sign(cov/var) * max(c(abs(cov/var) - lambda/(2*var), 0))
      if (sum((beta - beta.prev)**2) < 1e-6 ) { return(beta) }

      beta.prev <- beta
  }

  beta
}

