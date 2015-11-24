# `probit.iwls`
#
# The function requires the arguments:
#  1) the dependent variable vector y,
#  2) the explanatory variables matrix X
#
# The function returns the ML estimator of the parameter beta of the Probit model using the IWLS algorithm
#
probit.iwls <- function(y, X) {
  max.iter <- 100
  beta <- rep(0, ncol(X))
  beta.prev <- beta
  beta.trace <- matrix(0, max.iter+1, length(beta))
  eta <- X %*% beta
  mu <- (y+0.5)/2
  beta.trace[1,] <- beta

  for (iter in 1:max.iter) {
    z <- eta + (y-mu)*1/dnorm(qnorm(mu))
    w <- 1/(mu*(1-mu))*(dnorm(qnorm(mu))**2)
    W <- diag(as.numeric(w))
    beta <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% z)
    eta <- X %*% beta
    mu <- pnorm(eta)

    beta.trace[iter+1,] <- beta

    if (sum(beta - beta.prev)**2 < 1e-4) {
      return(beta)
    }

    beta.prev <- beta
  }
  beta
}

# `probit.cov`
#
# The function requires the arguments:
#  1) the dependent variable vector y,
#  2) the explanatory variables matrix X, and,
#  3) the ML estimator beta.hat
#
# The function returns the asymptotic variance covariance matrix of the estimator
#
probit.cov <- function(y, X, beta) {
  mu <- pnorm(X%*%beta)
  w <- 1/(mu*(1-mu))*(dnorm(qnorm(mu))**2)
  W <- diag(as.numeric(w))

  solve(t(X) %*% W %*% X)
}

# `probit.fitted`
#
# The function requires the arguments:
#  1) the dependent variable vector y,
#  2) the explanatory variables matrix X, and,
#  3) the ML estimator beta.hat
#
# The function returns the ﬁtted probabilities of the probit model
#
probit.fitted <- function(y, X, beta.hat) {
  pnorm(X%*%beta.hat)
}
