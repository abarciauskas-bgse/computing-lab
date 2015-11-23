probit.mean <- function(eta) {pnorm(eta)}
probit.var <- function(mu) mu*(1-mu)

probit.iwls <- function(y, X) {
  max.iter <- 100
  beta <- rep(0, ncol(X))
  beta.prev <- beta
  beta.trace <- matrix(0, max.iter+1, length(beta))
  eta <- X %*% beta
  mu <- (y+0.5)/2
  beta.trace[1,] <- beta

  for (iter in 1:max.iter) {
    cat('.')
    z <- eta + (y-mu)/probit.var(mu)
    w <- probit.var(mu)
    W <- diag(as.numeric(w))
    beta <- solve(t(X) %*% W %*% X, t(X) %*% W %*% z)
    eta <- X %*% beta

    mu <- probit.mean(eta)

    beta.trace[iter+1,] <- beta

    if (sum(beta - beta.prev)**2 < 1e-4) {
      cat('\n')
      return(beta)
    }

    beta.prev <- beta
  }
  cat('\n')
  beta
}


probit.cov <- function(y, X, beta) {
  mu <- (y+0.5)/2
  w <- probit.var(mu)
  W <- diag(as.numeric(w))

  solve(t(X) %*% W %*% X)
}

T <- 1000
y <- matrix(0,T,1)
X <- matrix(1,T,2)
eta <- matrix(0,T,1)
mu <- matrix(0,T,1)
beta0 <- 0.1
beta1 <- 2

X[,2] <- rnorm(T,0,1)

for (i in 1:T) {
  eta[i] <- X[i,] %*% c(beta0, beta1)
  mu[i] <- probit.mean(eta[i])
  y[i] <- runif(1) < mu[i]
}

beta.hat <- probit.iwls(y, X)
beta.cov <- probit.cov(y, X, beta.hat)
beta.se <- sqrt(diag(beta.cov))

myprobit <- glm(y ~ 0 + X, family=binomial(link="probit"))

probit.fitted <- function(y, X, beta.hat) {
  probit.mean(X%*%beta.hat)
}

y.fitted <- probit.fitted(y,X,beta.hat)
plot(X[,2], y, col = 'blue')
points(X[,2], y.fitted, col = 'red')
