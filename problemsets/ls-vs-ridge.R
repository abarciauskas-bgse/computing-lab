# OLS vs Ridge

S <- 1000
T <- 500
P <- 10

y      <- matrix(0,T,1)
X      <- matrix(0,T,P)
beta   <- matrix(0,P,1)
beta[] <- 0.5
rho    <- 0.999
lambda <- 0.1

Sigma.X      <- matrix(rho,P,P) + (1-rho) * diag(P)

ev    <- eigen(Sigma.X)$values
cond  <- max(ev)/min(ev)

Sigma.X.sqrt <- chol(Sigma.X)
for( t in 1:T ){
  X[t,] <- Sigma.X.sqrt %*% rnorm(P,1)
}

beta.hat.ols   <- matrix(0,S,P)
beta.hat.ridge <- matrix(0,S,P)

for( s in 1:S ){
  eps <- rt(T,10)  
  y   <- X %*% beta + eps
  beta.hat.ols[s,]   <- solve( t(X)%*%X , t(X)%*%y )
  beta.hat.ridge[s,] <- ridge.reg(y, X, lambda)#solve( t(X)%*%X+lambda*diag(P) , t(X)%*%y )
}

beta.ols.bias   <- colMeans(beta.hat.ols)-beta
beta.ols.var    <- colMeans(beta.hat.ols**2) - colMeans(beta.hat.ols)**2
beta.ols.mse    <- beta.ols.bias**2 + beta.ols.var

beta.ridge.bias <- colMeans(beta.hat.ridge)-beta
beta.ridge.var  <- colMeans(beta.hat.ridge**2) - colMeans(beta.hat.ridge)**2
beta.ridge.mse  <- beta.ridge.bias**2 + beta.ridge.var

cat('Percentage Improvement')
cbind(  round( 1 - beta.ridge.mse/beta.ols.mse , 2 ) )

