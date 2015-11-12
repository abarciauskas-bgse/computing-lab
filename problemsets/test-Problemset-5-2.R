T <- 500 
P <- 20

y <- matrix(0,T,1) 
X <- matrix(0,T,P)
beta <- matrix(0,P,1)

eps <- rt(T,10) 
X[] <- rnorm(T*P,0,1)

beta[] <- sample(c(-1,1),replace=TRUE,P) * rbinom(P,1,0.25)

for( i in 1:T ){ 
y[i] = X[i,] %*% beta + eps[i] 
}

beta.hat <- solve( t(X)%*%X , t(X)%*%y ) 
beta.lasso <- lasso.reg( y , X , 0.2*T )

round( cbind( beta , beta.hat , beta.lasso ) , 3)

lambda.range <- seq(0,5,0.5)

beta.trace <- matrix(0,length(lambda.range),P) 

for( l in 1:length(lambda.range) ){ 
  beta.trace[l,] <- lasso.reg( y , X , lambda.range[l]*T ) 
} 

matplot(lambda.range,beta.trace,t='b')
