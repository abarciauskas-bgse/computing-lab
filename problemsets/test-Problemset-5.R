# TEST lin.reg
#
# T <-500
# P <-10
# y <- matriX(0,T,1)
# X <- matriX(0,T,P)
# beta <- matriX(0,P,1)

# eps <- rt(T,10)
# X[] <- rnorm(T*P,0,1)
# beta[] <- 0.5

# for(i in 1:T) { y[i] = X[i,] %*% beta + eps[i] }

# my.beta <- lin.reg(y, X)
# # From notes
# beta.hat <- solve( t(X)%*%X , t(X)%*%y )

# Write a function called "ridge.reg"