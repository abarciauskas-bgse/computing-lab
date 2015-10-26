# `my.chol` performs the cholesky decomposition of a symmetric positive definite matrix
#
# The function requires the argument: A ∈ Rn×n, a symmetric positive definite matrix
# The function returns the cholesky decomposition of A
# 
my.chol <- function(A) {
  L <- matrix(0, nrow = nrow(A), ncol = ncol(A))
  for (i in 1:nrow(L)) {
    for (j in 1:ncol(L)) {
      if (i > j) {
        products <- c(0)
        for (k in 1:(j-1)) products <- append(products, L[j,k] * L[i,k])
        L[i,j] <- (1/L[j,j]) * (A[i,j] - sum(products))
      } else if (i == j) {
        sum_of_squares <- sum(sapply(L[i,1:(i-1)], `^`, 2))
        L[i,i] <- sqrt(A[i,i] - sum_of_squares)
      }
    }
  }
  L
}

# `my.forward.solve` solves the linear system of equations Lx = b
#
# The function requires the arguments (in this order):
#   1) `L`: the matrix L,
#   2) `b`: the vector b
#
# The function returns the vector x.
#
my.forward.solve <- function(L, b) {
  x <- rep(0, length(b))
  for (i in 1:length(x)) {
    x[i] <- (1/L[i,i]) * (b[i] - sum(L[i,1:(i-1)] * x[1:(i-1)]))
  }
  return(x = x)
}

# `my.back.solve` solves the linear system of equations Ux = b
#
# The function requires the arguments (in this order):
#   1) `U`: the matrix U,
#   2) `b`: the vector b
#
# The function returns the vector x.
#
my.back.solve <- function(U, b) {
  x <- rep(0, length(b))
  n <- length(x)
  i <- n
  while (i > 0) {
    sum_of_products <- 0
    if (i < n) { sum_of_products <- sum(U[i,(i+1):n] * x[(i+1):n]) }
    x[i] <- (b[i] - sum_of_products) / U[i,i]
    i <- i - 1
  }
  return(x = x)
}

# `my.solve` solves th linear system of equations Ax = b, # where A ∈ Rn×n is
#   a symmetric positive definite matrix, b ∈ Rn. The function has to use the
#   my.chol, my.forward.solve and my.back.solve.
#
# The function requires the arguments (in this order):
#   1) `A`: the matrix A, and
#   2) `b`: the vector b
#
# The function returns the vector x
#
my.solve <- function(A, b) {
  L <- my.chol(A)
  y <- my.forward.solve(L, b)
  x <- my.back.solve(t(L), y)
  return(x = x)
}


# TEST #

# generate_matrix <- function(n) {
#   ev = runif(n, 0, 10)
#   Z <- matrix(ncol=n, rnorm(n^2))
#   decomp <- qr(Z)
#   Q <- qr.Q(decomp)
#   R <- qr.R(decomp)
#   d <- diag(R)
#   ph <- d / abs(d)
#   O <- Q %*% diag(ph)
#   Z <- t(O) %*% diag(ev) %*% O
#   Z
# }

# test <- function(n)
# {
#   A <- generate_matrix(n)
#   print ("A")
#   print (A)
  
#   b <- rnorm(n, 1, 10)
#   print("b")
#   print(b)
  
#   my.x <- my.solve(A, b)
#   r.x <- solve(A, b)
#   print("x")
#   print(cbind(my.x,r.x))

#   print("Ax")
#   print(cbind(A%*%my.x, A%*%r.x))
# }

# test(4)
