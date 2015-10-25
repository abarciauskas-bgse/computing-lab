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
        sum_of_products <- sum(sapply(L[i,1:(j-1)], `*`, L[j,1:(j-1)]))
        L[i,j] <- (1/L[j,j]) * (A[i,j] - sum_of_products)
      } else if (i == j) {
        sum_of_squares <- sum(sapply(L[i,1:(i-1)], `^`, 2))
        L[i,i] <- sqrt(A[i,i] - sum_of_squares)
      }
    }
  }
  L
}

my.chol(Sigma)
