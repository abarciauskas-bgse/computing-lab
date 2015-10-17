# Wald Test Size
S <- 1000
N <- seq(5,150,5)

alpha <- 0.05

reject <- matrix(0,S,length(N))

dimnames(reject)[[2]] <- sprintf('N=%d', N)

for (i in 1:length(N)) {
  for (s in 1:S) {
    cat('.')

    X <- 1 + rt(N[i], 2.5)
    Y <- 1 + rt(N[i], 2.5)

    reject[s,i] <- wald.test(X, Y, alpha)[2][[1]]
  }
  cat('/n')
}

cat('Size of the test')
colMeans(reject*100)

# This should have a plot like
# 10 | o
#    |   o                 o 
#  5 |-----o---o--o--o--o-----o---
#    |       o                 
#  0 |____________________________
#                                150
plot(N, colMeans(reject*100), t='b', ylim=c(0,10), lwd=2, ylab='Test Size',xlab='Sample Size')
abline(h=5,col='red',lwd=2)



