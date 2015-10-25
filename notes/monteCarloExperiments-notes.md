# Notes from Computing Lab: Monte-Carlo Methods

The **size** of a test is the probability of rejecting the null when it is actually true.

In the note this **size** is denoted `α`.

The point of the size is to determine at what size of sample you need to get a desired "size".

From the below experiment, we need a size of ~25 to get a size of 5.


```{r}
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

    D <- X - Y

    wald_stat <- mean(D) / (sd(D) / sqrt(N[i]))

    reject[s,i] <- (abs(wald_stat) > qnorm(1 - alpha/2))
  }
  cat('/n')
}

cat('Size of the test')
colMeans(reject*100)

plot(N, colMeans(reject*100), t='b', ylim=c(0,10), lwd=2, ylab='Test Size',xlab='Sample Size')
abline(h=5,col='red',lwd=2)
```
