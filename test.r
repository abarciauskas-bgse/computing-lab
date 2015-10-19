S <- 10000
N <- 100
delta <- seq(0,1,0.05)
alpha <- 0.05

# monte carlo experiments
reject <- matrix(0,S,length(delta))
dimnames(reject)[[2]] <- sprintf('delta=%2.2f',delta)

for( i in 1:length(delta) ){
  for( s in 1:S ){
    cat('.')

    X <- 1+rt(N,2.5)
    Y <- 1+delta[i]+rt(N,2.5)

    d <- X-Y
    stat <- mean(d)/(sd(d)/sqrt(N))
    reject[s,i] <- (abs(stat) > qnorm(1-alpha/2))

  }
  cat('\n')
}
cat('Power of the test')
colMeans(reject*100)
plot(delta,colMeans(reject*100),t='b',ylim=c(0,100),lwd=2,col='darkblue',ylab='Test Power',xlab='Violation of Null')
abline(h=5,col='red',lwd=2)

D <- cbind(delta,colMeans(reject*100))
write.table(file='wald-power.csv',D,col.names=FALSE,row.names=FALSE)