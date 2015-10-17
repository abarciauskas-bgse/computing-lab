# `wald.test(y, x, alpha)`
# Performs the Wald Test for H0: mx - my = 0, H1: mx < my
#
# The function requires the arguments (in this order):
#   1) `y`: the ﬁrst sample,
#   2) `x`: the second sample, and,
#   3) `alpha`, the signiﬁcance level of the test.
#
# The function returns a structure containing three ﬁelds:
#   1) `stat`: containing the value of the standardized test statistic,
#   2) `pval`: containing the p–value of the test, and,
#   3) `reject`: TRUE / FALSE corresponding to if the null is rejected at the α conﬁndence level
#
# D = 0
# H0: mx - my = 0
# H1: mx < my (or mx - my < 0)
#
wald.test <- function(y, x, alpha) {
  my <- mean(y)
  mx <- mean(x)

  # stat: wald test statistic
  stat <- (my - mx) / sqrt((var(x)/length(x)) + (var(y)/length(y)))
  # pval of stat
  # This would be qnorm(1 - stat/2) if we were interested in a 2-tailed test
  pval <- dnorm(stat)
  # pval of alpha (e.g. size)
  alpha_stat <- qnorm(1 - alpha)
  # Reject H0 if stat > alpha_pval
  reject <- stat > alpha_stat
  list(stat, pval, reject)
}

# # Test
#
# alpha <- 0.05
# ntests <- 1000
# sample_sizes <- seq(5,300,5)
# reject <- matrix(nrow = ntests, ncol = length(sample_sizes))
# dimnames(reject)[[2]] <- sprintf('sample size = %d', sample_sizes)

# for (s in 1:length(sample_sizes)) {
#   for (n in 1:ntests) {
#     cat('.')
#     x <- 1 + rt(sample_sizes[s], 2.5)
#     y <- 1 + rt(sample_sizes[s], 2.5)
#     test_result <- wald.test(y, x, alpha)
#     reject[n,s] <- test_result[3][[1]]
#   }
#   cat('\n')
# }

# plot(sample_sizes,colMeans(reject*100),t='b',ylim=c(0,10),lwd=2,col='darkblue',ylab='Test Size',xlab='Sample Size')


# `mann.whitney.test(y, x, alpha)`
# Performs the Mann–Whitney Test for H0: mx - my = 0, H1: mx < my
#
# The function requires the arguments (in this order):
#   1) `y`: the first sample;
#   2) `x`: the second sample; and 
#   3) `alpha`: the significance level of the test.
#
# The function returns a structure containing three fields:
#   1) `stat`: containing the value of the standardized test statistic;
#   2) `pval`:, containing the p–value of the test; and,
#   3) `reject`: which is TRUE if the null is rejected at the alpha confindence level (and FALSE otherwise)
#
mann.whitney.test <- function(y, x, alpha) {
  ny <- length(y)
  nx <- length(x)
  pooled <- c(y, x)
  ranks <- rank(pooled)
  ranky <- ranks[1:ny]
  sumranky <- sum(ranky)

  U <- ny*nx + ((ny*(ny+1))/2) - sumranky
  mU <- ny*nx/2
  varU <- ((ny*nx) * (ny+nx+1)) / 12
  sdU <- sqrt(varU)

  stat <- (U - mU) / sdU
  pval <- dnorm(stat)
  # If the alternative is the one-sided hypothesis that the location of
  #   population 1 is higher than that of population 2, the decision rule is
  #   reject H0 when stat < -z(alpha).
  reject <- stat < -(qnorm(1 - alpha))

  list(stat, pval, reject)
}

# # Test

# alpha <- 0.05
# ntests <- 1000
# sample_sizes <- seq(5,300,5)
# reject <- matrix(nrow = ntests, ncol = length(sample_sizes))
# dimnames(reject)[[2]] <- sprintf('sample size = %d', sample_sizes)

# for (s in 1:length(sample_sizes)) {
#   for (n in 1:ntests) {
#     cat('.')
#     x <- 1 + rt(sample_sizes[s], 2.5)
#     y <- 1 + rt(sample_sizes[s], 2.5)
#     test_result <- mann.whitney.test(y, x, alpha)
#     reject[n,s] <- test_result[3][[1]]
#   }
#   cat('\n')
# }

# plot(sample_sizes,colMeans(reject*100),t='b',ylim=c(0,10),lwd=2,col='darkblue',ylab='Test Size',xlab='Sample Size')



