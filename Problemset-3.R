# BEGIN CODE
#
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
# H1: mx - my < 0 (e.g. mx < my)
#
wald.test <- function(y, x, alpha) {
  my <- mean(y)
  mx <- mean(x)

  # stat: wald test statistic
  stat <- (mx - my - 0) / sqrt((var(x)/length(x)) + (var(y)/length(y)))
  # pval of stat
  # This would be qnorm(1 - stat/2) if we were interested in a 2-tailed test
  pval <- pnorm(stat)
  # pval of alpha
  alpha_stat <- qnorm(1 - alpha)
  # Reject H0 if stat > alpha_pval
  reject <- stat < -alpha_stat
  list(stat, pval, reject)
}

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
  pval <- pnorm(stat)
  alpha_stat <- qnorm(1 - alpha)
  # If the alternative is the one-sided hypothesis than the location of
  #   population 1 is higher than that of population 2, the decision rule is
  #   reject H0 when stat < -z(alpha).
  reject <- stat < -alpha_stat

  list(stat, pval, reject)
}

# `mc.test.size(test, alpha, S, N)`
#
# The function requires the arguments (in this order):
#   1) `test`: the function performing the test;
#   2) `alpha`: the significance level of the test;
#   3) `S`: the number of simulations;
#   4) `N`: an m × 2 matrix with elements ni1 and ni2 denoting the sample sizes of the first and second sample.
#
# For each row of N, the function computes the size of the test by performing
# a monte carlo experiment. In each replication of the monte carlo experiment
# the function simulates population one and two from a t distribution with 2.1
# degrees of freedom. The sizes of the populations are ni1 and ni2.
#
# The function returns a vector of size m containg the monte carlo estimate of the test size.
# (The i–th element of the vector contains the size corresponding to row i of the N matrix)
#
mc.test.size <- function(test, alpha, S, N) {
  m <- nrow(N)
  ni1 <- N[,1]
  ni2 <- N[,2]
  test_sizes <- rep(0, m)

  for (row in 1:m) {
    cat('.')
    test_sizes_for_row <- rep(0, S)
    for (sim in 1:S) {
      first_sample_size <- ni1[row]
      second_sample_size <- ni2[row]
      first_sample <- rt(first_sample_size, 2.1)
      second_sample <- rt(second_sample_size, 2.1)
      test_results <- test(first_sample, second_sample, alpha)
      test_sizes_for_row[sim] <- test_results[3][[1]]
    }
    test_sizes[row] <- mean(test_sizes_for_row)
    cat('\n')
  }

  test_sizes
}

# `mc.test.power(test, alpha, S, N, delta)`
#
# The function requires the arguments (in this order):
#   1) `test`: the function performing the test; 
#   2) `alpha`: the significance level of the test;
#   3) `S`: the number of simulations;
#   4) `N`: a vector of size two containing the sample sizes of the first and second sample;
#   5) `delta`: a vector containing a sequence of mean values for the first sample.
#
# For each element of delta, the function computes the power of the test by
#   performing a monte carlo experiment. In each replication of the monte carlo
#   experiment the function simulates population one and two from a t
#   distribution with 2.1 degrees of freedom. Sample 1 has mean of delta[i] and a size
#   of N1 while sample 2 has mean zero and a size of N2.
#
# The function returns a vector of size m containg the monte carlo estimate of
#   the power of the test. (The i–th element of the vector contains the power
#   corresponding to i–th element of delta)
#
mc.test.power <- function(test, alpha, S, N, delta) {
  first_sample_size <- N[1]
  second_sample_size <- N[2]
  powers <- rep(0, length(delta))

  for (first_sample_mean_index in 1:length(delta)) {
    cat('.')
    test_powers_for_row <- rep(0, S)
    for (sim in 1:S) {
      first_sample <- delta[first_sample_mean_index] + rt(first_sample_size, 2.1)
      second_sample <- 0 + rt(second_sample_size, 2.1)
      test_results <- test(first_sample, second_sample, alpha)
      test_powers_for_row[sim] <- test_results[3][[1]]
    }
    powers[first_sample_mean_index] <- mean(test_powers_for_row)
    cat('\n')
  }

  powers
}

# END CODE
