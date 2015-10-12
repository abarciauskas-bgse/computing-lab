# Tuning parameters for Linear Congruential Generator
a = 48271
m = 2^31 - 1
c = 1

# Linear Congruential Random Number Generator
# Simulates draws from a uniform random variable
#
# The function requires two arguments (in this order):
#   1) `n`: the number of draws, and
#   2) `seed`: seed of the congruential generator.
#
# The function returns a vector containing `n` uniform draws.
#
# Example:
# congruential.generator(10, 12345)
#
congruential.generator <- function(n, seed) {
  x <- rep(0,n+1)
  x[1] <- seed

  for(s in 2:(n+1)) {
    x[s] <- (a*x[s-1] + c) %% m
  }

  x[2:(n+1)]/m
}

# Normal Random Number Generator
# Simulate draws from a normal random variable using the Box–Muller algorithm.
# 
# The function requires three arguments (in this order):
#  1) `u`: a vector of random draws from a uniform random variable (which is expected to be even-numbered),
#  2) `m`: the mean, and
#  3) `d`: the standard deviation of the normal.
#
# The function returns a vector containing n normal draws (each one corresponding to the input uniform draws)
#
normal.simulator <- function(u, m, d) {
  # Determine how many randoms to return
  ndraws <- length(u)
  # Initialize y, the vector of randoms to return
  y <- rep(0, ndraws)

  # Track position in list of uniform randoms
  position <- 1

  while (position < ndraws) {
    # Pick the first two values from the vector of uniforms
    u1 <- u[position]
    u2 <- u[position+1]

    z1 <- sqrt(-2 * log(u2)) * cos(2 * pi * u1)
    z2 <- sqrt(-2 * log(u2)) * sin(2 * pi * u1)

    # Project to general normal with mean m and standard deviation d.
    y[position] <- z1 * d + m
    y[position+1] <- z2 * d + m

    # Advance to next pair of normals (which is ok because we assume and even-numbered list).
    position <- position + 2
  }

  y
}


# Beta Random Number Generator
# Simulates draws from a beta random variable using rejection sampling using a uniform ([0, 1]) proposal
#
# The function requires four arguments (in this order):
#   1) `n`: The number of draws of the beta distribution to the simulated,
#   2) `a`: the α parameter,
#   3) `b`: the β parameters, and,
#   4) `k`: the K constant of the rejection sampling algorithm
#
# The function returns a vector containing n beta draws.
#
beta.simulator <- function(n, a, b, K) {
  y <- rep(0, n)

  for (i in 1:n) {
    repeat {
      x <- runif(1)
      u <- runif(1)
      bool <- u < dbeta(x, a, b) / (K * (dunif(x)))
      if (bool) {
        y[i] <- x
        break
      }
    }
  }

  y
}


# Pareto Random Number Generator
# Simulates draws from a Pareto distribution on the basis of the inversion method.
#
# The function requires two arguments (in this order):
#   1) `u`: a vector of random draws from a uniform random variable, and,
#   2) `a`: the α parameter.
#
# The function returns a vector containing n Pareto draws (each one corresponding to the input uniform draws)
#
# The inverse of the pareto CDF is (1 - p)^(-1/a)
#
pareto.simulator <- function(u, a) u**(-1/a)
