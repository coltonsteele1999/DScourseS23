library(nloptr)
library(modelsummary)

set.seed(100)

N <- 100000
K <- 10
sigma <- 0.5

# Create matrix X with first column of 1's and remaining columns as normally distributed random numbers
X <- matrix(rnorm(N * K, mean = 0, sd = 1), nrow = N, ncol = K)
X[, 1] <- 1

# Create vector eps with N random numbers distributed N(0, sigma^2)
eps <- rnorm(N, mean = 0, sd = sigma)

# Create vector beta with the given values
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Generate Y vector as Y = X*beta + eps
Y <- X %*% beta + eps

# Compute beta_hat_ols using the closed-form solution
beta_hat_ols <- solve(t(X) %*% X) %*% t(X) %*% Y

# Print beta_hat_ols
beta_hat_ols

# Set the learning rate and the initial value of beta_hat
learning_rate <- 0.0000003
beta_hat_gd <- rep(0, K)

# Define the gradient function
grad <- function(beta_hat) {
  t(X) %*% (X %*% beta_hat - Y)
}

# Perform gradient descent
for (i in 1:10000) {
  beta_hat_gd <- beta_hat_gd - learning_rate * grad(beta_hat_gd)
}

# Print beta_hat_gd
beta_hat_gd

# Define the objective function (i.e., the sum of squared errors)
objfun <- function(beta_hat) {
  sum((X %*% beta_hat - Y)^2)
}

# Set the initial value of beta_hat
beta_hat_l_bfgs <- rep(0, K)

# Set the optimization options
opts <- list("algorithm" = "NLOPT_LD_LBFGS",
             "xtol_rel" = 1.0e-8,
             "maxeval" = 10000)

# Perform the optimization
result_l_bfgs <- nloptr(x0 = beta_hat_l_bfgs, eval_f = objfun, opts = opts)

# Extract the estimated values of beta
beta_hat_l_bfgs <- result_l_bfgs$solution

# Print beta_hat_l_bfgs
beta_hat_l_bfgs


# Set the optimization options
opts <- list("algorithm" = "NLOPT_LN_NELDERMEAD",
             "xtol_rel" = 1.0e-8,
             "maxeval" = 10000)

# Perform the optimization
result_nelder_mead <- nloptr(x0 = beta_hat_l_bfgs, eval_f = objfun, opts = opts)

# Extract the estimated values of beta
beta_hat_nelder_mead <- result_nelder_mead$solution

# Print beta_hat_nelder_mead
beta_hat_nelder_mead



# Define the log-likelihood function
loglik <- function(theta, Y, X) {
  beta <- theta[1:(length(theta)-1)]
  sigma <- theta[length(theta)]
  n <- length(Y)
  -n/2*log(2*pi) - n/2*log(sigma^2) - 1/(2*sigma^2) * sum((Y - X %*% beta)^2)
}

# Define the gradient function
gradient <- function(theta, Y, X) {
  beta <- theta[1:(length(theta)-1)]
  sigma <- theta[length(theta)]
  n <- length(Y)
  -c(t(X) %*% (Y - X %*% beta) / sigma^2, n/sigma - sum((Y - X %*% beta)^2) / sigma^3)
}

# Set the initial values of theta
theta <- c(rep(0, K), 1)

# Set the optimization options
opts <- list("algorithm" = "NLOPT_LD_LBFGS",
             "xtol_rel" = 1.0e-8,
             "maxeval" = 10000)

# Perform the optimization
result <- nloptr(x0 = theta, eval_f = loglik, eval_grad_f = gradient, lb = c(rep(-Inf, K), 0), ub = rep(Inf, K+1), opts = opts, Y = Y, X = X)

# Extract the estimated values of beta and sigma
beta_hat_mle <- result$solution[1:K]
sigma_hat_mle <- result$solution[K+1]

# Print beta_hat_mle and sigma_hat_mle
beta_hat_mle
sigma_hat_mle

# Compute OLS estimate using lm()
ols_lm <- lm(Y ~ X - 1)

# Print summary of the OLS regression
ols_lm_summary <- summary(ols_lm)
modelsummary(ols_lm_summary)


