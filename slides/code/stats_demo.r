# I used co-pilot to program some these examples. In particular, since I am new
# to using R, but have a lot of experience in another language, I used the
# "chat" functionality to help translate code snippets. It also helped me
# understand how "boot" works, suggested ideas for datasets to use in examples
# and helped correct string formatting issues and debug broadcasting errors.

library(boot)
library(ggplot2)

# Function to calculate the mean:
calc_mean <- function(data, indices) {
  return(mean(data[indices]))
}

# Generate a non-Gaussian distribution
set.seed(2024)
data <- rexp(10000, rate = 0.5)

# First, the jackknife:

jackknife_estimates <- numeric(length(data))

# Jackknife sampling where $\theta$ is the mean
for (i in seq_along(data)) {
  jackknife_estimates[i] <-
    calc_mean(data, -i)
}

# Calculate the jackknife estimate and standard error:
jackknife_estimate <- mean(jackknife_estimates)

jackknife_se <-
    sqrt((length(data) - 1) *
        mean((jackknife_estimates - jackknife_estimate)^2))

# Calculate the jackknife bias
jackknife_bias <- jackknife_estimate - calc_mean(data)

# Print the jackknife estimate, standard error and bias:
cat(paste("Estimate: ", calc_mean(data), "\n",
          "Jackknife estimate: ", jackknife_estimate, "\n",
          "Jackknife SE: ", jackknife_se, "\n",
          "Jackknife bias: ", jackknife_bias, "\n", sep = ""))


# Using Jackknife to assess an OLS model

# Use library(MASS) to access the mtcars dataset
library(MASS)

# Define the model
model <- lm(mpg ~ hp + wt, data = mtcars)
model_summary <- summary(model)

# Define a function to calculate the OLS coefficients
calc_coef <- function(data, indices) {
    model <- lm(mpg ~ hp + wt, data = mtcars[indices, ])
    return(list(coef(model), summary(model)$r.squared))
}

# Initialize a matrix to store the jackknife estimates
jackknife_estimates <- matrix(nrow = nrow(mtcars),
                              ncol = length(coef(model)) + 1)

# Perform jackknife resampling
for (ii in seq_along(mtcars[, 1])) {
    jackknife_estimates[ii, ] <- unlist(calc_coef(mtcars, -i))
}

# Calculate the jackknife estimate, se and bias
jackknife_estimate <- colMeans(jackknife_estimates)
jackknife_se <- sqrt((nrow(mtcars) - 1) *
    colMeans((t(t(jackknife_estimates) - jackknife_estimate))^2))
jackknife_bias <- jackknife_estimate -
    unlist(list(coef(model), model_summary$r.squared))

# Print the original OLS coefficients, the jackknife estimates, jacknife SE,
# and jackknife bias
for (ii in seq_along(coef(model[1]))) {
    cat(paste(
        "Coefficient: ", names(coef(model)[ii]), "\n",
        "Estimate: ", toString(coef(model)[ii]), "\n",
        "Jackknife estimate: ", toString(jackknife_estimate[ii]), "\n",
        "Jackknife SE: ", jackknife_se[ii], "\n",
        "Jackknife bias: ", jackknife_bias[ii], "\n", sep = ""))
}

ii <- ii + 1
cat(paste(
    "R^2:", "\n",
    "Estimate: ", toString(model_summary$r.squared), "\n",
    "Jackknife estimate: ", toString(jackknife_estimate[ii]), "\n",
    "Jackknife SE: ", jackknife_se[ii], "\n",
    "Jackknife bias: ", jackknife_bias[ii], "\n", sep = ""))


######################################################################
# Bootstrapping

# Use the boot function to perform bootstrapping
boot_results <- boot(data, calc_mean, R = 10000)

sorted_boot_results <- sort(boot_results$t)

# Plot sorted results with ggplot
ggplot(data.frame(sorted_boot_results),
    aes(x=seq_along(sorted_boot_results),
    y = sorted_boot_results)) +
    geom_line()

# Calculate standard error (central 68% of the distribution ):

ci <- quantile(sorted_boot_results, c(0.16, 0.84))
std_err <- 0.5 * (ci[2] - ci[1])
cat(paste(
    "Standard Error of the Mean: ", std_err, "\n"))

# Calculate standard error (std of the distribution):
std_err <- sd(sorted_boot_results)
cat(paste(
    "Standard Error of the Mean: ", std_err, "\n"))

# Calculate 95% CI:
ci <- quantile(sorted_boot_results, c(0.025, 0.975))

cat(paste(
    "95% Confidence Interval: ", ci[1], ci[2], "\n"))

# Define a non-linear model function
nonlinear_model <- function(x, alpha, beta, kappa) {
    alpha - (alpha - beta) * exp(- x / kappa)
}


# Generate some sample data
set.seed(123)
x <- seq(1, 100, length.out = 100)
y <- nonlinear_model(x, 100, 10, 20) + rnorm(length(x), mean = 0, sd = 2)

ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_line()

# Define the function to estimate the parameters using the non-linear model
estimate_parameters <- function(data, indices) {
    x <- data$X[indices]
    y <- data$Y[indices]
    fit <- nls(
        y ~ nonlinear_model(x, alpha, beta, kappa),
             start = list(
                alpha = 100, beta = 10, kappa = 20))
    coef(fit)
}

# Perform bootstrap estimation
boot_results <- boot(
    data = data.frame(X = x, Y = y),
    statistic = estimate_parameters, R = 10000)

# Get the bootstrap estimates of the parameters
bootstrap_estimates <- boot_results$t

# Calculate the variance of the bootstrap estimates
variance_estimates <- apply(bootstrap_estimates, 2, var)

# Calculate the confidence intervals for the parameters
conf_intervals <- boot.ci(boot_results,
                          conf=0.95,
                          type = "prec")
print(conf_intervals)
