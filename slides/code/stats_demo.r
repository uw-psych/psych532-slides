# I used co-pilot to program some these examples. In particular, co-pilot helped
# me understand how "boot" works, suggested ideas for datasets to use in
# examples and helped correct string formatting issues. I also used it to
# debug broadcasting in R (which is differet from Python...).

library(boot)

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


# Next, bootstrapping

# Use the boot function to perform bootstrapping
boot_results <- boot(data, calc_mean, R = 10000)

# Calculate the standard error
std_error <- sd(boot_results$t)
print(paste("Standard Error of the Mean: ", std_error))


# Define the non-linear model function
nonlinear_model <- function(x, a, tau) {
    1 - a * exp(x/tau)
}

# Generate some sample data
set.seed(123)
x <- seq(-10, 10, length.out = 100)
y <- nonlinear_model(x, 2, 3) + rnorm(length(x), mean = 0, sd = 2)

# Define the function to estimate the parameters using the non-linear model
estimate_parameters <- function(data, indices) {
    x <- data$X[indices]
    y <- data$Y[indices]
    fit <- nls(y ~ nonlinear_model(x, a, tau), start = list(a = 1, tau = 1))
    coef(fit)
}

# Perform bootstrap estimation
boot_results <- boot(data = data.frame(X = x, Y = y), statistic = estimate_parameters, R = 1000)

# Get the bootstrap estimates of the parameters
bootstrap_estimates <- boot_results$t

# Calculate the variance of the bootstrap estimates
variance_estimates <- apply(bootstrap_estimates, 2, var)

# Print the variance estimates
print(variance_estimates)

# Plot the bootstrap distribution of the parameters with ggplot:
library(ggplot2)
bootstrap_df <- data.frame(a = bootstrap_estimates[,1], tau = bootstrap_estimates[,2])
ggplot(bootstrap_df, aes(x = a)) +
    geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black") +
    labs(title = "Bootstrap Distribution of Parameter a") +
    theme_minimal()


ggplot(bootstrap_df, aes(x = tau)) +
    geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black") +
    labs(title = "Bootstrap Distribution of Parameter tau") +
    theme_minimal()

# Calculate the confidence intervals for the parameters
conf_intervals <- boot.ci(boot_results[, 1], type = "bca", index = 1:2)
print(conf_intervals)

# Jacknife example
# Generate a non-Gaussian distribution
set.seed(123)
data <- rexp(1000, rate = 0.5)


# Initialize a vector to store the jackknife estimates
jackknife_estimates <- numeric(length(data))


# Calculate with bootstrap:
# Define a function to calculate the mean
calc_mean <- function(data, indices) {
  return(mean(data[indices]))
}

# Use the boot function to perform bootstrapping
boot_results <- boot(data, calc_mean, R = 1000)
