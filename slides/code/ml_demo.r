# Based on https://github.com/tyarkoni/PPS2016

if(!require("lars")){
  install.packages("lars")
  library("lars")
}
data("diabetes")

X <- diabetes$x
(N <- nrow(X))
pairs(X, pch=20, gap=0, col=rgb(0,0,0,.2))

y <- diabetes$y
hist(y, col="gray")

reg <- lm(y ~ X)
summary(reg)

# Cross-validation "by hand"

set.seed(2024)
new_indices <- sample(nrow(X))
y <- y[new_indices]
X <- X[new_indices,]

breaks <- round(quantile(seq(N), probs = seq(0, 1, .1)))
groups <- cut(seq(N), breaks = breaks, include.lowest = TRUE)
indices <- split(seq(N), groups)
str(indices)


# empty vector to hold results
rmse <- numeric(10)
# do this stuff for each fold
for(i in 1:10){
  # regress y (leaving out ith fold) on X (leaving out ith fold)
  mod <- lm(y[-indices[[i]]] ~ X[-indices[[i]],])
  # use mod's coefficients to get predicted values for the ith fold.
  # we obtain these by matrix multiplication of the new X and old Beta
  predictions <- cbind(1, X[indices[[i]], ]) %*% coef(mod)
  # compute and save RMSE
  errors <- y[indices[[i]]] - predictions
  rmse[i] <- sqrt(mean(errors^2))
}

mean(rmse)
sd(rmse)

# Cross-validation the easy way

if(!require("caret")){
  install.packages("caret")
  library("caret")
}


ctrl <- trainControl(method = "cv", number = 10)
model <- train(y = y, x = X, trControl = ctrl, method = "lm")
model


# Lasso regression
if(!require("glmnet")){
  install.packages("glmnet")
  library("glmnet")
}

lasso <- glmnet(x = X, y = y)
cv <- cv.glmnet(x = X, y = y)

# Fit a model with tiny lambda values ("unregularized")
reg_cv <- cv.glmnet(x = X, y = y, lambda = exp(-100:-99))
# adjust the plot layout to make a two-panel plot
layout(rbind(1,2))
par(mar=c(3,3,2,1)+.1)

# coefficient path plot
plot(lasso, xvar="lambda", xlim=c(-4,4), mgp=2:0)

# CV error plot
plot(cv, xlim = c(-4,4), mgp = 2:0)

# add the baseline performance of the ordinary regression model
with(reg_cv, polygon(x = c(-10, 10, 10, -10, -10),
                     y = c(cvup[1], cvup[1], cvlo[1], cvlo[1], cvup[1]),
                     border = NA, col = rgb(0, 0, 0, .25)))

abline(h = reg_cv$cvm[1], lty = 2, lwd = 2)

legend("topleft", lty = 2, lwd = 2,
       legend = "Ordinary regression MSE", bty = "n")



