# Purpose of this is to demo the np_glm_gof function. This function
# compares a log transformed fit of a np_glm object with an untransformed
# version. These are compared with the AEL statistic, with both models
# compared on the original (nonlog scale).


# Get package

library(bayesics)

# Generate normal data and fit models
set.seed(123)
x <- runif(n = 100)
y <- 2 + 2 * x + rnorm(n = 100, mean = 0, sd = 0.25) # True beta is 2

dat <- data.frame(x = x, y = y)

plot(dat$x, dat$y)

fit_untransformed <- np_glm_b(formula = y ~ x, family = "gaussian", data = dat)
fit_transformed <- np_glm_b(formula = log(y) ~ x, family = "gaussian", data = dat)

mean(abs(predict(fit_untransformed)$`Post Mean` - fit_untransformed$data$y))
mean(abs(exp(predict(fit_transformed)$`Post Mean`) - fit_transformed$data$y))


# Data generated on exponential scale
set.seed(123)
x <- runif(n = 1000)
y <- 2 + 2 * x + rnorm(n = 1000, mean = 0, sd = 0.25) # True beta is 2

exp_dat <- data.frame(y = exp(y), x = x)

plot(exp_dat$x, exp_dat$y)


exp_fit_untransformed <- np_glm_b(formula = y ~ x, family = "gaussian", data = exp_dat) #Nontransformed
exp_fit_transformed <- np_glm_b(formula = log(y) ~ x, family = "gaussian", data = exp_dat) # Log transformed

## Get AEL for log transformed model

# Get draws of beta
t(get_posterior_draws(exp_fit_transformed)) # 2 x 10000

# Get model matrix
model.matrix(exp_fit_transformed) # 100 x 2

# Get set of posterior predictions on the log scale
model.matrix(exp_fit_transformed) %*% t(get_posterior_draws(exp_fit_transformed))

# Convert these posterior predictions on the regular y scale
y_post_draws_1 <- exp(model.matrix(exp_fit_transformed) %*% t(get_posterior_draws(exp_fit_transformed)))

# Get MAP estimates means of the y values
y_post_means_1 <- rowMeans(y_post_draws_1) 

# Get AEL by comparing MAP posterior of y's to the actual y values
mean(abs(y_post_means_1 - exp_dat$y))

plot(exp_dat$x, y_post_means_1)
points(exp_dat$x, exp_dat$y, col = "red", xlim = c(0, 1), ylim = c(0, 50))


## Do the same for the non log-transformed model

# Get draws of beta
t(get_posterior_draws(exp_fit_untransformed)) # 2 x 10000

# Get model matrix
model.matrix(exp_fit_untransformed) # 100 x 2

# Get set of posterior predictions
y_post_draws_2 <- model.matrix(exp_fit_untransformed) %*% t(get_posterior_draws(exp_fit_untransformed))

# Get MAP estimates means of the y values
y_post_means_2 <- rowMeans(y_post_draws_2) 

# Get AEL by comparing MAP posterior of y's to the actual y values
mean(abs(y_post_means_2 - exp_dat$y))

plot(exp_dat$x, exp_dat$y, xlim = c(0, 1), ylim = c(0, 50))
points(exp_dat$x, y_post_means_2, col = "blue")
points(exp_dat$x, y_post_means_1, col = "red")


# Question: with large sample approximation, is it appropriate to treat posterior draws generated from the 
# distribution as that distribution itself? i.e. comparing individual draws from N(betahat, Sigma)
# to get approximate GOF? It feels as if using the values from MCMC or some kind of computational
# would be a better approach for GOF.
