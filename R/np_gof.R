#' np_glm_b Goodness of Fit
#'
#' Return goodness of fit from np_glm_b object.
#'
#' @param object a np_glm_b fit
#' @details
#'
#'
#'
#'
#' @export
#'

np_glm_b_gof <- function(fit, log_transformed = FALSE){

  if (fit$family$family == "gaussian" & log_transformed == FALSE){
      y_draws <- model.matrix(fit) %*% t(get_posterior_draws(fit))
      y_post_means <- rowMeans(y_draws)
      rslt <- mean(abs(y_post_means - fit$data[as.character(fit$formula)[2]]))

  if (fit$family$family == "gaussian" & log_transformed == TRUE){
      logy_draws <- model.matrix(exp_fit_transformed) %*% t(get_posterior_draws(exp_fit_transformed))
      y_draws <- exp(logy_draws)
      y_post_means <- rowMeans(y_draws)
      rslt <- mean(abs(y_post_means - fit$data[as.character(fit$formula)[2]]))


  }


  }

  




}

# x <- runif(n = 1000)
# y <- 10 + 3*x + rnorm(n = 1000, mean = 0, sd = 1)
#
# dat <- data.frame(x = x, y = y)
# log_dat <- data.frame(y = log(y), x = x)
#
# model <- np_glm_b(formula = y ~ x, data = dat, family = "gaussian")
# log_model <- np_glm_b(formula = y ~ x, data = dat, family = "gaussian")
#
#
#
#
#
# a <- runif(n = 1000)
# y <- 100 + 3 * a + rnorm(n = 1000, mean = 0, sd = 2)
# ylog <- log(y)
#
# mydat_exp <- data.frame("a" = a, "y" = log(y))
# mydat <- data.frame(a,y)
#
# fit <- np_glm_b(formula = y ~ a, family = "gaussian", data = mydat)
# fit <- np_glm_b(formula = y ~ a, family = "gaussian", data = mydat)
# fit_exp <- np_glm_b(formula = log(y) ~ a, family = "gaussian", data = mydat_exp)
#
# sum(abs(predict(fit)$`Post Mean` - fit$data$y))
#
# sum(abs(exp(predict(fit_exp)$`Post Mean`) - fit_exp$data$y))
#
#
# bayesics::get_posterior_draws(fit)
# bayesics::
#
# fit <- bayesics::np_glm_b(formula = bmi ~ skin + glu, family = "gaussian", data = pima, seed = 123)
# get_posterior_draws(fit, seed = 123) -> theta_draws
# a <- rowMeans(model.matrix(fit$formula, fit$data) %*% t(theta_draws))
# b <- predict(fit, seed = 123, n_draws = 100)$`Post Mean`
# max(abs(a - b))
