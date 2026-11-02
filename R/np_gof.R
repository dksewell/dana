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

np_glm_b_gof <- function(fit, log_transformed = FALSE, response_var = NULL){

  if ((fit$family$family == "gaussian") && (log_transformed == FALSE)){
      y_draws <- model.matrix(fit$formula, fit$data) %*% t(get_posterior_draws(fit))
      y_post_means <- rowMeans(y_draws)
      rslt <- mean(abs(y_post_means - fit$data[[response_var]]))
  }

   if ((fit$family$family == "gaussian") && (log_transformed == TRUE)){
       logy_draws <- model.matrix(fit$formula, fit$data) %*% t(get_posterior_draws(fit))
       y_draws <- exp(logy_draws)
       y_post_means <- rowMeans(y_draws)
      rslt <- mean(abs(y_post_means - fit$data[[response_var]]))


  }


  return(rslt)

}

 # pima <- MASS::Pima.te
 # #pima$skin <- log(pima$skin)
 # npfit <- bayesics::np_glm_b(formula = log(skin) ~ type, data = pima, family = "gaussian")
 # np_glm_b_gof(npfit, log_transformed = TRUE, response_var = "skin")
# np_glm_b_gof(npfit, log_transformed = FALSE, response_var = "skin")
#
# npfit <- bayesics::np_glm_b(formula = skin ~ type, data = pima, family = "gaussian")
#
# np_glm_b_gof(npfit, log_transformed = TRUE, response_var = "skin")
# np_glm_b_gof(npfit, log_transformed = FALSE, response_var = "skin")

