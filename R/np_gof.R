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
  }

  if (fit$family$family == "gaussian" & log_transformed == TRUE){
      logy_draws <- model.matrix(exp_fit_transformed) %*% t(get_posterior_draws(exp_fit_transformed))
      y_draws <- exp(logy_draws)
      y_post_means <- rowMeans(y_draws)
     rslt <- mean(abs(y_post_means - fit$data[as.character(fit$formula)[2]]))


  }


  return(rslt)

}

