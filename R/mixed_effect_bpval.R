#' Pearson Statistic BPval
#'
#' Calculates bayesian P-value based on pearson statistic for rstanrm mixed models
#'
#' @param object Mixed effect GLM from package rstanarm
#' @details Returns a bayesian P-value based on model fit. The statistic
#' is calculated by fitting predicted residuals vs and fitted values, and
#' fitting observed residuals vs fitted values. For both of these relationships,
#' a loess curve is fit and the loess-fit predicted residuals. The sum of
#' squares of these loess-fit predicted residuals is compared across multiple
#' posterior draws. In particular:
#'
#' Define \eqn{y_i^{(d)}} as the \eqn{i}th posterior predicted value from the \eqn{d}th posterior draw of \eqn{\theta}.
#' Similarly, define \eqn{\mu_i^{(d)} = E[y_i|\theta^{(d)}]} as the expected value of \eqn{y_i} under the \eqn{d}th posterior draw of $\theta$.
#'
#' Consider the \eqn{n} observed residuals and the \eqn{n} predicted residuals:
#'
#'\deqn{r_{i, obs}^{(d)} = y_i - \mu_i^{(d)}}
#'\deqn{r_{i, pred}^{(d)} = y_i^{(d)} - \mu_i^{(d)} }
#'
#'If we fit two loess curves:
#'
#'\deqn{\text{LOESS}(\mathbf{r}_{obs}^{(d)}, \boldsymbol{\mu}^{(d)})}
#'\deqn{\text{LOESS}(\mathbf{r}_{pred}^{(d)}, \boldsymbol{\mu}^{(d)})}
#'
#'
#'These two models give rise to the predicted residuals: \eqn{\hat{\mathbf{r}}_{obs}^{(d)}}
#'  and \eqn{\hat{\mathbf{r}}_{pred}^{(d)}}. Define the statistics
#'
#'\deqn{T_{obs}^{(d)} = \frac{1}{n}\sum_{i = 1}^n \left(\hat{r}_{obs}^{(d)}\right)^2}
#'\deqn{T_{pred}^{(d)} = \frac{1}{n}\sum_{i = 1}^n \left(\hat{r}_{pred}^{(d)}\right)^2}
#'
#'The bayesian p-value is given by
#'
#'\deqn{\frac{1}{D}\sum_{d = 1}^{D} I(T_{pred}^{(d)} > T_{obs}^{(d)})}
#'
#'
#' @returns A list containing the bayesian P-value, and a plot of \eqn{T_{obs}} vs \eqn{T_{pred}}
#'
#' @export

glme_bpv <- function(object){

  # Response
  y_obs <- object$y

  # Predicted expectations
  mu <- rstanarm::posterior_epred(object)

  # Response vs fitted residuals
  res_obs <- matrix(data = y_obs,
                    nrow = nrow(mu),
                    ncol = NROW(y_obs),
                    byrow = T) - mu

  # Predicted response
  y_pred <- rstanarm::posterior_predict(object)

  # Predicted response vs fitted residuals
  res_pred <- y_pred - mu

  loess_obs <- function(i){

    # Fit loess to residuals vs fitted
    loess_fit = loess(res_obs[i,] ~ mu[i,]) |> predict()

    sum(loess_fit^2)

  }

  loess_pred <- function(i){

    # Fit loess to residuals vs fitted
    loess_fit = loess(res_pred[i,] ~ mu[i,]) |> predict()

    sum(loess_fit^2)

  }

  T_obs <- sapply(1:nrow(res_obs), loess_obs)
  T_pred <- sapply(1:nrow(res_pred), loess_pred)


  rslt <- list(
    bpval = mean(T_obs > T_pred),
    bpplot =
      ggplot2::ggplot(data.frame(T_obs, T_pred)) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = T_obs, y = T_pred)) +
      ggplot2::geom_abline(slope = 1, intercept = 0)
  )

  return(rslt)

}
