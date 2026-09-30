#' Fit Continuous Outcome For Cross Sectional Data
#'
#'
#' @param data
#' @param prior
#' @param response_var description
#' @param pred_cat_vars description
#' @param pred_cont_vars description
#' @param pred_cat_vars_ref_levels description
#' @param ci_level description
#' @details
#' 
#' Returns a list subject to model cascade. Information includes
#' final model fit, any intermediary test statistics.
#'
#' This is a object-like list - other functions expect list with same setup.
#'
#' @export
#'
#'
#'


fit_cross_sectional_continuous <- function(
    data,
    prior = "improper",
    response_var = NULL,
    pred_cat_vars = NULL,
    pred_cont_vars = NULL,
    pred_cat_vars_ref_levels = NULL,
    ci_level = NULL) {
      

  # Preprocessing --------------------------------------------------------------

  ref_list <- lapply(X = seq_along(pred_cat_vars_ref_levels), FUN = function(i){
    pred_cat_vars_ref_levels[[i]]
  }) |>
    setNames(pred_cat_vars)

  ## Apply reference levels to all categorical predictors
  if (!is.null(pred_cat_vars_ref_levels)){
    ref_level_list <- lapply(setNames(pred_cat_vars_ref_levels, pred_cat_vars), function(x) x)

    data <- data |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(pred_cat_vars), ~ {
        relevel(factor(as.character(.x)),
                ref = ref_list[[dplyr::cur_column()]])
      }))
  }


  # Model Cascade --------------------------------------------------------------

  rslt <- list()

  rslt$log_transformed <- FALSE

  ## Pass on arguments
  rslt$response_var <- response_var
  rslt$pred_cat_vars <- pred_cat_vars
  rslt$pred_cont_vars <- pred_cont_vars
  rslt$pred_cat_vars_ref_levels <- pred_cat_vars_ref_levels

  untransformed_formula <- as.formula(paste0(response_var, " ~ ", paste(c(pred_cat_vars, pred_cont_vars), collapse = " + ")))
  log_transformed_formula <- as.formula(paste0("log(", response_var, ") ~ ", paste(c(pred_cat_vars, pred_cont_vars), collapse = " + ")))

  ## Parametric Untransformed model
  rslt$parametric_untransformed <- bayesics::glm_b(
    family = "gaussian",
    formula = untransformed_formula,
    data = data
    )
  rslt$bpval_parametric_untransformed <- bayesics::bayes_pvalue(rslt$parametric_untransformed)$bpval
  print(rslt$bpval_parametric_untransformed)

  ## Positive outcome
  rslt$positive_response <- ifelse(all(data[[response_var]] > 0), yes = TRUE, no = FALSE)

  ## Terminal Node 1
  if (rslt$bpval_parametric_untransformed > 0.05 && rslt$bpval_parametric_untransformed < 0.95) {
    rslt$fit <- rslt$parametric_untransformed
    rslt$term_node <- 1
    rslt$fit_class <- class(rslt$fit)
    rslt$fit_family <- rslt$fit$family$family
    return(rslt)
  }

  ## Terminal Node 2
  if (rslt$positive_response == FALSE) {
    rslt$nonparametric_untransformed <- bayesics::np_glm_b(
      family = "gaussian",
      formula = untransformed_formula,
      data = data
    )
    rslt$fit <- rslt$nonparametric_untransformed
    rslt$term_node <- 2
    rslt$log_transformed <- TRUE
    rslt$fit_class <- class(rslt$fit)
    rslt$fit_family <- rslt$fit$family$family
    return(rslt)
  }


  ## Parametric Log transformed model
  rslt$parametric_log_transformed <- bayesics::glm_b(
    family = "gaussian",
    formula = log_transformed_formula,
    data = data,
    prior = prior,
    )

  rslt$bpval_parametric_log_transformed <- bayesics::bayes_pvalue(rslt$parametric_log_transformed)$bpval

  ## Terminal Node 3
  if (rslt$bpval_parametric_log_transformed > 0.05 && rslt$bpval_parametric_log_transformed < 0.95) {
    rslt$fit <- rslt$parametric_log_transformed
    rslt$log_transformed <- TRUE
    rslt$term_node <- 3
    rslt$fit_class <- class(rslt$fit)
    rslt$fit_family <- rslt$fit$family$family
    return(rslt)
  }

  ## Nonparametric log transformed model
  rslt$nonparametric_log_transformed <- bayesics::np_glm_b(
    family = "gaussian",
    formula = log_transformed_formula,
    data = data
  )

  rslt$gof_nonparametric_log_transformed <- 1
  rslt$gof_nonparametric_untransformed <- 2
  print("replacement needed! XXX")

  rslt$gof_comparison <- rslt$gof_nonparametric_log_transformed < rslt$gof_nonparametric_untransformed

  ## Terminal Node 4
  if (rslt$gof_nonparametric_log_transformed < rslt$gof_nonparametric_untransformed){
    rslt$fit <- rslt$nonparametric_log_transformed
    rslt$term_node <- 4
    rslt$log_transformed <- TRUE
    rslt$fit_class <- class(rslt$fit)
    rslt$fit_family <- rslt$fit$family$family
    return(rslt)
  }

  ## Terminal Node 5
  if (rslt$gof_nonparametric_log_transformed >= rslt$gof_nonparametric_untransformed){
    rslt$fit <- rslt$nonparametric_untransformed
    rslt$term_node <- 5
    rslt$fit_class <- class(rslt$fit)
    rslt$fit_family <- rslt$fit$family$family
    return(rslt)
  }

}



# pima <- MASS::Pima.te
# pima$npreg <- factor(pima$npreg)
# pima_fit1 <- fit_cross_sectional_continuous(pima, response_var = "skin", pred_cat_vars = c("type", "npreg"),
#                                     pred_cont_vars = "glu", pred_cat_vars_ref_levels = c("Yes", "3"))
