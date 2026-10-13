#' Fit Count Outcome For Cross Sectional Data
#'
#'
#' @param data
#' @param prior
#' @param response_var description
#' @param pred_cat_vars description
#' @param pred_cont_vars description
#' @param pred_cat_vars_ref_levels description
#' @param offset_var description
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
#'
#'
#'


fit_cross_sectional_count <- function(
    data,
    prior = "improper",
    response_var = NULL,
    pred_cat_vars = NULL,
    pred_cont_vars = NULL,
    pred_cat_vars_ref_levels = NULL,
    offset_var = NULL,
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

rslt$offset <- !is.null(offset_var)

## Pass on arguments
rslt$response_var <- response_var
rslt$pred_cat_vars <- pred_cat_vars
rslt$pred_cont_vars <- pred_cont_vars
rslt$pred_cat_vars_ref_levels <- pred_cat_vars_ref_levels
rslt$offset_var <- offset_var

if (is.null(offset_var)){
  model_formula <- as.formula(paste0(response_var, " ~ ", paste(c(pred_cat_vars, pred_cont_vars), collapse = " + ")))
} else {
  model_formula <- as.formula(paste0(response_var, " ~ ", paste(c(pred_cat_vars, pred_cont_vars), collapse = " + "), " + offset(log(", offset_var, "))"))
}

## Poisson glm
rslt$poisson <- bayesics::glm_b(
  family = "poisson",
  formula = model_formula,
  data = data
)
rslt$bpval_poisson <- bayesics::bayes_pvalue(rslt$poisson)$bpval

## Terminal Node 1
if (rslt$bpval_poisson > 0.05 && rslt$bpval_poisson < 0.95) {
  rslt$fit <- rslt$poisson
  rslt$term_node <- 1
  rslt$fit_class <- class(rslt$fit)
  rslt$fit_family <- rslt$fit$family$family
  return(rslt)
}

## Negative Binomial glm
rslt$nb <- bayesics::glm_b(
  family = negbinom(),
  formula = model_formula,
  data = data
)

rslt$bpval_nb <- bayesics::bayes_pvalue(rslt$nb)$bpval

## Terminal Node 2
if (rslt$bpval_nb > 0.05 && rslt$bpval_nb < 0.95) {
  rslt$fit <- rslt$nb
  rslt$term_node <- 2
  rslt$fit_class <- class(rslt$fit)
  rslt$fit_family <- rslt$fit$family$family
  return(rslt)
}

## Nonparametric fit
rslt$nonparametric <- bayesics::np_glm_b(
  family = "poisson",
  formula = model_formula,
  data = data
)

## Terminal Node 3
if (rslt$bpval_nb < 0.05 || rslt$bpval_nb > 0.95) {
  rslt$fit <- rslt$nonparametric
  rslt$term_node <- 3
  rslt$fit_class <- class(rslt$fit)
  rslt$fit_family <- rslt$fit$family$family
  return(rslt)
}

}

  # pima <- MASS::Pima.te
  # pima_fit1 <- fit_cross_sectional_count(pima, response_var = "npreg", pred_cat_vars = c("type"),
  #                                     pred_cont_vars = "glu", pred_cat_vars_ref_levels = c("Yes"), offset_var = "age")

  
  