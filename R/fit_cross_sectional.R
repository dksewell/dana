#' Cross Sectional Router
#'
#' Selects appropriate fitting functions and return values for cross
#' sectional designs
#'
#' @param response_type One of "Continuous", "Binary", "Count"
#' @param data
#' @param prior
#' @param response_var description
#' @param pred_cat_vars description
#' @param pred_cont_vars description
#' @param pred_cat_vars_ref_levels description
#' @param ci_level description
#' @details
#' 
#' @returns List with flowchart, summary table of final fit,
#' @details Placeholder
#'
#' @export
#'
#' 

fit_cross_sectional <- function(
  response_type = NULL,
  data,
  prior = "improper",
  response_var = NULL,
  pred_cat_vars = NULL,
  pred_cont_vars = NULL,
  pred_cat_vars_ref_levels = NULL,
  ci_level = NULL) {
  
  rslt <- list()

    if (!(response_type %in% c("Continuous", "Count", "Binary"))){
      stop("Unrecognized outcome type")
    }

    # Continuous
    if (response_type == "Continuous"){
      
      dana_fit <- fit_cross_sectional_continuous(
          data = data,
          prior = prior,
          response_var = response_var,
          pred_cat_vars = pred_cat_vars,
          pred_cont_vars = pred_cont_vars,
          pred_cat_vars_ref_levels = pred_cat_vars_ref_levels,
          ci_level = ci_level
      )
      if (dana_fit$fit_class == "lm_b"){

        rslt$dana_fit <- interpret.lm_b(
          dana_fit = dana_fit
        )

      if (dana_fit$fit_class == "np_glm_b"){
        stop("np_glm_interpret needed!")
      }
      }
      rslt$flowchart <- modify_continuous_flowchart(dana_fit) |> DiagrammeR::generate_dot()
      
    }
  
  return(rslt)
}


# analysis <- fit_cross_sectional(
#   response_type = "Continuous",
#   data = pima,
#   prior = "improper",
#   response_var = "bmi",
#   pred_cat_vars = "type",
#   pred_cont_vars = NULL,
#   pred_cat_vars_ref_levels = "Yes",
#   ci_level = 0.95)

