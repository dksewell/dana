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

        interpretation <- interpret.lm_b(
          dana_fit = dana_fit
        )

      # Results list
      rslt$flowchart <- modify_continuous_flowchart(dana_fit) |> DiagrammeR::generate_dot()
      rslt$interpretation <- interpret.lm_b(dana_fit = dana_fit)
      rslt$dana_fit <- dana_fit
      rslt$summary <- dana_fit$fit$summary |>
        setNames(
          c("Variable",
            "Estimate", "Lower CI Bound",
            "Upper CI Bound",
            "PDir", "ROPE", "ROPE Bounds")
        ) |>
        DT::datatable() |>
        DT::formatSignif(columns = 2:6, digits = 3)
      rslt$interpret_notes <- paste0("For gaussian (continuous) data, the ROPE is built under the principle that a predictor's effect is considered negligible ",
               "if a large change in the predictor (two standard deviations) would shift the average outcome by a very ",
               "small amount (a tenth of the outcome's standard deviation). The ROPE bounds describe the range of coefficient values ",
               "that meet this condition. A large probability that the effect is outside the ROPE indicates practically ",
               "significant association between the predictor and outcome.")
      rslt$fit_info <- data.frame("Formula" = deparse(dana_fit$fit$formula),
                                  "Prior" = dana_fit$fit$prior) |>
        DT::datatable()
    }

  return(rslt)
}

# pima <- MASS::Pima.tr
# analysis <- fit_cross_sectional(
#   response_type = "Continuous",
#   data = pima,
#   prior = "improper",
#   response_var = "bmi",
#   pred_cat_vars = "type",
#   pred_cont_vars = NULL,
#   pred_cat_vars_ref_levels = "Yes",
#   ci_level = 0.95)


# mydat <- data.frame(
#   a = runif(50,0,1),
#   b = runif(50,0,1),
#   y = rexp(n = 50, 4)
# )

# custom_fit1 <- fit_cross_sectional(mydat, pred_cont_vars = c("a","b"), response_var = "y", response_type = "Continuous")


