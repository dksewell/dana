#' Generate Formula Objects for Cross Sectional Regression
#'
#' Generates formulas based on regression type and selected variables
#'
#' @param family family specifying distributional assumption on outcome
#' @param pred_cat A vector of strings referring to categorical predictor variables
#' @param pred_cont A vector of strings referring to continuous predictor variables
#' @param outcome_var A string referring to the outcome variable
#' @param offset_var A string referring to offset variable. Ignored unless family == "poisson"
#' @returns A formula object
#' @details
#'  Manipulates provided string objects into a ready-to-use formula object
#'
#' @export
#'


cross_sectional_formula <- function(family,
                                    pred_cat,
                                    pred_cont,
                                    outcome_var,
                                    offset_var = NULL){
  if (family == "binomial"){

    rslt <- as.formula(paste0(outcome_var, " ~ ", paste(c(pred_cat, pred_cont), collapse = " + ")))

  } else if (family == "gaussian"){

    rslt <- as.formula(paste0(outcome_var, " ~ ", paste(c(pred_cat, pred_cont), collapse = " + ")))


  } else if (family == "poisson"){
    if (is.null(offset_var) || offset_var == "None"){

      rslt <- as.formula(paste0(outcome_var, " ~ ", paste(c(pred_cat, pred_cont), collapse = " + ")))

    } else {

      rslt <- as.formula(paste0(outcome_var, " ~ ", paste(c(pred_cat, pred_cont), collapse = " + "), " + offset(log(", offset_var, "))"))

    }
  }

  return(rslt)
}


#' Bayesian Inference for Continuous Outcomes
#'
#' Wrapper for lm_b for continuous outcomes, with additional diagnostics and defaults.
#'
#' @param data A data frame with containing the variables specified in the formula.
#' @param prior A string of either "improper", "zellner", or "conjugate"
#' @param family family specifying distributional assumption on outcome, either "gaussian", "poisson", or "binomial"
#' @param outcome_var A string referring to the outcome variable
#' @param offset_var A string referring to offset variable. Ignored unless family == "poisson"
#' @param pred_cat A vector of strings referring to categorical predictor variables
#' @param pred_cont A vector of strings referring to continuous predictor variables
#' @param ref_levels A string vector specifying the reference levels for all categorical variables
#' @param ci_level A numeric value specifying the credible interval level
#'
#' @returns A list containing the results from the lm_b fit, along with potential diagnostic plots.
#' @details
#' Performs bayesian linear models with improper prior. Also generates
#' basic diagnostic plots depending on the model type. The plot outputs
#' are of class ggplot2::ggplot, tables are of class DT::dt, and info is of class list.
#' The following are provided for each model type:
#'
#' Gaussian - Table, info, interpretations, QQ plot, homoscedasticity plot
#' Poisson - Table, info, interpretations
#' Binomial - Table, info, interpretations
#'
#'
#' @export
#'

cross_sectional <- function(
    data,
    prior = "improper",
    family = "gaussian",
    offset_var = NULL,
    outcome_var = NULL,
    pred_cat = NULL,
    pred_cont = NULL,
    ref_levels = NULL,
    ci_level = NULL) {

  # ----------------------------------------------------------------------------
  # Evaluate possible errors

  if (!is.null(pred_cat)){
    if (outcome_var %in% pred_cat){
      errorCondition(message = "Outcome variable is specified as a categorical predictor.")

    }
  }
  if (!is.null(pred_cont)){
    if (outcome_var %in% pred_cont){

      errorCondition(message = "Outcome variable is specified as a continuous predictor.")
    }
  }
  if (!is.null(offset_var) && (offset_var %in% c(pred_cat, pred_cont))){
    errorCondition(message = "Offset variable is specified as a predictor.")
  }
  if (length(ref_levels) != length(pred_cat)){
    errorCondition(message = "Reference levels do not match provided categorical predictors")
  }

  # ----------------------------------------------------------------------------
  ## General results

  rslt <- list()

  # Build formula
  formula <- cross_sectional_formula(
    family = family,
    pred_cat = pred_cat,
    pred_cont = pred_cont,
    outcome_var = outcome_var,
    offset_var = offset_var)

  # Define levels of reference categories for all categorical predictors
  if (!is.null(ref_levels)){
    ref_list <- lapply(setNames(ref_levels, pred_cat), function(x) x)

    # Apply reference categories to all categorical predictor columns
    data <- data |>
      dplyr::mutate(dplyr::across(dplyr::all_of(pred_cat), ~ {
        relevel(factor(as.character(.x)), ref = ref_list[[dplyr::cur_column()]])
      }))
  }

  # Fit model
  fit <- bayesics::glm_b(
    family = family,
    formula,
    data,
    prior = prior,
    CI_level = ci_level)

  # Bayes p-value
  pval <- bayesics::bayes_pvalue(fit)$bpvalue
  bpval <- bayesics::bayes_pvalue(fit)$bpvalue
  rslt$diagnostic <- paste0("A ", family, " regression model was used. The bayesian p-value for deviance is: ",
                            round(pval, digits = 4), ". This indicates the model reasonably fits the data.")

  # Model cascade if p-value is bad
  if (pval > 0.95 || pval < 0.05) {
    if (family == "gaussian" || family == "binomial") {
      fit <- bayesics::np_glm_b(
        formula,
        data,
        family,
        CI_level = ci_level
      )
      rslt$diagnostic <- paste0("The results are based on a non-parametric regression model. A ", family,
                                " regression model was initially fitted, but the Bayesian p-value for deviance of ",
                                round(pval, digits = 4), " suggested lack of fit.")
    }

    if (family == "poisson") {
      fit <- bayesics::glm_b(
        formula,
        data,
        family = "negbinom",
        prior = prior,
        CI_level = ci_level
      )
      pval_nb <- bayesics::bayes_pvalue(fit)$bpvalue
      rslt$diagnostic <- paste0("The results are based on a negative binomial regression model. The Bayesian p-value for deviance of ",
                                round(pval_nb, digits = 4), " indicates adequate model fit. A poisson regression model was initially fitted, but the Bayesian p-value for deviance of ",
                                round(pval, digits = 4), " suggested lack of fit.")

      if (pval_nb > 0.95 || pval_nb < 0.05) {
        fit <- bayesics::np_glm_b(
          formula,
          data,
          family = "poisson",
          CI_level = ci_level
        )
        rslt$diagnostic <- paste0("The results are based on a non-parametric regression model. A parametric poisson and negative binomial regression model were initially fitted, but the Bayesian p-values for deviance of ",
                                  round(pval, digits = 4), " and ", round(pval_nb, digits = 4), " suggested lack of fit.")
      }
    }
  }

  # Table displaying fit information
  rslt$table <- fit$summary |>
    DT::datatable() |>
    DT::formatSignif(columns = 2:6, digits = 3)

  # Human readable version of fit information
  rslt$table_hr  <- fit$summary %>% filter(Variable != "log(phi)")
  colnames(rslt$table_hr) <- c("Variable", "Estimate", "Lower CI Bound",
                               "Upper CI Bound", "PDir", "ROPE", "ROPE Bounds")

  if (family %in% c("poisson", "binomial")) {
    rslt$table_hr <- rslt$table_hr |>
      dplyr::mutate(`Estimate` = exp(`Estimate`),
                    `Lower CI Bound` = exp(`Lower CI Bound`),
                    `Upper CI Bound` = exp(`Upper CI Bound`))
    rope_bound <- strsplit(gsub("(", "", gsub(")", "", rslt$table_hr$`ROPE Bounds`), fixed = TRUE), ",")
    rslt$table_hr$`ROPE Bounds` <-
      lapply(seq_along(rope_bound), function(i) {
        paste0("(", round(exp(as.numeric(rope_bound[[i]][1])),3), ", ", round(exp(as.numeric(rope_bound[[i]][2])), 3), ")")
      })
  }

  rslt$table_hr <- rslt$table_hr |>
    DT::datatable() |>
    DT::formatSignif(columns = 2:6, digits = 3)

  # Extract values
  variable <- rslt$table_hr$x$data$Variable[-1]
  cat_var <- variable
  match_cat <- paste(paste0("^", pred_cat), collapse = "|")
  cat_level <- gsub(match_cat, "", variable)
  if (identical(match_cat, "^")) {
    cat_ind <- replicate(length(variable), FALSE)
  } else {
    cat_ind <- grepl(match_cat, variable)
  }
  cat_ref_levels <- cat_level
  for (i in seq_len(length(pred_cat))) {
    index <- grepl(pred_cat[i], variable)
    cat_var[index] <- pred_cat[i]
    cat_ref_levels[index] <- ref_levels[i]
  }
  coef <- rslt$table_hr$x$data$"Estimate"[-1]
  ci_lower <- rslt$table_hr$x$data$"Lower CI Bound"[-1]
  ci_upper <- rslt$table_hr$x$data$"Upper CI Bound"[-1]
  pdir_val <- round(rslt$table_hr$x$data$"PDir", 4)[-1]
  pdir_val <- ifelse(pdir_val == 1, 0.9999, pdir_val)
  rope_val <- round(rslt$table_hr$x$data$"ROPE", 4)[-1]
  rope_bounds <- rslt$table_hr$x$data$"ROPE Bounds"[-1]


  # ----------------------------------------------------------------------------
  ## Family specific functionalities

  if (family == "gaussian"){

    rslt$interpret <- setNames(
      lapply(seq_along(variable), function(i) {
        if (cat_ind[i]) {
          list(
            estimate = paste0("We estimate that the mean ", outcome_var, " for observations with ", cat_var[i], " = ",
                              cat_level[i], " is ", abs(round(coef[i], 3)),  " unit ", ifelse(coef[i] == 0, " change", ifelse(coef[i] > 0, "higher", "lower")),
                              " (", ci_level * 100, "% CI: ", abs(round(ci_lower[i], 3)), " unit ", ifelse(ci_lower[i] == 0, " change", ifelse(ci_lower[i] > 0, "higher", "lower")), " to ",
                              abs(round(ci_upper[i], 3)), " unit ", ifelse(coef[i] == 0, " change)", ifelse(coef[i] > 0, "higher)", "lower)")), " than for observations with ",
                              cat_var[i], " = ", cat_ref_levels[i], "."),
            pdir = paste0("We are ", pdir_val[i] * 100, "% sure that observations with ", cat_var[i], " = ",
                          cat_level[i], " has ", ifelse(coef[i] == 0, "the same ", ifelse(coef[i] > 0, "a higher ", "a lower ")), "mean ", outcome_var,
                          ifelse(coef[i] == 0, " as ", " than "), "observations with ", cat_var[i], " = ", cat_ref_levels[i], "."),
            rope = paste0("The probability that the difference in the mean ", outcome_var, " is outside the region of practical equivalence is estimated to be ",
                          ifelse(rope_val[i] == 0, 0.9999, ifelse(rope_val[i] > 0.9999, "< 0.0001", round(1 - rope_val[i], 4))), ".")
          )
        } else {
          list(
            estimate = paste0("We estimate that a 1-unit increase in ", variable[i], " leads to a ",
                              abs(round(coef[i], 4)), " unit ", ifelse(coef[i] == 0, "change ", ifelse(coef[i] > 0, "increase ", "decrease ")),
                              " (", ci_level * 100, "% CI: ", abs(round(ci_lower[i], 4)),  " unit ", ifelse(ci_lower[i] == 0, "change ", ifelse(ci_lower[i] > 0, "increase ", "decrease ")),
                              "to ", abs(round(ci_upper[i], 4)), " unit ", ifelse(ci_upper[i] == 0, "change", ifelse(ci_upper[i] > 0, "increase", "decrease")) , ") in the mean ",
                              outcome_var, "."),
            pdir = paste0("We are ", pdir_val[i] * 100, "% sure that an increase in ", variable[i],
                          " leads to ", ifelse(coef[i] == 0, "no change", ifelse(coef[i] > 0, "an increase", "a decrease")),
                          " in the mean ", outcome_var, "."),
            rope = paste0("The probability that the effect of ", variable[i], " is outside the region of practical equivalence is estimated to be ",
                          ifelse(rope_val[i] == 0, 0.9999, ifelse(rope_val[i] > 0.9999, "< 0.0001", round(1 - rope_val[i], 4))), ".")
          )
        }
      }), variable
    )

    rslt$fit_table <- tibble(
      formula = deparse(fit$formula),
      prior = prior,
      family = family,
      pvalue = pval,
      CILevel = fit$CI_level
    ) |>
      DT::datatable(
        options = list(searching = FALSE,
                       paging = FALSE)
      ) |>
      DT::formatSignif(columns = 4, digits = 4)

    rslt$interpret_notes <-
      paste0("For gaussian (continuous) data, the ROPE is built under the principle that a predictor's effect is considered negligible ",
             "if a large change in the predictor (two standard deviations) would shift the average outcome by a very ",
             "small amount (a tenth of the outcome's standard deviation). The ROPE bounds describe the range of coefficient values ",
             "that meet this condition. A large probability that the effect is outside the ROPE indicates practically ",
             "significant association between the predictor and outcome.")

  } else if (family == "poisson") {

    rslt$fit_table <- tibble(
      formula = deparse(fit$formula),
      prior = prior,
      family = family,
      pvalue = pval,
      offset = offset_var,
      CILevel = fit$CI_level
    ) |>
      DT::datatable(
        options = list(searching = FALSE,
                       paging = FALSE)
      ) |>
      DT::formatSignif(columns = 4, digits = 4)

    rslt$interpret <- setNames(
      lapply(seq_along(variable), function(i) {
        if (cat_ind[i]) {
          list(
            estimate = paste0("We estimate that the expected ", ifelse(is.null(offset_var) || offset_var == "None", "count of ", "rate of "),
                              outcome_var, " for observations with ", cat_var[i], " = ",
                              cat_level[i], " is ", round(abs(coef[i] - 1), 3)*100, "% ", ifelse(coef[i] == 1, " different", ifelse(coef[i] > 1, "higher", "lower")),
                              " (", ci_level * 100, "% CI: ", round(abs(ci_lower[i] - 1), 3)*100, "%", ifelse(ci_lower[i] == 1, " change", ifelse(ci_lower[i] > 1, " higher", " lower")),
                              " to ", round(abs(ci_upper[i] - 1), 4)*100, "%", ifelse(ci_upper[i] == 1, " change", ifelse(ci_upper[i] > 1, " higher", " lower")),
                              ") than for observations with ",
                              cat_var[i], " = ", cat_ref_levels[i], "."),
            pdir = paste0("We are ", pdir_val[i] * 100, "% sure that observations with ", cat_var[i], " = ",
                          cat_level[i], " has ", ifelse(coef[i] == 1, "the same ", ifelse(coef[i] > 1, "a higher ", "a lower ")), "expected ",
                          ifelse(is.null(offset_var) || offset_var == "None", "count of ", "rate of "), outcome_var,
                          ifelse(coef[i] == 1, " as ", " than "), "observations with ", cat_var[i], " = ", cat_ref_levels[i], "."),
            rope = paste0("The probability that the effect of ", cat_var[i], " for observations with ", cat_var[i], " = ", cat_level[i],
                          " relative to the reference level (", cat_var[i], " = ", cat_ref_levels[i], ") is outside the region of practical equivalence is estimated to be ",
                          ifelse(rope_val[i] == 0, 0.9999, ifelse(rope_val[i] > 0.9999, "< 0.0001", round(1 - rope_val[i], 4))), ".")
          )
        } else {
          list(
            estimate = paste0("We estimate that a 1-unit increase in ", variable[i], " is associated with a ", round(abs(coef[i] - 1), 3)*100, "%",
                              ifelse(coef[i] == 1, " change", ifelse(coef[i] > 1, " increase", " decrease")), " (", ci_level * 100, "% CI: ", round(abs(ci_lower[i] - 1), 3)*100, "%",
                              ifelse(coef[i] == 1, " change", ifelse(ci_lower[i] > 1, " increase", " decrease")), " to ", round(abs(ci_upper[i] - 1), 3)*100, "% ",
                              ifelse(coef[i] == 1, " change", ifelse(ci_upper[i] > 1, " increase", " decrease")), ") in the expected ",
                              ifelse(is.null(offset_var) || offset_var == "None", "count of ", "rate of "),
                              outcome_var, "."),
            pdir = paste0("We are ", pdir_val[i] * 100, "% sure that an increase in ", variable[i],
                          " leads to ", ifelse(coef[i] == 1, "no change", ifelse(coef[i] > 1, "an increase", "a decrease")),
                          " in the expected ", ifelse(is.null(offset_var) || offset_var == "None", "count of ", "rate of "), outcome_var, "."),
            rope = paste0("The probability that the effect of ", variable[i], " is outside the region of practical equivalence is estimated to be ",
                          ifelse(rope_val[i] == 0, 0.9999, ifelse(rope_val[i] > 0.9999, "< 0.0001", round(1 - rope_val[i], 4))), ".")
          )
        }
      }), variable
    )

    rslt$interpret_notes <-
      paste0("For poisson (count) data, the ROPE is built under the principle that a predictor's effect is considered negligible ",
             "if a large change in the predictor (two standard deviations) would change the rate ratio by a very ",
             "small amount (1.125). The ROPE bounds describe the range of coefficient values ",
             "that meet this condition. A large probability that the effect is outside the ROPE indicates practically ",
             "significant association between the predictor and outcome.")

  } else if (family == "binomial") {

    rslt$fit_table <- tibble(
      formula = deparse(fit$formula),
      prior = prior,
      family = family,
      pvalue = pval,
      offset = offset_var,
      CILevel = fit$CI_level
    ) |>
      DT::datatable(
        options = list(searching = FALSE,
                       paging = FALSE)
      ) |>
      DT::formatSignif(columns = 4, digits = 4)

    rslt$interpret <- setNames(
      lapply(seq_along(variable), function(i) {
        if (cat_ind[i]) {
          list(
            estimate = paste0("We estimate that the odds of ", outcome_var, " for observations with ", cat_var[i], " = ",
                              cat_level[i], " is ", round(abs(coef[i] - 1), 3)*100, "% ", ifelse(coef[i] == 1, "different", ifelse(coef[i] > 1, "higher", "lower")), " than for observations with ",
                              cat_var[i], " = ", cat_ref_levels[i], " (", ci_level * 100, "% CI: ", round(abs(ci_lower[i] - 1), 3)*100, "% ", ifelse(ci_lower[i] == 1, "different", ifelse(ci_lower[i] > 1, "higher", "lower")),
                              " to ", round(abs(ci_upper[i] - 1), 3)*100, "%", ifelse(ci_upper[i] == 1, "different", ifelse(ci_upper[i] > 1, "higher", "lower")), ")."),
            pdir = paste0("We are ", pdir_val[i] * 100, "% sure that observations with ", cat_var[i], " = ",
                          cat_level[i], " has ", ifelse(coef[i] == 1, "the same ", ifelse(coef[i] > 1, "a higher ", "a lower ")), "odds of ", outcome_var,
                          ifelse(coef[i] == 1, " as ", " than "), "observations with ", cat_var[i], " = ", cat_ref_levels[i], "."),
            rope = paste0("The probability that the effect of ", cat_var[i], " for observations with ", cat_var[i], " = ", cat_level[i],
                          " relative to the reference level (", cat_var[i], " = ", cat_ref_levels[i], ") is outside the region of practical equivalence is estimated to be ",
                          ifelse(rope_val[i] == 0, 0.9999, ifelse(rope_val[i] > 0.9999, "< 0.0001", round(1 - rope_val[i], 4))), ".")
          )
        } else {
          list(
            estimate = paste0("We estimate that a 1-unit increase in ", variable[i], " is associated with a ", round(abs(coef[i] - 1), 3)*100, "%",
                              ifelse(coef[i] == 1, " change", ifelse(coef[i] > 1, " increase", " decrease")), " (", ci_level * 100, "% CI: ", round(abs(ci_lower[i] - 1), 3)*100, "%",
                              ifelse(coef[i] == 1, " change", ifelse(ci_lower[i] > 1, " increase", " decrease")), " to ", round(abs(ci_upper[i] - 1), 3)*100, "% ",
                              ifelse(coef[i] == 1, " change", ifelse(ci_upper[i] > 1, " increase", " decrease")), ") in the odds of ", outcome_var, "."),
            pdir = paste0("We are ", pdir_val[i] * 100, "% sure that an increase in ", variable[i],
                          " leads to ", ifelse(coef[i] == 1, "no change", ifelse(coef[i] > 1, "an increase", "a decrease")),
                          " in the odds of ", outcome_var, "."),
            rope = paste0("The probability that the effect of ", variable[i], " is outside the region of practical equivalence is estimated to be ",
                          ifelse(rope_val[i] == 0, 0.9999, ifelse(rope_val[i] > 0.9999, "< 0.0001", round(1 - rope_val[i], 4))), ".")
          )
        }
      }), variable
    )

    rslt$interpret_notes <-
      paste0("For binomial data, the ROPE is built under the principle that a predictor's effect is considered negligible ",
             "if a large change in the predictor (two standard deviations) would change the log odds ratio of the outcome by a very ",
             "small amount (a tenth of the outcome's standard deviation). The ROPE bounds describe the range of coefficient values ",
             "that meet this condition. A large probability that the effect is outside the ROPE indicates practically ",
             "significant association between the predictor and outcome.")

  }

  rslt$plots <- plot(fit, return_as_list = TRUE, type = c("cred band", "pred band"))

  rslt$bpval <- bpval
  rslt$pval <- pval

  return(rslt)

}
