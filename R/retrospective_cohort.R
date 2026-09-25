#' Bayesian Inference for Retrospective Cohort Designs
#'
#' Wrapper for stan_glmer for cohort designs, with additional diagnostics and defaults.
#'
#' @param data A data frame with containing the variables specified in the formula
#' @param family family specifying distributional assumption on outcome
#' @param outcome_var A string referring to the outcome variable
#' @param time_var A string referring to the time variable
#' @param pred_cat A vector of strings referring to categorical predictor variables
#' @param pred_cont A vector of strings referring to continuous predictor variables
#' @param group_var A string referring to the group variable
#' @param ref_levels A string vector specifying the reference levels for all categorical variables
#' @param offset_var A string referring to offset variable. Ignored unless family == "poisson"
#' @param ci_level A numeric value specifying the credible interval level
#'
#' @returns A list containing the results from the stan_glm.
#' @details
#' Performs bayesian generalized linear mixed-effects models.
#'
#'
#' @export


retrospective_cohort_regression <- function(data,
                                            family = "gaussian",
                                            outcome_var,
                                            time_var,
                                            pred_cat,
                                            pred_cont,
                                            group_var,
                                            ref_levels = NULL,
                                            offset_var = NULL,
                                            ci_level = NULL) {
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

  # Contain results
  rslt <- list()

  # Apply reference levels to categorical predictors ---------------------------
  if (!is.null(ref_levels)){
    ref_list <- lapply(setNames(ref_levels, pred_cat), function(x) x)

    # Apply reference categories to all categorical predictor columns
    data <- data |>
      dplyr::mutate(dplyr::across(dplyr::all_of(pred_cat), ~ {
        relevel(factor(as.character(.x)), ref = ref_list[[dplyr::cur_column()]])
      }))
  }
  # data <- dana::apply_refs(data, pred_cat, ref_levels)

  # Define formula
  fit_formula <- if (family == "binomial" || family == "gaussian") {
    as.formula(paste(outcome_var, "~", time_var, "+",
                     paste(c(pred_cat, pred_cont), collapse = "+"), "+ (1|", group_var, ")"))
  } else if (family == "poisson"){
    if (is.null(offset_var) || offset_var == "None"){
      as.formula(paste(outcome_var, "~", time_var, "+",
                       paste(c(pred_cat, pred_cont), collapse = "+"), "+ (1|", group_var, ")"))
    } else {
      as.formula(paste(outcome_var, "~", time_var, "+",
                       paste(c(pred_cat, pred_cont), collapse = "+"),
                       " + offset(log(", offset_var, ")) + (1|", group_var, ")"))
    }
  }

  # Fit model
  fit <- rstanarm::stan_glmer(
    formula = fit_formula,
    data = data,
    family = family,
    refresh = 0)

  # Extract Bayesian pval
  bp_fit <- dana::glme_bpv(fit)
  bp <- bp_fit$bpval

  # Diagnostic
  if (0.05 < bp && bp < 0.95) {
    rslt$diagnostic <- paste0("A mixed effect model was used. The bayesian p-value is: ",
                              round(bp, digits = 4), ". This indicates the model reasonably fits the data.")
  }

  # Model cascade if p-value is bad --------------------------------------------

  if (bp < 0.05 || bp > 0.95) {

    if (family == "gaussian") {
      if (all(data[outcome_var] > 0)) {
        ## Log transform
        fit_formula <- as.formula(paste("log(", outcome_var,")", "~",
                                        time_var, "+",
                                        paste(c(pred_cat, pred_cont), collapse = "+"), "+",
                                        "(1|", group_var, ")"))

        fit <- rstanarm::stan_glmer(
          formula = fit_formula,
          data = data,
          refresh = 0)

        # Extract bpvals
        bp_fit <- dana::glme_bpv(fit)
        bp_log <- bp_fit$bpval

        ## Diagnostic
        rslt$diagnostic <- paste0("The results are based on a log transformed mixed
                              effects regression model. This type of transformation
                              is typically performed on right-skewed outcome data.
                              A non-transformed model
                              was initially fitted, but the Bayesian p-value
                              was ",
                                  round(bp, digits = 4), "
                              suggesting a lack of fit. The Bayesian p-value
                              for the log transformed model is ", round(bp_log, digits = 4),
                                  ", suggesting model fit")

        if (bp_log < 0.05 || bp_log > 0.95) {
          rslt$diagnostic <- paste0("Placeholder for non-parametric model for gaussian outcome")
        }
      }

    } else if (family == "poisson") {
      fit <- rstanarm::stan_glmer.nb(
        formula = fit_formula,
        data = data,
        refresh = 0
      )

      # Extract bpvals
      bp_fit <- dana::glme_bpv(fit)
      bp_nb <- bp_fit$bpval

      ## Diagnostic
      rslt$diagnostic <- paste0("The results are based on a negative binomial mixed
                              effects regression model. The Bayesian p-value of ",
                                round(bp_nb, digits = 4), " indicates adequate model fit.
                              A poisson regression model was initially fitted,
                              but the Bayesian p-value of ", round(bp, digits = 4),
                                " suggested lack of fit.")

      ## Diagnostic
      if (bp_nb < 0.05 || bp_nb > 0.95) {
        rslt$diagnostic <- paste0("Placeholder for non-parametric model for poisson outcome")
      }

    } else {
      rslt$diagnostic <- paste0("Placeholder for non-parametric model for binomial outcome")
    }
  }

  # Assemble result ------------------------------------------------------------

  rslt$fit <- fit
  rslt$fit_table <- dplyr::tibble(
    formula = deparse(fit$formula),
    family = fit$family$family,
  ) |>
    DT::datatable(
      options = list(searching = FALSE,
                     paging = FALSE)
    )

  rslt$table <- fit$stan_summary[names(rstanarm::fixef(fit)), ] |>
    DT::datatable() |>
    DT::formatSignif(columns = 1:12, digits = 3)

  ci <- rstanarm::posterior_interval(fit, prob = ci_level) |>
    tibble::as_tibble(rownames = "Variable")
  names(ci)[-1] <- c("Lower CI Bound", "Upper CI Bound")

  estimate <- rstanarm::fixef(fit) |>
    tibble::as_tibble(rownames = "Variable")
  names(estimate)[-1] <- "Estimate"

  rslt$table_hr <- inner_join(estimate, ci, by = "Variable")

  if (grepl("^log", formula(fit)[2]) || family == "poisson" || family == "binomial") {
    rslt$table_hr <- rslt$table_hr |>
      dplyr::mutate(`Estimate` = exp(`Estimate`),
                    `Lower CI Bound` = exp(`Lower CI Bound`),
                    `Upper CI Bound` = exp(`Upper CI Bound`))
  }

  pdir_fit <- bayestestR::pd(fit) %>% filter(Parameter != "reciprocal_dispersion")
  pdir_val <- pdir_fit$pd
  pdir_val <- ifelse(pdir_val == 1, 0.9999, pdir_val)

  rslt$table_hr <- rslt$table_hr |>
    dplyr::mutate(PDir = pdir_val) |>
    DT::datatable() |>
    DT::formatSignif(columns = 2:4, digits = 4)

  rslt$bpval <- bp
  rslt$plots <- list(bp_fit$bpplot, rstanarm::pp_check(fit))

  # Extract variables
  rslt$variables$all <- rslt$table_hr$x$data$"Variable"[-1]
  rslt$variables$outcome_var <- outcome_var
  rslt$variables$pred_cat <- pred_cat
  rslt$variables$group_var <- group_var
  rslt$variables$ref_levels <- ref_levels
  rslt$ci_level <- ci_level
  rslt$family <- family

  return(rslt)

}
