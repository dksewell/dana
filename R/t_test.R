#' Bayesian T Test
#'
#'
#' @param data dataframe/tibble
#' @param outcome_var column in data referring to outcome of interest
#' @param group_var column in data that contains group labels
#' @param two_sample T/F indicating two sample comparison
#' @param two_sample_group_vals values of the group variable corresponding to two populations for comparison
#' @param one_sample_group_val Group value for one sample test
#' @param ci_level Credible interval level
#' @details
#' Conducts bayesian t-test using bayesics::t_test_b function. There are 3 main setups for
#' testing:
#'
#' Two Sample - Conducts t-test on outcome variable, grouped by the values in two_sample_group_vals
#' One Sample - Conducts t-test on outcome variable for the entire data column
#' One Sample (by group) - Conducts t-test on outcome variable but only for the
#' group specified by one_sample_group_val
#'
#' @examples
#' # t_test(data = iris, outcome_var = "Sepal.Length", group_var = "Species", two_sample = TRUE, one_sample_group_val = c("setosa", "virginica")
#' # t_test(data = iris, outcome_var = "Sepal.Length", group_var = "Species", two_sample = FALSE, one_sample_group_val = "virginica")
#' # t_test(data = iris, outcome_var = "Sepal.Length", group_var = "Species", two_sample = FALSE)
#' @export
#'
#'
#'


t_test <- function(data,
                   outcome_var,
                   group_var,
                   two_sample = FALSE,
                   two_sample_group_vals,
                   one_sample_group_val = NULL,
                   ci_level = NULL){

  if (group_var == outcome_var){
    one_sample_group_val <- NULL
  }

  rslt <- list()

  if (two_sample == TRUE){
    data <- data.frame(outcome = data[[outcome_var]],
                       group_var = as.character(data[[group_var]]))
    data <- data[data$group_var %in% two_sample_group_vals, ]

    fit_homo <- bayesics::t_test_b(x = outcome ~ group_var,
                                   data = data,
                                   heteroscedastic = FALSE,
                                   CI_level = ci_level)

    fit_hetero <- bayesics::t_test_b(x = outcome ~ group_var,
                                     data = data,
                                     heteroscedastic = TRUE,
                                     CI_level = ci_level)

    # Check homoscedasticity assumptions
    homo_test <- bayesics::heteroscedasticity_test(fit_hetero$object_fit, fit_homo$object_fit)
    if (homo_test$BF > 1) {
      fit <- fit_homo
      rslt$diagnostic <- paste0("The results are based on a two-sample t-test with homoscedasticity assumption. A Bayes factor of ",
                                round(homo_test$BF, 4), " indicates the level of evidence is: ", homo_test$Interpretation)
    } else {
      fit <- fit_hetero
      rslt$diagnostic <- paste0("The results are based on a two-sample t-test with heteroscedasticity assumption. A Bayes factor of ",
                                round(homo_test$BF, 4), " indicates the evidence is ", homo_test$Interpretation)
    }

    rslt$plots <- list(fit$plot)

    rslt$fit_table <- dplyr::tibble(
      groups = paste0(two_sample_group_vals, collapse = " "),
      prior = fit$prior,
      CILevel = fit$CI_level
    ) |>
      DT::datatable(
        options = list(searching = FALSE,
                       paging = FALSE)
      )

    rslt$table <- fit$results |>
      DT::datatable() |>
      DT::formatSignif(columns = 2:7, digits = 3)

    # Human readable version
    rslt$table_hr <- fit$results
    colnames(rslt$table_hr) <- c("Quantity", "Estimate", "Lower CI Bound", "Upper CI Bound",
                                 "ROPE", "Lower ROPE Bound", "Upper ROPE Bound")
    rslt$table_hr <- rslt$table_hr |>
      DT::datatable() |>
      DT::formatSignif(columns = 2:7, digits = 3)

    rslt$interpret_notes <- paste0("For the two-sample t-test, the ROPE bounds are built under the principle of \"half of a small effect size\". ",
                                   "Using Cohen's d of 0.2 as a small effect size, the ROPE is set to -0.1 to 0.1. Differences within this range ",
                                   "are considered practically negligible, while a large probability outside the ROPE indicates a practically ",
                                   "meaningful difference between the two groups.")

    quantity <- tolower(fit$results$Quantity)[4]
    estimate <- round(fit$results$`Post Mean`, 4)
    lower_ci <- round(fit$results$Lower, 4)
    upper_ci <- round(fit$results$Upper, 4)
    rope <- round(fit$results$Pr_in_ROPE, 4)[4]

    rslt$interpret <- list(
      mean = paste0("We estimate that the population mean of ", outcome_var, " among observations with ",
                    group_var, " = ", two_sample_group_vals, " is ", estimate[1:2], " (", ci_level * 100, "% CI: ",
                    lower_ci[1:2], ", ", upper_ci[1:2], ")."),
      var = paste0("We estimate that the population variance of ", outcome_var, " among observations with ",
                   group_var, " = ", two_sample_group_vals, " is ", estimate[3:4], " (", ci_level * 100, "% CI: ",
                   lower_ci[3:4], ", ", upper_ci[3:4], ")."),
      diff = paste0("We estimate that the ", quantity, " of ", outcome_var,
                    " is ", estimate[4], " (", ci_level * 100, "% CI: ", lower_ci[4], ", ", upper_ci[4],
                    "). The probability that the difference is outside the region of practical equivalence is estimated to be ",
                    ifelse(rope == 0, 0.9999, ifelse(rope > 0.9999, "< 0.0001", round(1 - rope, 4))), ".")
    )

  }

  if (two_sample == FALSE) {

    if (is.null(one_sample_group_val) == TRUE){

      fit <- bayesics::t_test_b(x = data[[outcome_var]],
                                CI_level = ci_level)

      rslt$plots <- list(fit$plot)

      rslt$fit_table <- dplyr::tibble(
        prior = fit$prior,
        CILevel = fit$CI_level
      ) |>
        DT::datatable(
          options = list(searching = FALSE,
                         paging = FALSE)
        )

      rslt$table <- fit$results |>
        DT::datatable(
          options = list(searching = FALSE,
                         paging = FALSE)
        ) |>
        DT::formatSignif(columns = 2:7, digits = 3)

      quantity <- tolower(fit$results$Quantity)
      estimate <- round(fit$results$`Post Mean`, 4)
      lower_ci <- round(fit$results$Lower, 4)
      upper_ci <- round(fit$results$Upper, 4)

      rslt$interpret <- paste0("We estimate that the ", quantity, " of ", outcome_var,
                               " is ", estimate, " (", ci_level * 100, "% CI: ", lower_ci, ", ", upper_ci, ").")

    }

    if (is.null(one_sample_group_val) == FALSE) {

      data <- data[data[[group_var]] == one_sample_group_val, ]

      fit <- bayesics::t_test_b(x = data[[outcome_var]],
                                CI_level = ci_level)

      rslt$plots <- list(fit$plot)

      rslt$fit_table <- dplyr::tibble(
        group = one_sample_group_val,
        prior = fit$prior,
        CILevel = fit$CI_level
      ) |>
        DT::datatable(
          options = list(searching = FALSE,
                         paging = FALSE)
        )

      rslt$table <- fit$results |>
        DT::datatable(
          options = list(searching = FALSE,
                         paging = FALSE)
        ) |>
        DT::formatSignif(columns = 2:7, digits = 3)

      quantity <- tolower(fit$results$Quantity)
      estimate <- round(fit$results$`Post Mean`, 4)
      lower_ci <- round(fit$results$Lower, 4)
      upper_ci <- round(fit$results$Upper, 4)
      rslt$interpret <- paste0("We estimate that the ", quantity, " of ", outcome_var, " among observations with ",
                               group_var, " = ", one_sample_group_val, " is ", estimate, " (", ci_level * 100, "% CI: ",
                               lower_ci, ", ", upper_ci, ").")

    }

    rslt$table_hr <- fit$results
    colnames(rslt$table_hr) <- c("Quantity", "Estimate", "Lower CI Bound", "Upper CI Bound")
    rslt$table_hr <- rslt$table_hr |>
      DT::datatable(
        options = list(searching = FALSE,
                       paging = FALSE)
      ) |>
      DT::formatSignif(columns = 2:4, digits = 3)

  }

  return(rslt)

}
