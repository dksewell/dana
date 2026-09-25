#' Interpret lm_b Output
#'
#' Create human-readable interpretations for objects
#' of class bayesics::lm_b
#'
#' @param object
#' @details
#' Provides a written description of model results, including
#' estimates, credible intervals, and ROPE.
#'
#' @export

interpret.glm_b <- function(fit){

  # Define variables
  vars <- names(fit)
  cat_vars <- names(a$fit$xlevels)
  cont_vars <- setdiff(attr(a$fit$terms, "term.labels"), cat_vars)

  # Get all reference levels for categorical summary variables
  cat_summary_var_ref_levels <- lapply(fit$xlevels, function(x){
    levels(x)[1]
  })

  # Model matrix to extract summary variables
  mm <- model.matrix(fit$terms, fit$data)
  summary_var_nonref <- split(
    colnames(mm)[-1],
    attr(fit$terms, "term.labels")[attr(mm, "assign")[-1]]
  )

  summary_var_levels <- lapply(names(fit$xlevels), function(x){
    substring(summary_var_nonref[[x]], nchar(x) + 1)
  }) |>
    setNames(names(cat_summary_var_ref_levels))

  summary_var_df <- lapply(summary_var_nonref, function(x){
    fit$summary[fit$summary$Variable %in% x,]
  })

  rslt <- lapply(names(summary_var_df), function(x){
    df <- summary_var_df[[x]] |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))) |>
      dplyr::mutate(Change = dplyr::case_when(
        `Post Mean` > 0 ~ "larger",
        `Post Mean` < 0 ~ "smaller"
      )) |>
      dplyr::mutate(Association = dplyr::case_when(
        `Post Mean` > 0 ~ "positive",
        `Post Mean` < 0 ~ "negative"
      ))

    var <- x
    outcome <- all.vars(fit$formula)[1]
    level <- summary_var_levels[[x]]
    ref_level <- cat_summary_var_ref_levels[[x]]

    # Interpret categorical variables
    if (var %in% cat_vars){
    estimate = paste0("We estimate that the mean value of ",
                      outcome,
                      " for observations with ",
                      var, " = ", level, " is ",
                      abs(df$`Post Mean`), " ",
                      df$Change, " (", df$Lower, ", ",
                      df$Upper, ") than for observations with ",
                      var, " = ", ref_level, ".")

    pdir = paste0("We are ",
                  df$`Prob Dir` * 100,
                  "% sure that observations with ",
                  var, " = ",
                  level, " have a ",
                  df$Change, " mean ", outcome,
                  " compared to observations with ",
                  var, " = ", ref_level, ".")
    rope = paste0("The probability that the difference in the mean ",
                  outcome, " is outside the region of practical equivalence is estimated to be ",
                  df$ROPE, ".")
    }

    # Interpret continuous variables
    if (var %in% cont_vars){
      estimate = paste0("We estimate that a one unit increase in ",
                        var,
                        " is associated with a ",
                        abs(df$`Post Mean`), " ",
                        df$Change, " mean ",
                        outcome, " value.")

      pdir = paste0("We are ",
                    df$`Prob Dir` * 100,
                    "% sure that ",
                    var, " has a ",
                    df$Association, " association with ",
                    outcome, "."
      )
      rope = paste0("The probability that the effect of ",
                    var, " is outside the region of practical equivalence is estimated to be ",
                    df$ROPE, ".")
    }


    rslt = list("estimate" = estimate,
                "pdir" = pdir,
                "rope" = rope)

  }) |>
    setNames(names(summary_var_df))

  return(rslt)

}

