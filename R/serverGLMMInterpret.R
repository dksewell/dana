#' GLMMInterpret Server Logic
#'
#' Create server logic to create interpretations for generalized linear mixed-effects models
#'
#' @param id shiny input/output id
#' @param analysis glmm model analysis
#'
#' @details
#' This function is placed in an R shiny
#' server function with a specified id and model analysis.
#' Creates a dropdown of variables and interpretations for generalized linear mixed-effects models
#'
#' @export
#'
#'

serverGLMMInterpret <- function(id, analysis) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Extract variables
      variable <- analysis$variables$all
      pred_cat <- analysis$variables$pred_cat
      ref_levels <- analysis$variables$ref_levels
      outcome_var <- analysis$variables$outcome_var
      group_var <- analysis$variables$group_var

      # Extract factor levels and reference level
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

      coef <- analysis$table_hr$x$data$"Estimate"[-1]
      ci_lower <- analysis$table_hr$x$data$"Lower CI Bound"[-1]
      ci_upper <- analysis$table_hr$x$data$"Upper CI Bound"[-1]
      ci_level <- analysis$ci_level
      pdir_val <- analysis$table_hr$x$data$"PDir"[-1]
      family <- analysis$family

      if (family == "gaussian") {

        if (grepl("^log", formula(analysis$fit)[2])) {
          interpret <- setNames(
            lapply(seq_along(variable), function(i) {
              if (cat_ind[i]) {
                list(
                  estimate = paste0("We estimate that the mean ", outcome_var, " for ", group_var, " with ", cat_var[i], " = ",
                                    cat_level[i], " is ", round(abs(coef[i] - 1), 3)*100, "% ", ifelse(coef[i] == 1, " different", ifelse(coef[i] > 1, "higher", "lower")),
                                    " (", ci_level * 100, "% CI: ", round(abs(ci_lower[i] - 1), 3)*100, "% ",
                                    ifelse(ci_lower[i] == 1, " different", ifelse(ci_lower[i] > 1, "higher", "lower")), " to ", round(abs(ci_upper[i] - 1), 3)*100, "% ",
                                    ifelse(ci_upper[i] == 1, " different", ifelse(ci_upper[i] > 1, "higher", "lower")), ") than for ",
                                    group_var, " with ", cat_var[i], " = ", cat_ref_levels[i], "."),
                  pdir = paste0("We are ", pdir_val[i] * 100, "% sure that ", group_var, " with ", cat_var[i], " = ",
                                cat_level[i], " has ", ifelse(coef[i] == 1, "the same ", ifelse(coef[i] > 1, "a higher ", "a lower ")), "mean ", outcome_var,
                                ifelse(coef[i] == 1, " as ", " than "), group_var, " with ", cat_var[i], " = ", cat_ref_levels[i], ".")
                )
              } else {
                list(
                  estimate = paste0("We estimate that a 1-unit increase in ", variable[i], " is associated with a ", round(abs(coef[i] - 1), 3)*100,
                                    "% ", ifelse(coef[i] == 1, "change", ifelse(coef[i] > 1, "increase", "decrease")), " (", ci_level * 100, "% CI: ",
                                    round(abs(ci_lower[i] - 1), 3)*100, "% ", ifelse(ci_lower[i] == 1, "change", ifelse(ci_lower[i] > 1, "increase", "decrease")), " to ",
                                    round(abs(ci_upper[i] - 1), 3)*100, "% ", ifelse(ci_upper[i] == 1, "change", ifelse(ci_upper[i] > 1, "increase", "decrease")), ") in the mean ",
                                    outcome_var, "."),
                  pdir = paste0("We are ", pdir_val[i] * 100, "% sure that an increase in ", variable[i],
                                " leads to ", ifelse(coef[i] == 1, "no change", ifelse(coef[i] > 1, "an increase", "a decrease")),
                                " in the mean ", outcome_var, ".")
                )
              }
            }), variable
          )
        } else {
          interpret <- setNames(
            lapply(seq_along(variable), function(i) {
              if (cat_ind[i]) {
                list(
                  estimate = paste0("We estimate that the mean ", outcome_var, " for ", group_var, " with ", cat_var[i], " = ",
                                    cat_level[i], " is ", abs(round(coef[i], 3)), " unit ", ifelse(coef[i] == 0, " change", ifelse(coef[i] > 0, "higher", "lower")),
                                    " (", ci_level * 100, "% CI: ", abs(round(ci_lower[i], 3)), " unit ", ifelse(ci_lower[i] == 0, "change", ifelse(ci_lower[i] > 0, "higher", "lower")), " to ",
                                    abs(round(ci_upper[i], 3)), " unit ", ifelse(ci_upper[i] == 0, "change", ifelse(ci_upper[i] > 0, "higher", "lower")),
                                    ") than for ", group_var, " with ", cat_var[i], " = ", cat_ref_levels[i], "."),
                  pdir = paste0("We are ", pdir_val[i] * 100, "% sure that ", group_var, " with ", cat_var[i], " = ",
                                cat_level[i], " has ", ifelse(coef[i] == 0, "the same ", ifelse(coef[i] > 0, "a higher ", "a lower ")), "mean ", outcome_var,
                                ifelse(coef[i] == 0, " as ", " than "), group_var, " with ", cat_var[i], " = ", cat_ref_levels[i], ".")
                )
              } else {
                list(
                  estimate = paste0("We estimate that a 1-unit increase in ", variable[i], " leads to a ",
                                    abs(round(coef[i], 3)), " unit ", ifelse(coef[i] == 0, " change", ifelse(coef[i] > 0, "increase", "decrease")),
                                    " (", ci_level * 100, "% CI: ", abs(round(ci_lower[i], 3)), " unit ", ifelse(ci_lower[i] == 0, " change", ifelse(ci_lower[i] > 0, "increase", "decrease")), " to ",
                                    abs(round(ci_upper[i], 3)), " unit ", ifelse(ci_upper[i] == 0, " change", ifelse(ci_upper[i] > 0, "increase", "decrease")),
                                    ") in the mean ", outcome_var, "."),
                  pdir = paste0("We are ", pdir_val[i] * 100, "% sure that an increase in ", variable[i],
                                " leads to ", ifelse(coef[i] == 0, "no change", ifelse(coef[i] > 0, "an increase", "a decrease")),
                                " in the mean ", outcome_var, ".")
                )
              }
            }), variable
          )
        }

      } else if (family == "poisson") {

        interpret <- setNames(
          lapply(seq_along(variable), function(i) {
            if (cat_ind[i]) {
              list(
                estimate = paste0("We estimate that the expected ", ifelse(is.null(offset_var) || offset_var == "None", "count of ", "rate of "),
                                  outcome_var, " for ", group_var, " with ", cat_var[i], " = ",
                                  cat_level[i], " is ", round(abs(coef[i] - 1), 3)*100, "% ", ifelse(coef[i] == 1, " different", ifelse(coef[i] > 1, "higher", "lower")),
                                  " (", ci_level * 100, "% CI: ", round(abs(ci_lower[i] - 1), 3)*100, "%", ifelse(ci_lower[i] == 1, " change", ifelse(ci_lower[i] > 1, " higher", " lower")),
                                  " to ", round(abs(ci_upper[i] - 1), 4)*100, "%", ifelse(ci_upper[i] == 1, " change", ifelse(ci_upper[i] > 1, " higher", " lower")),
                                  ") than for ", group_var, " with ", cat_var[i], " = ", cat_ref_levels[i], "."),
                pdir = paste0("We are ", pdir_val[i] * 100, "% sure that ", group_var, " with ", cat_var[i], " = ",
                              cat_level[i], " has ", ifelse(coef[i] == 1, "the same ", ifelse(coef[i] > 1, "a higher ", "a lower ")), "expected ",
                              ifelse(is.null(offset_var) || offset_var == "None", "count of ", "rate of "), outcome_var,
                              ifelse(coef[i] == 1, " as ", " than "), group_var, " with ", cat_var[i], " = ", cat_ref_levels[i], ".")
              )
            } else {
              list(
                estimate = paste0("We estimate that a 1-unit increase in ", variable[i], " is associated with a ", round(abs(coef[i] - 1), 3)*100, "%",
                                  ifelse(coef[i] == 1, " change", ifelse(coef[i] > 1, " increase", " decrease")), " (", ci_level * 100, "% CI: ", round(abs(ci_lower[i] - 1), 3)*100, "%",
                                  ifelse(coef[i] == 1, " change", ifelse(ci_lower[i] > 1, " increase", " decrease")), " to ", round(abs(ci_upper[i] - 1), 3)*100, "%",
                                  ifelse(coef[i] == 1, " change", ifelse(ci_upper[i] > 1, " increase", " decrease")), ") in the expected ",
                                  ifelse(is.null(offset_var) || offset_var == "None", "count of ", "rate of "),
                                  outcome_var, "."),
                pdir = paste0("We are ", pdir_val[i] * 100, "% sure that an increase in ", variable[i],
                              " leads to ", ifelse(coef[i] == 1, "no change", ifelse(coef[i] > 1, "an increase", "a decrease")),
                              " in the expected ", ifelse(is.null(offset_var) || offset_var == "None", "count of ", "rate of "), outcome_var, ".")
              )
            }
          }), variable
        )

      } else if (family == "binomial") {
        interpret <- setNames(
          lapply(seq_along(variable), function(i) {
            if (cat_ind[i]) {
              list(
                estimate = paste0("We estimate that the odds of ", outcome_var, " for ", group_var, " with ", cat_var[i], " = ",
                                  cat_level[i], " is ", round(abs(coef[i] - 1), 3)*100, "% ", ifelse(coef[i] == 1, "different", ifelse(coef[i] > 1, "higher", "lower")), " than for observations with ",
                                  cat_var[i], " = ", cat_ref_levels[i], " (", ci_level * 100, "% CI: ", round(abs(ci_lower[i] - 1), 3)*100, "% ", ifelse(ci_lower[i] == 1, "different", ifelse(ci_lower[i] > 1, "higher", "lower")),
                                  " to ", round(abs(ci_upper[i] - 1), 3)*100, "% ", ifelse(ci_upper[i] == 1, "different", ifelse(ci_upper[i] > 1, "higher", "lower")), ")."),
                pdir = paste0("We are ", pdir_val[i] * 100, "% sure that ", group_var, " with ", cat_var[i], " = ",
                              cat_level[i], " has ", ifelse(coef[i] == 1, "the same", ifelse(coef[i] > 1, "a higher", "a lower")), " odds of ", outcome_var,
                              ifelse(coef[i] == 1, " as ", " than "), group_var, " with ", cat_var[i], " = ", cat_ref_levels[i], ".")
              )
            } else {
              list(
                estimate = paste0("We estimate that a 1-unit increase in ", variable[i], " is associated with a ", round(abs(coef[i] - 1), 3)*100, "%",
                                  ifelse(coef[i] == 1, " change", ifelse(coef[i] > 1, " increase", " decrease")), " (", ci_level * 100, "% CI: ", round(abs(ci_lower[i] - 1), 3)*100, "%",
                                  ifelse(coef[i] == 1, " change", ifelse(ci_lower[i] > 1, " increase", " decrease")), " to ", round(abs(ci_upper[i] - 1), 3)*100, "% ",
                                  ifelse(coef[i] == 1, " change", ifelse(ci_upper[i] > 1, " increase", " decrease")), ") in the odds of ", outcome_var, "."),
                pdir = paste0("We are ", pdir_val[i] * 100, "% sure that an increase in ", variable[i],
                              " leads to ", ifelse(coef[i] == 1, "no change", ifelse(coef[i] > 1, "an increase", "a decrease")),
                              " in the odds of ", outcome_var, ".")
              )
            }
          }), variable
        )
      }

      shiny::observe({
        shiny::req(interpret)

        shiny::updateSelectInput(
          session,
          "vars_interpret",
          choices = names(interpret),
          selected = character(0)
        )
      })

      shiny::observeEvent(input$select_all_vars_interpret, {
        updateSelectInput(
          session,
          "vars_interpret",
          selected = names(interpret)
        )
      })

      shiny::observeEvent(input$clear_all_vars_interpret, {
        updateSelectInput(
          session,
          "vars_interpret",
          selected = character(0)
        )
      })


      shiny::reactive({
        shiny::req(length(input$vars_interpret) > 0)
        tagList(
          lapply(input$vars_interpret, function(x) {
            var <- interpret[[x]]
            tagList(
              tags$h4(x),
              tags$p(
                tags$strong("Effect size: "),
                var$estimate
              ),
              tags$p(
                tags$strong("PDir: "),
                var$pdir
              ),
              tags$div(style = "margin-bottom: 25px;")
            )
          })
        )
      })
    }
  )
}

