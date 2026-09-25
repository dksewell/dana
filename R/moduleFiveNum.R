#' Five Number Summary Server Logic
#'
#' Create server logic for five number summary table
#'
#' @param id shiny input/output id
#' @param plotly_list  a list of plotly objects
#' @details
#' Paired with uiFiveNum through the id argument.
#' This function is placed in an R shiny
#' server function with a specified id and dataset.
#' When rendered in combination with uiFiveNum, creates a summary
#' table for the variables in the dataset. Summary table
#' is created with table1 package.
#'
#' Includes options for displaying summary by grouping
#' variable.
#'
#' @export
#'
#'
#'

serverFiveNum <- function(id, data, cat_vars, cont_vars, group_var){
  shiny::moduleServer(
    id,
    function(input, output, session){
      output$table <- shiny::renderUI({

        if (group_var == "None" | is.null(group_var)){
        # Logic for non-grouped summary table
          data <- data |>
            dplyr::mutate(dplyr::across(cont_vars, as.numeric)) |>
            dplyr::mutate(dplyr::across(c(cat_vars), as.factor))

          summary_formula <- as.formula(paste0("~", paste(c(cat_vars, cont_vars), collapse = "+")))
          tbl <- table1::table1(summary_formula, data = data)
        } else {
        # Logic for grouped summary table

          data <- data |>
            dplyr::mutate(dplyr::across(cont_vars, as.numeric)) |>
            dplyr::mutate(dplyr::across(c(cat_vars, group_var), as.factor))

          summary_formula <- as.formula(paste0("~", paste(c(cat_vars, cont_vars), collapse = "+"), "|", group_var))
          tbl <- table1::table1(summary_formula, data = data)

        }

        # Render as an HTML table
        HTML(tbl)

    }
  )
    }
  )
}








#' ContScrollPlot UI Logic
#'
#' Create UI logic for scrolling plotly outputs for continuous variables
#'
#' @param id shiny input/output id
#' @details
#' Paired with serverContScrollPlot through the id argument.
#'  This function is placed in an R shiny
#' ui function with a paired serverContScrollPlot.
#' Creates a dynamic
#' plotly object with actionButtons that scroll through the list.
#'
#' @export

uiFiveNum <- function(id){
  # Include CSS for table formatting
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h3(shiny::tags$b("Tabular Summary")),
    div(
      style = "overflow-x: auto; height: 500px; width: 100%; border: 1px solid #ccc; padding: 5px;",
      shiny::uiOutput(ns("table"))
    )
  )

}
