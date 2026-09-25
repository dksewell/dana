#' DataTable Server Logic
#'
#' Create server logic to generate a table showcasing the uploaded data
#'
#' @param id shiny input/output id
#' @param data  dataframe
#'
#' @details
#' Paired with uiDataTable through the id argument.
#' This function is placed in an R shiny
#' server function with a specified id and data frame/tibble.
#' When rendered in combination with uiDataTable, creates an output
#' data table that can be browsed by the user.
#'
#' @export
#'
#'
#'

serverDataTable <- function(id, data){
  shiny::moduleServer(
    id,
    function(input, output, session){
        output$data_table <- DT::renderDT(data)
    })
}


#' DataTable UI Logic
#'
#' Create UI logic for creating data table
#'
#' @param id shiny input/output id
#' @details
#' Paired with serverDataTable through the id argument.
#' This function is placed in an R shiny
#' ui function with a paired serverDataTable which shows
#' the data table associated with the data uploaded by the user.
#'
#' @export

uiDataTable <- function(id, label = "datatable"){
  ns <- shiny::NS(id)
  DT::DTOutput(ns("data_table"))
}
