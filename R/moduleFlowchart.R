#' Flowchart Server Logic
#'
#' Create server logic for displaying modified flowcharts
#'
#' @param id shiny input/output id
#'
#' @export
#'
#'
#'

serverFlowchart <- function(id, vals, final_node){
  shiny::moduleServer(
    id,
    function(input, output, session){

         binary_dgr <- binary_flowchart(vals = vals,
                          final_node = final_node)

         binary_dot <- generate_dot(binary_dgr)

         output$flowchart <- renderGrViz({
           grViz(binary_dot)
         })

    }
  )
}








#' Flowchart UI Logic
#'
#' Create UI logic for rendering modified binary flowchart
#'
#' @param id shiny input/output id
#' @details
#' Paired with serverFlowchart through the id argument.
#'
#' @export

uiFlowchart <- function(id, label = "flowchart"){
  ns <- shiny::NS(id)
    DiagrammeR::grVizOutput(ns("flowchart"))
}