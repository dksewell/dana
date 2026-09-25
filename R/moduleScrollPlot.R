#' ScrollPlot Server Logic
#'
#' Create server logic for scrolling ggplot outputs
#'
#' @param id shiny input/output id
#' @param ggplot_list  a list of ggplot objects
#' @details
#' Paired with uiScrollPlot through the id argument.
#' This function is placed in an R shiny
#' server function with a specified id and list of ggplot objects.
#' When rendered in combination with uiScrollPlot, creates a dynamic
#' ggplot object with actionButtons that scroll through the list.
#'
#' Behaves dynamically depending on the length of the argument ggplot_list.
#'
#' - Lenght 0 will not show any plots/buttons
#' - Length 1 will show only the plot
#' - Length >1 will show all plots and buttons
#'
#' @export

serverScrollPlot <- function(id, ggplot_list){
  shiny::moduleServer(
    id,
    function(input, output, session){
      if (length(ggplot_list) > 1){
      max_index <- length(ggplot_list)
      min_index <- 1
      index <- shiny::reactiveVal(1)
      shiny::observeEvent(input$buttonforwards, {
        if (index() == max_index){
          index(1)
          output$scrollindexinfo <- shiny::renderText(paste0("Displaying ", index(), " of ", max_index))
        } else {
          index(index() + 1)
          output$scrollindexinfo <- shiny::renderText(paste0("Displaying ", index(), " of ", max_index))
        }
      })
      shiny::observeEvent(input$buttonbackwards, {
        if (index() == min_index){
          index(max_index)
          output$scrollindexinfo <- shiny::renderText(paste0("Displaying ", index(), " of ", max_index))
        } else {
          index(index() - 1)
          output$scrollindexinfo <- shiny::renderText(paste0("Displaying ", index(), " of ", max_index))
        }
      })
      output$plot <- shiny::renderPlot({
        ggplot_list[[index()]]
      })
      }
      if (length(ggplot_list) == 1){
        output$plot <- shiny::renderPlot({
          ggplot_list[[1]]
        })}
    }
  )
}


#' ScrollPlot UI Logic
#'
#' Create UI logic for scrolling ggplot outputs
#'
#' @param id shiny input/output id
#' @details
#' Paired with serverScrollPlot through the id argument.
#'  This function is placed in an R shiny
#' ui function with a paired serverScrollPlot.
#' Creates a dynamic
#' ggplot object with actionButtons that scroll through the list.
#'
#' @export

uiScrollPlot <- function(id, label = "scrollplot"){
   ns <- shiny::NS(id)
   shiny::tagList(
     shiny::plotOutput(ns("plot")),
     shiny::actionButton(ns("buttonbackwards"), label = "Backwards"),
     shiny::actionButton(ns("buttonforwards"), label = "Forwards"),
     shiny::textOutput(ns("scrollindexinfo"))
   )
}
