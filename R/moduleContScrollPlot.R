#' ContScrollPlot Server Logic
#'
#' Create server logic for scrolling plotly outputs based on continuous variables
#'
#' @param id shiny input/output id
#' @param plotly_list  a list of plotly objects
#' @details
#' Paired with uiContScrollPlot through the id argument.
#' This function is placed in an R shiny
#' server function with a specified id and list of plotly objects.
#' When rendered in combination with uiContScrollPlot, creates a dynamic
#' plotly object with actionButtons that scroll through the list.
#'
#' @export
#'
#'
#'

serverContScrollPlot <- function(id, data, cont_vars, group_var = NULL){
  shiny::moduleServer(
    id,
    function(input, output, session){

      # Construct plotly list if grouping variable is specified
      ## Create group plot
      if (!(is.null(group_var) == TRUE || group_var == "None")){
        cont_plot_list <- setdiff(cont_vars, group_var) |>
          lapply(function(x){
            plotly::ggplotly(
              dplyr::as_tibble(data[c(x, group_var)]) |>
                dplyr::mutate(!!group_var := as.factor(.data[[group_var]])) |>
                ggplot2::ggplot() +
                ggplot2::geom_histogram(mapping = ggplot2::aes(x = .data[[x]]), position = "identity", alpha = 0.6, bins = 30) +
                ggplot2::facet_grid(
                  rows = ggplot2::vars(.data[[group_var]]),
                  labeller = label_both,
                  ) + ggplot2::scale_fill_viridis_d() +
                ggplot2::labs(x = x, y = "Count") + ggplot2::theme_minimal()
            )
          })




        ## Create group plot
        if (group_var %in% cont_vars){
          group_plot <- group_var |>
            lapply(function(x){
              ggplot2::ggplot(dplyr::as_tibble(data[c(x)])) +
                ggplot2::geom_histogram(mapping = ggplot2::aes(x = .data[[x]]), position = "identity", alpha = 0.6, bins = 30) + ggplot2::scale_fill_viridis_d() +
                ggplot2::labs(x = x, y = "Count") + theme_minimal()
            })
          ## Combine into a single list
          cont_plot_list <- c(group_plot, cont_plot_list)
        }


      }

      # Construct plotly list if no grouping variable is selected
      if (is.null(group_var) == TRUE || group_var == "None"){
        cont_plot_list <- cont_vars |>
          lapply(function(x){
            ggplot2::ggplot(dplyr::as_tibble(data[c(x)])) +
              ggplot2::geom_histogram(mapping = ggplot2::aes(x = .data[[x]]), position = "identity", alpha = 0.6, bins = 30) + ggplot2::scale_fill_viridis_d() +
              ggplot2::labs(x = x, y = "Count") + ggplot2::theme_minimal()
          })
      }

      # Construct scrollplot functions
      max_index <- length(cont_vars)
      min_index <- 1
      index <- shiny::reactiveVal(1)
      output$scrollindexinfo <- shiny::renderText(paste0("Displaying ", index(), " of ", max_index))
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
      output$plot <- plotly::renderPlotly({
        cont_plot_list[[index()]]
      })
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

uiContScrollPlot <- function(id, label = "contscrollplot"){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h3(shiny::tags$b("Continuous Variables")),
    plotly::plotlyOutput(ns("plot"), height = "1000px"),
    shiny::actionButton(ns("buttonbackwards"), label = "Backwards"),
    shiny::actionButton(ns("buttonforwards"), label = "Forwards"),
    shiny::textOutput(ns("scrollindexinfo"))
  )
}
