#' CatScrollPlot Server Logic
#'
#' Create server logic for scrolling plotly outputs based on categorical variables
#'
#' @param id shiny input/output id
#' @param plotly_list a list of plotly objects
#' @details
#' Paired with uiCatScrollPlot through the id argument.
#' This function is placed in an R shiny
#' server function with a specified id and list of plotly objects.
#'
#' When rendered in combination with uiCatScrollPlot, displays plotly objects
#' from the list, which can be scrolled with the associated ActionButton.
#'
#'
#' @seealso
#'  \code{\link{uiCatScrollPlot}}
#'
#'
#' @export
#'
#'
#'

serverCatScrollPlot <- function(id, data, cat_vars, group_var){
  shiny::moduleServer(
    id,
    function(input, output, session){

      # Construct plotly list if grouping variable is specified ----
      ## Create group plot
      if ((!(is.null(group_var) == TRUE) & (group_var != "None"))){

        cat_plot_list <- setdiff(cat_vars, group_var) |>
          lapply(function(x){
            plotly::ggplotly(
              dplyr::as_tibble(data[c(x, group_var)]) |>
                dplyr::mutate(!!group_var := as.factor(.data[[group_var]])) |>
                ggplot2::ggplot() +
                ggplot2::geom_bar(mapping = ggplot2::aes(x = .data[[x]], group = .data[[group_var]]), position = "identity", alpha = 0.7) +
                ggplot2::labs(x = x, y = "Count") + ggplot2::theme_minimal()
            )
          })


        if (group_var %in% cat_vars){

          group_plot <- group_var |>
            lapply(function(x){
              ggplot2::ggplot(dplyr::as_tibble(data[c(x)])) +
              ggplot2::geom_bar(mapping = ggplot2::aes(x = .data[[x]]), position = "identity", alpha = 0.7) +
              ggplot2::labs(x = x, y = "Count") + theme_minimal()
          })
        ## Combine into a single list
        cat_plot_list <- c(group_plot, cat_plot_list)
        }
      }

      # Construct plotly list if no grouping variable is selected ----
      if (is.null(group_var) == TRUE || group_var == "None"){

        cat_plot_list <- lapply(cat_vars, function(x){
          plotly::ggplotly(
            ggplot2::ggplot(dplyr::as_tibble(data[x])) +
              geom_bar(ggplot2::aes(x = .data[[x]]))
          )
        })

      }

      # Construct scrollplot functions
      max_index <- length(cat_vars)
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
      cat_plot_list[[index()]]

      }
      )
    }
  )
}








#' CatScrollPlot UI Logic
#'
#' Create UI logic for scrolling plotly outputs for catinuous variables
#'
#' @param id shiny input/output id
#' @details
#'
#' Paired with serverCatScrollPlot through the id argument.
#'
#' @seealso
#'  \code{\link{uiCatScrollPlot}}
#'
#' @export

uiCatScrollPlot <- function(id, label = "catscrollplot"){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h3(shiny::tags$b("Categorical Variables")),
    plotly::plotlyOutput(ns("plot"), width = 500, height = 500),
    shiny::actionButton(ns("buttonbackwards"), label = "Backwards"),
    shiny::actionButton(ns("buttonforwards"), label = "Forwards"),
    shiny::textOutput(ns("scrollindexinfo"))
  )
}
