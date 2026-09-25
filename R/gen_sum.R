#' General Summary Statistics
#'
#' Computes summary statistics and graphs ready for input into shiny
#'
#' @param placeholder test
#' @returns placeholder
#' @details
#' placeholder
#'
#' @export

gen_sum <- function(data,
                    cont_vars,
                    cat_vars,
                    group_var = NULL){

  if (is.null(cont_vars) && is.null(cat_vars)){
    return(NULL)
  }

  rslt <- list()

  rslt$summary_table <- data |>
    gtsummary::tbl_summary(by = group_var) |>
    gtsummary::as_gt()

  # List of variable-specific dataframes

  cont_plot_list <- cont_vars |>
    lapply(function(x){
        plotly::ggplotly(
        ggplot2::ggplot(dplyr::as_tibble(data[c(x, group_var)])) +
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = .data[[x]], fill = .data[[group_var]], group = .data[[group_var]]), position = "identity", alpha = 0.7) +
        ggplot2::labs(x = x, y = "Count")
        )
    })

  rslt$cont_plot_list <- cont_plot_list

  cat_plot_list <- cat_vars |>
    lapply(function(x){
      ggplot2::ggplot(dplyr::as_tibble(data[x, group_var])) + ggplot2::geom_bar(mapping = ggplot2::aes(x = .data[[x]])) +
        ggplot2::labs(x = x, y = "Count")
    })

  rslt$cat_plot_list <- cat_plot_list

  return(rslt)

}



