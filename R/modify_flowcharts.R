#' Modify binary flowchart
#'
#' Highlights flowchart path and inputs test values
#'
#' @param placeholder ph
#'
#' @returns placeholder
#' @details Placeholder
#'
#'
#' @export
#'


binary_flowchart <- function(vals, final_node){

  # Get df of test nodes
  test_node_df <- get_node_df(binary_base_flowchart) |>
    filter(type == "test")

  n_test_nodes <- nrow(test_node_df)

  # Update test node labels
  test_node_labels_old <- test_node_df |>
    select(label)

  test_node_labels_old <- test_node_labels_old |>
    paste0("\n(", vals, ")")


  if(n_test_nodes != length(vals)){
    stop("Number of values must match number of test nodes")
  }

  if (max(test_node_df["id"]) > final_node){
    stop("Final node ID must be greater than final test node")
  }

  # Modify base graph
  modified_flowchart <- binary_base_flowchart |>
    select_nodes(conditions = type == "test") |>
    set_node_attrs_ws(node_attr = "label", value = test_node_labels_old) |>
    clear_selection() |>
    select_nodes_by_id(nodes = 1:final_node) |>
    set_node_attrs_ws(node_attr = "penwidth", value = 2) |>
    set_node_attrs_ws(node_attr = "color", value = "black")

  return(modified_flowchart)

}





#' Modify Continuous flowchart
#'
#' Highlights flowchart path and inputs test values
#'
#' @param dana_fit List returned by fit_cross_sectional_continuous
#'
#' @returns DiagrammeR graph
#' @details Placeholder
#'
#' @export
#'

modify_continuous_flowchart <- function(dana_fit){

  base_node_df <- get_node_df(continuous_base_flowchart)
  base_edge_df <- get_edge_df(continuous_base_flowchart)


  # Modify node df -------------------------------------------------------------
  ## Index of test nodes
  test_index <- base_node_df$type == "test"

  ## Labels from base flowchart
  test_labels <- base_node_df |>
    dplyr::filter(type == "test") |>
    dplyr::pull(label)

  ## Test values
  test_vals <- c(dana_fit$bpval_parametric_untransformed,
                 as.logical(dana_fit$positive_response),
                 dana_fit$bpval_parametric_log_transformed,
                 dana_fit$gof_nonparametric_log_transformed,
                 dana_fit$gof_nonparametric_untransformed,
                 dana_fit$gof_comparison
  ) |>
    round(digits = 3) |>
    as.character()

  ## Append test values to old labels
  test_labels[seq_along(test_vals)] <- paste0(test_labels[seq_along(test_vals)], "\n (", test_vals, ")")

  ## Apply changes
  base_node_df$label[test_index] <- test_labels
  modified_node_df <- base_node_df



  # Modify edge df -------------------------------------------------------------

  modified_flowchart <- create_graph(nodes_df = modified_node_df,
                                     edges_df = base_edge_df)

                                     
                                     
  # Modify depending on final node ---------------------------------------------
  if (dana_fit$term_node == 1){
    modified_flowchart <- modified_flowchart |>
    select_nodes_by_id(nodes = c(6, 13, 1, 7)) |>
    invert_selection() |>
    set_node_attrs_ws(node_attr = fillcolor, value = "grey") |>
    select_edges_by_node_id(nodes = c(6, 13, 1)) |>
    set_edge_attrs_ws(edge_attr = color, value = "black") |>
    invert_selection() |>
    set_edge_attrs_ws(edge_attr = color, value = "grey")
  }

  if (dana_fit$term_node == 2){
    modified_flowchart <- modified_flowchart |>
    select_nodes_by_id(nodes = c(6, 13, 7, 8, 2)) |>
    invert_selection() |>
      set_node_attrs_ws(node_attr = fillcolor, value = "grey") |>
        select_edges_by_node_id(nodes = c(6, 13, 7, 2)) |>
          set_edge_attrs_ws(edge_attr = color, value = "black") |>
            invert_selection() |>
              set_edge_attrs_ws(edge_attr = color, value = "grey")
            
          }
          
  if (dana_fit$term_node == 3){
    modified_flowchart <- modified_flowchart |>
      select_nodes_by_id(nodes = c(6, 13, 14, 7, 8, 3, 9)) |>
      invert_selection() |>
      set_node_attrs_ws(node_attr = fillcolor, value = "grey") |>
      select_edges_by_node_id(nodes = c(6, 13, 3, 14)) |>
      set_edge_attrs_ws(edge_attr = color, value = "black") |>
      invert_selection() |>
      set_edge_attrs_ws(edge_attr = color, value = "grey")

  }

  if (dana_fit$term_node == 4){
    modified_flowchart <- modified_flowchart |>
    select_nodes_by_id(nodes = c(6, 13, 7, 8, 14, 9, 16, 15, 11, 10, 12, 4)) |>
    invert_selection() |>
      set_node_attrs_ws(node_attr = fillcolor, value = "grey") |>
        select_edges_by_node_id(nodes = c(6, 13, 7, 8, 14, 9, 16, 15, 11, 10, 4)) |>
          set_edge_attrs_ws(edge_attr = color, value = "black") |>
            invert_selection() |>
              set_edge_attrs_ws(edge_attr = color, value = "grey")
            
          }
  if (dana_fit$term_node == 5){
    modified_flowchart <- modified_flowchart |>
    select_nodes_by_id(nodes = c(6, 13, 7, 8, 14, 9, 16, 15, 11, 10, 12, 5)) |>
    invert_selection() |>
      set_node_attrs_ws(node_attr = fillcolor, value = "grey") |>
        select_edges_by_node_id(nodes = c(6, 13, 7, 8, 14, 9, 16, 15, 11, 10, 5)) |>
          set_edge_attrs_ws(edge_attr = color, value = "black") |>
            invert_selection() |>
              set_edge_attrs_ws(edge_attr = color, value = "grey")
            
          }


  return(modified_flowchart)

}






# pima <- MASS::Pima.te
# pima$npreg <- factor(pima$npreg)
# pima_fit1 <- fit_cross_sectional_continuous(pima, response_var = "skin", pred_cat_vars = c("type", "npreg"),
#                                     pred_cont_vars = "glu", pred_cat_vars_ref_levels = c("Yes", "3"))
#                                     # 
# modify_continuous_flowchart(pima_fit1) |> render_graph()
# 
# pima_fit2 <- fit_cross_sectional_continuous(pima, response_var = "skin",
#                                     pred_cont_vars = c("glu"))

# mydat <- data.frame(
#   a = runif(50,0,1),
#   b = runif(50,0,1),
#   y = exp(rnorm(50, 2))
# )
# 
# custom_fit1 <- fit_cross_sectional_continuous(mydat,
#                                               pred_cont_vars = c("a","b"), response_var = "y")
# 
# modify_continuous_flowchart(pima_fit2) |> render_graph()
# 
# modify_continuous_flowchart(custom_fit1) |> render_graph()
# 
# modify_continuous_flowchart(b) |> render_graph()






