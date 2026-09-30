library(DiagrammeR)
library(viridisLite)
library(dplyr)
library(devtools)


continuous_base_flowchart <- create_graph() |>

  # Add nodes
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Untransformed \n Gaussian GLM", type = "final_model") |>
  set_node_position(node = 1, x = 2, y = 6) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Untransformed NP GLM", type = "final_model") |>
  set_node_position(node = 2, x = 3.5, y = 6) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Log Transformed Gaussian GLM", type = "final_model") |>
  set_node_position(node = 3, x = 9, y = 7) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Log Transformed NP GLM", type = "final_model") |>
  set_node_position(node = 4, x = 5, y = 1) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Untransformed NP GLM", type = "final_model") |>
  set_node_position(node = 5, x = 5, y = -1) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Continuous Outcome", type = "outcome") |>
  set_node_position(node = 6, x = 0, y = 5.5) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "P_SW Small/Large", type = "test") |>
  set_node_position(node = 7, x = 2, y = 4) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Positive \n Outcomes", type = "test") |>
  set_node_position(node = 8, x = 3.5, y = 4) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "P_SW Small/Large", type = "test") |>
  set_node_position(node = 9, x = 9, y = 4) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "GOF2", type = "test") |>
  set_node_position(node = 10, x = 10, y = 0) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "GOF1", type = "test") |>
  set_node_position(node = 11, x = 8, y = 0) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "GOF1 < GOF2", type = "test") |>
  set_node_position(node = 12, x = 8.5, y = -1) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Untransformed \n Gaussian GLM", type = "model") |>
  set_node_position(node = 13, x = 0, y = 4) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Log Transformed Gaussian GLM", type = "model") |>
  set_node_position(node = 14, x = 5, y = 5) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Untransform Transform NP GLM", type = "model") |>
  set_node_position(node = 15, x = 10, y = 2) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Log Transform NP GLM", type = "model") |>
  set_node_position(node = 16, x = 8, y = 2) |>

  # Add edges
  add_edge(from = 6, to = 13) |>
  add_edge(from = 13, to = 7) |>
  add_edge(from = 14, to = 9) |>
  add_edge(from = 16, to = 11) |>
  add_edge(from = 15, to = 10) |>
  add_edge(from = 11, to = 12) |>
  add_edge(from = 10, to = 12) |>
  add_edge(from = 7, to = 1, edge_aes = edge_aes(label = "No")) |>
  add_edge(from = 8, to = 2, edge_aes = edge_aes(label = "No")) |>
  add_edge(from = 9, to = 3, edge_aes = edge_aes(label = "No")) |>
  add_edge(from = 8, to = 14, edge_aes = edge_aes(label = "Yes")) |>
  add_edge(from = 7, to = 8, edge_aes = edge_aes(label = "Yes")) |>
  add_edge(from = 9, to = 15, edge_aes = edge_aes(label = "Yes")) |>
  add_edge(from = 9, to = 16, edge_aes = edge_aes(label = "Yes")) |>
  add_edge(from = 12, to = 4, edge_aes = edge_aes(label = "Yes")) |>
  add_edge(from = 12, to = 5, edge_aes = edge_aes(label = "No")) |>


  # Apply global formatting
  select_nodes() |>
  set_node_attrs(node_attr = "fontcolor", values = "black") |> #Set all text to black
  set_node_attrs(node_attr = "penwidth", values = 1) |>
  clear_selection() |>

  select_edges() |>
  set_edge_attrs_ws( edge_attr = "color", value = "black") |>
  clear_selection() |>

  # Apply formatting to outcome nodes
  select_nodes(conditions = type == "outcome") |>
  set_node_attrs_ws(node_attr = "fillcolor", value = "white") |>
  set_node_attrs_ws(node_attr = "shape", value = "square") |>
  clear_selection() |>

  # Apply formatting to model nodes
  select_nodes(conditions = type == "model") |>
  set_node_attrs_ws(node_attr = "fillcolor", value = "lightblue") |>
  set_node_attrs_ws(node_attr = "shape", value = "rectangle") |>
  clear_selection() |>

  # Apply formatting to test nodes
  select_nodes(conditions = type == "test") |>
  set_node_attrs_ws(node_attr = "fillcolor", value = "lightyellow") |> #Set all text to black
  set_node_attrs_ws(node_attr = "shape", value = "rectangle") |>
  clear_selection() |>

  # Apply formatting to final_model nodes
  select_nodes(conditions = type == "final_model") |>
  set_node_attrs_ws(node_attr = "fillcolor", value = "orange") |> #Set all text to black
  set_node_attrs_ws(node_attr = "shape", value = "rectangle") |>
  clear_selection()

continuous_base_flowchart |> render_graph()

binary_base_flowchart <- create_graph() |>

  # Add nodes
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Binary Outcome", type = "outcome") |>
  set_node_position(node = 1, x = 0, y = 5.5) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Bayesian Binomial GLM", type = "model") |>
  set_node_position(node = 2, x = 0, y = 4) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "P_d Small/Large", type = "test") |>
  set_node_position(node = 3, x = 2.5, y = 4) |>
  add_node(node_aes = node_aes(fixedsize = FALSE), label = "Nonparametric GLM", type = "model") |>
  set_node_position(node = 4, x = 5, y = 4) |>

  # Add edges
  add_edge(from = 1, to = 2) |>
  add_edge(from = 2, to = 3) |>
  add_edge(from = 3, to = 4, edge_aes = edge_aes(label = "Yes")) |>

  # Apply global formatting
  select_nodes() |>
  set_node_attrs(node_attr = "fontcolor", values = "black") |> #Set all text to black
  set_node_attrs(node_attr = "penwidth", values = 1) |>
  clear_selection() |>

  select_edges() |>
  set_edge_attrs_ws( edge_attr = "color", value = "black") |>
  clear_selection() |>

    # Apply formatting to outcome nodes
  select_nodes(conditions = type == "outcome") |>
  set_node_attrs_ws(node_attr = "fillcolor", value = "white") |>
  set_node_attrs_ws(node_attr = "shape", value = "square") |>
  clear_selection() |>

  # Apply formatting to model nodes
  select_nodes(conditions = type == "model") |>
  set_node_attrs_ws(node_attr = "fillcolor", value = "lightblue") |>
  set_node_attrs_ws(node_attr = "shape", value = "rectangle") |>
  clear_selection() |>

  # Apply formatting to test nodes
  select_nodes(conditions = type == "test") |>
  set_node_attrs_ws(node_attr = "fillcolor", value = "lightyellow") |> #Set all text to black
  set_node_attrs_ws(node_attr = "shape", value = "circle") |>
  clear_selection()

usethis::use_data(binary_base_flowchart,
                  continuous_base_flowchart, internal = TRUE, overwrite = TRUE)






