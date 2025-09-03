# Make visualizations for memos

# load packages ----
library(tidyverse)
library(here)
library(igraph)
library(ggraph)
library(latex2exp)
library(patchwork)

# make flowcharts ----
## preproc 1 ----
### note: D = initial dataset (vars collated and created), M = imputed dataset, Y = start year
flowchart_tbl_1 <- tibble(
  from = c(
    "X", "X", "X", "X", "X", "m[1]", "m[2]", "m[3]", "m[4]", "m[5]", "s[1]", "s[1]", "s[2]", "s[2]", "s[3]", "s[3]", "s[4]", "s[4]", "s[5]", "s[5]"
    ),
  to = c(
    "m[1]", "m[2]", "m[3]", "m[4]", "m[5]", "s[1]", "s[2]", "s[3]", "s[4]", "s[5]", "s[1]y[1]", "s[1]y[2]", "s[2]y[1]", "s[2]y[2]", "s[3]y[1]", "s[3]y[2]", "s[4]y[1]", "s[4]y[2]", "s[5]y[1]", "s[5]y[2]"
    )
  )

new_labels_1 <- c(
  "X" = "X",
  "m[1]" = "m[1]",
  "m[2]" = "m[2]",
  "m[3]" = "m[3]",
  "m[4]" = "m[4]",
  "m[5]" = "m[5]",
  "s[1]" = "s[1]",
  "s[2]" = "s[2]",
  "s[3]" = "s[3]",
  "s[4]" = "s[4]",
  "s[5]" = "s[5]",
  "s[1]y[1]" = "y[1]",
  "s[1]y[2]" = "y[2]",
  "s[2]y[1]" = "y[1]",
  "s[2]y[2]" = "y[2]",
  "s[3]y[1]" = "y[1]",
  "s[3]y[2]" = "y[2]",
  "s[4]y[1]" = "y[1]",
  "s[4]y[2]" = "y[2]",
  "s[5]y[1]" = "y[1]",
  "s[5]y[2]" = "y[2]"
  )

graph_init_1 <- flowchart_tbl_1 |> 
  graph_from_data_frame()

coords_1 <- graph_init_1 |> 
  layout_as_tree(root = "X")

preproc_1_g <- graph_init_1 |> 
  ggraph(layout = coords_1) +
  geom_edge_link(color = "#586e75") +
  geom_node_label(
    parse = TRUE,
    aes(label = new_labels_1),
    color = "#586e75"
    ) +
  theme_graph(background = "#fdf6e3")

## preproc 2 ----
### Y = start year, L = lagged imputed dataset
flowchart_tbl_2 <- tibble(
  from = c("y[list(i,j)]", "y[list(i,j)]", "y[list(i,j)]", "y[list(i,j)]", "y[list(i,j)]", "y[list(i,j)]", "y[list(i,j)]", "y[list(i,j)]"),
  to = c("l[1]", "l[2]", "l[3]", "l[4]", "l[5]", "l[6]", "l[7]", "l[8]")
  )

graph_init_2 <- flowchart_tbl_2 |> 
  graph_from_data_frame()

coords_2 <- graph_init_2 |> 
  layout_as_tree(root = "y[list(i,j)]")

preproc_2_g <- graph_init_2 |> 
  ggraph(layout = coords_2) +
  geom_edge_link(color = "#586e75") +
  geom_node_label(
    parse = TRUE,
    aes(label = name),
    color = "#586e75"
    ) +
  theme_graph(background = "#fdf6e3")

### patchwork ----
preproc_flow_viz <- preproc_1_g / preproc_2_g + plot_annotation(
  tag_levels = "A",
  title = "Preprocessing Steps",
  subtitle = "From Data Wrangling to Temporal Lagging"
  ) & theme(
  plot.tag = element_text(color = "#586e75", size = 11),
  plot.title = element_text(color = "#586e75"),
  plot.subtitle = element_text(color = "#586e75"),
  plot.background = element_rect("#fdf6e3")
  )

## dml ----
### note: L = single lag-model, R = repeat, F = fold, B = final repeat fit (using "best" lambda), F = selected repeat fit (median of these)
flowchart_tbl_3 <- tibble(
  from = c(
    "l[list(i,j,p)]", "l[list(i,j,p)]", "l[list(i,j,p)]", "r[1]", "r[1]", "r[1]", "r[1]", "r[1]", "r[2]", "r[2]", "r[2]", "r[2]", "r[2]", "r[3]", "r[3]", "r[3]", "r[3]", "r[3]", "r[1]k[1]", "r[1]k[2]", "r[1]k[3]", "r[1]k[4]", "r[1]k[5]", "r[2]k[1]", "r[2]k[2]", "r[2]k[3]", "r[2]k[4]", "r[2]k[5]", "r[3]k[1]", "r[3]k[2]", "r[3]k[3]", "r[3]k[4]", "r[3]k[5]", "theta[1]", "theta[2]", "theta[3]"
    ),
  to = c(
    "r[1]", "r[2]", "r[3]", "r[1]k[1]", "r[1]k[2]", "r[1]k[3]", "r[1]k[4]", "r[1]k[5]", "r[2]k[1]", "r[2]k[2]", "r[2]k[3]", "r[2]k[4]", "r[2]k[5]", "r[3]k[1]", "r[3]k[2]", "r[3]k[3]", "r[3]k[4]", "r[3]k[5]", "theta[1]", "theta[1]", "theta[1]", "theta[1]", "theta[1]", "theta[2]", "theta[2]", "theta[2]", "theta[2]", "theta[2]", "theta[3]", "theta[3]", "theta[3]", "theta[3]", "theta[3]", "tilde(theta)[list(i,j,p)]", "tilde(theta)[list(i,j,p)]", "tilde(theta)[list(i,j,p)]"
    )
  )

new_labels_3 <- c(
  "l[list(i,j,p)]" = "l[list(i,j,p)]",
  "r[1]" = "r[1]",
  "r[2]" = "r[2]",
  "r[3]" = "r[3]",
  "r[1]k[1]" = "k[1]",
  "r[1]k[2]" = "k[2]",
  "r[1]k[3]" = "k[3]",
  "r[1]k[4]" = "k[4]",
  "r[1]k[5]" = "k[5]",
  "r[2]k[1]" = "k[1]",
  "r[2]k[2]" = "k[2]",
  "r[2]k[3]" = "k[3]",
  "r[2]k[4]" = "k[4]",
  "r[2]k[5]" = "k[5]",
  "r[3]k[1]" = "k[1]",
  "r[3]k[2]" = "k[2]",
  "r[3]k[3]" = "k[3]",
  "r[3]k[4]" = "k[4]",
  "r[3]k[5]" = "k[5]",
  "theta[1]" = "theta[1]",
  "theta[2]" = "theta[2]",
  "theta[3]" = "theta[3]",
  "tilde(theta)[list(i,j,p)]" = "tilde(theta)[list(i,j,p)]"
  )

graph_init_3 <- graph_from_data_frame(flowchart_tbl_3)

coords_3 <- layout_as_tree(graph_init_3, root = "l[list(i,j,p)]")
coords_3[which(V(graph_init_3)$name == "theta[1]"), ] <- coords_3[which(V(graph_init_3)$name == "r[1]k[3]"), ] + c(0, -1)
coords_3[which(V(graph_init_3)$name == "theta[2]"), ] <- coords_3[which(V(graph_init_3)$name == "r[2]k[3]"), ] + c(0, -1)
coords_3[which(V(graph_init_3)$name == "theta[3]"), ] <- coords_3[which(V(graph_init_3)$name == "r[3]k[3]"), ] + c(0, -1)
coords_3[which(V(graph_init_3)$name == "tilde(theta)[list(i,j,p)]"), ] <- coords_3[which(V(graph_init_3)$name == "theta[2]"), ] + c(0, -1)

dml_flow_viz <- graph_init_3 |> 
  ggraph(layout = coords_3) +
  geom_edge_link(color = "#586e75") +
  geom_node_label(
    parse = TRUE,
    aes(label = new_labels_3),
    color = "#586e75"
    ) +
  theme_graph(
    background = "#fdf6e3"
    ) +
  plot_annotation(
    title = "DML Modelling",
    subtitle = "Features Repeated K-Fold Cross-Validation"
    ) &
  theme(
    plot.title = element_text(color = "#586e75"),
    plot.subtitle = element_text(color = "#586e75"),
    plot.background = element_rect("#fdf6e3")
    )

## pool ----
flowchart_tbl_4 <- tibble(
  from = c(
    "tilde(theta)[list(1,j,p)]",
    "tilde(theta)[list(2,j,p)]",
    "tilde(theta)[list(3,j,p)]",
    "tilde(theta)[list(4,j,p)]",
    "tilde(theta)[list(5,j,p)]"
    ),
  to = c(rep("theta[list(j,p)]", 5))
  )

graph_init_4 <- graph_from_data_frame(flowchart_tbl_4)
coords_4 <- layout_as_tree(graph_init_4, root = "theta[list(j,p)]", flip.y = F)

pool_flow_viz <- graph_init_4 |> 
  ggraph(layout = coords_4) +
  geom_edge_link(color = "#586e75") +
  geom_node_label(
    parse = TRUE,
    aes(label = name),
    color = "#586e75"
    ) +
  theme_graph(
    background = "#fdf6e3"
    ) +
  plot_annotation(
    title = "Pooling of Coefficients",
    subtitle = TeX("$\\tilde{\\theta}_{i..n,j,p}$ Generated from DML Modelling")
    ) &
  theme(
    plot.title = element_text(color = "#586e75"),
    plot.subtitle = element_text(color = "#586e75"),
    plot.background = element_rect("#fdf6e3")
    )

# note on subscripts: i = imputation id, j = start-year id, k = lag id

# save ----
ggsave(
  preproc_flow_viz,
  width = 1787,
  height = 2587,
  units = "px",
  file = here("visualizations/memos/preproc_flow_viz.png")
  )
ggsave(
  dml_flow_viz,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("visualizations/memos/dml_flow_viz.png")
  )
ggsave(
  pool_flow_viz,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("visualizations/memos/pool_flow_viz.png")
  )
