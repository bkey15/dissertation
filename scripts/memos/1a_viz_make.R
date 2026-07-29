# Make visualizations for memos

# load packages ----
library(tidyverse)
library(here)
library(igraph)
library(ggraph)
library(ggforce)
library(tidygraph)
library(latex2exp)
library(patchwork)
library(gt)
library(rnaturalearth)
library(countrycode)
library(naniar)

# load data ----
load(here("data/ch1/results/imputations/imp_base.rda"))
imp_base_1 <- imp_base |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  relocate(.imp, .id) |> 
  mutate(
    year = as.numeric(levels(year))[year]
    ) |> 
  filter(
    .imp == 0,
    year > 1976
    )

load(here("data/ch2/results/imputations/imp_base.rda"))
imp_base_2 <- imp_base |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  relocate(.imp, .id) |> 
  mutate(
    year = as.numeric(levels(year))[year]
    ) |> 
  filter(
    .imp == 0,
    year > 1980
    )

load(here("data/ch3/results/imputations/imp_base.rda"))
imp_base_3 <- imp_base |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  relocate(.imp, .id) |> 
  filter(.imp == 0)

rm(imp_base)

# make flowcharts ----
## methodology ----
flowchart_tbl_0 <- tibble(
  from = c(
    "Data Wrangling",
    "Multiple Imputation (1)",
    "Lags & Start-Years",
    "DML"
    ),
  to = c(
    "Multiple Imputation (1)",
    "Lags & Start-Years",
    "DML",
    "Multiple Imputation (2)"
    )
  )

graph_init_0 <- flowchart_tbl_0 |> 
  graph_from_data_frame()
coords_0 <- graph_init_0 |> 
  layout_as_tree(root = "Data Wrangling")

left_curve <- tibble(
  x = c(0, -0.1, 0),
  y = c(3, 1.5, 0),
  group = "left"
  )
right_curve <- tibble(
  x = c(0, 0.1, 0),
  y = c(3, 1.5, 0),
  group = "right"
  )
curves <- left_curve |> 
  full_join(right_curve)

method_flow_viz <- graph_init_0 |> 
  ggraph(layout = coords_0) +
  geom_edge_link(
    arrow = arrow(length = unit(2, "mm")),
    aes(end_cap = square(10, "mm")),
    color = "#586e75"
    ) +
  geom_bezier(
    data = curves,
    aes(
      x = x,
      y = y,
      group = group
      ),
    linetype = "dotted",
    color = "#586e75"
    ) +
  geom_node_label(
    aes(label = name),
    color = "#586e75"
    ) +
  theme_graph(background = "#fdf6e3") +
  plot_annotation(
    title = "Methodology",
    subtitle = "A Bird's-Eye View"
    ) &
  theme(
    plot.title = element_text(color = "#586e75"),
    plot.subtitle = element_text(color = "#586e75"),
    plot.background = element_rect("#fdf6e3"),
    legend.position = "none"
    )

## preproc 1 ----
### note: D = initial dataset (vars collated and created), M = imputed dataset, Y = start year
flowchart_tbl_1 <- tibble(
  from = c(
    rep("S", 5),
    paste0("m[", 1:5, "]"),
    str_sort(rep(paste0("p[", 1:5, "]"), 2))
    ),
  to = c(
    paste0("m[", 1:5, "]"),
    paste0("p[", 1:5, "]"),
    str_sort(paste0("p[", rep(1:5, 2), "]t[", 1:2, "]"))
    )
  )

new_labels_1 <- c(
  "S" = "S",
  "m[1]" = "m[1]",
  "m[2]" = "m[2]",
  "m[3]" = "m[3]",
  "m[4]" = "m[4]",
  "m[5]" = "m[5]",
  "p[1]" = "p[1]",
  "p[2]" = "p[2]",
  "p[3]" = "p[3]",
  "p[4]" = "p[4]",
  "p[5]" = "p[5]",
  "p[1]t[1]" = "t[list(1,1)]",
  "p[1]t[2]" = "t[list(1,2)]",
  "p[2]t[1]" = "t[list(2,1)]",
  "p[2]t[2]" = "t[list(2,2)]",
  "p[3]t[1]" = "t[list(3,1)]",
  "p[3]t[2]" = "t[list(3,2)]",
  "p[4]t[1]" = "t[list(4,1)]",
  "p[4]t[2]" = "t[list(4,2)]",
  "p[5]t[1]" = "t[list(5,1)]",
  "p[5]t[2]" = "t[list(5,2)]"
  )

graph_init_1 <- flowchart_tbl_1 |> 
  graph_from_data_frame()

coords_1 <- graph_init_1 |> 
  layout_as_tree(root = "S")

preproc_1_g <- graph_init_1 |> 
  ggraph(layout = coords_1) +
  geom_edge_link(
    arrow = arrow(length = unit(2, "mm")),
    aes(end_cap = square(10, "mm")),
    color = "#586e75") +
  geom_node_label(
    parse = TRUE,
    aes(label = new_labels_1),
    color = "#586e75"
    ) +
  theme_graph(background = "#fdf6e3")

## preproc 2 ----
### Y = start year, V = lagged imputed dataset
flowchart_tbl_2 <- tibble(
  from = rep("t[list(i,j)]", 8),
  to = paste0("v[list(i,j,", 1:8, ")]")
  )

graph_init_2 <- flowchart_tbl_2 |> 
  graph_from_data_frame()

coords_2 <- graph_init_2 |> 
  layout_as_tree(root = "t[list(i,j)]")

preproc_2_g <- graph_init_2 |> 
  ggraph(layout = coords_2) +
  geom_edge_link(
    arrow = arrow(length = unit(2, "mm")),
    aes(end_cap = square(10, "mm")),
    color = "#586e75"
    ) +
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
### tuning ----
#### get base tbl
flowchart_tbl_3 <- tibble(
  from = c(
    rep("v[list(i,j,l)]", 9),
    "dummy",
    "lambda",
    paste0("k[", 1:5, "]"),
    "tilde(lambda)",
    "hat(lambda)",
    "dummy[list(\u2113,1)]",
    "lambda[list(\u2113,1)]",
    paste0("lambda[list(\u2113,1)]e[", 1:5, "]"),
    "tilde(lambda)[list(\u2113,1)]",
    "hat(lambda)[\u2113]",
    "dummy[list(\u2113,n)]",
    "lambda[list(\u2113,n)]",
    paste0("lambda[list(\u2113,n)]e[", 1:5, "]"),
    "tilde(lambda)[list(\u2113,n)]",
    "hat(lambda)[list(n,none)]",
    "dummy[list(m,1)]",
    "lambda[list(m,1)]",
    paste0("lambda[list(m,1)]e[", 1:5, "]"),
    "tilde(lambda)[list(m,1)]",
    "hat(lambda)[m]"
    ),
  to = c(
    "dummy",
    "lambda",
    paste0("k[", 1:5, "]"),
    "tilde(lambda)",
    "hat(lambda)",
    "dummy[list(\u2113,1)]",
    "lambda[list(\u2113,1)]",
    paste0("lambda[list(\u2113,1)]e[", 1:5, "]"),
    "tilde(lambda)[list(\u2113,1)]",
    "hat(lambda)[\u2113]",
    "dummy[list(\u2113,n)]",
    "lambda[list(\u2113,n)]",
    paste0("lambda[list(\u2113,n)]e[", 1:5, "]"),
    "tilde(lambda)[list(\u2113,n)]",
    "hat(lambda)[list(n,none)]",
    "dummy[list(m,1)]",
    "lambda[list(m,1)]",
    paste0("lambda[list(m,1)]e[", 1:5, "]"),
    "tilde(lambda)[list(m,1)]",
    "hat(lambda)[m]",
    "dummy[list(m,n)]",
    "lambda[list(m,n)]",
    paste0("lambda[list(m,n)]e[", 1:5, "]"),
    "tilde(lambda)[list(m,n)]",
    "hat(lambda)[list(m,none)]"
    )
  )

#### get new labels
new_labels_3 <- c(
  "v[list(i,j,l)]" = "v[list(i,j,l)]",
  "dummy" = NA,
  "lambda" = NA,
  "k[1]" = "k[1]",
  "k[2]" = "k[2]",
  "k[3]" = "k[3]",
  "k[4]" = "k[4]",
  "k[5]" = "k[5]",
  "tilde(lambda)" = NA,
  "hat(lambda)" = NA,
  "dummy[list(\u2113,1)]" = NA,
  "lambda[list(\u2113,1)]" = "lambda[list(\u2113,1)]",
  "lambda[list(\u2113,1)]e[1]" = "e[1]",
  "lambda[list(\u2113,1)]e[2]" = "e[2]",
  "lambda[list(\u2113,1)]e[3]" = "e[3]",
  "lambda[list(\u2113,1)]e[4]" = "e[4]",
  "lambda[list(\u2113,1)]e[5]" = "e[5]",
  "tilde(lambda)[list(\u2113,1)]" = "tilde(lambda)[list(\u2113,1)]",
  "hat(lambda)[\u2113]" = "hat(lambda)[\u2113]",
  "dummy[list(\u2113,n)]" = NA,
  "lambda[list(\u2113,n)]" = "lambda[list(\u2113,n)]",
  "lambda[list(\u2113,n)]e[1]" = "e[1]",
  "lambda[list(\u2113,n)]e[2]" = "e[2]",
  "lambda[list(\u2113,n)]e[3]" = "e[3]",
  "lambda[list(\u2113,n)]e[4]" = "e[4]",
  "lambda[list(\u2113,n)]e[5]" = "e[5]",
  "tilde(lambda)[list(\u2113,n)]" = "tilde(lambda)[list(\u2113,n)]",
  "hat(lambda)[list(n,none)]" = NA,
  "dummy[list(m,1)]" = NA,
  "lambda[list(m,1)]" = "lambda[list(m,1)]",
  "lambda[list(m,1)]e[1]" = "e[1]",
  "lambda[list(m,1)]e[2]" = "e[2]",
  "lambda[list(m,1)]e[3]" = "e[3]",
  "lambda[list(m,1)]e[4]" = "e[4]",
  "lambda[list(m,1)]e[5]" = "e[5]",
  "tilde(lambda)[list(m,1)]" = "tilde(lambda)[list(m,1)]",
  "hat(lambda)[m]" = "hat(lambda)[m]",
  "dummy[list(m,n)]" = NA,
  "lambda[list(m,n)]" = "lambda[list(m,n)]",
  "lambda[list(m,n)]e[1]" = "e[1]",
  "lambda[list(m,n)]e[2]" = "e[2]",
  "lambda[list(m,n)]e[3]" = "e[3]",
  "lambda[list(m,n)]e[4]" = "e[4]",
  "lambda[list(m,n)]e[5]" = "e[5]",
  "tilde(lambda)[list(m,n)]" = "tilde(lambda)[list(m,n)]",
  "hat(lambda)[list(m,none)]" = NA
  )

#### get base tree coordinates
graph_init_3 <- graph_from_data_frame(flowchart_tbl_3)
coords_3 <- layout_as_tree(graph_init_3, root = "v[list(i,j,l)]")

#### add, remove, edit edges
##### get vertex indices from: V(graph_init_3)$name
graph_init_3 <- graph_init_3 |> 
  add_edges(
    c(
      12, 13,
      13, 14,
      14, 15,
      15, 16,
      16, 17,
      17, 18,
      18, 19,
      21, 22,
      22, 23,
      23, 24,
      24, 25,
      25, 26,
      26, 27,
      27, 19,
      30, 31,
      31, 32,
      32, 33,
      33, 34,
      34, 35,
      35, 36,
      36, 37,
      39, 40,
      40, 41,
      41, 42,
      42, 43,
      43, 44,
      44, 45,
      45, 37
      )
    )

##### get edge indices from: print(E(graph_init_3), full = T)
graph_init_3 <- graph_init_3 |> 
  delete_edges(
    c(1, 2, 8, 9, 10, 11, 17, 18, 19, 20, 26, 27, 28, 29, 35, 36, 37, 38, 44, 45)
    )

#### adjust coords
coords_3 <- coords_3 |> 
  as_tibble() |> 
  mutate(
    V2 = case_when(
      V1 == 4 & V2 == 3 ~ 2.5, 
      V1 == 4 & V2 == 1 ~ 0.5,
      .default = V2
      )
    ) |> 
  as.matrix()

#### specify custom edges
arrow_edges_base <- tibble(
  node1.name = c(
    rep("v[list(i,j,l)]", 5),
    "lambda[list(\u2113,1)]",
    paste0("e[", 1:5, "]"),
    "tilde(lambda)[list(\u2113,1)]",
    "lambda[list(\u2113,n)]",
    paste0("e[", 1:5, "]"),
    "tilde(lambda)[list(\u2113,n)]",
    "lambda[list(m,1)]",
    paste0("e[", 1:5, "]"),
    "tilde(lambda)[list(m,1)]",
    "lambda[list(m,n)]",
    paste0("e[", 1:5, "]"),
    "tilde(lambda)[list(m,n)]"
    ),
  x = c(rep(0, 5), rep(-3:3, 4)),
  y = c(rep(5, 5), rep(3, 7), rep(2, 7), rep(1, 7), rep(0, 7)),
  node2.name = c(
    paste0("k[", 1:5, "]"),
    paste0("e[", 1:5, "]"),
    "tilde(lambda)[list(\u2113,1)]",
    "hat(lambda)[\u2113]",
    paste0("e[", 1:5, "]"),
    "tilde(lambda)[list(\u2113,n)]",
    "hat(lambda)[\u2113]",
    paste0("e[", 1:5, "]"),
    "tilde(lambda)[list(m,1)]",
    "hat(lambda)[m]",
    paste0("e[", 1:5, "]"),
    "tilde(lambda)[list(m,n)]",
    "hat(lambda)[m]"
    ),
  xend = c(-2:2, rep(-2:4, 4)),
  yend = c(rep(4, 5), rep(3, 6), 2.5, rep(2, 6), 2.5, rep(1, 6), 0.5, rep(0, 6), 0.5),
  edge.id = c(1:5, 26:53),
  lty = rep("solid", 33),
  label = case_when(
    str_detect(node1.name, "^tilde") & str_detect(node1.name, "1\\)\\]$") ~ "either",
    str_detect(node1.name, "^tilde") & str_detect(node1.name, "n\\)\\]$") ~ "or",
    .default = NA
    )
  )

label_edges <- arrow_edges_base |> 
  filter(!is.na(label))
arrow_edges <- arrow_edges_base |> 
  filter(is.na(label)) |> 
  select(-label)

dot_edges <- tibble(
  x = rep(-2:2, 4),
  y = c(rep(4, 5), rep(3, 5), rep(2, 5), rep(1, 5)),
  xend = rep(-2:2, 4),
  yend = c(rep(3, 5), rep(2, 5), rep(1, 5), rep(0, 5)),
  edge.id = c(6:25),
  lty = rep("dotted", 20)
  )

#### finalize
dml_flow_viz_1 <- graph_init_3 |> 
  as_tbl_graph() |> 
  ggraph(layout = coords_3) +
  geom_edge_link(
    data = arrow_edges,
    arrow = arrow(length = unit(2, "mm")),
    aes(
      linetype = lty,
      end_cap = square(10, "mm")
      ),
    color = "#586e75"
    ) +
  geom_edge_link(
    data = label_edges,
    arrow = arrow(length = unit(2, "mm")),
    aes(
      label = label,
      linetype = lty,
      end_cap = square(10, "mm")
      ),
    angle_calc = "along",
    label_dodge = unit(2.5, "mm"),
    label_push = unit(-1.5, "mm"),
    label_size = 2.5,
    label_colour = "#586e75",
    color = "#586e75"
    ) +
  geom_edge_link(
    data = dot_edges,
    aes(
      linetype = lty,
      start_cap = square(1, "mm"),
      end_cap = square(1, "mm")
      ),
    color = "#586e75"
    ) +
  geom_node_label(
    parse = TRUE,
    aes(label = new_labels_3),
    color = "#586e75"
    ) +
  scale_edge_linetype_manual(
    values = c(
      "solid" = "solid",
      "dotted" = "dotted",
      show.legend = FALSE
      )
    ) +
  theme_graph(
    background = "#fdf6e3"
    ) +
  plot_annotation(
    title = "DML Modeling",
    subtitle = "Hyperparameter Tuning (Ridge)"
    ) &
  theme(
    plot.title = element_text(color = "#586e75"),
    plot.subtitle = element_text(color = "#586e75"),
    plot.background = element_rect("#fdf6e3"),
    legend.position = "none"
    )

### fits ----
#### get base tbl
flowchart_tbl_4 <- tibble(
  from = c(
    "v[list(i,j,l)]", "v[list(i,j,l)]", "v[list(i,j,l)]", "r[1]", "r[1]", "r[1]", "r[2]", "r[2]", "r[2]", "r[3]", "r[3]", "r[3]", "r[1]k[1]", "r[1]k[1]", "r[1]k[...]", "r[1]k[...]", "r[1]k[K^2]", "r[1]k[K^2]", "r[2]k[1]", "r[2]k[1]", "r[2]k[...]", "r[2]k[...]", "r[2]k[K^2]", "r[2]k[K^2]", "r[3]k[1]", "r[3]k[1]", "r[3]k[...]", "r[3]k[...]", "r[3]k[K^2]", "r[3]k[K^2]", "r[1]k[1]e[list(1,\u2113)]", "r[1]k[1]e[list(1,m)]", "r[1]k[...]e[list(...,\u2113)]", "r[1]k[...]e[list(...,m)]", "r[1]k[K^2]e[list(K^2,\u2113)]", "r[1]k[K^2]e[list(K^2,m)]", "r[2]k[1]e[list(1,\u2113)]", "r[2]k[1]e[list(1,m)]", "r[2]k[...]e[list(...,\u2113)]", "r[2]k[...]e[list(...,m)]", "r[2]k[K^2]e[list(K^2,\u2113)]", "r[2]k[K^2]e[list(K^2,m)]", "r[3]k[1]e[list(1,\u2113)]", "r[3]k[1]e[list(1,m)]", "r[3]k[...]e[list(...,\u2113)]", "r[3]k[...]e[list(...,m)]", "r[3]k[K^2]e[list(K^2,\u2113)]", "r[3]k[K^2]e[list(K^2,m)]", "tilde(theta)[1]", "tilde(theta)[2]", "tilde(theta)[3]"
    ),
  to = c(
    "r[1]", "r[2]", "r[3]", "r[1]k[1]", "r[1]k[...]", "r[1]k[K^2]", "r[2]k[1]", "r[2]k[...]", "r[2]k[K^2]", "r[3]k[1]", "r[3]k[...]", "r[3]k[K^2]", "r[1]k[1]e[list(1,\u2113)]", "r[1]k[1]e[list(1,m)]", "r[1]k[...]e[list(...,\u2113)]", "r[1]k[...]e[list(...,m)]", "r[1]k[K^2]e[list(K^2,\u2113)]", "r[1]k[K^2]e[list(K^2,m)]", "r[2]k[1]e[list(1,\u2113)]", "r[2]k[1]e[list(1,m)]", "r[2]k[...]e[list(...,\u2113)]", "r[2]k[...]e[list(...,m)]", "r[2]k[K^2]e[list(K^2,\u2113)]", "r[2]k[K^2]e[list(K^2,m)]", "r[3]k[1]e[list(1,\u2113)]", "r[3]k[1]e[list(1,m)]", "r[3]k[...]e[list(...,\u2113)]", "r[3]k[...]e[list(...,m)]", "r[3]k[K^2]e[list(K^2,\u2113)]", "r[3]k[K^2]e[list(K^2,m)]", "tilde(theta)[1]", "tilde(theta)[1]", "tilde(theta)[1]", "tilde(theta)[1]", "tilde(theta)[1]", "tilde(theta)[1]", "tilde(theta)[2]", "tilde(theta)[2]", "tilde(theta)[2]", "tilde(theta)[2]", "tilde(theta)[2]", "tilde(theta)[2]", "tilde(theta)[3]", "tilde(theta)[3]", "tilde(theta)[3]", "tilde(theta)[3]", "tilde(theta)[3]", "tilde(theta)[3]", "hat(theta)[list(i,j,l)]", "hat(theta)[list(i,j,l)]", "hat(theta)[list(i,j,l)]"
    )
  )

#### get new labels
new_labels_4 <- c(
  "v[list(i,j,l)]" = "v[list(i,j,l)]",
  "r[1]" = "r[1]",
  "r[2]" = "r[2]",
  "r[3]" = "r[3]",
  "r[1]k[1]" = "k[1]",
  "r[1]k[...]" = "k[...]",
  "r[1]k[K^2]" = "k[K^2]",
  "r[2]k[1]" = "k[1]",
  "r[2]k[...]" = "k[...]",
  "r[2]k[K^2]" = "k[K^2]",
  "r[3]k[1]" = "k[1]",
  "r[3]k[...]" = "k[...]",
  "r[3]k[K^2]" = "k[K^2]",
  "r[1]k[1]e[list(1,\u2113)]" = "e[list(1,\u2113)]",
  "r[1]k[1]e[list(1,m)]" = "e[list(1,m)]",
  "r[1]k[...]e[list(1,\u2113)]" = "e[list(...,\u2113)]",
  "r[1]k[...]e[list(1,m)]" = "e[list(...,m)]",
  "r[1]k[K^2]e[list(1,\u2113)]" = "e[list(K^2,\u2113)]",
  "r[1]k[K^2]e[list(1,m)]" = "e[list(K^2,m)]",
  "r[2]k[1]e[list(1,\u2113)]" = "e[list(1,\u2113)]",
  "r[2]k[1]e[list(1,m)]" = "e[list(1,m)]",
  "r[2]k[...]e[list(1,\u2113)]" = "e[list(...,\u2113)]",
  "r[2]k[...]e[list(1,m)]" = "e[list(...,m)]",
  "r[2]k[K^2]e[list(1,\u2113)]" = "e[list(K^2,\u2113)]",
  "r[2]k[K^2]e[list(1,m)]" = "e[list(K^2,m)]",
  "r[3]k[1]e[list(1,\u2113)]" = "e[list(1,\u2113)]",
  "r[3]k[1]e[list(1,m)]" = "e[list(1,m)]",
  "r[3]k[...]e[list(1,\u2113)]" = "e[list(...,\u2113)]",
  "r[3]k[...]e[list(1,m)]" = "e[list(...,m)]",
  "r[3]k[K^2]e[list(1,\u2113)]" = "e[list(K^2,\u2113)]",
  "r[3]k[K^2]e[list(1,m)]" = "e[list(K^2,m)]",
  "tilde(theta)[1]" = "tilde(theta)[1]",
  "tilde(theta)[2]" = "tilde(theta)[2]",
  "tilde(theta)[3]" = "tilde(theta)[3]",
  "hat(theta)[list(i,j,l)]" = "hat(theta)[list(i,j,l)]"
  )

#### get & adjust coords
graph_init_4 <- graph_from_data_frame(flowchart_tbl_4)

coords_4 <- layout_as_tree(graph_init_4, root = "v[list(i,j,l)]")

coords_4[which(V(graph_init_4)$name == "tilde(theta)[1]"), ] <- coords_4[which(V(graph_init_4)$name == "r[1]k[...]"), ] + c(0, -2)
coords_4[which(V(graph_init_4)$name == "tilde(theta)[2]"), ] <- coords_4[which(V(graph_init_4)$name == "r[2]k[...]"), ] + c(0, -2)
coords_4[which(V(graph_init_4)$name == "tilde(theta)[3]"), ] <- coords_4[which(V(graph_init_4)$name == "r[3]k[...]"), ] + c(0, -2)
coords_4[which(V(graph_init_4)$name == "hat(theta)[list(i,j,l)]"), ] <- coords_4[which(V(graph_init_4)$name == "tilde(theta)[2]"), ] + c(0, -1)

#### specify custom edges
##### get edge indices from: print(E(graph_init_4), full = T)
label_edges_2 <- tibble(
  node1.name = paste0("tilde(theta)[", 1:3, "]"),
  x = c(-6, 0, 6),
  y = 1,
  node2.name = "hat(theta)[list(i,j,l)]",
  xend = 0,
  yend = 0,
  edge.id = 49:51,
  lty = "solid",
  label = c(
    "either",
    rep("or", 2)
    )
  )

#### finalize
dml_flow_viz_2 <- graph_init_4 |> 
  ggraph(layout = coords_4) +
  geom_edge_link(
    arrow = arrow(length = unit(2, "mm")),
    aes(end_cap = square(10, "mm")),
    color = "#586e75"
    ) +
  geom_edge_link(
    data = label_edges_2,
    arrow = arrow(length = unit(2, "mm")),
    aes(
      label = label,
      linetype = lty,
      end_cap = square(10, "mm")
      ),
    angle_calc = "along",
    label_dodge = unit(2.5, "mm"),
    label_push = unit(-1.5, "mm"),
    label_size = 2.5,
    label_colour = "#586e75",
    color = "#586e75"
    ) +
  geom_node_label(
    parse = TRUE,
    aes(label = new_labels_4),
    color = "#586e75"
    ) +
  theme_graph(
    background = "#fdf6e3"
    ) +
  plot_annotation(
    title = "DML Modeling",
    subtitle = TeX("With Repeated $K^2$ Cluster-Robust Cross-Fitting")
    ) &
  theme(
    plot.title = element_text(color = "#586e75"),
    plot.subtitle = element_text(color = "#586e75"),
    plot.background = element_rect("#fdf6e3"),
    legend.position = "none"
    )

## pool ----
flowchart_tbl_5 <- tibble(
  from = paste0("hat(theta)[list(", 1:5, ",j,l)]"),
  to = rep("hat(theta)[list(j,l)]", 5)
  )

graph_init_5 <- graph_from_data_frame(flowchart_tbl_5)
coords_5 <- layout_as_tree(graph_init_5, root = "hat(theta)[list(j,l)]", flip.y = F)

pool_flow_viz <- graph_init_5 |> 
  ggraph(layout = coords_5) +
  geom_edge_link(
    arrow = arrow(length = unit(2, "mm")),
    aes(end_cap = square(10, "mm")),
    angle_calc = "along",
    label_dodge = unit(2.5, "mm"),
    label_size = 2.5,
    label_colour = "#586e75",
    color = "#586e75"
    ) +
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
    subtitle = TeX("All $\\hat{\\theta}_{i,j,l}$ Generated from DML Modeling")
    ) &
  theme(
    plot.title = element_text(color = "#586e75"),
    plot.subtitle = element_text(color = "#586e75"),
    plot.background = element_rect("#fdf6e3")
    )

# note on subscripts: i = imputation id, j = start-year id, k = lag id

# appendix a ----
## cow dat ----
keep_cows <- c(31, 54, 55, 56, 57, 58, 60, 80, 221, 223, 232, 265, 331, 680, 835, 946, 947, 955, 970, 983, 986, 987, 990)

appx_a_dat <- states::cowstates |> 
  rename(cow = cowcode) |> 
  filter(cow %in% keep_cows) |> 
  mutate(
    country_name = case_when(
      cow == 265 ~ "East Germany",
      cow == 680 ~ "South Yemen",
      .default = country_name
      )
    ) |> 
  select(cow, country_name, microstate)

## get world dat ----
world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
  ) |> 
  arrange(subunit) |> 
  mutate(
    cow = countrycode(
      sourcevar = iso_a3,
      origin = "iso3c",
      destination = "cown"
      ),
    cow = case_when(
      iso_a3 == "SRB" ~ 345,
      subunit == "France" ~ 220,
      subunit == "Kosovo" ~ 347,
      subunit == "Norway" ~ 385,
      .default = cow
      )
    ) |> 
  select(cow, region_wb) |> 
  arrange(cow)

## combine ----
appx_a_dat <- appx_a_dat |> 
  left_join(world) |> 
  mutate(
    region_wb = case_when(
      cow == 265 ~ "Europe & Central Asia",
      cow == 680 ~ "Middle East & North Africa",
      .default = region_wb
      )
    ) |> 
  select(-geometry)

## add paliestine ----
pal_dat <- tibble(
  cow = rep(NA, 2),
  country_name = c("Palestine/West Bank", "Palestine/Gaza"),
  microstate = rep(NA, 2),
  region_wb = rep("Middle East & North Africa", 2)
  )

appx_a_slice_1 <- appx_a_dat |> 
  filter(cow < 680)
appx_a_slice_2 <- appx_a_dat |> 
  filter(cow > 679)

appx_a_dat <- appx_a_slice_1 |> 
  rbind(pal_dat, appx_a_slice_2)

## create new ids ----
appx_a_dat <- appx_a_dat |> 
  mutate(
    in_fariss = if_else(
      is.na(cow), FALSE, TRUE
      ),
    in_vdem = if_else(
      is.na(cow) | cow == 265 | cow == 680, TRUE, FALSE),
    in_ne = case_when(
      is.na(cow) ~ NA,
      cow == 265 | cow == 680 ~ FALSE,
      .default = TRUE
      ),
    in_2fe = if_else(in_ne != FALSE | is.na(in_ne), FALSE, TRUE),
    in_spat = FALSE
    )

## make viz ----
appx_a_viz <- appx_a_dat |> 
  gt(
    rowname_col = "country_name",
    groupname_col = "region_wb"
    ) |> 
  cols_label(
    cow = md("*COW Code*"),
    microstate = md("*Microstate*"),
    in_fariss = md("*In Fariss et al. (2022)*"),
    in_vdem = md("*In V-Dem*"),
    in_ne = md("*In Natural Earth*"),
    in_2fe = md("*In 2FE Models*"),
    in_spat = md("*In Spatial Models*")
    ) |> 
  tab_style(
    style = list(
      cell_fill(color = "gray35"),
      cell_text(color = "white")
      ),
    locations = cells_row_groups()
    ) |> 
  tab_style(
    style = list(
      cell_borders(
        sides = c("left", "right"),
        style = "solid",
        color = "gray35"
        )
      ),
    locations = cells_body()
    ) |> 
  opt_table_outline() |> 
  fmt_tf(tf_style = "circles") |> 
  sub_missing() |> 
  tab_footnote(
    footnote = "Status assigned in the COW State System Membership List (v2011).",
    locations = cells_column_labels(columns = microstate)
    ) |> 
  tab_footnote(
    footnote = "Palestine is uncovered in the COW project.",
    locations = cells_body(
      columns = c(cow, microstate),
      rows = contains("Palestine")
      )
    ) |> 
  tab_footnote(
    footnote = "Natural Earth represents Palestine as a single polygon.",
    locations = cells_body(
      columns = in_ne,
      rows = contains("Palestine")
      )
    ) |> 
  tab_source_note(source_note = "Sources: COW (v2011), Fariss et al. (2022, v1), Natural Earth (v4.1.0), & V-Dem (v15).") |> 
  tab_header(
    title = md("**Countries Excluded from Models**"),
    subtitle = "Cases lack coverage in at least one critical dataset"
    )

# appendix b ----
## make dat ----
appx_b_dat <- tibble(
  name = c("Czechia", "Germany", "Serbia", "Yemen", "Czechoslovakia", "West Germany", "Yugoslavia", "North Yemen"),
  stat = c("Successor", "Successor", "Successor", "Successor", "Predecessor", "Predecessor", "Predecessor", "Predecessor"),
  cow_hr = c(316, 255, 345, 679, 316, 260, 345, 678),
  cow_vdem = c(316, 255, 345, 679, 315, 260, 345, 678),
  id_vdem = c(157, 77, 198, 14, 157, 77, 198, 14),
  dec_cow = c(316, 255, 345, 679, 316, 255, 345, 679)
  )

## make viz ----
### https://waldyrious.net/viridis-palette-generator/, categories = 10
appx_b_viz <- appx_b_dat |> 
  gt(
    rowname_col = "name",
    groupname_col = "stat"
    ) |> 
  tab_header(
    title = md("**Manually-Coded Country IDs**"),
    subtitle = "Resolves discrepancies between predecessor-successor continuities"
    ) |> 
  cols_label(
    cow_hr = md("*COW (HRS)*"),
    cow_vdem = md("*COW (V-Dem)*"),
    id_vdem = md("*ID (V-Dem)*"),
    dec_cow = md("*Decision (COW)*"),
    ) |> 
  tab_footnote(footnote = html("Shared COW (HR Scores) = <span style='display:inline-block;width:12px;height:12px;background-color:#6ece58;margin-left:4px;'></span>")) |> 
  tab_footnote(footnote = html("Shared COW (V-Dem) = <span style='display:inline-block;width:12px;height:12px;background-color:#fde725;margin-left:4px;'></span>")) |> 
  tab_footnote(footnote = html("Shared ID (V-Dem) = <span style='display:inline-block;width:12px;height:12px;background-color:#1f9e89;margin-left:4px;'></span>")) |> 
  tab_source_note(source_note = "Sources: HR Scores (v4) & V-Dem (v15)") |> 
  tab_style(
    style = cell_fill(color = "#1f9e89"),
    locations = cells_body(
      columns = id_vdem
      )
    ) |> 
  tab_style(
    style = cell_fill(color = "#6ece58"),
    locations = cells_body(
      columns = cow_hr,
      rows = name == "Czechia" | name == "Czechoslovakia" | name == "Serbia" | name == "Yugoslavia"
      )
    ) |> 
  tab_style(
    style = cell_fill(color = "#fde725"),
    locations = cells_body(
      columns = cow_vdem,
      rows = name == "Serbia" | name == "Yugoslavia"
      )
    ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = dec_cow)
    ) |> 
  tab_style(
    style = list(
      cell_fill(color = "gray35"),
      cell_text(color = "white")
      ),
    locations = cells_row_groups()
    ) |> 
  tab_style(
    style = list(
      cell_borders(
        sides = c("left", "right"),
        style = "solid",
        color = "gray35"
        )
      ),
    locations = cells_body()
    ) |> 
  opt_table_outline()

# appendix c ----
## make dat ----
appx_c_dat <- tibble(
  chp_mod = c(
    rep("Chapter 1 (PTAs)", 2), rep("Chapter 2 (BITs)", 2), "Chapter 3 (Sanctions)"
    ),
  st_yr = c("Start 1977", "Start 1990", "Start 1981", "Start 1990", "Start 1990"),
  n_cs_2fe = c(176, 174, 176, 174, 139),
  n_cs_spat = c(174, 174, 174, 174, 139)
  )

## make viz ----
appx_c_viz <- appx_c_dat |> 
  gt(
    rowname_col = "st_yr",
    groupname_col = "chp_mod"
    ) |> 
  tab_header(
    title = md("**Total Number of Countries Included per Model**"),
    subtitle = "Varies by Type and Dataset Start Year"
    ) |> 
  cols_label(
    n_cs_2fe = md("*2FE Models*"),
    n_cs_spat = md("*Spatial Models*")
    ) |> 
  tab_style(
    style = list(
      cell_fill(color = "gray35"),
      cell_text(color = "white")
      ),
    locations = cells_row_groups()
    ) |> 
  tab_style(
    style = list(
      cell_borders(
        sides = c("left", "right"),
        style = "solid",
        color = "gray35"
        )
      ),
    locations = cells_body()
    ) |> 
  opt_table_outline() |> 
  tab_footnote(
    footnote = md("The universe of cases in Chapter 3 is limited to \"Global South\" countries."),
    locations = cells_row_groups(
      groups = contains("Sanctions")
      )
    ) |> 
  tab_source_note(source_note = md("Note: values are taken from the most \"general\" models available (i.e., for chapters 1 and 2, those sans Global South/Global North conditionalities), and with temporal lag $t-1$."))

# appendix d ----
## get covars ----
ch1_covars <- imp_base_1 |> 
  select(
    year,
    starts_with(
      c("v2x", "wdi", "e_", "p_", "bop_")
      ),
    ends_with("log10"),
    inv,
    hras
    )

ch2_covars <- imp_base_2 |> 
  select(
    year,
    starts_with(
      c("v2", "wdi", "e_", "p_", "bop_")
      ),
    ends_with("log10")
    )

ch3_covars <- imp_base_3 |> 
  select(
    gov_kill,
    coup_success,
    cont_elect,
    starts_with(
      c("v2", "wdi", "e_", "p_", "bop_", "pol_")
      ),
    ends_with(
      c("log10", "west")
      )
    )

## get miss figs ----
### ch1 ----
ch1_pct_miss_case_1977 <- ch1_covars |> 
  pct_miss_case()
ch1_avg_miss_rt_1977 <- ch1_covars |> 
  pct_miss()

ch1_pct_miss_case_1990 <- ch1_covars |> 
  filter(year > 1989) |> 
  pct_miss_case()
ch1_avg_miss_rt_1990 <- ch1_covars |> 
  filter(year > 1989) |> 
  pct_miss()

### ch2 ----
ch2_pct_miss_case_1981 <- ch2_covars |> 
  pct_miss_case()
ch2_avg_miss_rt_1981 <- ch2_covars |> 
  pct_miss()

ch2_pct_miss_case_1990 <- ch2_covars |> 
  filter(year > 1989) |> 
  pct_miss_case()
ch2_avg_miss_rt_1990 <- ch2_covars |> 
  filter(year > 1989) |> 
  pct_miss()

### ch3 ----
ch3_pct_miss_case <- ch3_covars |> 
  pct_miss_case()
ch3_avg_miss_rt <- ch3_covars |> 
  pct_miss()

### combine ----
appx_d_dat <- tibble(
  chp_mod = c(
    rep("Chapter 1 (PTAs)", 2), rep("Chapter 2 (BITs)", 2), "Chapter 3 (Sanctions)"
    ),
  st_yr = c("Start 1977", "Start 1990", "Start 1981", "Start 1990", "Start 1990"),
  pct_miss_case = c(ch1_pct_miss_case_1977, ch1_pct_miss_case_1990, ch2_pct_miss_case_1981, ch2_pct_miss_case_1990, ch3_pct_miss_case),
  avg_miss_rt = c(ch1_avg_miss_rt_1977, ch1_avg_miss_rt_1990, ch2_avg_miss_rt_1981, ch2_avg_miss_rt_1990, ch3_avg_miss_rt)
  )

## make viz ----
appx_d_viz <- appx_d_dat |> 
  gt(
    rowname_col = "st_yr",
    groupname_col = "chp_mod"
    ) |> 
  tab_header(
    title = md("**Summary of Missingness**"),
    subtitle = md("Average missing data rate suggests $m = 5$ is sufficient")
    ) |> 
  cols_label(
    pct_miss_case = md("*% Incomplete Cases*"),
    avg_miss_rt = md("*Avg. Missing Data Rate*")
    ) |> 
  tab_style(
    style = list(
      cell_fill(color = "gray35"),
      cell_text(color = "white")
      ),
    locations = cells_row_groups()
    ) |> 
  tab_style(
    style = list(
      cell_borders(
        sides = c("left", "right"),
        style = "solid",
        color = "gray35"
        )
      ),
    locations = cells_body()
    ) |> 
  opt_table_outline() |> 
  fmt_percent(
    scale_values = FALSE,
    decimals = 2
    ) |> 
  tab_footnote(
    footnote = "Gives the proportion of country-year observations possessing at least one missing value.",
    locations = cells_column_labels(columns = pct_miss_case)
    ) |> 
  tab_footnote(
    footnote = md("Gives the proportion of values among the covariates, $X$, that are missing. These figures center on $X$ because its elements are theoretically eligible for imputation---unlike the treatments, fixed effects, and outcome, which are complete by design. Hence, ignoring structurally complete variables, these data provide a \"conservative\" picture of missingness."),
    locations = cells_column_labels(columns = avg_miss_rt)
    )

# save ----
ggsave(
  method_flow_viz,
  width = 1200,
  height = 1740,
  units = "px",
  file = here("visualizations/memos/method_flow_viz.png")
  )
ggsave(
  preproc_flow_viz,
  width = 1787,
  height = 2587,
  units = "px",
  file = here("visualizations/memos/preproc_flow_viz.png")
  )
ggsave(
  dml_flow_viz_1,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("visualizations/memos/dml_flow_viz_1.png")
  )
ggsave(
  dml_flow_viz_2,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("visualizations/memos/dml_flow_viz_2.png")
  )
ggsave(
  pool_flow_viz,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("visualizations/memos/pool_flow_viz.png")
  )

gtsave(
  appx_a_viz,
  zoom = 5,
  filename = here("visualizations/memos/appx_a_viz.png")
  )

gtsave(
  appx_b_viz,
  zoom = 5,
  filename = here("visualizations/memos/appx_b_viz.png")
  )

gtsave(
  appx_c_viz,
  zoom = 5,
  filename = here("visualizations/memos/appx_c_viz.png")
  )

gtsave(
  appx_d_viz,
  zoom = 5,
  filename = here("visualizations/memos/appx_d_viz.png")
  )
