library(tidyverse)
library(igraph)
library(ggraph)

# make flowcharts ----
## preproc to lags ----
flowchart_tbl_1 <- tibble(
  from = c(
    "P", "P", "P", "P", "P", "M1", "M1", "M1", "M1", "M1", "M1", "M1", "M1", "M2", "M2", "M2", "M2", "M2", "M2", "M2", "M2", "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M4", "M4", "M4", "M4", "M4", "M4", "M4", "M4", "M5", "M5", "M5", "M5", "M5", "M5", "M5", "M5"
    ),
  to = c(
    "M1", "M2", "M3", "M4", "M5", "M1L1", "M1L2", "M1L3", "M1L4", "M1L5", "M1L6", "M1L7", "M1L8", "M2L1", "M2L2", "M2L3", "M2L4", "M2L5", "M2L6", "M2L7", "M2L8", "M3L1", "M3L2", "M3L3", "M3L4", "M3L5", "M3L6", "M3L7", "M3L8", "M4L1", "M4L2", "M4L3", "M4L4", "M4L5", "M4L6", "M4L7", "M4L8", "M5L1", "M5L2", "M5L3", "M5L4", "M5L5", "M5L6", "M5L7", "M5L8"
    )
  )

g1 <- graph_from_data_frame(flowchart_tbl_1)
coords1 <- layout_as_tree(g1, root = "P")
ggraph(g1, layout = coords1) +
  geom_edge_link() +
  geom_node_label(aes(label = name))

## lag (indv.) to final est. ----
flowchart_tbl_2 <- tibble(
  from = c(
    "L", "L", "L", "R1", "R1", "R1", "R1", "R1", "R2", "R2", "R2", "R2", "R2", "R3", "R3", "R3", "R3", "R3", "R1F1", "R1F2", "R1F3", "R1F4", "R1F5", "R2F1", "R2F2", "R2F3", "R2F4", "R2F5", "R3F1", "R3F2", "R3F3", "R3F4", "R3F5", "E1", "E2", "E3"
    ),
  to = c(
    "R1", "R2", "R3", "R1F1", "R1F2", "R1F3", "R1F4", "R1F5", "R2F1", "R2F2", "R2F3", "R2F4", "R2F5", "R3F1", "R3F2", "R3F3", "R3F4", "R3F5", "E1", "E1", "E1", "E1", "E1", "E2", "E2", "E2", "E2", "E2", "E3", "E3", "E3", "E3", "E3", "E", "E", "E"
    )
  )

g2 <- graph_from_data_frame(flowchart_tbl_2)
coords2 <- layout_as_tree(g2, root = "L")
coords2[which(V(g2)$name == "E1"), ] <- coords2[which(V(g2)$name == "R1F3"), ] + c(0, -1)
coords2[which(V(g2)$name == "E2"), ] <- coords2[which(V(g2)$name == "R2F3"), ] + c(0, -1)
coords2[which(V(g2)$name == "E3"), ] <- coords2[which(V(g2)$name == "R3F3"), ] + c(0, -1)
coords2[which(V(g2)$name == "E"), ] <- coords2[which(V(g2)$name == "E2"), ] + c(0, -1)
ggraph(g2, layout = coords2) +
  geom_edge_link() +
  geom_node_label(aes(label = name))
