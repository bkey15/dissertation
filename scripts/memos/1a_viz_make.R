# Make visualizations for memos

# load packages ----
library(tidyverse)
library(here)
library(igraph)
library(ggraph)
library(latex2exp)
library(patchwork)
library(gt)
library(rnaturalearth)
library(countrycode)

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
