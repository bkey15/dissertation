# load packages ----
library(tidyverse)
library(here)
library(knitr)
library(ggthemes)
library(latex2exp)

# load data ----
load(here("data/ch3/viz_prep/imp_dml_pool_viz_dfs.rda"))

# get new labels ----
## for facet wraps
new_labels <- c(
  "n_ems" = "Econ. Sanctions (#)",
  "v2x_polyarchy_x_n_ems" = "Polyarchy * Econ. Sanctions (#)",
  "any_inforce1" = "Econ. Sanction (Y/N)",
  "v2x_polyarchy_x_any_inforce" = "Polyarchy * Econ. Sanction (Y/N)"
  )

# make plots ----
imp_dml_pool_plots <- list()
eff_cat <- names(imp_dml_pool_viz_dfs)

for(cat in eff_cat){
  list_1 <- imp_dml_pool_viz_dfs[[cat]]
  interact_stat <- names(list_1)
  for(stat in interact_stat){
    if(stat == "no_interactions"){
      list_2 <- list_1[[stat]]
      start_yrs <- names(list_2)
      for(year in start_yrs){
        list_3 <- list_2[[year]]
        treat_names <- names(list_3)
        for(treat in treat_names){
          plot <- list_3[[treat]] |> 
            ggplot(
              aes(
                x = as.factor(n_lag),
                y = estimate
                )
              ) +
            geom_point() +
            geom_errorbar(
              aes(
                ymin = conf_low,
                ymax = conf_high
                ),
              width = 0.2
              ) +
            geom_hline(
              yintercept = 0,
              lty = 2
              ) +
            labs(
              x = "Lag (Years)",
              y = TeX("$\\Delta$ in HR Scores"),
              title = "Effect Estimates",
              subtitle = paste("Treatment:", new_labels[[treat]]),
              caption = "Note: bars = CI (95%)"
              ) +
            theme_solarized() +
            theme(
              strip.text.x = element_text(color = "#586e75"),
              axis.text = element_text(color = "#586e75")
              )
          
          imp_dml_pool_plots[[cat]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
        }
      }
    }
    else{
      list_2 <- list_1[[stat]]
      start_yrs <- names(list_2)
      for(year in start_yrs){
        list_3 <- list_2[[year]]
        treat_names <- names(list_3)
        for(treat in treat_names){
          plot <- list_3[[treat]] |> 
            ggplot(
              aes(
                x = as.factor(n_lag),
                y = estimate
                )
              ) +
            facet_wrap(
              ~ term,
              labeller = labeller(term = new_labels)
              ) +
            geom_point() +
            geom_errorbar(
              aes(
                ymin = conf_low,
                ymax = conf_high
                ),
              width = 0.2
              ) +
            geom_hline(
              yintercept = 0,
              lty = 2
              ) +
            labs(
              x = "Lag (Years)",
              y = TeX("$\\Delta$ in HR Scores"),
              title = "Effect Estimates",
              subtitle = "Treatment + Interaction Term",
              caption = "Note: bars = CI (95%)"
              ) +
            theme_solarized() +
            theme(
              strip.background = element_blank(),
              strip.text.x = element_text(color = "#586e75"),
              axis.text = element_text(color = "#586e75")
              )
          
          imp_dml_pool_plots[[cat]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
        }
      }
    }
  }
}

# save ----
imp_dml_pool_plots |> 
  save(file = here("visualizations/ch3/imp_dml_pool_plots.rda"))
