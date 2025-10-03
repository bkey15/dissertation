# load packages ----
library(tidyverse)
library(here)
library(knitr)
library(ggthemes)
library(latex2exp)

# load data ----
load(here("data/ch1/viz_prep/regularize/imp_dml_pool_viz_dfs.rda"))

# reorganize data ----
## note: doing this to simplify for-loops
gen_dfs <- list(
  "2fe" = imp_dml_pool_viz_dfs[["2fe"]][["general"]],
  spat_regfe = imp_dml_pool_viz_dfs[["spat_regfe"]][["general"]]
  )

ns_dfs <- list(
  "2fe_south" = imp_dml_pool_viz_dfs[["2fe"]][["south"]],
  "2fe_north" = imp_dml_pool_viz_dfs[["2fe"]][["north"]],
  spat_regfe_south = imp_dml_pool_viz_dfs[["spat_regfe"]][["south"]],
  spat_regfe_north = imp_dml_pool_viz_dfs[["spat_regfe"]][["north"]]
  )

# get new labels ----
## for facet wraps
new_labels <- c(
  "lech_hr_mean" = "Mean L Score (no wt.)",
  "v2x_polyarchy_x_lech_hr_mean" = "Polyarchy * Mean L Score (no wt.)",
  "lech_hr_gdp_mean" = "Mean L Score (wt. by Partner GDP)",
  "v2x_polyarchy_x_lech_hr_gdp_mean" = "Polyarchy * Mean L Score (wt. by Partner GDP)",
  "lech_hr_gdppc_mean" = "Mean L Score (wt. by Partner GDPpc)",
  "v2x_polyarchy_x_lech_hr_gdppc_mean" = "Polyarchy * Mean L Score (wt. by Partner GDPpc)",
  "ss_lech_hr_mean" = "Mean L Score, Global South PTAs (no wt.)",
  "nn_lech_hr_mean" = "Mean L Score, Global North PTAs (no wt.)",
  "ns_lech_hr_mean" = "Mean L Score, N-S PTAs (no wt.)",
  "ss_lech_hr_gdp_mean" = "Mean L Score, South PTAs (wt. by Partner GDP)",
  "nn_lech_hr_gdp_mean" = "Mean L Score, North PTAs (wt. by Partner GDP)",
  "ns_lech_hr_gdp_mean" = "Mean L Score, N-S PTAs (wt. by Partner GDP)",
  "ss_lech_hr_gdppc_mean" = "Mean L Score, South PTAs (wt. by Partner GDPpc)",
  "nn_lech_hr_gdppc_mean" = "Mean L Score, North PTAs (wt. by Partner GDPpc)",
  "ns_lech_hr_gdppc_mean" = "Mean L Score, N-S PTAs (wt. by Partner GDPpc)",
  "v2x_polyarchy_x_ss_lech_hr_mean" = "Polyarchy * Mean L Score, Global South PTAs (no wt.)",
  "v2x_polyarchy_x_nn_lech_hr_mean" = "Polyarchy * Mean L Score, Global North PTAs (no wt.)",
  "v2x_polyarchy_x_ns_lech_hr_mean" = "Polyarchy * Mean L Score, N-S PTAs (no wt.)",
  "v2x_polyarchy_x_ss_lech_hr_gdp_mean" = "Polyarchy * Mean L Score, South PTAs (wt. by Partner GDP)",
  "v2x_polyarchy_x_nn_lech_hr_gdp_mean" = "Polyarchy * Mean L Score, North PTAs (wt. by Partner GDP)",
  "v2x_polyarchy_x_ns_lech_hr_gdp_mean" = "Polyarchy * Mean L Score, N-S PTAs (wt. by Partner GDP)",
  "v2x_polyarchy_x_ss_lech_hr_gdppc_mean" = "Polyarchy * Mean L Score, South PTAs (wt. by Partner GDPpc)",
  "v2x_polyarchy_x_nn_lech_hr_gdppc_mean" = "Polyarchy * Mean L Score, North PTAs (wt. by Partner GDPpc)",
  "v2x_polyarchy_x_ns_lech_hr_gdppc_mean" = "Polyarchy * Mean L Score, N-S PTAs (wt. by Partner GDPpc)"
  )

# make plots ----
## general ----
gen_plots <- list()
mod_type <- names(gen_dfs)

for(mod in mod_type){
  list_1 <- gen_dfs[[mod]]
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
          
          gen_plots[[mod]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
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
          
          gen_plots[[mod]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
        }
      }
    }
  }
}

## north/south ----
ns_plots <- list()
mod_type <- names(ns_dfs)

for(mod in mod_type){
  list_1 <- ns_dfs[[mod]]
  interact_stat <- names(list_1)
  for(stat in interact_stat){
    if(stat == "no_interactions"){
      list_2 <- list_1[[stat]]
      start_yrs <- names(list_2)
      for(year in start_yrs){
        list_3 <- list_2[[year]]
        treat_names <- names(list_3)
        for(treat in treat_names){
          if(str_detect(treat, "AND", negate = TRUE)){
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
            
            ns_plots[[mod]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
          }
          else{
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
                subtitle = "Treatments (Intra- & Interregional)",
                caption = "Note: bars = CI (95%)"
                ) +
              theme_solarized() +
              theme(
                strip.background = element_blank(),
                strip.text.x = element_text(color = "#586e75"),
                axis.text = element_text(color = "#586e75")
                )
            
            ns_plots[[mod]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
          }
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
          plot_base <- list_3[[treat]] |> 
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
              caption = "Note: bars = CI (95%)"
              ) +
            theme_solarized() +
            theme(
              strip.background = element_blank(),
              strip.text.x = element_text(color = "#586e75"),
              axis.text = element_text(color = "#586e75")
              )
          if(str_count(treat, "AND") == 1){
            plot <- plot_base +
              labs(subtitle = "Treatment + Interaction Term")
            ns_plots[[mod]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
          }
          else{
            plot <- plot_base +
              labs(subtitle = "Treatments + Interaction Terms")
            ns_plots[[mod]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
          }
        }
      }
    }
  }
}

# reorganize plots ----
imp_dml_pool_plots <- list()
plot_type <- c(names(gen_plots), names(ns_plots))

for(type in plot_type){
  if(str_detect(type, "2fe")){
    list_1 <- gen_plots[[type]]
    list_2 <- ns_plots[[type]]
    if(type == "2fe"){
      imp_dml_pool_plots[["2fe"]][["general"]] <- list_1
    }
    else if(str_detect(type, "south")){
      imp_dml_pool_plots[["2fe"]][["south"]] <- list_2
    }
    else{
      imp_dml_pool_plots[["2fe"]][["north"]] <- list_2
    }
  }
  else{
    list_1 <- gen_plots[[type]]
    list_2 <- ns_plots[[type]]
    if(type == "spat_regfe"){
      imp_dml_pool_plots[["spat_regfe"]][["general"]] <- list_1
    }
    else if(str_detect(type, "south")){
      imp_dml_pool_plots[["spat_regfe"]][["south"]] <- list_2
    }
    else{
      imp_dml_pool_plots[["spat_regfe"]][["north"]] <- list_2
    }
  }
}

# save ----
imp_dml_pool_plots |> 
  save(file = here("visualizations/ch1/regularize/imp_dml_pool_plots.rda"))
