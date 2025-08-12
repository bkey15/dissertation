# load packages ----
library(tidyverse)
library(here)
library(knitr)
library(ggthemes)
library(latex2exp)

# load data ----
load(here("data/ch2/viz_prep/imp_dml_pool_2fe_viz_dfs.rda"))
load(here("data/ch2/viz_prep/imp_dml_pool_spat_regfe_viz_dfs.rda"))

# reorganize data ----
## note: doing this to simplify for-loops
gen_dfs <- list(
  "2fe" = imp_dml_pool_2fe_viz_dfs[["general"]],
  spat_regfe = imp_dml_pool_spat_regfe_viz_dfs[["general"]]
  )

ns_dfs <- list(
  "2fe_south" = imp_dml_pool_2fe_viz_dfs[["south"]],
  "2fe_north" = imp_dml_pool_2fe_viz_dfs[["north"]],
  spat_regfe_south = imp_dml_pool_spat_regfe_viz_dfs[["south"]],
  spat_regfe_north = imp_dml_pool_spat_regfe_viz_dfs[["north"]]
  )

# get new labels ----
## for facet wraps
new_labels <- c(
  "n_bits" = "BITs In-Force (#)",
  "e_polity2_x_n_bits" = "Polity * BITs In-Force (#)",
  "partner_mean_gdp" = "Mean GDP (BIT Partners)",
  "e_polity2_x_partner_mean_gdp" = "Polity * Mean GDP (BIT Partners)",
  "partner_mean_gdppc" = "Mean GDPpc (BIT Partners)",
  "e_polity2_x_partner_mean_gdppc" = "Polity * Mean GDPpc (BIT Partners)",
  "ss_n_bits" = "Global South BITs In-Force (#)",
  "nn_n_bits" = "Global North BITs In-Force (#)",
  "ns_n_bits" = "North-South BITs In-Force (#)",
  "ss_partner_mean_gdp" = "Mean GDP (South BIT Partners)",
  "nn_partner_mean_gdp" = "Mean GDP (North BIT Partners)",
  "ns_partner_mean_gdp" = "Mean GDP (N-S BIT Partners)",
  "ss_partner_mean_gdppc" = "Mean GDPpc (South BIT Partners)",
  "nn_partner_mean_gdppc" = "Mean GDPpc (North BIT Partners)",
  "ns_partner_mean_gdppc" = "Mean GDPpc (N-S BIT Partners)",
  "e_polity2_x_ss_n_bits" = "Polity * Global South BITs In-Force (#)",
  "e_polity2_x_nn_n_bits" = "Polity * Global North BITs In-Force (#)",
  "e_polity2_x_ns_n_bits" = "Polity * North-South BITs In-Force (#)",
  "e_polity2_x_ss_partner_mean_gdp" = "Polity * Mean GDP (S BIT Partners)",
  "e_polity2_x_nn_partner_mean_gdp" = "Polity * Mean GDP (N BIT Partners)",
  "e_polity2_x_ns_partner_mean_gdp" = "Polity * Mean GDP (N-S BIT Partners)",
  "e_polity2_x_ss_partner_mean_gdppc" = "Polity * Mean GDPpc (S BIT Partners)",
  "e_polity2_x_nn_partner_mean_gdppc" = "Polity * Mean GDPpc (N BIT Partners)",
  "e_polity2_x_ns_partner_mean_gdppc" = "Polity * Mean GDPpc (N-S BIT Partners)"
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
imp_dml_pool_2fe_plots <- list()
imp_dml_pool_spat_regfe_plots <- list()
plot_type <- c(names(gen_plots), names(ns_plots))

for(type in plot_type){
  if(str_detect(type, "2fe")){
    list_1 <- gen_plots[[type]]
    list_2 <- ns_plots[[type]]
    if(type == "2fe"){
      imp_dml_pool_2fe_plots[["general"]] <- list_1
      }
    else if(str_detect(type, "south")){
      imp_dml_pool_2fe_plots[["south"]] <- list_2
      }
    else{
      imp_dml_pool_2fe_plots[["north"]] <- list_2
    }
  }
  else{
    list_1 <- gen_plots[[type]]
    list_2 <- ns_plots[[type]]
    if(type == "spat_regfe"){
      imp_dml_pool_spat_regfe_plots[["general"]] <- list_1
      }
    else if(str_detect(type, "south")){
      imp_dml_pool_spat_regfe_plots[["south"]] <- list_2
      }
    else{
      imp_dml_pool_spat_regfe_plots[["north"]] <- list_2
    }
  }
}

# save ----
imp_dml_pool_2fe_plots |> 
  save(file = here("visualizations/ch2/imp_dml_pool_2fe_plots.rda"))
imp_dml_pool_spat_regfe_plots |> 
  save(file = here("visualizations/ch2/imp_dml_pool_spat_regfe_plots.rda"))
