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
  spat_regfe_north = imp_dml_pool_spat_regfe_viz_dfs[["north"]],
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
          plot_base <- list_3[[treat]] |> 
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
            theme_solarized() +
            theme(
              strip.text.x = element_text(color = "#586e75"),
              axis.text = element_text(color = "#586e75")
              )
          if(treat == "n_bits"){
            plot <- plot_base +
              labs(
                x = "Lag (Years)",
                y = TeX("$\\Delta$ in HR Scores"),
                title = "Effect Estimates",
                subtitle = "Treatment: BITs In-Force (#)",
                caption = "Note: bars = CI (95%)"
                )
            gen_plots[[mod]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
            }
          else if(treat == "partner_mean_gdp"){
            plot <- plot_base +
              labs(
                x = "Lag (Years)",
                y = TeX("$\\Delta$ in HR Scores"),
                title = "Effect Estimates",
                subtitle = "Treatment: Mean GDP (BIT Partners)",
                caption = "Note: bars = CI (95%)"
                )
              gen_plots[[mod]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
              }
          else{
            plot <- plot_base +
              labs(
                x = "Lag (Years)",
                y = TeX("$\\Delta$ in HR Scores"),
                title = "Effect Estimates",
                subtitle = "Treatment: Mean GDPpc (BIT Partners)",
                caption = "Note: bars = CI (95%)"
                )
            gen_plots[[mod]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
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
          if(treat == "n_bits_AND_e_polity2_x_n_bits"){
            new_labels <- c(
              "n_bits" = "BITs In-Force (#)",
              "e_polity2_x_n_bits" = "Polity * BITs In-Force (#)"
              )
            
            plot <- plot_base +
              facet_wrap(
                ~ term,
                labeller = labeller(term = new_labels)
                )
            
            gen_plots[[mod]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
            }
          else if(treat == "partner_mean_gdp_AND_e_polity2_x_partner_mean_gdp"){
            new_labels <- c(
              "partner_mean_gdp" = "Mean GDP (BIT Partners)",
              "e_polity2_x_partner_mean_gdp" = "Polity * Mean GDP (BIT Partners)"
              )
            
            plot <- plot_base +
              facet_wrap(
                ~ term,
                labeller = labeller(term = new_labels)
                )
            
            gen_plots[[mod]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
            }
          else{
            new_labels <- c(
              "partner_mean_gdppc" = "Mean GDPpc (BIT Partners)",
              "e_polity2_x_partner_mean_gdppc" = "Polity * Mean GDPpc (BIT Partners)"
              )
            
            plot <- plot_base +
              facet_wrap(
                ~ term,
                labeller = labeller(term = new_labels)
                )
            
            gen_plots[[mod]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
          }
        }
      }
    }
  }
}

## north/south ----
ns_plots <- list()
mod_type <- names(ns_dfs)

for(type in mod_type){
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
          plot_base <- list_3[[treat]] |> 
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
            theme_solarized() +
            theme(
              strip.text.x = element_text(color = "#586e75"),
              axis.text = element_text(color = "#586e75")
              )
          
          if(treat == "n_ems"){
          plot <- plot_base +
            labs(
              x = "Lag (Years)",
              y = TeX("$\\Delta$ in HR Scores"),
              title = "Effect Estimates",
              subtitle = "Treatment: Econ. Sanctions (#)",
              caption = "Note: bars = CI (95%)"
              )
          
          imp_dml_pool_spat_regfe_plots[[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
        }
        else{
          plot <- plot_base +
            labs(
              x = "Lag (Years)",
              y = TeX("$\\Delta$ in HR Scores"),
              title = "Effect Estimates",
              subtitle = "Treatment: Econ. Sanction (Y/N)",
              caption = "Note: bars = CI (95%)"
              )
          
          imp_dml_pool_spat_regfe_plots[[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
        }
      }
    }
  }
  else{
    list_1 <- imp_dml_pool_spat_regfe_viz_dfs[[stat]]
    start_yrs <- names(list_1)
    for(year in start_yrs){
      list_2 <- list_1[[year]]
      treat_names <- names(list_2)
      for(treat in treat_names){
        plot_base <- list_2[[treat]] |> 
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
            subtitle = "Treatment + Interaction Term",
            caption = "Note: bars = CI (95%)"
            ) +
          theme_solarized() +
          theme(
            strip.background = element_blank(),
            strip.text.x = element_text(color = "#586e75"),
            axis.text = element_text(color = "#586e75")
            )
        
        if(treat == "n_ems_AND_v2x_polyarchy_x_n_ems"){
          new_labels <- c(
            "n_ems" = "Econ. Sanctions (#)",
            "v2x_polyarchy_x_n_ems" = "Polyarchy * Econ. Sanctions (#)"
            )
          
          plot <- plot_base +
            facet_wrap(
              ~ term,
              labeller = labeller(term = new_labels)
              )
          
          imp_dml_pool_spat_regfe_plots[[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
          
        }
        else{
          new_labels <- c(
            "any_inforce1" = "Econ. Sanction (Y/N)",
            "v2x_polyarchy_x_any_inforce" = "Polyarchy * Econ. Sanction (Y/N)"
            )
          
          plot <- plot_base +
            facet_wrap(
              ~ term,
              labeller = labeller(term = new_labels)
              )
          
          imp_dml_pool_spat_regfe_plots[[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- plot
        }
      }
    }
  }
  }
}

# reorganize plots ----

# save ----
imp_dml_pool_2fe_plots |> 
  save(file = here("visualizations/ch3/imp_dml_pool_2fe_plots.rda"))
imp_dml_pool_spat_regfe_plots |> 
  save(file = here("visualizations/ch3/imp_dml_pool_spat_regfe_plots.rda"))
