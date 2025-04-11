library(ggplot2)

x_range <- c(-3.5, 3.5)
y_range <- c(-1, 1)  # Similarly for y-range if needed

#Paritial plots
library(visreg)
common_theme <- theme(
  axis.title = element_text(size = 40, face = 'bold'),  # Set axis title size and bold
  axis.text = element_text(size = 36),                 # Set axis text size
  panel.background = element_rect(fill = "white"),     # Set background color to white
  panel.grid.major = element_line(color = "grey"),     # Set grid lines to grey
  panel.grid.minor = element_line(color = "lightgrey") # Optional: minor grid lines
)

TN_glmm_step_depth <- lmer(
  formula = log_TN ~ season_col + Cropland_500.s + Pond_dries_cat + Depth.s + sum_P.s 
  + Strat + Animals_cont.s + (1 | PondCode) + (1 | Country:Pondscape) + (1 | Country) 
  + season_col:Pond_dries_cat,data=df_modelling, na.action = "na.fail")

plots_TN <- visreg(TN_glmm_step_depth, gg = TRUE,partial=TRUE,scale='response')
plots_TN <- plots_TN[c(2, 4, 5, 7)]


plot_labels <- list(
  list(y = "TN\nf(Cropland 500m)", x = "Cropland (500m)"),
  list(y = "TN\nf(Depth)", x = "Depth"),
  list(y = "TN\nf(Annual precipitation)", x = "Annual precipitation"),
  list(y = "TN\nf(Livestock index)", x = "Livestock index"),
  list(y = "TN\nf(Hydroperiod Regime)", x = "Season")
  
)

plots_TN_minimal <- lapply(seq_along(plots_TN), function(i) {
  plots_TN[[i]] + 
    common_theme +  # Add common theme
    coord_cartesian(xlim = x_range, ylim = y_range) +  # Standardize x and y axis ranges
    geom_point(size = 4, alpha = 0.6) +  # Adjust point size and transparency
    labs(  # Add specific labels for each plot
      y = plot_labels[[i]]$y,
      x = plot_labels[[i]]$x
    )
})
plots_TN_minimal
TP_glmm_step_area <- lmer(log_TP ~ season_col + Cropland_500.s + Area.s + Depth.s +
                            Animals_cont.s + (1 | PondCode) + (1 | Country:Pondscape) +
                            (1 | Country) + season_col:Area.s, data = df_modelling, na.action = "na.fail")


plots_TP <- visreg(TP_glmm_step_area, gg = TRUE,partial=TRUE,scale='response')
plots_TP <- plots_TP[c(2,4,3, 5)]
plots_TP

# Define labels for each plot
plot_labels <- list(
  list(y = "TP\nf(Cropland 500m)", x = "Cropland (500m)"),
  list(y = "TP\nf(Depth)", x = "Depth"),
  list(y = "TP\nf(Area)", x = "Area"),
  list(y = "TP\nf(Livestock index)", x = "Livestock index")
)


x_range <- c(-3.5, 3.5)
y_range <- c(-2, 0.5) 

plots_TP_minimal <- lapply(seq_along(plots_TP), function(i) {
  plots_TP[[i]] + 
    common_theme +  # Add common theme
    coord_cartesian(xlim = x_range, ylim = y_range) +  # Standardize x and y axis ranges
    geom_point(size = 4, alpha = 0.6) +  # Adjust point size and transparency
    labs(  # Add specific labels for each plot
      y = plot_labels[[i]]$y,
      x = plot_labels[[i]]$x
    )
})




all_plots <- c( plots_TN_minimal,plots_TP_minimal)
all_plots <- c(plots_TN_minimal,list(season_pond_TN), plots_TP_minimal )

all_plots

full_combined_plot <- wrap_plots(all_plots,ncol=4 )
full_combined_plot <- full_combined_plot +
  theme(plot.margin = margin(30, 30, 30, 30)) 

# Wrap it like this to keep layout tidy
all_plots <- c(plots_TN_minimal[1:2], list(season_pond_TN), plots_TN_minimal[3:4])
full_combined_plot <- wrap_plots(all_plots, ncol = 4)

# Save the combined plot as a PNG file
out_dir <- '~/Desktop/check.png'
ggsave(out_dir, plot = full_combined_plot, width = 33, height = 22, dpi = 300)


###
season_pond_TN <- visreg(TN_glmm_step_depth, "season_col", by = "Pond_dries_cat",partial=T, gg = TRUE, scale = "response")

season_pond_TN <- season_pond_TN + common_theme
season_pond_TN
library(patchwork)

all_plots <- c(plots_TN_minimal, plots_TP_minimal, list(season_pond_TN))
full_combined_plot <- wrap_plots(all_plots, ncol = 4)
full_combined_plot <- full_combined_plot + 
  theme(plot.margin = margin(30, 30, 30, 30))
full_combined_plot
