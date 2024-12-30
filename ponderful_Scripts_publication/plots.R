global_x_range <- c(-3, 3)

tp_alt<- glmer(TP~Cropland_500.s +Depth.s+bio12.s+ Natural_5.s+Animals_cont.s + bio1.s +Pastures.and.open.nature_500.s+
                 (1|altregion)+(1|altregion:Pondscape), family = Gamma(link = "log"), 
               data = model_df)

tn_alt_selected<- lmer(log_TN ~ Cropland_500.s +
                         Depth.s +
                         +bio12.s+(1|altregion)+(1|altregion:Pondscape), 
                       data = model_df)
plots_TN <- visreg(tn_alt_selected, gg = TRUE,partial=TRUE,scale='response')

plots_TP <- visreg(tp_alt, gg = TRUE,partial=TRUE,scale='linear')

common_theme <- theme(
  axis.title = element_text(size = 40, face = 'bold'),  # Set axis title size and bold
  axis.text = element_text(size = 36),                 # Set axis text size
  panel.background = element_rect(fill = "white"),     # Set background color to white
  panel.grid.major = element_line(color = "grey"),     # Set grid lines to grey
  panel.grid.minor = element_line(color = "lightgrey") # Optional: minor grid lines
)

# Define labels for each plot
plot_labels <- list(
  list(y = "TN\nf(Cropland 500m)", x = "Cropland (500m)"),
  list(y = "TN\nf(Depth)", x = "Depth"),
  list(y = "TN\nf(Annual precipitation)", x = "Annual precipitation"),
  list(y = "TP\nf(Natural 500m)", x = "Natural (500m)"),
  list(y = "TP\nf(Livestock index)", x = "Livestock index"),
  list(y = "TP\nf(Annual mean temperature)", x = "Annual mean temperature"),
  list(y = "TP\nf(Pasture and open nature 500m)", x = "Pasture and open nature (500m)"))

# Apply the common theme, coordinate adjustment, and labels
plots_TP_minimal <- lapply(seq_along(plots_TP), function(i) {
  plots_TP[[i]] +
    common_theme +  # Add common theme
    coord_cartesian(xlim = global_x_range) +  # Standardize x-axis range
    geom_point(size = 4, alpha = 0.6) +  # Adjust point size and transparency
    labs(  # Add specific labels
      y = plot_labels[[i]]$y,
      x = plot_labels[[i]]$x
    )
})

# Inspect a specific plot if needed
plots_TP_minimal

plots_TP_minimal <- plots_TP_minimal[1:6]
# Apply the common theme, coordinate adjustment, and labels
plots_TN_minimal <- lapply(seq_along(plots_TN), function(i) {
  plots_TN[[i]] +
    common_theme +  # Add common theme
    coord_cartesian(xlim = global_x_range) +  # Standardize x-axis range
    geom_point(size = 4, alpha = 0.6) +  # Adjust point size and transparency
    labs(  # Add specific labels
      y = plot_labels[[i]]$y,
      x = plot_labels[[i]]$x
    )
})
plots_TN_minimal <-plots_TN_minimal[1:2]
plots_TN_minimal



all_plots <- c( plots_TN_minimal,plots_TP_minimal)
all_plots

full_combined_plot <- wrap_plots(all_plots,byrow = )
full_combined_plot <- full_combined_plot +
  theme(plot.margin = margin(30, 30, 30, 30)) 


empty_plot <- ggplot() + theme_void()  # Create an empty plot
all_plots <- c(plots_TN_minimal,list(empty_plot),plots_TP_minimal)  # Add empty plot to adjust alignment
all_plots
# Arrange the plots into 3 rows (2, 3, 3 layout)
full_combined_plot <- wrap_plots(all_plots, nrow = 3)

# Add a theme adjustment
full_combined_plot <- full_combined_plot + 
  theme(plot.margin = margin(30, 30, 30, 30))

# Display the final combined plot
full_combined_plot

# Save the combined plot as a PNG file
output_path <- paste0(outwdir, "full_glmm_coefficients_comparison.png")
ggsave(output_path, plot = full_combined_plot, width = 33, height = 22, dpi = 300)
