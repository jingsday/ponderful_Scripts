
global_x_range <- c(-3, 3)

tp_alt<- glmer(TP~Cropland_500.s +Depth.s+bio12.s+ Natural_5.s+Animals_cont.s +Pastures.and.open.nature_500.s+
                 bio1.s + (1|altregion)+(1|altregion:Pondscape), family = Gamma(link = "log"), 
               data = model_df)
tn_alt_selected<- lmer(log_TN ~ Cropland_500.s +
                         Depth.s +
                         +bio12.s+(1|altregion)+(1|altregion:Pondscape), 
                       data = model_df)

# Generate visual regression plots for TN and TP models
plots_TN <- visreg(tn_alt_selected, gg = TRUE,partial=TRUE,scale='response')

plots_TP <- visreg(tp_alt, gg = TRUE,partial=TRUE,scale='linear')
visreg(TN_full_model, gg = TRUE)
visreg(TN_full_model, gg = TRUE,partial=TRUE,scale='response')
tab_model(TN_full_model)

plots_TN_minimal <- lapply(plots_TN, function(plot) { 
  plot + 
    theme(
      axis.title = element_text(size = 40, face = 'bold'),  # Set axis title size and bold
      axis.text = element_text(size = 36),                 # Set axis text size
      panel.background = element_rect(fill = "white"),     # Set background color to white
      panel.grid.major = element_line(color = "grey"),     # Set grid lines to grey
      panel.grid.minor = element_line(color = "lightgrey") # Optional: minor grid lines
    ) + 
    coord_cartesian(xlim = global_x_range)+
    geom_point(size = 3)# Standardize x-axis
})


plots_TP_minimal <- lapply(plots_TP, function(plot) { 
  plot + 
    theme(
      axis.title = element_text(size = 40, face = 'bold'),  # Set axis title size and bold
      axis.text = element_text(size = 36),                 # Set axis text size
      panel.background = element_rect(fill = "white"),     # Set background color to white
      panel.grid.major = element_line(color = "grey"),     # Set grid lines to grey
      panel.grid.minor = element_line(color = "lightgrey") # Optional: minor grid lines
    ) + 
    coord_cartesian(xlim = global_x_range)+ # Standardize x-axis
    geom_point(size = 3)# Standardize x-axis
})

plots_TN_minimal[[2]]


plots_TN_minimal[[1]] <- plots_TN_minimal[[1]] + 
  labs(
    y = "TN\nf(Cropland 500m)",  
    x = "Cropland (500m)"  
  )


plots_TN_minimal[[2]] <- plots_TN_minimal[[2]] + 
  labs(
    y = "TN\nf(Depth)",  # Replace with your desired label for the x-axis
    x = "Depth"   # Replace with your desired label for the y-axis
  )

plots_TN_minimal[[3]] <- plots_TN_minimal[[3]] + 
  labs(
    y = "TN\nf(Annual precipitation)",  # Replace with your desired label for the x-axis
    x = "Annual precipitation"   # Replace with your desired label for the y-axis
  )


plots_TP_minimal <- lapply(plots_TP, function(plot) {
  plot + 
    theme(
      axis.title = element_text(size = 40, face = 'bold'),  # Set axis title size and bold
      axis.text = element_text(size = 36),                 # Set axis text size
      panel.background = element_rect(fill = "white"),     # Set background color to white
      panel.grid.major = element_line(color = "grey"),     # Set grid lines to grey
      panel.grid.minor = element_line(color = "lightgrey") # Optional: minor grid lines
    ) + 
    coord_cartesian(xlim = global_x_range)+
    geom_point(size = 4,)# Standardize x-axis
})

plots_TP_minimal[[6]]

plots_TP_minimal[[1]] <- plots_TP_minimal[[1]] + 
  labs(
    y = "TP\nf(Cropland 500m)",  
    x = "Cropland (500m)"  
  )
plots_TP_minimal[[2]] <- plots_TP_minimal[[2]] + 
  labs(
    y = "TP\nf(Depth)",  # Replace with your desired label for the x-axis
    x = "Depth"   # Replace with your desired label for the y-axis
  )

plots_TP_minimal[[3]] <- plots_TP_minimal[[3]] + 
  labs(
    y = "TP\nf(Annual precipitation)",  # Replace with your desired label for the x-axis
    x = "Annual precipitation"   # Replace with your desired label for the y-axis
  )




plots_TP_minimal[[4]] <- plots_TP_minimal[[4]] + 
  labs(
    y = "TP\nf(Natural 500m)",  # Replace with your desired label for the x-axis
    x = "Natural (500m)"   # Replace with your desired label for the y-axis
  )


plots_TP_minimal[[6]] <- plots_TP_minimal[[6]] + 
  labs(
    y = "TP\nf(Pasture and open nature 500m)",  # Replace with your desired label for the x-axis
    x = "Pasture and open nature (500m)"   # Replace with your desired label for the y-axis
  )

plots_TP_minimal[[5]] <- plots_TP_minimal[[5]] + 
  labs(
    y = "TP\nf(Livestock index)",  # Replace with your desired label for the x-axis
    x = "Livestock index"   # Replace with your desired label for the y-axis
  )

plots_TP_minimal[[7]] <- plots_TP_minimal[[7]] + 
  labs(
    y = "TP\nf(Annual mean temperature)",  # Replace with your desired label for the x-axis
    x = "Annual mean temperature"   # Replace with your desired label for the y-axis
  )



# Combine all plots from TP and TN
all_plots <- c( plots_TN_minimal,plots_TP_minimal)
all_plots
# Wrap the plots into a single layout with 3 columns


plots_TN_group <- wrap_plots(plots_TN_minimal, ncol = 3) 


plots_TP_group <- wrap_plots(plots_TP_minimal, ncol = 3)

# Combine both groups into a single plot
full_combined_plot <- (plots_TN_group / plots_TP_group)
full_combined_plot <- wrap_plots(
  plots_TN_group, 
  plots_TP_group, 
  ncol = 1,  # Stack groups vertically
  heights = c(1, 1)  # Set equal heights for both groups
) +
  theme(plot.margin = margin(30, 30, 30, 30))  # Adjust margins

full_combined_plot
# Save the combined plot as a PNG file
output_path <- paste0(outwdir, "full_glmm_coefficients_comparison.png")
ggsave(output_path, plot = full_combined_plot, width = 33, height = 34, dpi = 300)

# #
# # # Create the full combined plot with 3 columns per row


plots_TP_minimal
plots_TN_minimal <- lapply(plots_TN, function(plot) plot + theme_minimal())
combined_plot_TN <- wrap_plots(plots_TN_minimal,ncol=3,nrow=1,heights=3)

combined_plot_TP <- wrap_plots(plots_TP_minimal,ncol=3,nrow=1,heights = 9)

combined_plot_TN
plots_TN_group

plots_TP_group
# Save the combined plot as a PNG file
output_path <- paste0(outwdir, "TP_coefficients_comparison.svg")
ggsave(output_path, plot = combined_plot_TP, width = 12, height = 3, dpi = 300)
#
# # Save the combined plot as a PNG file
output_path <- paste0(outwdir, "TN_coefficients_comparison.svg")
ggsave(output_path, plot = combined_plot_TN, width = 12, height = 3, dpi = 300)

```