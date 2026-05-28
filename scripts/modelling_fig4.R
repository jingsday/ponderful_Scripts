#################################
## Description: Generate modelling results and figures for visualisation
#################################


####################################################################################################################################
# Modelling
####################################################################################################################################
library(ggplot2)
library(visreg)
library(patchwork)
library(dplyr)

model_df <- read.csv('FINAL_ponderful_hydro_seasonality_glmm_input.csv')
model_df <- model_df[!is.na(model_df$Area),]
model_df <- model_df[!is.na(model_df$TP),] #721 

table(!is.na(model_df$post_Emerse_pond)) #missing one Turkish pond
model_df[is.na(model_df$Country),]


model_df <- model_df %>%
  mutate(
    Pond_dries_cat = case_when(
      Pond_dries == 0 ~ "Permanent",
      Pond_dries == 1 ~ "Semi-permanent",
      Pond_dries == 2 ~ "Temporary",
      TRUE ~ NA_character_  
    )
  )

df_modelling <- model_df[,c('PondCode', "Pondscape" ,"season_col","year" ,"altregion" ,"Country"  ,"Pond_dries_cat"  ,
                            "log_TN","log_TP",'Animals_cont.s','Area.s',
                            'Depth.s',"Aquatic_500.s",'post_Emerse_pond.s',
                            "Cropland_500.s","Forest_500.s","Pastures.and.open.nature_500.s" ,
                            "Urban_500.s", "natural_5_filled.s","MeanT.s","sum_P.s","Strat")]

df_modelling <- na.omit(df_modelling) 
df_modelling$Strat <- as.factor(df_modelling$Strat)
df_modelling$year <- as.factor(df_modelling$year)
df_modelling$season_col <- as.factor(df_modelling$season_col)
levels(df_modelling$season_col)
df_modelling$Pond_dries_cat <- as.factor(df_modelling$Pond_dries_cat)
df_modelling$Strat <- factor(df_modelling$Strat, levels = c(0, 1), labels = c("Absence", "Existence"))

####################################################################################################################################
# Modelling TN
####################################################################################################################################

TN_glmm_step_depth <- lmer(
  formula = log_TN ~ season_col + Cropland_500.s + Pond_dries_cat + Depth.s + sum_P.s 
  + Strat + Animals_cont.s + (1 | PondCode) + (1 | Country:Pondscape) + (1 | Country) 
  + season_col:Pond_dries_cat, data=df_modelling, na.action = "na.fail")

tab_model(TN_glmm_step_depth,transform=NULL,show.intercept = TRUE)


####################################################################################################################################
# Modelling TP
####################################################################################################################################

TP_glmm_step_area <- lmer(log_TP ~ season_col + Cropland_500.s + Area.s + Depth.s +
                            Animals_cont.s + (1 | PondCode) + (1 | Country:Pondscape) +
                            (1 | Country) + season_col:Area.s, data = df_modelling, na.action = "na.fail")
tab_model(TP_glmm_step_area,transform=NULL,show.intercept = TRUE)

####################################################################################################################################
# Plotting 
####################################################################################################################################

df_modelling$season_col <- factor(
  df_modelling$season_col,
  levels = c("Spring", "Summer", "Autumn")
)

df_modelling$Pond_dries_cat <- factor(
  as.character(df_modelling$Pond_dries_cat),
  levels = c("Permanent", "Semi-permanent", "Temporary")
)

# Theme
common_theme <- theme(
  axis.title = element_text(size = 40),  # Set axis title size and bold
  axis.text = element_text(size = 36),                 # Set axis text size
  panel.background = element_rect(fill = "white"),     # Set background color to white
  panel.grid.major = element_line(color = "grey"),     # Set grid lines to grey
  panel.grid.minor = element_line(color = "lightgrey") # Optional: minor grid lines
)

plots_TN <- visreg(TN_glmm_step_depth, gg = TRUE,partial=TRUE,scale='response')

plots_TN <- plots_TN[c(2, 4, 5, 7)]
plots_TN

plot_labels <- list(
  list(y = "TN\nf(Cropland 500m)", x = "Cropland (500m)"),
  list(y = "TN\nf(Depth)", x = "Depth"),
  list(y = "TN\nf(Annual Precipitation)", x = "Annual Precipitation"),
  list(y = "TN\nf(Livestock Index)", x = "Livestock Index")
)


x_range <- c(-3.5, 3.5)
y_range <- c(-1, 1)  


plots_TN_minimal <- lapply(seq_along(plots_TN), function(i) {
  plots_TN[[i]] + 
    common_theme + 
    coord_cartesian(x_range, ylim = y_range)+
    labs(  
      y = plot_labels[[i]]$y,
      x = plot_labels[[i]]$x
    )+
    geom_point(size = 0.5) 
})

plots_TN_minimal

# Strat and interactions
vis <- visreg(TN_glmm_step_depth, "Strat", partial = TRUE, plot = FALSE)
vis_data <- vis$res  

strat_plot_TN <- ggplot(df_modelling, aes(x = Strat, y = log_TN)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black",width = 0.6) +
  geom_jitter(data = vis_data, aes(x = Strat, y = visregRes), 
              width = 0.05, alpha = 0.5,size=0.5) +
  coord_cartesian( ylim = y_range) + 
  theme_minimal() +
  labs(y = "TN\nf(Thermal Stratification)", x = "Thermal Stratification") 

strat_plot_TN
strat_TN <- strat_plot_TN+common_theme
strat_TN

plots_TN_minimal <- c(plots_TN_minimal, list(strat_TN))

all_plots_TN <- c(plots_TN_minimal)
all_plots_TN

# TP
plots_TP <- visreg(TP_glmm_step_area, gg = T,partial=TRUE,scale='response')
plots_TP <- plots_TP[c(2,4, 5)]
plots_TP

plot_labels <- list(
  list(y = "TP\nf(Cropland 500m)", x = "Cropland (500m)"),
  list(y = "TP\nf(Depth)", x = "Depth"),
  list(y = "TP\nf(Livestock Index)", x = "Livestock Index")
)


x_range <- c(-3.5, 3.5)
y_range <- c(-2, 0.5) 

plots_TP_minimal <- lapply(seq_along(plots_TP), function(i) {
  plots_TP[[i]] + 
    common_theme +  
    coord_cartesian(xlim = x_range, ylim = y_range)+
    labs( 
      y = plot_labels[[i]]$y,
      x = plot_labels[[i]]$x
    ) +
    geom_point(size = 0.5) 
})

plots_TP_minimal

# Season and interactions
vis <- visreg(TP_glmm_step_area, "season_col", partial = TRUE, plot = FALSE)
vis_data <- vis$res  

season_plot_TP <- ggplot(df_modelling, aes(x = season_col, y = log_TP)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black",width = 0.6) +
  geom_jitter(data = vis_data, aes(x = season_col, y = visregRes), 
              width = 0.05, alpha = 0.5,size=0.5) +
  coord_cartesian(ylim = y_range) + 
  
  theme_minimal() +
  labs(y = "TP\nf(Season)", x = "Season") 


season_plot_TP
season_plot_TP <- season_plot_TP+common_theme
season_plot_TP

plots_TP_minimal <- c(plots_TP_minimal, list(season_plot_TP))
all_plots_TP <- c(plots_TP_minimal)

### TN interactions
vis <- visreg(TN_glmm_step_depth,"Pond_dries_cat" ,by="season_col", partial = TRUE, plot = FALSE)
vis_data <- vis$res  
table(df_modelling$Pond_dries_cat)

y_range <- c(-1, 1)  

interact_TN <- ggplot(df_modelling, 
                      aes(x = interaction(season_col, Pond_dries_cat), 
                          y = log_TN, 
                          fill = Pond_dries_cat)) + 
  geom_boxplot(width = 2) + 
  theme(
    axis.text.x = element_blank(),              
    axis.title.x = element_blank(),             
    axis.ticks.x = element_blank(),             
    strip.text = element_text(size = 40),       
    axis.title = element_text(size = 40),       
    axis.text = element_text(size = 36),        
    panel.background = element_rect(fill = "white"),  
    panel.grid.minor = element_line(color = "lightgrey"),  
    legend.text = element_text(size = 25),      
    legend.title = element_text(size = 25),    
    legend.position = "bottom"                 
  ) + 
  scale_fill_manual(values = c(
    "Semi-permanent" = "#bdbdbd",  
    "Temporary" = "#636363",       
    "Permanent" = "#f0f0f0"        
  )) + 
  facet_wrap(~ season_col) + 
  labs(y = "TN\nf(Hydroperiod Regime by Season)") + 
  coord_cartesian(ylim = y_range)  

interact_TN

# TP interactions
visreg(TP_glmm_step_area,"Area.s" ,by="season_col", partial = TRUE, plot = T)
vis <- visreg(TP_glmm_step_area, "Area.s", by = "season_col", partial = TRUE, plot = FALSE)

x_range <- c(-3.5, 3.5)
y_range <- c(-2, 0.5) 
interact_TP <- plot(vis, gg = TRUE) + 
  labs(y = "TP\nf(Area by Season)",x=" ")+
  coord_cartesian(xlim = x_range, ylim = y_range)+
  
  theme(
    strip.text = element_text(size = 40),  
    axis.title = element_text(size = 40),  
    axis.text = element_text(size = 36),                 
    panel.background = element_rect(fill = "white"),     
    panel.grid.major = element_line(color = "grey"),     
    panel.grid.minor = element_line(color = "lightgrey") 
  )


all_plots <- c(all_plots_TN,list(interact_TN),all_plots_TP,list(interact_TP))

full_combined_plot <- wrap_plots(all_plots, ncol = 3)
full_combined_plot

full_combined_plot <- full_combined_plot +
  theme(plot.margin = margin(30, 30, 30, 30)) 

full_combined_plot

output_path <- "~/Desktop/hydro_glmm_seasonality_legends.pdf"
ggsave(output_path, plot = full_combined_plot, width = 35, height = 33, dpi = 600)

