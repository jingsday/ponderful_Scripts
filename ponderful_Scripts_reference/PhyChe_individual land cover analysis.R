rm(list=ls())
setwd('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Analysis')

#####################################################################################################

dfqt <- read.csv('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/phy_che_oct_qt.csv', header=T, sep = ",")
dfall <- read.csv('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/PhyChe_XY.csv', header=T, sep = ",")

df2 <- merge(dfall, dfqt[,c(3,89:94,102:107)])

df = df2[df2$Country != "Uruguay",]
#####################################################################################################
#dominant landuse per pondscape

library(dplyr)

landuse_cols <- c("Aquatic_5000","Cropland_5000","Forest_5000","Pastures.and.open.nature_5000","Urban_5000")

# Sum the land use percentages across ponds per pondscape (region)
pondscape_landuse_sums <- df %>%
  group_by(Pondscape) %>%
  summarise(across(all_of(landuse_cols), sum, na.rm = TRUE))  # Sum percentages for each pondscape

# Find the dominant land use for each pondscape
pondscape_landuse_sums$dominant_landcover <- apply(pondscape_landuse_sums[, landuse_cols], 1, function(row) {
  max_col_index <- which.max(row)  # Find the index of the max value in each row
  colnames(pondscape_landuse_sums[, landuse_cols])[max_col_index]  # Return the land use column name
})

# Merge the dominant land use information back to the original data frame
# each pond will have the dominant land use of its pondscape
df <- df %>%
  left_join(pondscape_landuse_sums[, c("Pondscape", "dominant_landcover")], by = "Pondscape")

df$dominant_landcover <- factor(df$dominant_landcover,
                                levels = c("Cropland_5000","Pastures.and.open.nature_5000","Forest_5000","Aquatic_5000"),
                                labels = c("Cropland","Pastures and open nature","Forest","Aquatic"))

write.csv(df,"C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/PhyChe_XY_landcover.csv")


#####################################################################################################
#dominant landuse per pondscape scatter plot

library(ggplot2)
library(tidyr)
library(dplyr)
library(broom)


df <- df %>%
  mutate(log_TP = log10(TP),  
         log_TN = log10(TN))  

df_long <- df %>%
  pivot_longer(
    cols = c(log_TN,log_TP),  # Specify columns that start with 'log_' to be reshaped
    names_to = "nutrient",        # New column to hold nutrient types (log_TN, log_TP)
    values_to = "value"           # New column to hold the values for the nutrients
  )

# Fit linear models and extract p-values for each dominant_landcover and nutrient combination
p_values <- df_long %>%
  group_by(nutrient, dominant_landcover) %>%
  do(tidy(lm(value ~ Cropland_500.t, data = .))) %>%
  filter(term == "Cropland_500.t") %>%
  mutate(p_label = ifelse(p.value < 0.001, "p < 0.001", paste0("p = ", round(p.value, 3))))

p_values_qt <- df_long %>%
  group_by(nutrient, dominant_landcover) %>%
  do(tidy(lm(value ~ Cropland_500_qt, data = .))) %>%
  filter(term == "Cropland_500_qt") %>%
  mutate(p_label = ifelse(p.value < 0.001, "p < 0.001", paste0("p = ", round(p.value, 3))))

significant_groups <- p_values %>%
  filter(p.value < 0.05)



# Create a scatter plot for log(TP) vs cropland
pTNTPcropland <- ggplot(df_long, aes(x = Cropland_500.t, y = value)) +
  geom_point(aes(color = dominant_landcover), size = 2, alpha = 0.7) +  # Points colored by dominant land use
  geom_smooth(data = df_long %>%
                semi_join(significant_groups, by = c("nutrient", "dominant_landcover")),
              method = "lm", se = FALSE, aes(color = dominant_landcover), linetype = "solid") +  
  labs(title = "Dominant landcover in each pondscape",x = "Proportion of cropland (quantile transformation) around each pond", y = "") +
  theme_minimal(base_size = 15) +  # Minimal theme for better aesthetics
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Center title
    axis.title = element_text(size = 14),                # Axis title size
    axis.text = element_text(size = 12),                 # Axis text size
    axis.line = element_line(size = 0.5, colour = "black"),  # Add axis lines
    axis.ticks = element_line(size = 0.5),
    panel.grid.major = element_line(size = 0.5),         # Major grid line size
    panel.grid.minor = element_blank(),
    strip.placement = "outside",                        # Place facet labels outside the plot
    strip.text.y.left = element_text(angle = 90),
    legend.position = "none"
  ) +
  facet_grid(vars(nutrient), vars(dominant_landcover),
             labeller = labeller(nutrient = c("log_TN" = "log(TN)", "log_TP" = "log(TP)")),scales = "free_y",switch = "y")  +
  # Add p-values as text annotations
  geom_text(data = p_values, aes(x = Inf, y = Inf, label = p_label),
            hjust = 1.1, vjust = 1.5, size = 4, inherit.aes = FALSE, color = "gray35")

ggsave("cropland_vs_logTNTP_facet.png", plot = pTNTPcropland, width = 10, height = 7, dpi = 300)


table(df$Country,df$dominant_landcover)
