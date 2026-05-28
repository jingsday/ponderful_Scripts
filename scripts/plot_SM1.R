#################################
## Description: Distribution of nutrients concentrations
#################################

library(dplyr)
library(ggplot2)
library(nlme)
library(fitdistrplus)
library(bestNormalize)
library(visreg)
library(lmerTest)
library(corrplot)
library(sjPlot)
library(gratia) 
library(patchwork)
library(preprocessCore)
library(boot)
library(MuMIn)
library(lattice)

# Loading dataset and preparation
outwdir <- 'ponderful_Scripts/ponderful_OUTPUT/'
dom_landcover_nov <- read.csv('PhyChe_XY_landcover_nov.txt')
## Factors
### Country as factors
dom_landcover_nov$Country <- as.factor(dom_landcover_nov$Country)
levels(dom_landcover_nov$Country)

### Adding Depth > 200 cm Jan 20th
filtered <- dom_landcover_nov[dom_landcover_nov$Depth>=200,]
table(filtered$Country)
dom_landcover_nov <-dom_landcover_nov[dom_landcover_nov$Depth<200,]

## Bio climatic regions
dom_landcover_nov$Country <- as.character(dom_landcover_nov$Country)

### Initialize altregion as a copy of Country
dom_landcover_nov$altregion <- dom_landcover_nov$Country

dom_landcover_nov[dom_landcover_nov$Country %in% c('Belgium', 'Denmark', 'UK'), "altregion"] <- 'Atlantic'
dom_landcover_nov[dom_landcover_nov$Country %in% c('Switzerland','Germany'), "altregion"] <- 'Temperate'

### Check the changes
table(dom_landcover_nov$altregion)

### Factors and transformation 
dom_landcover_nov$altregion <- as.factor(dom_landcover_nov$altregion)
levels(dom_landcover_nov$altregion)

###Adding log_TN (and log_TP)
dom_landcover_nov$log_TN <- log(dom_landcover_nov$TN,base=10)
hist(dom_landcover_nov$log_TN)
shapiro.test(dom_landcover_nov$log_TN)

dom_landcover_nov$log_TP <- log(dom_landcover_nov$TP,base=10)
hist(dom_landcover_nov$log_TP)
shapiro.test(dom_landcover_nov$log_TP)

# Plot SM1
hist_TN <- ggplot(dom_landcover_nov, aes(x = TN)) + 
  geom_histogram(binwidth = 0.1,color = "black", alpha = 0.7) + 
  labs(
    title = "Distribution of Response Variable TN (n = 240)",
    x = "Total Nitrogen (TN, mg/L)",
    y = "Frequency"
  ) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    axis.line = element_line(linewidth = 0.5, colour = "black"), 
    panel.grid.major = element_line(linewidth = 0.5),            
    panel.grid.minor = element_blank()
  )

hist_TP <- ggplot(dom_landcover_nov, aes(x = TP)) + 
  geom_histogram(binwidth = 0.1,  color = "black", alpha = 0.7) + 
  labs(
    title = "Distribution of Response Variable TP (n = 240)",
    x = "Total Phosphorus (TP,  mg/L)",
    y = "Frequency")+
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    axis.line = element_line(linewidth = 0.5, colour = "black"), 
    panel.grid.major = element_line(linewidth = 0.5),             
    panel.grid.minor = element_blank()
  )

combined_plot <- wrap_plots(hist_TN,hist_TP)

