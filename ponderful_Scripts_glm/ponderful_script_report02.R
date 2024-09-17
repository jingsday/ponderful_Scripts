library(dplyr)
library(mgcv)
library(ggplot2)
library(nlme)

outwdir <- '/Users/lidiayung/PhD_project/project_PONDERFUL/ponderful_Scripts/ponderful_OUTPUT/'
#Part I: Prep
phy_che <- read.csv('/Users/lidiayung/PhD_project/project_PONDERFUL/ponderful_OUTPUT/Phy_Che_qt.csv')
##bio climatic regions
phy_che$bioregion <- 'Temperate'
phy_che[phy_che$Country == 'Spain',]$bioregion <- 'Mediterranean'
phy_che[phy_che$Country == 'Turkey',]$bioregion <- 'Continental'
phy_che[phy_che$Country == 'Uruguay',]$bioregion <- 'Subtropical'


##data preparation
phy_che$Country <- as.factor(phy_che$Country)
levels(phy_che$Country)


hist(phy_che$TN)
full_df_standardized_TN <- phy_che %>%
  mutate(across(c(Natural_5_qt, Aquatic_500_qt, Cropland_500_qt, 
                  Forest_500_qt, Pastures.and.open.nature_500_qt, 
                  Urban_500_qt, Animals_cont.t, Area.t, Depth.t, 
                  bio1.t,bio4.t,bio5.t,
                  bio6.t,bio7.t,
                  bio12.t,bio15.t,bio17.t), 
                ~ scale(.)[, 1])) # Standardizing each column




model_TN_df <- full_df_standardized_TN[,c('TN','Natural_5_qt', 'Aquatic_500_qt', 'Cropland_500_qt', 
                                          'Forest_500_qt', 'Pastures.and.open.nature_500_qt', 
                                          'Urban_500_qt', 'Animals_cont.t', 'Area.t', 'Depth.t', 
                                          'bio1.t','bio4.t','bio5.t',
                                          'bio6.t','bio7.t',
                                          'bio12.t','bio15.t','bio17.t','Country','bioregion','bio7')]

model_TN_df <-na.omit(model_TN_df)

plot(phy_che$TP)
full_df_standardized_TP <- phy_che %>%
  mutate(across(c(Natural_5_qt, Aquatic_500_qt, Cropland_500_qt, 
                  Forest_500_qt, Pastures.and.open.nature_500_qt, 
                  Urban_500_qt, Animals_cont.t, Area.t, Depth.t, 
                  bio1.t,bio4.t,bio5.t,
                  bio6.t,bio7.t,
                  bio12.t,bio15.t,bio17.t), 
                ~ scale(.)[, 1])) # Standardizing each column



model_TP_df <- full_df_standardized_TP[,c('TP','Natural_5_qt', 'Aquatic_500_qt', 'Cropland_500_qt', 
                                          'Forest_500_qt', 'Pastures.and.open.nature_500_qt', 
                                          'Urban_500_qt', 'Animals_cont.t', 'Area.t', 'Depth.t', 
                                          'bio1.t','bio4.t','bio5.t','bio6.t','bio7.t',
                                          'bio12.t','bio15.t','bio17.t','Country','bioregion')]
model_TP_df<-na.omit(model_TP_df)

#Part II: Modelling
## TP
###GAM gamma model (setting general in the beginning and remove insignificant vars then set select as TRUE)

TP_gam_model_gamma <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                            s(Cropland_500_qt) + s(Forest_500_qt) + 
                            s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=3)+
                            s(Area.t)+s(Depth.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                          data = model_TP_df, family = Gamma(link = "log"))

summary(TP_gam_model_gamma)

TP_gam_model_gamma_remove <- gam(TP ~ s(Aquatic_500_qt) + 
                                   s(Forest_500_qt) + 
                                   s(Pastures.and.open.nature_500_qt)+s(Animals_cont.t,k=3)+
                                   s(Area.t)+s(Depth.t)+s(bio1.t)+s(bio4.t)+s(bio12.t), 
                                 data = model_TP_df, family = Gamma(link = "log"),select=TRUE)


summary(TP_gam_model_gamma_remove)

TP_gam_model_selected <-gam(TP ~ Aquatic_500_qt + 
                              s(Forest_500_qt) + 
                              s(Pastures.and.open.nature_500_qt)+Animals_cont.t+
                              s(Area.t)+Depth.t+s(bio1.t)+s(bio4.t)+s(bio12.t), 
                            data = model_TP_df, family = Gamma(link = "log"),select=TRUE)
summary(TP_gam_model_selected)

# Open a PNG graphics device
png(filename = file_name, width = 800, height = 600)

# Plot the GAM model
plot(TP_gam_model_selected, page = 1, shade = TRUE, rug = TRUE, residuals = TRUE)

# Close the graphical device
dev.off()

##TN 
###GAM gamma model (setting general in the beginning and remove insignificant vars then set select as TRUE)

TN_gam_model_gamma <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                            s(Cropland_500_qt) + s(Forest_500_qt) + 
                            s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=3)+
                            s(Area.t)+s(Depth.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                          data = model_TN_df, family = Gamma(link = "log"))

summary(TN_gam_model_gamma)


TN_gam_model_gamma_remove <- gam(TN ~ s(Forest_500_qt) + 
                                   s(Pastures.and.open.nature_500_qt,k=3)+Animals_cont.t+
                                   s(Depth.t)+s(bio1.t)+s(bio4.t)+s(bio12.t), 
                                 data = model_TN_df, family = Gamma(link = "log"),select=TRUE)


summary(TN_gam_model_gamma_remove)
