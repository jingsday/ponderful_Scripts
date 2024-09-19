library(dplyr)
library(mgcv)
library(ggplot2)
library(nlme)
library(effects)
library(fitdistrplus)


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
                              Forest_500_qt + 
                              s(Pastures.and.open.nature_500_qt,k=3)+Animals_cont.t+
                              Depth.t+s(bio1.t,k=9)+s(bio4.t,k=3)+s(bio12.t,k=4), 
                            data = model_TP_df, family = Gamma(link = "log"))
summary(TP_gam_model_selected)



summary(TP_gam_model_test)
png(filename = paste0(outwdir, 'tp_gam_predictors.png'), width = 800, height = 600)

plot(TP_gam_model_selected, page = 1, shade = TRUE, rug = TRUE, residuals = TRUE)
dev.off()

plot(TP_gam_model_selected)

png(filename = paste0(outwdir, 'tp_gam_diagnostics.png'), width = 800, height = 800)
par(mfrow = c(2, 2), cex = 1.1)

gam.check(TP_gam_model_selected)
dev.off()

###GLM linear model 
library(MASS)
TP_linear_model <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                         Forest_500_qt + Pastures.and.open.nature_500_qt +
                         Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                         bio1.t + bio4.t + bio5.t + bio12.t, 
                       data = model_TP_df)

summary(TP_linear_model)
stepAIC(TP_linear_model, direction = "both")

TP_linear_model_selected <- glm(formula = TP ~ Natural_5_qt + Forest_500_qt + Pastures.and.open.nature_500_qt + 
                                  Animals_cont.t + Depth.t + bio5.t + bio12.t, data = model_TP_df)
summary(TP_linear_model_selected)
plot(TP_linear_model_selected)


TP_linear_model_selected <- 
summary(TP_linear_model_selected)

png(filename = paste0(outwdir, 'tp_glm_diagnostics.png'), width = 800, height = 800)
par(mfrow = c(2, 2), cex = 1.1)
plot(TP_linear_model_selected)
dev.off()

png(filename = paste0(outwdir, 'tp_glm_predictors.png'), width = 800, height = 800)
plot(allEffects(TP_linear_model_selected))
dev.off()
summary(lm(formula = TP ~ Natural_5_qt + Forest_500_qt + Pastures.and.open.nature_500_qt + 
             Animals_cont.t + Depth.t + bio5.t + bio12.t, data = model_TP_df))
### GLM random effect 


###bio-clamatic region 
####Medi 
input <- 'Mediterranean'
df <-model_TP_df[model_TP_df$bioregion == input,][,c('TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t','bio4.t','bio5.t','bio12.t')]
hist(df$TP)

TP_gam_medi <- gam(TP ~ s(Natural_5_qt, k=8) + 
                     s(Aquatic_500_qt, k=5) + 
                     s(Cropland_500_qt) + 
                     s(Pastures.and.open.nature_500_qt) + 
                     s(Urban_500_qt) + 
                     s(Animals_cont.t, k=5) + 
                     s(Depth.t) + 
                     s(bio1.t), 
                   data = df, 
                   family = Gamma(link = "log"))

length(unique(df$bio1.t))
summary(TP_gam_medi)


TP_gam_medi_remove <- gam(TP ~ Natural_5_qt + s(Cropland_500_qt,k=3) +
                     Urban_500_qt + Animals_cont.t + bio1.t, data = df, family = Gamma(link = "log"),SELECT=TRUE)
 
summary(TP_gam_medi_remove)

TP_gam_medi_selected <- gam(TP ~ Natural_5_qt + s(Cropland_500_qt,k=3) +
                            Urban_500_qt + Animals_cont.t + bio1.t, data = df, family = Gamma(link = "log"))

summary(TP_gam_medi_selected)

png(filename = paste0(outwdir, 'tp_medi_gam_predictors.png'), width = 800, height = 600)
plot(TP_gam_medi_selected,shade=TRUE,rug=TRUE,residuals=TRUE,page=1)
dev.off()

png(filename = paste0(outwdir, 'tp_medi_gam_diagnostics.png'), width = 800, height = 600)
par(mfrow = c(2, 2), cex = 1.1)
gam.check(TP_gam_medi_selected)
dev.off()

####Medi glm 
TP_medi_linear_model <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                     Pastures.and.open.nature_500_qt + Urban_500_qt + 
                     Animals_cont.t + Depth.t + bio1.t, data = df)
stepAIC(TP_medi_linear_model)

TP_medi_linear_model_selected <- glm(formula = TP ~ Natural_5_qt + Cropland_500_qt + Animals_cont.t + bio1.t, data = df)
summary(TP_medi_linear_model_selected)

####Medi glm plots
png(filename = paste0(outwdir, 'tp_medi_glm_diagnostics.png'), width = 800, height = 800)
par(mfrow = c(2, 2), cex = 1.1)
plot(TP_medi_linear_model_selected)
dev.off()

png(filename = paste0(outwdir, 'tp_medi_glm_predictors.png'), width = 800, height = 600)
plot(allEffects(TP_medi_linear_model_selected))
dev.off()
summary(lm(formula = TP ~ Natural_5_qt + Cropland_500_qt + Animals_cont.t + bio1.t, data = df))


####TP Continental
input <-'Continental'
df <-model_TP_df[model_TP_df$bioregion == input,][,c('TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Forest_500_qt','bio7.t')]
hist(df$TP)
descdist(df$TP,discrete=FALSE)

TP_gam_ctn <- gam(TP ~ s(Natural_5_qt) + s(Forest_500_qt)+s(Cropland_500_qt) +
                    s(Urban_500_qt) + s(Depth.t)+ s(bio7.t,k=8),  
                  data = df, family = Gamma(link = "log"))

summary(TP_gam_ctn)
plot(TP_gam_ctn)
TP_gam_ctn_remove <- gam(TP ~  s(Forest_500_qt) + Depth.t+ s(bio7.t),  
                         data = df, family = Gamma(link = "log"),select=TRUE)

summary(TP_gam_ctn_remove)

hist(df$Forest_500_qt)
TP_gam_ctn_selected <- gam(TP ~  s(Forest_500_qt) + Depth.t+ s(bio7.t),  
                         data = df, family = Gamma(link = "log"))
summary(TP_gam_ctn_selected)
gam.check(TP_gam_ctn_selected)

png(filename = paste0(outwdir, 'tp_ctn_gam_predictors.png'), width = 800, height = 600)
plot(TP_gam_ctn_selected,shade=TRUE,rug=TRUE,residuals=TRUE,page=1)
dev.off()


png(filename = paste0(outwdir, 'tp_ctn_gam_diagnostics.png'), width = 800, height = 600)
par(mfrow = c(2, 2), cex = 1.1)
gam.check(TP_gam_ctn_selected)
dev.off()
####CTN glm
TP_ctn_linear_model <- glm(TP ~ Natural_5_qt + Forest_500_qt+Cropland_500_qt +Urban_500_qt + Depth.t+ bio7.t,  data = df)
stepAIC(TP_ctn_linear_model)

TP_ctn_linear_model_selected <- glm(formula = TP ~ Forest_500_qt + Cropland_500_qt + bio7.t, 
                                    data = df)
summary(TP_ctn_linear_model_selected)


####CTN glm plots
png(filename = paste0(outwdir, 'tp_ctn_glm_diagnostics.png'), width = 800, height = 800)
par(mfrow = c(2, 2), cex = 1.1)
plot(TP_ctn_linear_model_selected)
dev.off()

png(filename = paste0(outwdir, 'tp_ctn_glm_predictors.png'), width = 800, height = 600)
plot(allEffects(TP_ctn_linear_model_selected))
dev.off()
summary(lm(formula = TP ~ Forest_500_qt + Cropland_500_qt + bio7.t, 
           data = df))


####TP Temperate
input <-'Temperate'
df <-model_TP_df[model_TP_df$bioregion == input,][,c('TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t','bio5.t')]
hist(df$TP)
descdist(df$TP,discrete = FALSE)

TP_gam_temp <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                     s(Cropland_500_qt) +
                     s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                     s(Animals_cont.t,k=4) + s(Depth.t)+s(bio1.t),  
                   data = df, family = Gamma(link = "log"))
summary(TP_gam_temp)

TP_gam_temp_remove <- gam(TP ~ 
                            s(Cropland_500_qt) +
                            s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                            s(Animals_cont.t,k=4) + s(Depth.t)+s(bio1.t),  
                          data = df, family = Gamma(link = "log"),SELECT=TRUE)
summary(TP_gam_temp_remove)
####################
TP_gam_temp_selected <- gam(TP ~ 
                              s(Cropland_500_qt) +
                              Pastures.and.open.nature_500_qt + s(Urban_500_qt) +
                              s(Animals_cont.t,k=4) + Depth.t+s(bio1.t),  
                            data = df, family = Gamma(link = "log"))
summary(TP_gam_temp_selected)

png(file=paste0(outwdir,'tp_temp_gam_predictors.png'),width=800,height=600)
plot(TP_gam_temp_selected,shade=TRUE, rug=TRUE,page=1,residuals=TRUE)
dev.off()


png(file=paste0(outwdir,'tp_temp_gam_diagnostics.png'),width=800,height=600)
par(mfrow = c(2, 2), cex = 1.1)
gam.check(TP_gam_temp_selected)
dev.off()

####TEMP GLM

TP_temp_linear_model <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                     Pastures.and.open.nature_500_qt + Urban_500_qt +Animals_cont.t + Depth.t+bio1.t,  
                   data = df)

stepAIC(TP_temp_linear_model)
TP_temp_linear_model_selected <- glm(formula = TP ~ Natural_5_qt + Cropland_500_qt + Animals_cont.t + 
                                       Depth.t, data = df)
  
summary(TP_temp_linear_model_selected)

####TEMP glm plots
png(filename = paste0(outwdir, 'tp_temp_glm_diagnostics.png'), width = 800, height = 800)
par(mfrow = c(2, 2), cex = 1.1)
plot(TP_temp_linear_model_selected)
dev.off()

png(filename = paste0(outwdir, 'tp_temp_glm_predictors.png'), width = 800, height = 600)
plot(allEffects(TP_temp_linear_model_selected))
dev.off()
summary(lm(formula = TP ~ Natural_5_qt + Cropland_500_qt + Animals_cont.t + 
             Depth.t, data = df))


####TP Subtropical
input <-'Subtropical'
df <-model_TP_df[model_TP_df$bioregion == input,][,c('TP','Natural_5_qt',  'Cropland_500_qt', 'Animals_cont.t',
                                                     'Pastures.and.open.nature_500_qt','Aquatic_500_qt',
                                                     'Urban_500_qt',  'Depth.t', 'bio1.t')]
hist(df$TP)

TP_gam_subt <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                     s(Cropland_500_qt) +
                     s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                     s(Animals_cont.t,k=3) + s(Depth.t)+ bio1.t,
                   data = df, family = Gamma(link = "log"),select=TRUE)
summary(TP_gam_subt)
length(unique(df$bio1.t))


TP_gam_subt_remove <- gam(TP ~  s(Natural_5_qt) + s(Aquatic_500_qt) + 
                            s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +bio1.t,
                          data = df, family = Gamma(link = "log"),select=TRUE)

TP_gam_subt_remove <- gam(TP ~  s(Natural_5_qt) +
                            s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +bio1.t,
                          data = df, family = Gamma(link = "log"),select=TRUE) #good as well

TP_gam_subt_remove <- gam(TP ~ Natural_5_qt+
                            s(Cropland_500_qt) +
                            s(Pastures.and.open.nature_500_qt),
                          data = df, family = Gamma(link = "log"),SELECT=TRUE)

summary(TP_gam_subt_remove)


TP_gam_subt_selected <- gam(TP ~ Natural_5_qt+
                            s(Cropland_500_qt) +
                            s(Pastures.and.open.nature_500_qt),
                          data = df, family = Gamma(link = "log"))
summary(TP_gam_subt_selected)
gam.check(TP_gam_subt_selected)

png(file=paste0(outwdir,'tp_subt_predictors.png'),width=800,height=600)
plot(TP_gam_subt_selected,shade=TRUE,residuals=TRUE,rug=TRUE,page=1)
dev.off()

png(filename = paste0(outwdir, 'tp_subt_diagnostics.png'), width = 800, height = 800)
par(mfrow = c(2, 2), cex = 1.1)
gam.check(TP_gam_subt_selected)
dev.off()
#### subp glm

TP_subp_linear_model <- glm(TP~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                     Pastures.and.open.nature_500_qt + Urban_500_qt +
                     Animals_cont.t + Depth.t+ bio1.t,
                   data = df)
stepAIC(TP_subp_linear_model)

TP_subp_linear_model_selected <- glm(formula = TP ~ Aquatic_500_qt + Pastures.and.open.nature_500_qt, 
                                     data = df)
summary(TP_subp_linear_model_selected)


####subp glm plots
png(filename = paste0(outwdir, 'tp_subp_glm_diagnostics.png'), width = 800, height = 800)
par(mfrow = c(2, 2), cex = 1.1)
plot(TP_subp_linear_model_selected)
dev.off()

png(filename = paste0(outwdir, 'tp_subp_glm_predictors.png'), width = 800, height = 600)
plot(allEffects(TP_subp_linear_model_selected))
dev.off()
summary(lm(formula = TP ~ Aquatic_500_qt + Pastures.and.open.nature_500_qt, 
           data = df))


##TN 
###GAM gamma model (setting general in the beginning and remove insignificant vars then set select as TRUE)

TN_gam_model_gamma <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                            s(Cropland_500_qt) + s(Forest_500_qt) + 
                            s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=3)+
                            s(Area.t)+s(Depth.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                          data = model_TN_df, family = Gamma(link = "log"),SELECT=TRUE)
hist(model_TN_df$TN)
descdist(model_TN_df$TN, discrete = FALSE)
summary(TN_gam_model_gamma)

TN_gam_model_gamma_remove <- gam(TN ~ s(Forest_500_qt) + s(Pastures.and.open.nature_500_qt)+s(Animals_cont.t,k=3)+
                                   s(Depth.t)+s(bio1.t)+s(bio4.t)+s(bio12.t), data = model_TN_df, family = Gamma(link = "log"),SELECT=TRUE)

summary(TN_gam_model_gamma_remove)


TN_gam_model_gamma_selected <- gam(TN ~ s(Forest_500_qt) + 
                              
                                   s(Depth.t)+s(bio1.t)+s(bio4.t)+s(bio12.t), 
                                 data = model_TN_df, family = Gamma(link = "log"))


summary(TN_gam_model_gamma_selected)

png(filename=paste0(outwdir,'tn_gam_predictors.png'),width=800,height=600)
plot(TN_gam_model_gamma_selected,shade=TRUE,page=1,rug=TRUE,residuals=TRUE)
dev.off()


png(filename=paste0(outwdir,'tn_gam_diagnostics.png'),width=800,height=600)
par(mfrow = c(2, 2), cex = 1.1)
gam.check(TN_gam_model_gamma_selected)
dev.off()


###GLM gamma model
TN_linear_model <- glm(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                         Forest_500_qt + Pastures.and.open.nature_500_qt +
                         Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                         bio1.t + bio4.t + bio5.t + bio12.t, 
                       data = model_TN_df)

summary(TN_linear_model)
stepAIC(TN_linear_model, direction = "both")

TN_linear_model_selected <- glm(formula = TN ~ Forest_500_qt + Pastures.and.open.nature_500_qt + 
                                  Area.t + Depth.t + bio4.t + bio5.t, data = model_TN_df)

summary(TN_linear_model_selected)
png(filename=paste0(outwdir,'tn_glm_step_predictors.png'),width=800,height=600)
plot(allEffects(TN_linear_model_selected))

dev.off()


summary(lm(formula = TN ~ Forest_500_qt + Pastures.and.open.nature_500_qt + 
                                  Area.t + Depth.t + bio4.t + bio5.t, data = model_TN_df))

###TN bio-clamatic region 
####Medi 
input <- 'Mediterranean'
df <-model_TN_df[model_TN_df$bioregion == input,][,c('TN','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t','bio4.t','bio5.t','bio12.t')]
hist(df$Animals_cont.t)

descdist(c(df$TN),discrete=FALSE)

TN_gam_medi <- gam(TN ~ s(Natural_5_qt,k=9) + 
                     s(Aquatic_500_qt,k=5) + 
                     s(Cropland_500_qt) + 
                     s(Pastures.and.open.nature_500_qt) + 
                     s(Urban_500_qt) + 
                     s(Animals_cont.t,k=3) + 
                     s(Depth.t) + 
                     s(bio1.t), data = df,  family = Gamma(link = "log"),SELECT=TRUE)
length(unique(df$bio1.t))

plot(page=1,TN_gam_medi,residuals=TRUE,shade=TRUE,rug=TRUE)
summary(TN_gam_medi)


TN_gam_medi_remove <-gam(TN ~ s(Urban_500_qt) +  
                         
                           s(Cropland_500_qt) + 
                           s(Depth.t) + 
                           s(bio1.t), 
                         data = df, 
                         family = Gamma(link = "log"),SELECT=TRUE)

TN_gam_medi_remove <-gam(TN ~  
                          
                           Cropland_500_qt + 
                           Depth.t + 
                           bio1.t, 
                         data = df, 
                         family = Gamma(link = "log"),SELECT=TRUE)


summary(TN_gam_medi_remove)
###??
TN_gam_medi_selected <- gam(TN ~  s(Cropland_500_qt) +s(Urban_500_qt)  +s(Depth.t)+s(bio1.t), data = df, family = Gamma(link = "log"))

summary(TN_gam_medi_selected)
plot(TN_gam_medi_selected)
png(filename = paste0(outwdir, 'tn_medi_gam_predictors.png'), width = 800, height = 600)
plot(TN_gam_medi_selected,shade=TRUE,rug=TRUE,residuals=TRUE,page=1)
dev.off()

png(filename = paste0(outwdir, 'tn_medi_gam_diagnostics.png'), width = 800, height = 600)
par(mfrow = c(2, 2), cex = 1.1)

gam.check(TN_gam_medi_selected)
dev.off()


####Medi glm 
TN_medi_linear_model <- glm(TN ~ Natural_5_qt +  Aquatic_500_qt +  Cropland_500_qt + 
                              Pastures.and.open.nature_500_qt +  Urban_500_qt + 
                              Animals_cont.t + Depth.t + bio1.t, data = df)

summary(TN_medi_linear_model)
stepAIC(TN_medi_linear_model, direction = "both")

TN_medi_linear_model_selected <- glm(formula = TN ~ Cropland_500_qt + Depth.t, data = df)
summary(lm(formula = TN ~ Cropland_500_qt + Depth.t, data = df))
summary(TN_medi_linear_model_selected)

png(filename=paste0(outwdir,'tn_medi_glm_diagnostics.png'),width=800,height=600)
par(mfrow = c(2, 2))
plot(TN_medi_linear_model_selected)
dev.off()

png(filename=paste0(outwdir,'tn_medi_glm_predictors.png'),width=800,height=600)
plot(allEffects(TN_medi_linear_model_selected))

dev.off()

####Continental
input <-'Continental'
df <-model_TN_df[model_TN_df$bioregion == input,][,c('TN','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Forest_500_qt','bio7.t')]
hist(df$TN)

TN_gam_ctn <- gam(TN ~ s(Natural_5_qt) + s(Forest_500_qt)+s(Cropland_500_qt) +
                    s(Urban_500_qt) + s(Depth.t)+ s(bio7.t),  
                  data = df, family = Gamma(link = "log"),select=TRUE)

summary(TN_gam_ctn)

TN_gam_ctn_remove <- gam(TN ~  s(Natural_5_qt,k=5) + s(Cropland_500_qt) +
                           s(Urban_500_qt) + s(bio7.t),  
                         data = df, family = Gamma(link = "log"),select=TRUE)

summary(TN_gam_ctn_remove)

TN_gam_ctn_selected <- gam(TN ~   s(Natural_5_qt,k=5) + s(Cropland_500_qt,k=8) +
                             s(Urban_500_qt,k=6) + s(bio7.t,k=8),  
                           data = df, family = Gamma(link = "log"))

TN_gam_ctn_selected <- gam(TN ~   s(Natural_5_qt) + s(Cropland_500_qt) +
                             s(Urban_500_qt) + s(bio7.t),  
                           data = df, family = Gamma(link = "log"))

summary(TN_gam_ctn_selected)

png(filename = paste0(outwdir, 'tn_ctn_gam_predictors.png'), width = 800, height = 600)
plot(TN_gam_ctn_selected,shade=TRUE,rug=TRUE,residuals=TRUE,page=1)
dev.off()


png(filename = paste0(outwdir, 'tn_ctn_gam_diagnostics.png'), width = 800, height = 600)
par(mfrow = c(2, 2))
gam.check(TN_gam_ctn_selected)
dev.off()

#### GLM

TN_ctn_linear_model <- glm(TN ~ Natural_5_qt + Forest_500_qt+Cropland_500_qt +
                    Urban_500_qt + Depth.t+ bio7.t,  
                  data = df)
stepAIC(TN_ctn_linear_model,direction='both')
TN_ctn_linear_model_selected <- glm(formula = TN ~ Depth.t, data = df)
summary(TN_ctn_linear_model_selected)

png(filename = paste0(outwdir, 'tn_ctn_glm_predictors.png'), width = 800, height = 600)
plot(allEffects(TN_ctn_linear_model_selected))
dev.off()


png(filename = paste0(outwdir, 'tn_ctn_glm_diagnostics.png'), width = 800, height = 600)
par(mfrow = c(2, 2))

plot(TN_ctn_linear_model_selected)
dev.off()


summary(lm(formula = TN ~ Depth.t, data = df))



####TN Temperate
input <-'Temperate'
df <-model_TN_df[model_TN_df$bioregion == input,][,c('TN','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t','bio5.t')]
hist(df$TN)
descdist(df$TN,discrete = FALSE)
TN_gam_temp <- gam(TN ~ s(Natural_5_qt) + 
                     s(Aquatic_500_qt) + 
                     s(Cropland_500_qt) + 
                     s(Pastures.and.open.nature_500_qt) + 
                     s(Urban_500_qt) + 
                     s(Animals_cont.t,k=3) + 
                     s(Depth.t) + 
                     s(bio1.t),
                   data = df, family = Gamma(link = "log"), select = TRUE)

summary(TN_gam_temp)

TN_gam_temp_remove <- gam(TN ~ 
                            s(Cropland_500_qt) + 
                            s(Pastures.and.open.nature_500_qt) + 
                            s(Urban_500_qt) + 
                            Animals_cont.t + 
                            s(Depth.t) + 
                            s(bio1.t),
                          data = df, family = Gamma(link = "log"), select = TRUE)

summary(TN_gam_temp_remove)


TN_gam_temp_selected <- gam(TN ~ 
                            s(Cropland_500_qt) + 
                            s(Pastures.and.open.nature_500_qt)+ 
                            s(Urban_500_qt) +                             Depth.t + 
                            s(bio1.t),
                          data = df, family = Gamma(link = "log"))
summary(TN_gam_temp_selected)


TN_gam_temp_selected <- gam(TN ~ 
                            s(Cropland_500_qt) + 
                            Pastures.and.open.nature_500_qt + 
                            s(Urban_500_qt) + 
                            s(Depth.t) +
                            s(bio1.t),
                          data = df, family = Gamma(link = "log"))

summary(TN_gam_temp_selected)

plot(TN_gam_temp_selected)

png(filename = paste0(outwdir, 'tn_temp_gam_predictors.png'), width = 800, height = 600)
plot(TN_gam_temp_selected,shade=TRUE,residuals=TRUE,rug=TRUE,page=1)
dev.off()


png(filename = paste0(outwdir, 'tn_tempt_gam_diagnostics.png'), width = 800, height = 600)
par(mfrow = c(2, 2))
gam.check(TN_temp_linear_model_selected)
dev.off()

####temp GLM

TN_temp_linear_model <- glm(TN ~ 
                              Cropland_500_qt + 
                              Pastures.and.open.nature_500_qt + 
                              Urban_500_qt + 
                            Animals_cont.t + 
                              Depth.t+ 
                              bio1.t,
                            data = df)

stepAIC(TN_temp_linear_model)
summary(TN_temp_linear_model_selected)
TN_temp_linear_model_selected <- glm(formula = TN ~ Cropland_500_qt + Depth.t, data = df)


png(filename = paste0(outwdir, 'tn_temp_glm_predictors.png'), width = 800, height = 600)
plot(allEffects(TN_temp_linear_model_selected))
dev.off()


png(filename = paste0(outwdir, 'tn_tempt_glm_diagnostics.png'), width = 800, height = 600)
par(mfrow = c(2, 2))

plot(TN_temp_linear_model_selected)
dev.off()


summary(lm(formula = TN ~ Cropland_500_qt + Depth.t, data = df))

####TN Subtropical
input <-'Subtropical'
df <-model_TN_df[model_TN_df$bioregion == input,][,c('TN','Natural_5_qt',  'Cropland_500_qt', 'Animals_cont.t',
                                                     'Pastures.and.open.nature_500_qt','Aquatic_500_qt',
                                                     'Urban_500_qt',  'Depth.t', 'bio1.t')]
hist(df$TN)
descdist(df$TN,discrete=FALSE)
fw <- fitdist(df$TN, "weibull")
fg <- fitdist(df$TN, "gamma")
fe <- fitdist(df$TN, "exp")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "gamma", "expo")
denscomp(list(fw, fg, fe), legendtext = plot.legend)
qqcomp(list(fw, fg, fe), legendtext = plot.legend)
cdfcomp(list(fw, fg, fe), legendtext = plot.legend)
ppcomp(list(fw, fg, fe), legendtext = plot.legend)
denscomp(list(fw, fg), legendtext = c("Weibull", "gamma"))
gofstat(list(fw, fg))

TN_gam_subt <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                     s(Cropland_500_qt) +
                     s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                     s(Animals_cont.t,k=3) + s(Depth.t)+ bio1.t,
                   data = df, family = Gamma(link = "log"),select=TRUE)#bio1.t df=2

summary(TN_gam_subt)
plot(TN_gam_subt,page=2)

TN_gam_subt_remove <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                                     s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                                     s(Animals_cont.t, k = 3) + s(Depth.t) + bio1.t,
                                   data = df, family = Gamma(link = "log"), select = TRUE)
summary(TN_gam_subt_remove)


TN_gam_subt_remove <-gam(TN ~ s(Natural_5_qt,k=8) + Aquatic_500_qt + 
                           + s(Urban_500_qt,k=7) ,
                         data = df, family = Gamma(link = "log"),select=TRUE)


plot(TN_gam_subt_remove,page=1)
TN_gam_subt_selected <- gam(TN ~ Natural_5_qt + Aquatic_500_qt + 
                              + s(Urban_500_qt) ,
                            data = df, family = Gamma(link = "log"))
summary(TN_gam_subt_selected)
gam.check(TN_gam_subt_selected)

png(file=paste0(outwdir,'TN_subt_predictors.png'),width=800,height=600)
plot(TN_gam_subt_selected,shade=TRUE,residuals=TRUE,rug=TRUE,page=1)
dev.off()


png(file=paste0(outwdir,'TN_subt_diagnostics.png'),width=800,height=600)
par(mfrow=c(2,2))
gam.check(TN_gam_subt_selected)
dev.off()
gam.check(TN_gam_subt_selected)

# Extracted fitted values and residuals
fitted_values <- fitted(TN_gam_subt_selected)
residuals <- residuals(TN_gam_subt_selected)
sqrt_resid <- sqrt(abs(residuals))  # For Scale-Location plot

# Plotting
png(filename="diagnostic_plots.png", width=800, height=800)
par(mfrow=c(2,2))

# 1. Residuals vs Fitted
plot(fitted_values, residuals,
     main="Residuals vs Fitted",
     xlab="Fitted Values",
     ylab="Residuals",
     pch=20, col="blue")
abline(h = 0, col = "red")

# 2. QQ Plot
qqnorm(residuals,
       main="Normal Q-Q",
       ylab="Residuals",
       xlab="Theoretical Quantiles")
qqline(residuals, col = "red")

# 3. Scale-Location Plot
plot(fitted_values, sqrt_resid,
     main="Scale-Location",
     xlab="Fitted Values",
     ylab="sqrt(|Residuals|)",
     pch=20, col="blue")
abline(h = 0, col = "red")

# 4. Residuals vs Leverage
plot(leverage, residuals,
     main="Residuals vs Leverage",
     xlab="Leverage",
     ylab="Residuals",
     pch=20, col="blue")
abline(h = 0, col = "red")
high_leverage <- which(leverage > (2 * mean(leverage)))
points(leverage[high_leverage], residuals[high_leverage], 
       col = "red", pch = 19)

# Close the plotting device
dev.off()


####glm 

tn_subt_linear_model <- glm(TN~Natural_5_qt+ Aquatic_500_qt + 
                              Cropland_500_qt +
                              Pastures.and.open.nature_500_qt + Urban_500_qt +
                             Animals_cont.t + Depth.t+ bio1.t,
                            data = df)
summary(tn_subt_linear_model)

stepAIC(tn_subt_linear_model)

tn_subt_linear_model_selected <- glm(formula = TN ~ Natural_5_qt + Aquatic_500_qt + Depth.t, data = df)
summary(tn_subt_linear_model_selected)

png(filename=paste0(outwdir,'tn_subt_glm_predictors.png'),width=800,height=600)
plot(allEffects(tn_subt_linear_model_selected))
dev.off()

png(filename=paste0(outwdir,'tn_subt_glm_diagnostics.png'),width=800,height=600)
par(mfrow=c(2,2))
plot(tn_subt_linear_model_selected)
dev.off()

summary(lm(formula = TN ~ Natural_5_qt + Aquatic_500_qt + Depth.t, data = df))
