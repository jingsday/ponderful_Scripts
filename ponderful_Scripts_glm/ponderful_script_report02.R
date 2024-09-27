library(dplyr)
library(mgcv)
library(ggplot2)
library(nlme)
library(fitdistrplus)
library(bestNormalize)
library(visreg)
library(lmerTest)


outwdir <- '/Users/lidiayung/PhD_project/project_PONDERFUL/ponderful_Scripts/ponderful_OUTPUT/'
#=========================================================
# Part I: Prep
#=========================================================

phy_che <- read.csv('/Users/lidiayung/PhD_project/project_PONDERFUL/ponderful_OUTPUT/Phy_Che_qt.csv')
## Bio climatic regions
phy_che$bioregion <- 'Temperate'
phy_che[phy_che$Country == 'Spain',]$bioregion <- 'Mediterranean'
phy_che[phy_che$Country == 'Turkey',]$bioregion <- 'Continental'
phy_che[phy_che$Country == 'Uruguay',]$bioregion <- 'Subtropical'


##Factors and transformation 
phy_che$Country <- as.factor(phy_che$Country)
levels(phy_che$Country)

###log transformation TP 
shapiro.test(phy_che$TP)
test <- bestNormalize(phy_che$TP)
plot(test, leg_loc = "bottomright")

phy_che$log_TP <- log(phy_che$TP,base=10)
hist(phy_che$log_TP)
shapiro.test(phy_che$log_TP)
hist(phy_che$TP)

hist(phy_che$log_TN)
hist(phy_che$log_TP)

###log transformation TN 
shapiro.test(phy_che$TN)
test <- bestNormalize(phy_che$TN) #log10
plot(test, leg_loc = "bottomright")

phy_che$log_TN <- log(phy_che$TN,base=10)
hist(phy_che$log_TN)
shapiro.test(phy_che$log_TN)
hist(phy_che$log_TN)

full_df_standardized_TN <- phy_che %>%
  mutate(across(c(Natural_5_qt, Aquatic_500_qt, Cropland_500_qt, 
                  Forest_500_qt, Pastures.and.open.nature_500_qt, 
                  Urban_500_qt, Animals_cont.t, Area.t, Depth.t, 
                  bio1.t,bio4.t,bio5.t,
                  bio6.t,bio7.t,
                  bio12.t,bio15.t,bio17.t), 
                ~ scale(.)[, 1])) # Standardizing each column

unique(model_TN_df$Animals_cont.t)


model_TN_df <- full_df_standardized_TN[,c('log_TN','TN','Natural_5_qt', 'Aquatic_500_qt', 'Cropland_500_qt', 
                                          'Forest_500_qt', 'Pastures.and.open.nature_500_qt', 
                                          'Urban_500_qt', 'Animals_cont.t', 'Area.t', 'Depth.t', 
                                          'bio1.t','bio4.t','bio5.t',
                                          'bio6.t','bio7.t',
                                          'bio12.t','bio15.t','bio17.t','Country','bioregion','bio7')]

model_TN_df <-na.omit(model_TN_df)

full_df_standardized_TP <- phy_che %>%
  mutate(across(c(Natural_5_qt, Aquatic_500_qt, Cropland_500_qt, 
                  Forest_500_qt, Pastures.and.open.nature_500_qt, 
                  Urban_500_qt, Animals_cont.t, Area.t, Depth.t, 
                  bio1.t,bio4.t,bio5.t,
                  bio6.t,bio7.t,
                  bio12.t,bio15.t,bio17.t), 
                ~ scale(.)[, 1])) # Standardizing each column

colnames(phy_che)
model_TP_df <- full_df_standardized_TP[,c('log_TP','TP','Natural_5_qt', 'Aquatic_500_qt', 'Cropland_500_qt', 
                                          'Forest_500_qt', 'Pastures.and.open.nature_500_qt', 
                                          'Urban_500_qt', 'Animals_cont.t', 'Area.t', 'Depth.t', 
                                          'bio1.t','bio4.t','bio5.t','bio6.t','bio7.t',
                                          'bio12.t','bio15.t','bio17.t','Country','bioregion','')]
model_TP_df<-na.omit(model_TP_df)

library(corrplot)
##Correlation plots

###notes: bios 'bio1.t','bio4.t','bio5.t','bio6.t','bio7.t','bio12.t','bio15.t','bio17.t')]
df <-full_df_standardized_TP[,c('TP','Natural_5_qt', 'Aquatic_500_qt', 'Cropland_500_qt', 
                                'Forest_500_qt', 'Pastures.and.open.nature_500_qt', 
                                'Urban_500_qt', 'Animals_cont.t', 'Area.t', 'Depth.t', 
                                'bio1.t','bio7.t',
                                'bio12.t')]

df <-full_df_standardized_TN[,c('TN','Natural_5_qt', 'Aquatic_500_qt', 'Cropland_500_qt', 
                                'Forest_500_qt', 'Pastures.and.open.nature_500_qt', 
                                'Urban_500_qt', 'Animals_cont.t', 'Area.t', 'Depth.t', 
                                'bio1.t','bio7.t',
                                'bio12.t')]

df <-na.omit(df)
corrplot(cor(df), type = "upper", order = "alphabet",tl.col = "black", tl.srt = 45)
round(cor(df),2)

##corr per region
input<- 'Temperate'#Temperate, Mediterranean, Continental, Subtropical
df <-model_TP_df[model_TP_df$bioregion == input,][,c('TP','Natural_5_qt', 'Aquatic_500_qt', 'Cropland_500_qt', 
                                                       'Forest_500_qt', 'Pastures.and.open.nature_500_qt', 
                                                       'Urban_500_qt', 'Animals_cont.t', 'Area.t', 'Depth.t', 
                                                       'bio1.t','bio7.t',
                                                       'bio12.t')]
df <-na.omit(df)
dev.off()
corrplot(cor(df), type = "upper", order = "alphabet",tl.col = "black", tl.srt = 45)
cor(df)


#=========================================================
# Part II: Modelling
#=========================================================
## Modelling TP
###TP GAM gamma model (setting general in the beginning and remove insignificant vars then set select as TRUE)

TP_gam_model_gamma <- gam(TP ~ Natural_5_qt + Aquatic_500_qt + 
                            Cropland_500_qt + Forest_500_qt + 
                            Pastures.and.open.nature_500_qt+Urban_500_qt+Animals_cont.t+
                            s(Area.t)+s(Depth.t)+bio1.t+s(bio4.t)+s(bio12.t), 
                          data = model_TP_df,family=gaussian(link='log'),select=TRUE)
summary(TP_gam_model_gamma)

TP_gam_model_gamma_remove <- gam(TP ~ s(Aquatic_500_qt) + 
                                   s(Forest_500_qt) + 
                                   s(Pastures.and.open.nature_500_qt)+s(Animals_cont.t,k=3)+
                                   s(Area.t)+s(Depth.t)+s(bio1.t,k=1)+s(bio4.t)+s(bio12.t), 
                                 data = model_TP_df, family = Gamma(link = "log"),select=TRUE)
summary(TP_gam_model_gamma_remove)

TP_gam_model_selected <-gam(TP ~ Aquatic_500_qt + 
                              Forest_500_qt + 
                              s(Pastures.and.open.nature_500_qt,k=3)+Animals_cont.t+
                              Depth.t+s(bio1.t,k=9)+s(bio4.t,k=3)+s(bio12.t,k=4), 
                            data = model_TP_df, family = Gamma(link = "log"))
summary(TP_gam_model_selected)


test <-gam(TP ~ Aquatic_500_qt + 
             Forest_500_qt + 
             s(Pastures.and.open.nature_500_qt,k=3)+Animals_cont.t+
             Depth.t+s(bio1.t,k=9)+s(bio4.t,k=3)+s(bio12.t,k=4), 
           data = model_TP_df, family = Gamma(link = "log"), method='REML')
summary(test)

AIC(test,TP_gam_model_selected,TP_log_mixed_model_selected)
###TP GAM gamma model PLOT(summary, predictors, diagnostics)===================
summary(TP_gam_model_selected)

png(filename = paste0(outwdir, 'tp_gam_predictors.png'), width = 1200, height = 1000)
plot(TP_gam_model_selected, page=1,shade = TRUE, rug = TRUE, residuals = TRUE,scale = FALSE,all.terms = TRUE)
dev.off()

png(filename = paste0(outwdir, 'tp_gam_diagnostics.png'), width = 800, height = 800)
par(mfrow = c(2, 2), cex = 1.1)
gam.check(TP_gam_model_selected)
dev.off()

###TP GLM linear model 
library(MASS)
TP_linear_model <- glm(log_TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                         Forest_500_qt + Pastures.and.open.nature_500_qt +
                         Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                         bio1.t + bio4.t + bio12.t, 
                       data = model_TP_df)

summary(TP_linear_model)
stepAIC(TP_linear_model, direction = "both")

TP_linear_model_selected <- glm(formula = TP ~ Natural_5_qt + Forest_500_qt + Pastures.and.open.nature_500_qt + 
                                  Animals_cont.t + Depth.t + bio5.t + bio12.t, data = model_TP_df)
summary(TP_linear_model_selected)

###TP GLM model PLOT(summary, predictors, diagnostics, R2)===================
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

###test on linear gamma ===================
glm(y ~ temperature, data = d, family = Gamma(link = "log"))
TP_linear_model_test <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                         Forest_500_qt + Pastures.and.open.nature_500_qt +
                         Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                         bio1.t + bio7.t + bio12.t,data=model_TP_df, family = Gamma(link = "log"))

summary(TP_linear_model_test)
stepAIC(TP_linear_model_test, direction = "both")
TP_linear_model_test_selected <- glm(formula = TP ~ Natural_5_qt + Aquatic_500_qt + Animals_cont.t + 
                                       Depth.t + bio1.t + bio7.t + bio12.t, family = Gamma(link = "log"), 
                                     data = model_TP_df)
plot(TP_linear_model_test_selected)

summ(TP_linear_model_test_selected)
summary(lm(formula = TP ~ Natural_5_qt + Aquatic_500_qt + Animals_cont.t + 
              Depth.t + bio1.t + bio7.t + bio12.t, family = Gamma(link = "log"),data = model_TP_df))

###TP gamma mixed 
dev_resid <- residuals(TP_gam_model_selected,type="deviance")
shapiro.test(dev_resid)


gamm_intercept <- gam(TP ~ Aquatic_500_qt + 
                        Forest_500_qt + 
                        s(Pastures.and.open.nature_500_qt,k=3)+Animals_cont.t+
                        Depth.t+s(bio1.t,k=9)+s(bio4.t,k=3)+s(bio12.t,k=4)+s(Country, bs = "re"), 
                      data = model_TP_df, family = Gamma(link = "log"))
summary(gamm_intercept)$s.table


plot(gamm_intercept, select = 5)
gam(TP ~ Aquatic_500_qt + 
      Forest_500_qt + 
      s(Pastures.and.open.nature_500_qt,k=3)+Animals_cont.t+
      Depth.t+s(bio1.t,k=9)+s(bio4.t,k=3)+s(bio12.t,k=4), 
    data = model_TP_df, family = Gamma(link = "log"))
# examine model output
summary(gamm_intercept)$s.table



###TP bio-clamatic region ====================================================
####TP Medi ===========================
input <- 'Mediterranean'
df <-model_TP_df[model_TP_df$bioregion == input,][,c('TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t','bio4.t','bio5.t','bio12.t')]
hist(df$TP)
library(corrplot)
plot.new()
df <- model_TP_df %>% select(-Country, -bioregion)
df <- model_TP_df[,-c('Country','bioregion')]

corrplot(cor(df), type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)
cor(df)
plot.new()
df <-na.omit(df)
corrplot(cor(df), type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)
cor(df)

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

TP_gam_medi_selected <- gam(TP ~ s(Natural_5_qt) + s(Cropland_500_qt,k=3) +
                            s(Urban_500_qt) + Animals_cont.t + bio1.t, data = df, family = Gamma(link = "log"))

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


####TP Continental=====================
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
####TP CTN glm
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


####TP Temperate =================================
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

####TP TEMP GLM

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


####TP Subtropical ======================================
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
####==================================================

#### TP Subtropical glm 

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
##=============================================================================

##Modelling TN =================================================================
###TN GAM gamma model (setting general in the beginning and remove insignificant vars then set select as TRUE)

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


###GLM model=========================
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

###TN bio-clamatic region ====================================
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

####TN Continental ========================
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



####TN Temperate ===========================================
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
####==================================================

####TN Subtropical ====================================
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
####=========================================================================

#=========================================================
# Part III :transformed response vars modeled using GLM and GLMM 
#=========================================================

####TP general 
TP_log_linear_model <- glm(log_TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                         Forest_500_qt + Pastures.and.open.nature_500_qt +
                         Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                         bio1.t + bio7.t + bio12.t, 
                       data = model_TP_df)

summary(TP_log_linear_model)
stepAIC(TP_log_linear_model)

TP_log_linear_model_selected <- glm(formula = log_TP ~ Aquatic_500_qt + Forest_500_qt + Animals_cont.t + 
                                      Depth.t + bio1.t + bio7.t + bio12.t, data = model_TP_df)
summary(TP_log_linear_model_selected)
summary(lm(formula = log_TP ~ Aquatic_500_qt + Forest_500_qt + Animals_cont.t + 
             Depth.t + bio1.t + bio7.t + bio12.t, data = model_TP_df))
png(filename=paste0(outwdir,"tp_log_glm_diagnostics.png"), width=800, height=800)
par(mfrow=c(2,2))
plot(TP_log_linear_model)
dev.off()


png(filename=paste0(outwdir,"tp_log_glm_predictors.png"), width=800, height=800)
par(mfrow=c(4,2))
visreg(TP_log_linear_model_selected)
dev.off()

# Plot fitted values and residuals for each predictor
visreg(TP_log_linear_model_selected, partial = TRUE, rug = TRUE,page=1)
dev_resid <- residuals(TP_log_linear_model_selected,type="deviance")
shapiro.test(dev_resid)

lm.test <- lm(log_TP ~ Aquatic_500_qt + Forest_500_qt + Animals_cont.t + 
                Depth.t + bio1.t + bio7.t + bio12.t, data = model_TP_df)
lm.test.resid <- rstandard(lm.test)

plot(lm.test.resid ~ as.factor(model_TP_df$Country), xlab = "Countries",
     ylab = "Standardized residuals")

abline(0, 0, lty = 2)
anova(TP_log_linear_model_selected, TP_log_mixed_model_selected)

library(lmerTest)

#### TP MIXED MODEL ======================================================
TP_log_mixed_model<- lmer(log_TP ~ Aquatic_500_qt + Forest_500_qt + Animals_cont.t + 
                                      Depth.t + bio1.t + bio7.t + bio12.t+ (1 | Country), data = model_TP_df)

model_TP_df$dummy =1

TP_log_mixed_model_dummy<- lm(log_TP ~ Aquatic_500_qt + Forest_500_qt + Animals_cont.t + 
                            Depth.t + bio1.t + bio7.t + bio12.t, data = model_TP_df)
anova(TP_log_mixed_model_dummy,TP_log_mixed_model)
summary(TP_log_mixed_model)

step(TP_log_mixed_model)# Perform backward model selection
TP_log_mixed_model_selected <-lmer(log_TP ~ Aquatic_500_qt + Forest_500_qt + Depth.t + (1 | Country),data = model_TP_df)
summary(TP_log_mixed_model_selected)  
anova(lm.test,TP_log_mixed_model_selected)
library(MuMIn)
r.squaredGLMM(TP_log_mixed_model_selected)

plot(ranef(TP_log_mixed_model_selected))


deviance_value <- -2 * logLik(TP_log_mixed_model_selected)  # Deviance = -2 * log-likelihood
deviance_value

null_model <- lmer(log_TP ~ 1 + (1 | Country), data = model_TP_df)

# Deviance of the null model
null_deviance <- -2 * logLik(null_model)
# Compare deviance
deviance_improvement <- null_deviance - deviance_value
deviance_improvement

1 - (deviance_value / null_deviance)

#####Model Diagnostics
png(filename = paste0(outwdir, "tp_log_mixed_diagnostics.png"), width = 1200, height = 600)  # wider than tall

par(mfrow = c(1, 2))
plot(fitted(TP_log_mixed_model_selected), resid(TP_log_mixed_model_selected), 
     pch = 20, col = "black", lty = "dotted", 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted")

qqnorm(resid(TP_log_mixed_model_selected), main = "Normal Q-Q")
qqline(resid(TP_log_mixed_model_selected), col = "red")  # Add Q-Q line
# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1, 1))
dev.off()

hist(resid(TP_log_mixed_model_selected))

report::report(TP_log_mixed_model_selected)



library(lattice)
dotplot(ranef(TP_log_mixed_model_selected,condVar=TRUE),
        lattice.options=list(layout=c(1,2)))

library(performance)# Diagnostic plots

check_model(TP_log_mixed_model_selected)

library(jtools)
summ(TP_log_mixed_model_selected)


effect_plot(TP_log_mixed_model_selected,pred = Aquatic_500_qt, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)

visreg(TP_log_mixed_model_selected)


library(sjPlot)
tab_model(TP_log_mixed_model_selected)



#TP medi  removed forest, bio12, bio7
input <- 'Mediterranean'
colnames(model_TP_df)
df <-model_TP_df[model_TP_df$bioregion == input,][,c('log_TP','TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t','Area.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t')]
hist(df$log_TP)
dim(df)
TP_log_medi_linear_model <- glm(log_TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                            Pastures.and.open.nature_500_qt +
                             Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                             bio1.t, 
                           data = df)
summary(TP_log_medi_linear_model)
stepAIC(TP_log_medi_linear_model)

TP_log_medi_linear_model_selected <- glm(formula = log_TP ~ Natural_5_qt + Cropland_500_qt + Urban_500_qt + 
                                           Animals_cont.t + bio1.t, data = df)


summary(TP_log_medi_linear_model_selected)
summary(lm(formula = log_TP ~ Natural_5_qt + Cropland_500_qt + Urban_500_qt + 
             Animals_cont.t + bio1.t, data = df))

png(filename=paste0(outwdir,"tp_log_medi_glm_diagnostics.png"), width=800, height=800)
par(mfrow=c(2,2))
plot(TP_log_medi_linear_model_selected)
dev.off()

png(filename=paste0(outwdir,"tp_log_medi_glm_predictos.png"), width=800, height=800)
par(mfrow=c(2,4))
visreg(TP_log_medi_linear_model_selected)
dev.off()

#
input <- 'Temperate'
df <-model_TP_df[model_TP_df$bioregion == input,][,c('log_TP','TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t','Area.t', 'bio1.t','bio12.t')]
dim(df)
na.omit(df)
TP_log_temp_linear_model <- glm(log_TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                                  Pastures.and.open.nature_500_qt +
                                  Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                                  bio1.t +bio12.t , 
                                data = df)
summary(TP_log_temp_linear_model)
stepAIC(TP_log_temp_linear_model)

TP_log_temp_linear_model_selected <-  glm(formula = log_TP ~ Cropland_500_qt + Pastures.and.open.nature_500_qt + 
                                            Animals_cont.t + Depth.t + bio12.t, data = df)

summary(TP_log_temp_linear_model_selected)
summary(lm(formula = log_TP ~ Cropland_500_qt + Pastures.and.open.nature_500_qt + 
             Animals_cont.t + Depth.t + bio12.t, data = df))

png(filename=paste0(outwdir,"tp_log_temp_glm_diagnostics.png"), width=800, height=800)
par(mfrow=c(2,2))
plot(TP_log_temp_linear_model_selected)
dev.off()

png(filename=paste0(outwdir,"tp_log_temp_glm_predictos.png"), width=800, height=800)
par(mfrow=c(2,4))
visreg(TP_log_temp_linear_model_selected)
dev.off()

##TP temperate mixed 

df <-model_TP_df[model_TP_df$bioregion == input,][,c('log_TP','TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t','Area.t', 'bio1.t','bio12.t','Country')]

TP_log_temp_mixed_model<- lmer(log_TP  ~ Cropland_500_qt + Pastures.and.open.nature_500_qt + 
                            Animals_cont.t + Depth.t + bio12.t+ (1 | Country), data = df)
summary(TP_log_temp_mixed_model)
# Perform backward model selection
step(TP_log_temp_mixed_model)
TP_log_temp_mixed_model_selected <-glm(log_TP ~ Cropland_500_qt + Pastures.and.open.nature_500_qt + Animals_cont.t + Depth.t + bio12.t,data = model_TP_df)
summary(TP_log_temp_mixed_model_selected)  
dev.off()
plot(TP_log_mixed_model_selected,)


png(filename=paste0(outwdir,"tp_log_mixed_predictors.png"), width=800, height=800)
par(mfrow=c(2,2))
visreg(TP_log_mixed_model_selected)
dev.off()


# TP log CTN  removing Pastures.and.open.nature_500_qt, area.t,Aquatic_500_qt',
input <- 'Continental'
df <-model_TP_df[model_TP_df$bioregion == input,][,c('log_TP','TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Animals_cont.t', 'bio1.t','bio7.t','bio12.t')]
dim(df)
corrplot(cor(df), type = "upper", order = "alphabet",tl.col = "black", tl.srt = 45)
round(cor(df),2)
dim(df)

na.omit(df)
TP_log_ctn_linear_model <- glm(log_TP ~ Natural_5_qt+Urban_500_qt + Animals_cont.t + Depth.t +
                                 bio7.t+bio12.t, 
                                data = df)
summary(TP_log_ctn_linear_model)
stepAIC(TP_log_ctn_linear_model)
TP_log_ctn_linear_model_selected <-  glm(formula = log_TP ~ bio7.t + bio12.t, data = df)
summary(TP_log_ctn_linear_model_selected)
summary(lm(formula = log_TP ~ bio7.t + bio12.t, data = df))

png(filename=paste0(outwdir,"tp_log_ctn_glm_diagnostics.png"), width=800, height=800)
par(mfrow=c(2,2))
plot(TP_log_ctn_linear_model_selected)
dev.off()

png(filename=paste0(outwdir,"tp_log_ctn_glm_predictos.png"), width=800, height=800)
par(mfrow=c(1,2))
visreg(TP_log_ctn_linear_model_selected)
dev.off()

TP_log_ctn_linear_model_selected
#subt 
input <- 'Subtropical'
df <-model_TP_df[model_TP_df$bioregion == input,][,c('log_TP','TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t','Area.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t')]
dim(df)
corrplot(cor(df),type='upper', order = "alphabet",tl.col = "black", tl.srt = 45)
round(cor(df),2)
TP_log_subp_linear_model <- glm(log_TP ~ Natural_5_qt+Urban_500_qt + Animals_cont.t 
                                + Urban_500_qt+Depth.t +Cropland_500_qt+Area.t+Aquatic_500_qt+bio1.t,
                               data = df)
summary(TP_log_subp_linear_model)
stepAIC(TP_log_subp_linear_model)
TP_log_subp_linear_model_selected <-  glm(formula = log_TP ~ Depth.t + Area.t + Aquatic_500_qt, data = df)
summary(TP_log_subp_linear_model_selected)
summary(lm(formula = log_TP ~ Depth.t + Area.t + Aquatic_500_qt, data = df))

png(filename=paste0(outwdir,"tp_log_subp_glm_diagnostics.png"), width=800, height=800)
par(mfrow=c(2,2))
plot(TP_log_subp_linear_model_selected)
dev.off()

png(filename=paste0(outwdir,"tp_log_subp_glm_predictos.png"), width=800, height=800)
par(mfrow=c(2,2))
visreg(TP_log_subp_linear_model_selected)
dev.off()
########################################
#TN 

TN_log_linear_model <- glm(log_TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                             Forest_500_qt + Pastures.and.open.nature_500_qt +
                             Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                             bio1.t + bio7.t + bio12.t, 
                           data = model_TN_df)

summary(TN_log_linear_model)
stepAIC(TN_log_linear_model)

TN_log_linear_model_selected <- glm(formula = log_TN ~ Forest_500_qt + Pastures.and.open.nature_500_qt + 
                                      Area.t + Depth.t + bio1.t + bio7.t + bio12.t, data = model_TN_df)

summary(TN_log_linear_model_selected) 
summary(lm(formula = log_TN ~ Forest_500_qt + Pastures.and.open.nature_500_qt + 
             Area.t + Depth.t + bio1.t + bio7.t + bio12.t, data = model_TN_df))
        

png(filename=paste0(outwdir,"TN_log_glm_diagnostics.png"), width=800, height=800)
par(mfrow=c(2,2))
plot(TN_log_linear_model_selected)
dev.off()

png(filename=paste0(outwdir,"TN_log_glm_predictors.png"), width=800, height=800)
par(mfrow=c(2,4))
visreg(TN_log_linear_model_selected)
dev.off()
################### TN mixed 
TN_log_mixed_model<- lmer(log_TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                            Forest_500_qt + Pastures.and.open.nature_500_qt +
                            Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                            bio1.t + bio7.t + bio12.t+ (1 | Country), 
                          data = model_TN_df)
summary(TN_log_mixed_model)

# Perform backward model selection
step(TN_log_mixed_model)
TN_log_mixed_model_selected <-lmer(log_TN ~ Forest_500_qt + Area.t + Depth.t + (1 | Country),data = model_TN_df)
summary(TN_log_mixed_model_selected)  
plot(TN_log_mixed_model_selected)

AIC(TN_log_linear_model_selected, TN_log_mixed_model_selected)

#####Model Diagnostics
png(filename = paste0(outwdir, "TN_log_mixed_diagnostics.png"), width = 1200, height = 600)  # wider than tall

par(mfrow = c(1, 2))
plot(fitted(TN_log_mixed_model_selected), resid(TN_log_mixed_model_selected), 
     pch = 20, col = "black", lty = "dotted", 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted")

qqnorm(resid(TN_log_mixed_model_selected), main = "Normal Q-Q")
qqline(resid(TN_log_mixed_model_selected), col = "red")  # Add Q-Q line
# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1, 1))
dev.off()

hist(resid(TN_log_mixed_model_selected))

report::report(TN_log_mixed_model_selected)


png(filename=paste0(outwdir,"TN_log_mixed_predictors.png"), width=800, height=800)
par(mfrow=c(2,2))
visreg(TN_log_mixed_model_selected)
dev.off()


#TN medi  removed forest, bio12, bio7  SHould remove area.t
input <- 'Mediterranean'
colnames(model_TN_df)
df <-model_TN_df[model_TN_df$bioregion == input,][,c('log_TN','TN','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t','Area.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t')]
hist(df$log_TN)
dim(df)
round(cor(df),2)
TN_log_medi_linear_model <- glm(log_TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                                  Pastures.and.open.nature_500_qt +
                                  Urban_500_qt + Animals_cont.t + Depth.t +
                                  bio1.t, 
                                data = df)
summary(TN_log_medi_linear_model)
stepAIC(TN_log_medi_linear_model)

TN_log_medi_linear_model_selected <- glm(formula = log_TN ~ Cropland_500_qt + Urban_500_qt + Depth.t + 
                                           bio1.t, data = df)
summary(lm(formula = log_TN ~ Cropland_500_qt + Urban_500_qt + Depth.t + 
             bio1.t, data = df))

summary(TN_log_medi_linear_model_selected)
summary(lm(formula = log_TN ~ Natural_5_qt + Cropland_500_qt + Urban_500_qt + 
             Animals_cont.t + bio1.t, data = df))

png(filename=paste0(outwdir,"TN_log_medi_glm_diagnostics.png"), width=800, height=800)
par(mfrow=c(2,2))
plot(TN_log_medi_linear_model_selected)
dev.off()

png(filename=paste0(outwdir,"TN_log_medi_glm_predictors.png"), width=800, height=800)
par(mfrow=c(2,3))
visreg(TN_log_medi_linear_model_selected)
dev.off()

#
input <- 'Temperate'
df <-model_TN_df[model_TN_df$bioregion == input,][,c('log_TN','TN','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t','Area.t', 'bio1.t','bio12.t')]
dim(df)
na.omit(df)
round(cor(df),2)
TN_log_temp_linear_model <- glm(log_TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                                  Pastures.and.open.nature_500_qt +
                                  Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                                  bio1.t +bio12.t , 
                                data = df)
summary(TN_log_temp_linear_model)
stepAIC(TN_log_temp_linear_model)

TN_log_temp_linear_model_selected <-  glm(formula = log_TN ~ Cropland_500_qt + Pastures.and.open.nature_500_qt + 
                                            Animals_cont.t + Depth.t + bio1.t + bio12.t, data = df)

summary(TN_log_temp_linear_model_selected)
summary(lm(formula = log_TN ~ Cropland_500_qt + Pastures.and.open.nature_500_qt + 
             Animals_cont.t + Depth.t + bio1.t + bio12.t, data = df))

png(filename=paste0(outwdir,"TN_log_temp_glm_diagnostics.png"), width=800, height=800)
par(mfrow=c(2,2))
plot(TN_log_temp_linear_model_selected)
dev.off()

png(filename=paste0(outwdir,"TN_log_temp_glm_predictors.png"), width=800, height=800)
par(mfrow=c(2,3))
visreg(TN_log_temp_linear_model_selected)
dev.off()

#############TN temp mixed
input <- 'Temperate'

df <-model_TN_df[model_TN_df$bioregion == input,][,c('log_TN','TN','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t','Area.t', 'bio1.t','bio12.t','Country')]
dim(df)

TN_log_temp_mixed_model<- lmer(log_TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                            Pastures.and.open.nature_500_qt +
                            Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                            bio1.t +bio12.t+ (1 | Country) , 
                          data = df)
summary(TN_log_temp_mixed_model)

# Perform backward model selection
step(TN_log_temp_mixed_model)
TN_log_temp_mixed_model_selected <-lmer(log_TN ~ Cropland_500_qt + Depth.t + bio12.t + (1 | Country),data = model_TN_df)
summary(TN_log_temp_mixed_model_selected)  
dev.off()
plot(TN_log_temp_mixed_model_selected)
r.squaredGLMM(TN_log_temp_mixed_model_selected)


png(filename=paste0(outwdir,"TN_log_temp_mixed_predictors.png"), width=800, height=800)
par(mfrow=c(2,2))
visreg(TN_log_temp_mixed_model_selected)
dev.off()

#####Model Diagnostics
png(filename = paste0(outwdir, "TN_log_temp_mixed_diagnostics.png"), width = 1200, height = 600)  # wider than tall

par(mfrow = c(1, 2))
plot(fitted(TN_log_temp_mixed_model_selected), resid(TN_log_temp_mixed_model_selected), 
     pch = 20, col = "black", lty = "dotted", 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted")

qqnorm(resid(TN_log_temp_mixed_model_selected), main = "Normal Q-Q")
qqline(resid(TN_log_temp_mixed_model_selected), col = "red")  # Add Q-Q line
# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1, 1))
dev.off()

hist(resid(TN_log_temp_mixed_model_selected))

report::report(TN_log_temp_mixed_model_selected)
### TN log CTN  removing Pastures.and.open.nature_500_qt, area.t,Aquatic_500_qt',
input <- 'Continental'
df <-model_TN_df[model_TN_df$bioregion == input,][,c('log_TN','TN','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Animals_cont.t', 'bio1.t','bio7.t','bio12.t')]
dim(df)
corrplot(cor(df), type = "upper", order = "alphabet",tl.col = "black", tl.srt = 45)
round(cor(df),2)
dim(df)

na.omit(df)
TN_log_ctn_linear_model <- glm(log_TN ~ Natural_5_qt+Urban_500_qt + Animals_cont.t + Depth.t +
                                 bio7.t+bio12.t, 
                               data = df)
summary(TN_log_ctn_linear_model)
stepAIC(TN_log_ctn_linear_model)
TN_log_ctn_linear_model_selected <- glm(formula = log_TN ~ Depth.t + bio7.t + bio12.t, data = df)
summary(TN_log_ctn_linear_model_selected)
summary(lm(formula = log_TN ~ Depth.t + bio7.t + bio12.t, data = df))

png(filename=paste0(outwdir,"TN_log_ctn_glm_diagnostics.png"), width=800, height=800)
par(mfrow=c(2,2))
plot(TN_log_ctn_linear_model_selected)
dev.off()

png(filename=paste0(outwdir,"TN_log_ctn_glm_predictors.png"), width=800, height=800)
par(mfrow=c(2,2))
visreg(TN_log_ctn_linear_model_selected)
dev.off()

#subt removing area too
input <- 'Subtropical'
df <-model_TN_df[model_TN_df$bioregion == input,][,c('log_TN','TN','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t','Area.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t')]
dim(df)
corrplot(cor(df),type='upper', order = "alphabet",tl.col = "black", tl.srt = 45)
round(cor(df),2)
TN_log_subp_linear_model <-  glm(formula = log_TN ~ Natural_5_qt + Depth.t, data = df)

summary(TN_log_subp_linear_model)
stepAIC(TN_log_subp_linear_model)
TN_log_subp_linear_model_selected <-  glm(formula =log_TN ~ Natural_5_qt + Depth.t, data = df)
summary(TN_log_subp_linear_model_selected)
summary(lm(formula = log_TN ~ Natural_5_qt + Depth.t, data = df))

png(filename=paste0(outwdir,"TN_log_subp_glm_diagnostics.png"), width=800, height=800)
par(mfrow=c(2,2))
plot(TN_log_subp_linear_model_selected)
dev.off()

png(filename=paste0(outwdir,"TN_log_subp_glm_predictors.png"), width=800, height=800)
par(mfrow=c(2,2))
visreg(TN_log_subp_linear_model_selected)
dev.off()
#################Part extra:glm spatio autocorrelation 

gam_model_spatial <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                           s(Cropland_500_qt) + s(Forest_500_qt) + 
                           s(Pastures.and.open.nature_500_qt) + 
                           s(Urban_500_qt) + 
                           s(Depth.t) + 
                           s(bio1.t) + 
                           s(bio12.t) + 
                           s(Longitude, Latitude, bs = "tp"),  # Spatial smoother
                         data = model_TP_df, 
                         family = Gamma(link = "log"))

summary(gam_model_spatial)

#Part V: Euromedi (excluded Uruguay)
input <- 'Uruguay'
## TP Euromedi df
df <-model_TP_df[model_TP_df$Country != input,][,c('log_TP','TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 'Forest_500_qt','Area.t',
                                                       'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t','bio7.t','bio5.t','bio12.t')]
dim(df)
hist(df$Animals_cont.t)

corrplot(cor(df),type='upper',order = "alphabet",tl.col = "black", tl.srt = 45)
round(cor(df),2)

## TP Euromedi GAM model
TP_euromedi_gam_model_gamma <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                            s(Cropland_500_qt) + s(Forest_500_qt) + 
                            s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=5)+
                            s(Area.t)+s(Depth.t)+s(bio1.t)+s(bio5.t)+s(bio12.t), 
                          data = df,family=Gamma(link='log'),select=TRUE)
summary(TP_euromedi_gam_model_gamma)

TP_euromedi_gam_model_gamma_remove <- gam(TP ~ s(Cropland_500_qt) + 
                                   s(Pastures.and.open.nature_500_qt)+s(Animals_cont.t,k=4)+
                                   s(Area.t)+s(Depth.t)+s(bio1.t)+s(bio5.t)+s(bio12.t), 
                                 data = df, family = Gamma(link = "log"),select=TRUE)

test <- gam(TP ~ Aquatic_500_qt + 
                                  
                                   s(Animals_cont.t,k=5)+
                                    s(Area.t)+Depth.t+s(bio1.t)+s(bio5.t)+bio12.t, 
                                  data = df,family=Gamma(link='log'),select=TRUE)
summary(test)

TP_euromedi_gam_model_gamma_remove <- gam(TP ~ s(Cropland_500_qt) + 
                                            Pastures.and.open.nature_500_qt+s(Animals_cont.t,k=4)+
                                            s(Area.t)+Depth.t+s(bio1.t)+bio12.t, 
                                          data = df, family = Gamma(link = "log"))



AIC(TP_euromedi_gam_model_gamma_remove,TP_euromedi_gam_model_gamma)

summary(TP_euromedi_gam_model_gamma_remove)

TP_euromedi_gam_model_selected <-gam(TP ~ s(Cropland_500_qt) + 
                              Pastures.and.open.nature_500_qt+s(Animals_cont.t,k=4)+
                              s(Area.t)+Depth.t+s(bio1.t)+bio12.t, 
                            data = df, family = Gamma(link = "log"))
summary(TP_euromedi_gam_model_selected)

plot(TP_euromedi_gam_model_selected,shade=TRUE,page=1,rug=TRUE,all.terms = TRUE)

test <-gam(TP ~ Aquatic_500_qt + 
             Forest_500_qt + 
             s(Pastures.and.open.nature_500_qt,k=3)+Animals_cont.t+
             Depth.t+s(bio1.t,k=9)+s(bio4.t,k=3)+s(bio12.t,k=4), 
           data = model_TP_df, family = Gamma(link = "log"), method='REML')
summary(test)

AIC(test,TP_gam_model_selected,TP_log_mixed_model_selected)
###TP GAM gamma model PLOT(summary, predictors, diagnostics)===================
summary(TP_gam_model_selected)

png(filename = paste0(outwdir, 'tp_gam_predictors.png'), width = 1200, height = 1000)
plot(TP_gam_model_selected, page=1,shade = TRUE, rug = TRUE, residuals = TRUE,scale = FALSE,all.terms = TRUE)
dev.off()

png(filename = paste0(outwdir, 'tp_gam_diagnostics.png'), width = 800, height = 800)
par(mfrow = c(2, 2), cex = 1.1)
gam.check(TP_gam_model_selected)
dev.off()

###TP GLM linear model 
TP_euro_medi_linear_model <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                         Forest_500_qt + Pastures.and.open.nature_500_qt +
                         Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                         bio1.t + bio5.t + bio12.t, 
                       data = df)

summary(TP_euro_medi_linear_model)
stepAIC(TP_euro_medi_linear_model, direction = "both")

TP_euro_medi_linear_model_selected <- glm(formula = TP ~ Natural_5_qt + Forest_500_qt + Pastures.and.open.nature_500_qt + 
                                  Animals_cont.t + Depth.t + bio5.t + bio12.t, data = df)
summary(TP_euro_medi_linear_model_selected)

###TP GLM model PLOT(summary, predictors, diagnostics, R2)===================
summary(TP_euro_medi_linear_model_selected)

png(filename = paste0(outwdir, 'tp_glm_diagnostics.png'), width = 800, height = 800)
par(mfrow = c(2, 2), cex = 1.1)
plot(TP_linear_model_selected)
dev.off()

png(filename = paste0(outwdir, 'tp_glm_predictors.png'), width = 800, height = 800)
plot(allEffects(TP_linear_model_selected))
dev.off()
summary(lm(formula = TP ~ Natural_5_qt + Forest_500_qt + Pastures.and.open.nature_500_qt + 
             Animals_cont.t + Depth.t + bio5.t + bio12.t, data = model_TP_df))

##log TP 
TP_log_euromedi_linear_model <- glm(log_TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                                   Forest_500_qt + Pastures.and.open.nature_500_qt +
                                   Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                                   bio1.t + bio5.t + bio12.t, 
                                 data = df)

summary(TP_log_euromedi_linear_model)
stepAIC(TP_log_euromedi_linear_model, direction = "both")

TP_log_euromedi_linear_model_selected <- glm(formula = log_TP ~ Aquatic_500_qt + Forest_500_qt + Animals_cont.t + 
                                               Depth.t + bio5.t + bio12.t, data = df)

summary(TP_log_euromedi_linear_model_final)

TP_log_euromedi_linear_model_final <- glm(formula = log_TP ~ Forest_500_qt + Animals_cont.t + 
                                               Depth.t + bio5.t + bio12.t, data = df)


AIC(TP_log_euromedi_linear_model_selected,TP_log_euromedi_linear_model_final)

##log TP plot


##Euromedi log TP random effect 
TN_log_euromedi_mixed_model<- lmer(log_TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                                     Forest_500_qt + Pastures.and.open.nature_500_qt +
                                     Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                                     bio1.t + bio5.t + bio12.t, 
                                   data = df)
summary(TN_log_temp_mixed_model)

# Perform backward model selection
step(TN_log_temp_mixed_model)
TN_log_temp_mixed_model_selected <-lmer(log_TN ~ Cropland_500_qt + Depth.t + bio12.t + (1 | Country),data = model_TN_df)
summary(TN_log_temp_mixed_model_selected)  
dev.off()
plot(TN_log_temp_mixed_model_selected)
r.squaredGLMM(TN_log_temp_mixed_model_selected)


png(filename=paste0(outwdir,"TN_log_temp_mixed_predictors.png"), width=800, height=800)
par(mfrow=c(2,2))
visreg(TN_log_temp_mixed_model_selected)
dev.off()

#####Model Diagnostics
png(filename = paste0(outwdir, "TN_log_temp_mixed_diagnostics.png"), width = 1200, height = 600)  # wider than tall

par(mfrow = c(1, 2))
plot(fitted(TN_log_temp_mixed_model_selected), resid(TN_log_temp_mixed_model_selected), 
     pch = 20, col = "black", lty = "dotted", 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted")

qqnorm(resid(TN_log_temp_mixed_model_selected), main = "Normal Q-Q")
qqline(resid(TN_log_temp_mixed_model_selected), col = "red")  # Add Q-Q line
# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1, 1))
dev.off()

hist(resid(TN_log_temp_mixed_model_selected))

report::report(TN_log_temp_mixed_model_selected)
