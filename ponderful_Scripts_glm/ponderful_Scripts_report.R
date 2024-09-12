library(dplyr)

#Loading input
phy_che <- read.csv('/Users/lidiayung/PhD_project/project_PONDERFUL/ponderful_OUTPUT/Phy_Che_qt.csv')



#data preparation
phy_che$Country <- as.factor(phy_che$Country)
levels(phy_che$Country)


hist(phy_che$TN)
full_df_standardized_TN <- phy_che %>%
  mutate(across(c(Natural_5_qt, Aquatic_500_qt, Cropland_500_qt, 
                  Forest_500_qt, Pastures.and.open.nature_500_qt, 
                  Urban_500_qt, Animals_cont.t, Area.t, Depth.t, 
                  Hydeoperiod_length.t,bio1.t,bio4.t,bio5.t,
                  bio6.t,bio7.t,
                  bio12.t,bio15.t,bio17.t), 
                ~ scale(.)[, 1])) # Standardizing each column


model_TN_df <- full_df_standardized_TN[,c('TN','Natural_5_qt', 'Aquatic_500_qt', 'Cropland_500_qt', 
                         'Forest_500_qt', 'Pastures.and.open.nature_500_qt', 
                         'Urban_500_qt', 'Animals_cont.t', 'Area.t', 'Depth.t', 
                         'Hydeoperiod_length.t','bio1.t','bio4.t','bio5.t',
                         'bio6.t','bio7.t',
                         'bio12.t','bio15.t','bio17.t')]



plot(phy_che$TP)
full_df_standardized_TP <- phy_che %>%
  mutate(across(c(Natural_5_qt, Aquatic_500_qt, Cropland_500_qt, 
                  Forest_500_qt, Pastures.and.open.nature_500_qt, 
                  Urban_500_qt, Animals_cont.t, Area.t, Depth.t, 
                  Hydeoperiod_length,bio1.t,bio4.t,bio5.t,
                  bio6.t,bio7.t,
                  bio12.t,bio15.t,bio17.t), 
                ~ scale(.)[, 1])) # Standardizing each column



model_TP_df <- full_df_standardized_TP[,c('TP','Natural_5_qt', 'Aquatic_500_qt', 'Cropland_500_qt', 
                                          'Forest_500_qt', 'Pastures.and.open.nature_500_qt', 
                                          'Urban_500_qt', 'Animals_cont.t', 'Area.t', 'Depth.t', 
                                          'Hydeoperiod_length.t','bio1.t','bio4.t','bio5.t',
                                          'bio6.t','bio7.t',
                                          'bio12.t','bio15.t','bio17.t')]

#Best models 
## TP
###GAM gamma model 
TP_gam_model_gamma <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                s(Cropland_500_qt) + s(Forest_500_qt) + 
                s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=7)+
                s(Area.t)+s(Depth.t)+s(Hydeoperiod_length.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
              data = model_TP_df, family = Gamma(link = "log"))

summary(TP_gam_model_gamma)
###selected best gam model
TP_gam_model_gamma_selected <- gam(TP ~ s(Aquatic_500_qt) + 
                                     s(Forest_500_qt) + 
                                     s(Pastures.and.open.nature_500_qt)+s(Animals_cont.t,k=7)+
                                     s(Area.t)+s(Depth.t)+s(bio1.t)+s(bio4.t)+s(bio12.t), 
                                   data = model_TP_df, family = Gamma(link = "log"))

summary(TP_gam_model_gamma_selected)

par(mfrow = c(1, 2), cex = 1.1)

plot(TP_gam_model_gamma_selected$fitted.values, residuals(TP_gam_model_gamma_selected), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

qqnorm(residuals(TP_gam_model_gamma_selected))
qqline(residuals(TP_gam_model_gamma_selected), col = "red")

predictions <- predict(TP_gam_model_gamma_selected, type = "response")
TP_gam_model_gamma_selected$pred.formula
summary(predictions)
dev.off()
plot(TP_gam_model_gamma_selected,page=1)

###linear model 

TP_linear_model <- gam(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                      Forest_500_qt + Pastures.and.open.nature_500_qt + 
                      Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio1.t+bio4.t+bio5.t+bio12.t, data = full_df_standardized_TP)
summary(TP_linear_model)

TP_linear_model_selected <-gam(TP ~ Natural_5_qt + Animals_cont.t + Depth.t + bio12.t, data = full_df_standardized_TP)
summary(TP_linear_model_selected)
AIC(TP_linear_model_selected)

TP_glm_reg <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                    Forest_500_qt + Pastures.and.open.nature_500_qt + 
                    Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio4.t+bio5.t+
                    bio12.t+bio1.t, data = full_df_standardized_TP)


#Stepwise selected 
TP_glm_reg_stepwise <- glm(TP ~ Natural_5_qt + Forest_500_qt + Pastures.and.open.nature_500_qt + 
                             Animals_cont.t + Depth.t + bio5.t + bio12.t, data = full_df_standardized_TP)

summary(TP_glm_reg_stepwise)
AIC(TP_glm_reg_stepwise)

 1 - (45.76 / 59.98)
 
 ## TN
 ###GAMM gamma model 
 
TN_gam_model_re <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                       s(Cropland_500_qt) + s(Forest_500_qt) + 
                       s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                       s(Animals_cont.t, k = 7) + s(Area.t) + s(Depth.t) +
                       s(Hydeoperiod_length.t) + s(bio1.t) + s(bio4.t) +
                       s(bio5.t) + s(bio12.t) + 
                       s(Country, bs = "re"),  # Adding Country as a random effect
                     data = model_TN_df, family = Gamma(link = "log"))

summary(TN_gam_model_re)
library(gam)
gam.check(TN_gam_model_re)
step.Gam(TN_gam_model_re)


stepAIC(TN_gam_model_re,na.action=na.omit)
### selected best gam mixed effect model 

TN_gam_model_re_selected <- gam(TN ~ s(Forest_500_qt) + 
                         s(Pastures.and.open.nature_500_qt) + s(Depth.t) +s(Country, bs = "re"),  # Adding Country as a random effect
                       data = model_TN_df, family = Gamma(link = "log"))
summary(TN_gam_model_re_selected)

###Diagnostics 
 par(mfrow= c(1, 2), cex = 1.1)
 
plot(TN_gam_model_re$fitted.values, residuals(TN_gam_model_re), 
      xlab = "Fitted Values", ylab = "Residuals", 
      main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")
 
qqnorm(residuals(TN_gam_model_re))
qqline(residuals(TN_gam_model_re), col = "red")
 
 
predictions <- predict(TN_gam_model_re, type = "response")
summary(predictions)
plot(TN_gam_model_re,page=1)
 
###linear model (gam/glm)
TN_linear_model_re <- gam(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                       Forest_500_qt + Pastures.and.open.nature_500_qt + 
                       Urban_500_qt + Animals_cont.t + Area.t + Depth.t + 
                       Hydeoperiod_length.t + bio1.t + bio4.t + bio5.t + bio12.t +
                       s(Country, bs = "re"),  # Adding Country as a random effect
                     data = model_TN_df)
summary(TN_linear_model_re)
 
TN_linear_model_selected <-gam(TN ~ Forest_500_qt +Area.t + Depth.t+s(Country, bs = "re") , data = full_df_standardized_TP)
summary(TN_linear_model_selected)
AIC(TN_linear_model_selected)
 
library(nlme)
  
TN_glmm <- lme(fixed = TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                 Forest_500_qt + Pastures.and.open.nature_500_qt +
                 Urban_500_qt + Animals_cont.t + Area.t + Depth.t + 
                 Hydeoperiod_length.t + bio1.t + bio4.t + bio5.t + bio12.t,
               random = ~1 | Country,   # Random intercept for Country
               data = model_TN_df,  
               method = "ML",  # Maximum likelihood estimation
               na.action = na.omit,  # Handle missing values
              control = lmeControl(opt = "optim"))  # Optional optimizer control)
 
 
stepAIC(TN_glmm,control=lmeControl(opt = "optim"),na.action=na.omit)
 
TN_glmm <- lme(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                 Forest_500_qt + Pastures.and.open.nature_500_qt + 
                 Urban_500_qt + Animals_cont.t + Area.t + Depth.t + 
                 Hydeoperiod_length.t + bio1.t + bio4.t + bio5.t + bio12.t + 
                 (1 | Country), data = model_TN_df)
stepAIC(TN_glmm)
summary(TN_glmm)
AIC(TP_glm_reg_stepwise)
 
#Stepwise selected 
TP_glm_reg <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                    Forest_500_qt + Pastures.and.open.nature_500_qt + 
                    Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio4.t+bio5.t+
                    bio12.t+bio1.t, data = full_df_standardized_TP)
 1 - (45.76 / 59.98)

#Eco climatic regions
phy_che$bioregion <- 'Temprate'
phy_che[phy_che$Country == 'Spain',]$bioregion <- 'Mediterranean'
phy_che[phy_che$Country == 'Turkey',]$bioregion <- 'Continental'
phy_che[phy_che$Country == 'Uruguay',]$bioregion <- 'Subtropical'

#Correlation plots

library(corrplot)
corrplot(cor(path_df), type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)


# analysing on what MAPK activity depend
MAPK_df = cbind(MAPK = path_df$MAPK, PI3K = path_df$PI3K,
                KSR1 = RNA$KSR1, KSR2 = RNA$KSR2, IQGAP1 = RNA$IQGAP1, 
                IQGAP2 = RNA$IQGAP2, IQGAP3 = RNA$IQGAP3, GAB1 = RNA$GAB1, GAB2 = RNA$GAB2,
                KRAS = RNA$KRAS, NRAS = RNA$NRAS, HRAS = RNA$HRAS, BRAF=RNA$BRAF, 
                CRAF=RNA$RAF1, ARAF=RNA$ARAF)
corrplot(cor(MAPK_df), type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)
