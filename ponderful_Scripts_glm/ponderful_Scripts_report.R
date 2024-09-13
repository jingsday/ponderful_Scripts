library(dplyr)
library(mgcv)
library(ggplot2)

#Loading input
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

#Part II: Modelling
## TP
###GAM gamma model 
TP_gam_model_gamma <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                s(Cropland_500_qt) + s(Forest_500_qt) + 
                s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=5)+
                s(Area.t)+s(Depth.t)+bio1.t+s(bio4.t)+s(bio5.t)+s(bio12.t), 
              data = model_TP_df, family = Gamma(link = "log"))

summary(TP_gam_model_gamma)
gam.check(TP_gam_model_gamma)
plot(TP_gam_model_gamma,page=2,shade=TRUE,rug=TRUE)
TP_gam_model_gamma <- gam(TP ~  s(Aquatic_500_qt) + 
                            Forest_500_qt + 
                            s(Pastures.and.open.nature_500_qt)+s(Animals_cont.t,k=7)+
                            s(Area.t)+Depth.t+s(bio1.t)+s(bio4.t)+s(bio12.t), 
                          data = model_TP_df, family = Gamma(link = "log"))

summary(TP_gam_model_gamma)
gam.check(TP_gam_model_gamma)
plot(TP_gam_model_gamma,page=1,shade=TRUE,rug=TRUE)

###selected best gam model
TP_gam_model_gamma_selected <- gam(TP ~  s(Aquatic_500_qt,k=2) + 
                                     Forest_500_qt + 
                                     s(Pastures.and.open.nature_500_qt,k=3)+s(Animals_cont.t,k=7)+
                                     s(Area.t,k=2)+Depth.t+s(bio1.t,k=9)+s(bio4.t,k=3)+s(bio12.t,k=4), 
                                   data = model_TP_df, family = Gamma(link = "log"))

summary(TP_gam_model_gamma_selected)
AIC(TP_gam_model_gamma)
plot(TP_gam_model_gamma_selected,page=1,shade=TRUE,rug=TRUE)
gam.check(TP_gam_model_gamma_selected)
par(mfrow = c(1, 2), cex = 1.1)

plot(TP_gam_model_gamma_selected$fitted.values, residuals(TP_gam_model_gamma_selected), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

qqnorm(residuals(TP_gam_model_gamma_selected))
qqline(residuals(TP_gam_model_gamma_selected), col = "red")

predictions <- predict(TP_gam_model_gamma_selected, type = "response")

summary(predictions)
dev.off()
plot(predictions)

###linear model 
library(MASS)
TP_linear_model <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                         Forest_500_qt + Pastures.and.open.nature_500_qt +
                         Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                         bio1.t + bio4.t + bio5.t + bio12.t, 
                       data = model_TP_df)
  

summary(TP_linear_model)
stepAIC(TP_linear_model, direction = "both")


###stepwise selection 
TP_linear_model_selected <- glm(formula =  TP ~ Natural_5_qt + Forest_500_qt + Pastures.and.open.nature_500_qt + 
                                  Animals_cont.t + Depth.t + bio5.t + bio12.t, data = model_TP_df)


summary(TP_linear_model_selected)
AIC(TP_linear_model_selected)

###Disgnostics (Deviance explained= 1-(residual/null))
 1 - (45.76 / 59.99)
par(mfrow = c(2, 2))


plot(TP_linear_model_selected)
 
 
## TN
 ###GAMM gamma model 
TN_gam_model <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                          s(Cropland_500_qt) + s(Forest_500_qt) + 
                          s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                          s(Animals_cont.t, k = 7) + s(Area.t) + s(Depth.t)
                        + s(bio1.t) + s(bio4.t) +
                          s(bio5.t) + s(bio12.t),
                        data = model_TN_df, family = Gamma(link = "log"))
summary(TN_gam_model)
###Selected gam model
TN_gam_model_selected <- gam(TN ~ s(Forest_500_qt) + 
                               s(Pastures.and.open.nature_500_qt) +
                               s(Animals_cont.t, k = 7) + s(Depth.t)
                             + s(bio1.t) + s(bio4.t) +
                               s(bio5.t) + s(bio12.t),
                             data = model_TN_df, family = Gamma(link = "log"))
summary(TN_gam_model_selected)

TN_gam_model_re <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                       s(Cropland_500_qt) + s(Forest_500_qt) + 
                       s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                       s(Animals_cont.t, k = 7) + s(Area.t) + s(Depth.t)
                        + s(bio1.t) + s(bio4.t) +
                       s(bio5.t) + s(bio12.t) + 
                       s(Country, bs = "re"),  
                     data = model_TN_df, family = Gamma(link = "log"))
summary(TN_gam_model_re)


TN_gam_model_re <- gam(TN ~  s(Forest_500_qt) + 
                         s(Pastures.and.open.nature_500_qt) + s(Depth.t)+
                         s(Country, bs = "re"),  
                       data = model_TN_df, family = Gamma(link = "log"))
summary(TN_gam_model_re)

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

###linear model 
any(is.na(model_TN_df))
model_TN_df <-na.omit(model_TN_df)
TN_linear_model <- glm(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                         Forest_500_qt + Pastures.and.open.nature_500_qt +
                         Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                         bio1.t + bio4.t + bio5.t + bio12.t, 
                       data = model_TN_df)


summary(TN_linear_model)

stepAIC(TN_linear_model, direction = "both")


###stepwise selection 
TN_linear_model_selected <- glm(formula = TN ~ Forest_500_qt + Pastures.and.open.nature_500_qt + 
                                 Area.t + Depth.t + bio4.t + bio5.t, data = model_TN_df)


summary(TN_linear_model_selected)
#Diagnostics
 1 - (45.76 / 59.98)



#PART III: Bio-climatic regions 
##Correlation plots
library(corrplot)

input<- ''#Temperate, Mediterranean, Continental, Subtropical

df <-na.omit(df)
corrplot(cor(df), type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)
cor(df)

### Bio-climatic regions TP model
####TP Medi
input<- 'Mediterranean'
df <-model_TP_df[model_TP_df$bioregion == input,][,c('TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t','bio4.t','bio5.t','bio12.t')]
hist(df$TP)

TP_gam_medi <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                     s(Cropland_500_qt) +
                     s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                     s(Animals_cont.t) + s(Depth.t)+ s(bio1.t),  
                   data = df, family = Gamma(link = "log"))

length(unique(df$Aquatic_500_qt))
summary(TP_gam_medi)
gam.check(TP_gam_medi)

TP_gam_medi_selected<-gam(TP ~ Natural_5_qt + s(Cropland_500_qt,k=3) + Urban_500_qt +Animals_cont.t+bio1.t,
                         data = df, family = Gamma(link = "log"))
summary(TP_gam_medi_selected)
plot(TP_gam_medi_selected,page=1,rug=TRUE,shade=TRUE)
AIC(TP_gam_medi_selected)

###Diagnostics 

par(mfrow = c(2, 2))
gam.check(TP_gam_medi_selected)

predictions <- predict(TP_gam_medi_selected, type = "response")
summary(predictions)

####TP Continental
input <-'Continental'
df <-model_TP_df[model_TP_df$bioregion == input,][,c('TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Forest_500_qt','bio7.t')]
hist(df$TP)

TP_gam_ctn <- gam(TP ~ Natural_5_qt + s(Forest_500_qt,k=3)+s(Cropland_500_qt,k=2) +
                    s(Urban_500_qt,k=) + Depth.t+ bio7.t,  
                   data = df, family = Gamma(link = "log"))

summary(TP_gam_ctn)
plot(TP_gam_ctn)

TP_gam_ctn <- gam(TP ~ s(Forest_500_qt,k=5)+ Cropland_500_qt +s(Urban_500_qt,k=3) +  bio7.t,
                  data = df, family = Gamma(link = "log"))

TP_gam_ctn_selected <- gam(TP ~ s(Forest_500_qt) + Cropland_500_qt + s(Urban_500_qt, k = 2) + s(Depth.t,k=6) + bio7.t,  
                           data = df, family = Gamma(link = "log"))

summary(TP_gam_ctn_selected)
plot(TP_gam_ctn_selected,page=1,shade=TRUE,rug=TRUE)
AIC(TP_gam_ctn)

par(mfrow= c(2, 2), cex = 1.1)
gam.check(TP_gam_ctn_selected)


####TP Temperate
input <-'Temperate'
df <-model_TP_df[model_TP_df$bioregion == input,][,c('TP','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t','bio5.t')]
hist(df$TP)
TP_gam_temp <- gam(TP ~ Natural_5_qt + Aquatic_500_qt + 
                     Cropland_500_qt +
                     Pastures.and.open.nature_500_qt + Urban_500_qt +
                     s(Animals_cont.t,k=4) + Depth.t+s(bio1.t,k=5),  
                   data = df, family = Gamma(link = "log"))
gam.check(TP_gam_temp)
summary(TP_gam_temp)

gam.check(TP_gam_temp)
TP_gam_temp <- gam(TP ~  s(Cropland_500_qt,k=5) +
                    s(Pastures.and.open.nature_500_qt) + 
                    s(Animals_cont.t,k=4) + s(Depth.t)+ s(bio1.t),  
                  data = df, family = Gamma(link = "log"))
summary(TP_gam_temp)

TP_gam_temp_selected <- gam(TP ~ s(Cropland_500_qt, k = 5) + s(Pastures.and.open.nature_500_qt) + 
                              s(Animals_cont.t, k = 4) + s(Depth.t) + s(bio1.t),  
                           data = df, family = Gamma(link = "log"))

summary(TP_gam_temp_selected)
plot(TP_gam_temp_selected,page=1,shade=TRUE,rug=TRUE)
AIC(TP_gam_temp_selected)
####TP Subtropical
input <-'Subtropical'
df <-model_TP_df[model_TP_df$bioregion == input,][,c('TP','Natural_5_qt',  'Cropland_500_qt', 'Animals_cont.t',
                                                     'Pastures.and.open.nature_500_qt','Aquatic_500_qt',
                                                     'Urban_500_qt',  'Depth.t', 'bio1.t')]
hist(df$TP)

TP_gam_subt <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                    s(Cropland_500_qt) +
                    s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                    s(Animals_cont.t, k = 5) + s(Depth.t)+ s(bio1.t),
                  data = df, family = Gamma(link = "log"))
summary(TP_gam_subt)


### Bio-climatic regions TN model

####TN medi
input <- 'Mediterranean'
#medi
df <-model_TN_df[model_TN_df$bioregion == input,][,c('TN','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio1.t','Country')]
hist(df$TN)


TN_gam_medi <- gam(TN ~ s(Natural_5_qt,k=9) + s(Aquatic_500_qt,k=6) + 
                     s(Cropland_500_qt) +
                     s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                     s(Animals_cont.t,k=6) + s(Depth.t)+ s(bio1.t),  
                   data = df, family = Gamma(link = "log"))
length(unique(df$Animals_cont.t))
summary(TN_gam_medi)                                                                                     
####TN continental     
input<- 'Continental'

df <-model_TN_df[model_TN_df$bioregion == input,][,c('TN','Natural_5_qt',  'Cropland_500_qt', 'Urban_500_qt',  'Depth.t', 
                                                     'Forest_500_qt','bio7.t','bio7')] 

TN_gam_ctn <-gam(TN ~ s(Natural_5_qt,k=3) + s(Forest_500_qt,k=7)+
                   s(Cropland_500_qt) + s(Urban_500_qt) +s(Depth.t)+ s(bio7.t),  
                 data = df, family = Gamma(link = "log"))
summary(TN_gam_ctn)
plot(TN_gam_ctn,pages=1,shade=TRUE,rug=TRUE)


TN_gam_ctn_selected <-gam(TN ~ s(Forest_500_qt,k=4)+
                   Cropland_500_qt + Urban_500_qt +Depth.t+ s(bio7.t),  
                 data = df, family = Gamma(link = "log"))
summary(TN_gam_ctn_selected)
gam.check(TN_gam_ctn_selected)
coef(TN_gam_ctn_selected)
TN_gam_ctn_selected$smooth
plot(TN_gam_ctn_selected,pages=1,shade=TRUE,rug=TRUE)


plot(TN_gam_ctn_selected,pages=1,shade=TRUE,rug=TRUE)

#Temperate
input<- 'Temperate'
df <-model_TN_df[model_TN_df$bioregion == input,][,c('TN','Natural_5_qt',  'Cropland_500_qt', 
                                                     'Urban_500_qt',  'Depth.t', 
                                                     'Pastures.and.open.nature_500_qt', 'Aquatic_500_qt','Animals_cont.t', 'bio5.t','Country')]
table(df$Country)
df_country <-model_TN_df[model_TN_df$Country == 'UK',]
hist(df_country$TN)


hist(df$TN)
TN_gam_temp <-gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt)+s(Animals_cont.t,k=5)+s(Pastures.and.open.nature_500_qt)+
                   s(Cropland_500_qt) + s(Urban_500_qt) +s(Depth.t)+ s(bio5.t),  
                 data = df, family = Gamma(link = "log"))
length(unique(df$Animals_cont.t))
summary(TN_gam_temp)


#Sub
input<- 'Subtropical'

df <-model_TN_df[model_TN_df$bioregion == input,][,c('TN','Natural_5_qt',  'Cropland_500_qt', 'Animals_cont.t',
                                                     'Pastures.and.open.nature_500_qt','Aquatic_500_qt',
                                                     'Urban_500_qt',  'Depth.t', 'bio1.t')]  
hist(df$TN)
TN_gam_subt <-gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt)+s(Animals_cont.t,k=7)+s(Pastures.and.open.nature_500_qt)+
                    s(Cropland_500_qt) + s(Urban_500_qt) +s(Depth.t)+ s(bio1.t,k=3),  
                  data = df, family = Gamma(link = "log"))
length(unique(df$bio1.t))
summary(TN_gam_subt)

 

