library(dplyr)
library(mgcv)
library(ggplot2)
library(nlme)

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
###GAM gamma model 
TP_gam_model_noslt <- mgcv::gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                                  s(Cropland_500_qt) + s(Forest_500_qt) + 
                                  s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=3)+
                                  s(Area.t)+s(Depth.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                                data = model_TP_df, family = Gamma(link = "log"))
summary(TP_gam_model_noslt)

test <- gam(TP ~ s(Aquatic_500_qt) + 
              s(Forest_500_qt) + 
              s(Animals_cont.t,k=3)+
              s(Depth.t)+s(bio1.t)+s(bio5.t)+s(bio12.t), 
            data = model_TP_df, family = Gamma(link = "log"),,Select=TRUE)
summary(test)



TP_gam_model_gamma <- mgcv::gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                s(Cropland_500_qt) + s(Forest_500_qt) + 
                s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=3)+
                s(Area.t)+s(Depth.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
              data = model_TP_df, family = Gamma(link = "log"),select=TRUE)
step(TP_gam_model_gamma)

library(gamlss)
# Fit the model with the gamlss package
model <- gamlss(TP ~ pb(Aquatic_500_qt) + pb(Forest_500_qt) + pb(Pastures.and.open.nature_500_qt) +
                  Animals_cont.t + pb(Area.t) + Depth.t + pb(bio1.t) + pb(bio4.t) + pb(bio12.t),
                family = GA(), data = model_TP_df)  # GA() specifies the Gamma family

summary(model)

final_model <- gamlss(
  TP ~ pb(Aquatic_500_qt) + pb(Forest_500_qt) + pb(Pastures.and.open.nature_500_qt) +
    Animals_cont.t + pb(Area.t) + Depth.t + pb(bio1.t) + pb(bio4.t) + pb(bio12.t),
  family = GA(), 
  data = model_TP_df
)

summary(final_model)
# Final model without pb(Area.t)
refined_model <- gamlss(TP ~  pb(Forest_500_qt) + 
                          Animals_cont.t + Depth.t + pb(bio1.t) +  pb(bio12.t),
                        family = GA(), data = model_TP_df)

# Summary of the refined model
summary(refined_model)


# Fit the GAM model equivalent to the GAMLSS model
gam_model <- gam(
  TP ~ s(Forest_500_qt) + Animals_cont.t + Depth.t +
    s(bio1.t) + s(bio12.t),
  family = Gamma(link = "log"),
  data = model_TP_df
)

# Display the summary of the GAM model
summary(gam_model)


###removing non sig vars

TP_gam_model_remove <- gam(TP ~ s(Aquatic_500_qt) + 
                                    + s(Forest_500_qt) + 
                                     s(Pastures.and.open.nature_500_qt)+Animals_cont.t+
                                     s(Area.t)+Depth.t+s(bio1.t)+s(bio5.t)+s(bio12.t), 
                                   data = model_TP_df, family = Gamma(link = "log"),select=TRUE)

summary(TP_gam_model_remove)

gam.check(TP_gam_model_remove)

plot(test,page=1, shade=TRUE,rug=TRUE)
summary(test)



test <- gam(TP ~ s(Aquatic_500_qt) + 
                             + s(Forest_500_qt) + 
                             s(Pastures.and.open.nature_500_qt)+Animals_cont.t+
                             s(Area.t)+Depth.t+s(bio1.t)+s(bio5.t)+bio12.t, 
                           data = model_TP_df, family = Gamma(link = "log"),select=TRUE)

summary(test)

# Define the scope for model terms
TP_scope <- list(
  Natural_5_qt = c("1", "Natural_5_qt", "s(Natural_5_qt)"),
  Aquatic_500_qt = c("1", "Aquatic_500_qt", "s(Aquatic_500_qt)"),
  Cropland_500_qt = c("1", "Cropland_500_qt", "s(Cropland_500_qt)"),
  Forest_500_qt = c("1", "Forest_500_qt", "s(Forest_500_qt)"),
  Pastures_and_open_nature_500_qt = c("1", "Pastures.and.open.nature_500_qt", "s(Pastures.and.open.nature_500_qt)"),
  Urban_500_qt = c("1", "Urban_500_qt", "s(Urban_500_qt)"),
  Animals_cont_t = c("1", "Animals_cont.t", "s(Animals_cont.t, k=3)"),
  Area_t = c("1", "Area.t", "s(Area.t)"),
  Depth_t = c("1", "Depth.t", "s(Depth.t)"),
  bio1_t = c("1", "bio1.t", "s(bio1.t)"),
  bio4_t = c("1", "bio4.t", "s(bio4.t)"),
  bio5_t = c("1", "bio5.t", "s(bio5.t)"),
  bio12_t = c("1", "bio12.t", "s(bio12.t)")
)

# Run stepwise model selection
TP_gam_model_gamma <- mgcv::gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                                  s(Cropland_500_qt) + s(Forest_500_qt) + 
                                  s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) + 
                                  s(Animals_cont.t, k = 3) + s(Area.t) + 
                                  s(Depth.t) + s(bio1.t) + s(bio4.t) + s(bio5.t) + 
                                  s(bio12.t), 
                                data = model_TP_df, family = Gamma(link = "log"))

# Perform stepwise selection
step_gam_result <- step.Gam(TP_gam_model_gamma, scope = TP_scope, direction = "backward", trace = TRUE)


step.gam(TP_gam_model_gamma)

test <- gam(TP ~ s(Aquatic_500_qt)  +
              s(Forest_500_qt) + 
              s(Pastures.and.open.nature_500_qt)+Animals_cont.t+
              Depth.t+s(bio1.t)+s(bio12.t), 
            data = model_TP_df, family = Gamma(link = "log"),method='REML')
AIC(test)
summary(test)
plot(test,page=2,)

test <- gam(TP ~s(Aquatic_500_qt) + 
              s(Forest_500_qt) + 
              s(Pastures.and.open.nature_500_qt)+s(Animals_cont.t,k=3)+
              s(Depth.t)+s(bio1.t)+s(bio12.t), 
            data = model_TP_df, family = Gamma(link = "log"))

summary(test,sp.criterion=TRUE)


#new set remove 
test <- gam(TP ~ s(Aquatic_500_qt) + 
              s(Forest_500_qt) + 
              s(Pastures.and.open.nature_500_qt)+s(Animals_cont.t,k=3)+
              s(Depth.t)+s(bio1.t)+s(bio4.t)+s(bio12.t), 
            data = model_TP_df, family = Gamma(link = "log"))


summary(test,sp.criterion=TRUE)

test <- gam(TP ~ s(Aquatic_500_qt) + 
              s(Forest_500_qt) + 
              s(Pastures.and.open.nature_500_qt)+s(Animals_cont.t,k=3)+
              s(Depth.t)+s(bio1.t)+s(bio4.t)+s(bio12.t), 
            data = model_TP_df, family = Gamma(link = "log"))


summary(test,sp.criterion=TRUE)


test <- gam(TP ~ s(Aquatic_500_qt,k=2) + 
              Forest_500_qt + 
              s(Pastures.and.open.nature_500_qt,k=5)+Animals_cont.t+
              s(Area.t,k=3)+Depth.t+s(bio1.t,k=8)+s(bio4.t,k=3)+s(bio12.t,k=4), 
            data = model_TP_df, family = Gamma(link = "log"))

summary(test,sp.criterion=TRUE)
TP_gam_model_refined <- gam(TP ~ s(Aquatic_500_qt, k = 2) + Forest_500_qt + 
                              s(Pastures.and.open.nature_500_qt, k = 5) + 
                              Animals_cont.t + Depth.t + 
                              s(bio1.t, k = 8) + s(bio4.t, k = 3) + s(bio12.t, k = 4), 
                            data = model_TP_df, family = Gamma(link = "log"))
summary(test)


summary(test,sp.criterion=TRUE)

summary(test,sp.criterion=TRUE)

TP_gam_shrinkage <- gam(TP ~ s(Natural_5_qt, bs = "cs") +
                          s(Aquatic_500_qt, bs = "cs") +
                          s(Cropland_500_qt, bs = "cs") +
                          s(Forest_500_qt, bs = "cs") +
                          s(Pastures.and.open.nature_500_qt, bs = "cs") +
                          s(Urban_500_qt, bs = "cs") +
                          s(Animals_cont.t, k = 3, bs = "cs") +
                          s(Area.t, bs = "cs") +
                          s(Depth.t, bs = "cs") +
                          s(bio1.t, bs = "cs") +
                          s(bio4.t, bs = "cs") +
                          s(bio5.t, bs = "cs") +
                          s(bio12.t, bs = "cs"),
                        family = Gamma(link = "log"), data = model_TP_df)
                        
summary(TP_gam_shrinkage)
test <-  gam(TP ~ 
                                  s(Aquatic_500_qt, bs = "cs") +
                                
                                  s(Forest_500_qt, bs = "cs") +
                                  s(Pastures.and.open.nature_500_qt, bs = "cs") +
                                 
                                  s(Animals_cont.t, k = 3, bs = "cs") +
                                 
                                  s(Depth.t, bs = "cs") +
                                  s(bio1.t, bs = "cs") +
                                  s(bio4.t, bs = "cs") +
                                
                                  s(bio12.t, bs = "cs"),
                                family = Gamma(link = "log"), data = model_TP_df)                      
summary(test)  

TP_gam_shrinkage <- mgcv::gam(TP ~ s(Natural_5_qt, bs = "ts") +
                          s(Aquatic_500_qt, bs = "ts") +
                          s(Cropland_500_qt, bs = "ts") +
                          s(Forest_500_qt, bs = "ts") +
                          s(Pastures.and.open.nature_500_qt, bs = "ts") +
                          s(Urban_500_qt, bs = "ts") +
                          s(Animals_cont.t, k = 3, bs = "ts") +
                          s(Area.t, bs = "ts") +
                          s(Depth.t, bs = "ts") +
                          s(bio1.t, bs = "ts") +
                          s(bio4.t, bs = "ts") +
                          s(bio5.t, bs = "ts") +
                          s(bio12.t, bs = "ts"),
                        family = Gamma(link = "log"), data = model_TP_df,
                        method = "REML")

summary(TP_gam_shrinkage)

library(gam)

summary(TP_gam_model_gamma)

gam.check(TP_gam_model_gamma)
plot(TP_gam_shrinkage,page=2,shade=TRUE,rug=TRUE)
TP_gam_model_gamma <- gam(TP ~ Animals_cont.t+
                            Depth.t+s(bio1.t)+bio5.t+s(bio12.t,k=9), 
                          data = model_TP_df, family = Gamma(link = "log"),method='REML')

gam.fit3(TP_gam_model_gamma)
gam.fit3()gcv(TP_gam_model_gamma)
summary(TP_gam_model_gamma)
vis.gam(TP_gam_model_gamma)
gam.check(TP_gam_model_gamma)
plot(TP_gam_model_gamma,page=1,residual=TRUE,shade=TRUE,rug=TRUE)

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
TN_linear_model <- glm(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt +
                         Forest_500_qt + Pastures.and.open.nature_500_qt +
                         Urban_500_qt + Animals_cont.t + Area.t + Depth.t +
                         bio1.t + bio4.t + bio5.t + bio12.t, 
                       data = model_TN_df)
  

summary(TN_linear_model)
stepAIC(TN_linear_model, direction = "both")

1-(45.76/)

###stepwise selection 
TP_linear_model_selected <- glm(formula =  TP ~ Natural_5_qt + Forest_500_qt + Pastures.and.open.nature_500_qt + 
                                  Animals_cont.t + Depth.t + bio5.t + bio12.t, data = model_TP_df)


summary(TP_linear_model_selected)
AIC(TP_linear_model_selected)
MASS::
###Disgnostics (Deviance explained= 1-(residual/null))
 1 - (45.76 / 59.99)
library(lme4)

TP_linear_model_selected.rsquared()
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
                        data = model_TN_df, family = Gamma(link = "log"),select=TRUE)
summary(TN_gam_model)

TN_gam_model <- gam(TN ~  s(Forest_500_qt) + 
                      s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                       s(Depth.t)+s(bio1.t) + s(bio4.t) +
                      s(bio12.t),
                    data = model_TN_df, family = Gamma(link = "log"),SELECT=TRUE)
summary(TN_gam_model)
#GAM with TN

# Adjust margins
par(mar = c(4, 4, 2, 2))  # Adjust these values if needed

# Replot
plot(TN_gam_model, page = 2, rug = TRUE, shade = TRUE)


plot(TN_gam_model,page=2,rug=TRUE,shade=TRUE)







TN_gam_model <- gam(TN ~  s(Forest_500_qt,k=5) + TN_gam_model <- gam(TN ~  s(ForTRUEest_500_qt,k=5) + 
                      Animals_cont.t + s(Depth.t,k=4)
                    + s(bio1.t,k=5) + s(bio12.t,k=4)+s(bio4.t,k=9) ,
                    data = model_TN_df, family = Gamma(link = "log"))
summary(TN_gam_model)
gam.check(TN_gam_model)

###Selected gam model
TN_gam_model_selected <- gam(TN ~ Forest_500_qt + 
                               Depth.t+
                             + s(bio1.t,k=6) + s(bio12.t,k=7)+s(bio4.t,k=8) ,
                             data = model_TN_df, family = Gamma(link = "log"))
summary(TN_gam_model_selected)
plot(TN_gam_model_selected,page=1,shade=TRUE,rug=TRUE)


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

 

