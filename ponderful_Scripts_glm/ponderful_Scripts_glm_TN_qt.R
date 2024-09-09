library(dplyr)
phy_che <- read.csv('/Users/lidiayung/PhD_project/project_PONDERFUL/ponderful_OUTPUT/Phy_Che_qt.csv')

library(lme4)
library(nlme)

library(dplyr)
library(corrplot)
library(devtools)
library(factoextra)
library(tidyverse)
library(ggplot2)
library(MASS)


phy_che$bioregion <- 'Temprate'
phy_che[phy_che$Country == 'Spain',]$bioregion <- 'Mediterranean'
phy_che[phy_che$Country == 'Turkey',]$bioregion <- 'Continental'
phy_che[phy_che$Country == 'Uruguay',]$bioregion <- 'Subtropical'

phy_che$Country <- as.factor(phy_che$Country)
levels(phy_che$Country)


hist(phy_che$TN)
full_df_standardized_TN <- phy_che %>%
  mutate(across(c(Natural_5_qt, Aquatic_500_qt, Cropland_500_qt, 
                  Forest_500_qt, Pastures.and.open.nature_500_qt, 
                  Urban_500_qt, Animals_cont.t, Area.t, Depth.t, 
                  Hydeoperiod_length,bio1.t,bio4.t,bio5.t,
                  bio6.t,bio7.t,
                  bio12.t,bio15.t,bio17.t), 
                ~ scale(.)[, 1])) # Standardizing each column
library(car)
# Check for multicollinearity
vif_data <- full_df_standardized_TN %>%
  dplyr::select( TN, Natural_5_qt,Aquatic_500_qt ,Cropland_500_qt , Forest_500_qt , Pastures.and.open.nature_500_qt , Urban_500_qt, Animals_cont , Area , Depth  , Hydeoperiod_length,
                 ,bio4.t,bio5.t,bio12.t)

# Calculate VIF values
vif_values <- vif(lm( TN ~ ., data = vif_data))

vif_values                 

library(mgcv)
###linear model
linear_model <- gam(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                      Forest_500_qt + Pastures.and.open.nature_500_qt + 
                      Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio1.t+bio4.t+bio5.t+bio12.t, data = full_df_standardized_TN)
summary(linear_model)

gam_model_linear <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                          s(Cropland_500_qt) + s(Forest_500_qt) + 
                          s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=7)+
                          s(Area.t)+s(Depth.t)+s(Hydeoperiod_length.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                        data = full_df_standardized_TN)


summary(gam_model_linear)


gam_model_linear_re <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                      s(Cropland_500_qt) + s(Forest_500_qt) + 
                      s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                      s(Animals_cont.t, k = 7) + s(Area.t) + s(Depth.t) +
                      s(Hydeoperiod_length.t) + s(bio1.t) + s(bio4.t) +
                      s(bio5.t) + s(bio12.t) + 
                      s(Country, bs = "re"),  # Adding Country as a random effect
                    data = full_df_standardized_TN)
summary(gam_model_linear_re)


###poisson model
poisson_model <- gam(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                      Forest_500_qt + Pastures.and.open.nature_500_qt + 
                      Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio1.t+bio4.t+bio5.t+bio12.t, data = full_df_standardized_TN,family=poisson)
summary(poisson_model)


gam_model_poi <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                          s(Cropland_500_qt) + s(Forest_500_qt) + 
                          s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=7)+
                          s(Area.t)+s(Depth.t)+s(Hydeoperiod_length.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                        data = full_df_standardized_TN,family = poisson)

summary(gam_model_poi)
###glm lasso
library("glmnet")

x <- as.matrix(full_df_standardized_TN[, c('Natural_5_qt', 'Aquatic_500_qt', 'Cropland_500_qt', 
                                           'Forest_500_qt', 'Pastures.and.open.nature_500_qt', 
                                           'Urban_500_qt', 'Animals_cont.t', 'Area.t', 'Depth.t', 
                                           'Hydeoperiod_length.t','bio1.t','bio4.t','bio5.t','bio12.t')])
y <- full_df_standardized_TN$TN

which(is.na(x))
which(is.na(y))

x <- x[-c(212,200),]
y<-y[-c(212,200)]


TN_glm <- glmnet(x = x, y = y,family='poisson',alpha=1)

print(TN_glm)
cvfit<- cv.glmnet(x = x, y = y,family='poisson',alpha=1,measure='c')
plot(cvfit)


cvfit$lambda.min
cfs = coef(TN_glm,s=  cvfit$lambda.min)

meaning_coefs = rownames(cfs)[cfs[,1]!= 0]
meaning_vals = cfs[cfs[,1]!=0,]
meaning_vals


###gam model
gamma_model <- glm(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                     Forest_500_qt + Pastures.and.open.nature_500_qt + 
                     Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio4.t+bio5.t+
                     bio12.t+bio1.t,
                   data = full_df_standardized_TN, family = Gamma(link = "log"))

summary(gamma_model)

gam_model_simple <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                          s(Cropland_500_qt) + s(Forest_500_qt) + 
                          s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=7)+
                          s(Area.t)+s(Depth.t)+s(Hydeoperiod_length.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                        data = full_df_standardized_TN, family = Gamma(link = "log"))


summary(gam_model_simple)

#gam model + random effect 

gam_model_re <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                      s(Cropland_500_qt) + s(Forest_500_qt) + 
                      s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                      s(Animals_cont.t, k = 7) + s(Area.t) + s(Depth.t) +
                      s(Hydeoperiod_length.t) + s(bio1.t) + s(bio4.t) +
                      s(bio5.t) + s(bio12.t) + 
                      s(Country, bs = "re"),  # Adding Country as a random effect
                    data = full_df_standardized_TN, family = Gamma(link = "log"))
summary(gam_model_re)

plot(gam_model_re)
#inverse Gaussian 
gam_model_igau <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                      s(Cropland_500_qt) + s(Forest_500_qt) + 
                      s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                      s(Animals_cont.t, k = 7) + s(Area.t) + s(Depth.t) +
                      s(Hydeoperiod_length.t) + s(bio1.t) + s(bio4.t) +
                      s(bio5.t) + s(bio12.t),
                    data = full_df_standardized_TN, family = inverse.gaussian(link = "log"))

summary(gam_model_igau)

gam_model_igau_re <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                        s(Cropland_500_qt) + s(Forest_500_qt) + 
                        s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                        s(Animals_cont.t, k = 7) + s(Area.t) + s(Depth.t) +
                        s(Hydeoperiod_length.t) + s(bio1.t) + s(bio4.t) +
                        s(bio5.t) + s(bio12.t) + 
                        s(Country, bs = "re"),  # Adding Country as a random effect
                      data = full_df_standardized_TN, family = inverse.gaussian(link = "log"))


summary(gam_model_igau_re)

# Extract the null deviance and residual deviance
null_dev <- gamma_model$null.deviance
res_dev <- gamma_model$deviance

# Calculate deviance explained
deviance_explained <- 1 - (res_dev / null_dev)
deviance_explained

predictions <- predict(gamma_model, type = "response")
plot(gamma_model)


predictions <- predict(gamma_model,newdata=full_df_standardized_TN,type='response')
plot(predictions)
hist(full_df_standardized_TN$TN)

hist(full_df_standardized_TP$TP)
plot()###Mixed model
library(lme4)

# Create a factor variable indicating the segment
full_df_standardized_TN$segment <- ifelse(full_df_standardized_TN$TN <= 1, "low", "high")

# Fit a mixed model with different intercepts for each segment
mixed_model <- lmer(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                      Forest_500_qt + Pastures.and.open.nature_500_qt + 
                      Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio1.t+bio4.t+bio5.t+bio12.t + (1 | segment), data = full_df_standardized_TN)
summary(mixed_model)

null_model <- lmer(TN ~1 + (1|segment),data =full_df_standardized_TN)
# Calculate log-likelihood for both models
null_logLik <- logLik(null_model)
fitted_logLik <- logLik(mixed_model)

# Convert log-likelihood to deviance
null_deviance <- -2 * as.numeric(null_logLik)
fitted_deviance <- -2 * as.numeric(fitted_logLik)

# Print the deviance values
cat("Null Model Deviance:", null_deviance, "\n")
cat("Fitted Model Deviance:", fitted_deviance, "\n")

# Calculate the proportion of deviance explained
deviance_explained <- (null_deviance - fitted_deviance) / null_deviance

# Print the deviance explained
cat("Proportion of Deviance Explained:", deviance_explained * 100, "%\n")



# Calculate the deviance for both models
null_deviance <- deviance(null_model)
fitted_deviance <- deviance(mixed_model)

# Print the deviance values
cat("Null Model Deviance:", null_deviance, "\n")
cat("Fitted Model Deviance:", fitted_deviance, "\n")


##gamma model selected

TN_gamma_model <- glm(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                        Forest_500_qt + Pastures.and.open.nature_500_qt + 
                        Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio4.t+bio5.t+
                        bio1.t+ bio12.t,
                      data = full_df_standardized_TN, family = Gamma(link = "log"))

summary(TN_gamma_model)

null_dev <- TN_gamma_model$null.deviance
res_dev <- TN_gamma_model$deviance

# Calculate deviance explained
deviance_explained <- 1 - (res_dev / null_dev)
deviance_explained

#selected

gamma_model_selected <- glm(TN ~ Natural_5_qt + Animals_cont + Area + Depth + bio5.t+bio12.t,
                   data = full_df_standardized_TN, family = Gamma(link = "log"))

plot(gamma_model_selected)

gamm_slope <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                    s(Cropland_500_qt) + s(Forest_500_qt) + 
                    s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=7)+
                    s(Area.t,bs='re')+s(Depth.t)+s(Hydeoperiod_length.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                  data = full_df_standardized_TN, family = Gamma(link = "log"))

summary(gamm_slope)


#TN

full_df_standardized_TN <- phy_che %>%
  mutate(across(c(Natural_5_qt, Aquatic_500_qt, Cropland_500_qt, 
                  Forest_500_qt, Pastures.and.open.nature_500_qt, 
                  Urban_500_qt, Animals_cont, Area, Depth, 
                  Hydeoperiod_length,bio1.t,bio4.t,bio5.t,
                  bio6.t,bio7.t,
                  bio12.t,bio15.t,bio17.t), 
                ~ scale(.)[, 1])) # Standardizing each column

hist(full_df_standardized_TN$TN)



tn_gamma_model <- glm(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                     Forest_500_qt + Pastures.and.open.nature_500_qt + 
                     Urban_500_qt + Animals_cont + Area + Depth + Hydeoperiod_length+bio4.t+bio5.t+
                     bio6.t+bio7.t+
                     bio12.t+bio15.t,
                   data = full_df_standardized_TN, family = Gamma(link = "log"))

summary(tn_gamma_model)

for (i in unique(full_df_standardized_TN$Country)) {
  hist(full_df_standardized_TN[full_df_standardized_TN$Country == i, ]$TN, 
       main = paste("Histogram for", i), 
       xlab = "TN", 
       col = "lightblue")
}

library(fitdistrplus)

TN <- na.omit(full_df_standardized_TN$TN)
descdist(c(TN),discrete=FALSE)





predictions <- predict(gamma_model, type = "response")
plot(tn_gamma_model)

predictions <- predict(gamma_model,newdata=full_df_standardized_TN,type='response')
plot(predictions)
plot(full_df_standardized_TN$TN)



# Fit the GAM
gam_model <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + s(Cropland_500_qt) + 
                   s(Forest_500_qt) + s(Pastures.and.open.nature_500_qt) + 
                   s(Urban_500_qt) + s(Animals_cont) + s(Area) + s(Depth) + 
                   s(Hydeoperiod_length) +  s(bio5.t) + s(bio12.t) , 
                 data = full_df_standardized_TN, family = Gamma(link = "log"))

gam_model <- gam(TN ~ s(Natural_5_qt, k = 5) + s(Aquatic_500_qt, k = 5) + 
                   s(Cropland_500_qt, k = 5) + s(Forest_500_qt, k = 5) + 
                   s(Pastures.and.open.nature_500_qt, k = 5) + s(Urban_500_qt, k = 5) + 
                   s(Animals_cont, k = 5) + s(Area, k = 5) + s(Depth, k = 5) + 
                   s(Hydeoperiod_length, k = 5) + s(bio4.t, k = 5) + s(bio5.t, k = 5) + 
                   s(bio6.t, k = 5) + s(bio7.t, k = 5) + s(bio12.t, k = 5) + 
                   s(bio15.t, k = 5), 
                 data = full_df_standardized_TN, family = Gamma(link = "log"))


# Model summary
summary(gam_model)

# Deviance comparison
print(deviance(gamma_model))
print(deviance(gam_model))

# AIC comparison
print(AIC(gamma_model))
print(AIC(gam_model))

# Diagnostic plots
plot(gam_model$fitted.values, residuals(gam_model), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

qqnorm(residuals(gam_model))
qqline(residuals(gam_model), col = "red")

plot(gam_model, pages = 1)

# Predictions
predictions <- predict(gam_model, newdata = full_df_standardized_TN)


gam_model_simple <- gam(TN ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                          s(Cropland_500_qt) + s(Forest_500_qt) + 
                          s(Pastures.and.open.nature_500_qt), 
                        data = full_df_standardized_TN, family = Gamma(link = "log"))

summary(gam_model_simple)


vif(lm(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
         Forest_500_qt + Pastures.and.open.nature_500_qt + 
         Urban_500_qt + Animals_cont + Area + Depth + Hydeoperiod_length, 
       data = full_df_standardized_TN))


#inverse Gaussian Regression
inv_gaussian_model <- glm(TN ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                            Forest_500_qt + Pastures.and.open.nature_500_qt + 
                            Urban_500_qt + Animals_cont + Area + Depth + Hydeoperiod_length, 
                          data = full_df_standardized_TN, 
                          family = inverse.gaussian(link = "log"),
                          control = glm.control(maxit = 100))
par(mfrow = c(2, 2))
plot(inv_gaussian_model)


full_model_TN_arable <- lmer( TN ~  Natural_5_qt+Aquatic_500_qt+Cropland_500_qt+ Forest_500_qt+ Pastures.and.open.nature_500_qt+ Urban_500_qt+ Animals_cont+ Area + Depth+ Hydeoperiod_length +
                                (1 | Country) + 
                                (0 + Natural_5_qt | Country) + 
                                (0 + Aquatic_500_qt | Country) + 
                                (0 + Cropland_500_qt | Country) + 
                                (0 + Forest_500_qt | Country) +
                                (0+Pastures.and.open.nature_500_qt|Country)+
                                (0 + Urban_500_qt | Country)+
                                (0 + Animals_cont | Country)+
                                (0 + Hydeoperiod_length| Country),
                              data = full_df_standardized_TN, na.action = na.omit)


summ(full_model_TN_arable)

null_deviance <- inv_gaussian_model$null.deviance
residual_deviance <- inv_gaussian_model$deviance
deviance_explained <- 1 - (residual_deviance / null_deviance)
deviance_explained

aic_value <- AIC(inv_gaussian_model)
aic_value

residuals <- residuals(inv_gaussian_model, type = "deviance")
plot(fitted(inv_gaussian_model), residuals)
abline(h = 0, col = "red")


#AIC 
full_model_TN_ml <- lme(fixed=TN ~ Natural_5_qt+Aquatic_500_qt+Cropland_500_qt+ Forest_500_qt+ Pastures.and.open.nature_500_qt+ Urban_500_qt+ Animals_cont+ Area + Depth+ Hydeoperiod_length+T1+P1,
                        random = list(~1 | Country, ~0 + Natural_5_qt | Country, ~0 + Aquatic_500_qt | Country, ~0 + Cropland_500_qt | Country, 
                                      ~0 + Pastures.and.open.nature_500_qt | Country, ~0 + Forest_500_qt | Country, ~0 + Urban_500_qt | Country, ~0 + Animals_cont | Country, ~0 + Hydeoperiod_length | Country),
                        data = full_df_standardized_TN, method = 'ML', na.action = na.omit,control = lmeControl(opt = "optim")
)

stepAIC(full_model_TN_ml,control=lmeControl(opt = "optim"))

aic_model_TN<- lme(fixed= TN ~ Forest_500_qt + Pastures.and.open.nature_500_qt + Animals_cont +      Area + Depth ,
                   random = list( ~1|Country,~ 0+Open_nature_100|Country, ~0+Forest_100|Country,~0+Animals_cont|Country,  ~0+Area|Country, ~0+Depth|Country),
                   data = full_df_standardized_TN, method='ML',na.action = na.omit)

summary(aic_model_TN)


