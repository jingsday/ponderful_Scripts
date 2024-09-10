library(dplyr)
library(tidyverse)
#Loading input and preparation
phy_che <- read.csv('/Users/lidiayung/PhD_project/project_PONDERFUL/ponderful_OUTPUT/Phy_Che_qt.csv')


phy_che$bioregion <- 'Temprate'
phy_che[phy_che$Country == 'Spain',]$bioregion <- 'Mediterranean'
phy_che[phy_che$Country == 'Turkey',]$bioregion <- 'Continental'
phy_che[phy_che$Country == 'Uruguay',]$bioregion <- 'Subtropical'

phy_che$Country <- as.factor(phy_che$Country)
levels(phy_che$Country)

hist(phy_che$TP)
full_df_standardized_TP <- phy_che %>%
  mutate(across(c(Natural_5_qt, Aquatic_500_qt, Cropland_500_qt, 
                  Forest_500_qt, Pastures.and.open.nature_500_qt, 
                  Urban_500_qt, Animals_cont.t, Area.t, Depth.t, 
                  Hydeoperiod_length.t,bio1.t,bio4.t,bio5.t,
                  bio6.t,bio7.t,
                  bio12.t,bio15.t,bio17.t), 
                ~ scale(.)[, 1])) # Standardizing each column
which(is.na(full_df_standardized_TP$TP))

full_df_standardized_TP <- full_df_standardized_TP[-200,]
#Downstream analysis 
library(car)

vif_data <- full_df_standardized_TP %>%
  dplyr::select( TP, Natural_5_qt,Aquatic_500_qt ,Cropland_500_qt , Forest_500_qt , Pastures.and.open.nature_500_qt , Urban_500_qt, Animals_cont , Area , Depth  , Hydeoperiod_length,
                 ,bio4.t,bio5.t,bio12.t)

##Multicollinearity and  Calculate VIF values
vif_values <- vif(lm( TP ~ ., data = vif_data))

vif_values                 

##Models fitting 
library(mgcv)

###linear model (simple, generalized additive model, with random effect)

linear_model <- gam(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                      Forest_500_qt + Pastures.and.open.nature_500_qt + 
                      Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio1.t+bio4.t+bio5.t+bio12.t, data = full_df_standardized_TP)
summary(linear_model)
#par(mfrow = c(1, 2), cex = 1.1)
# Diagnostic plots
plot(linear_model$fitted.values, residuals(linear_model), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

qqnorm(residuals(linear_model))
qqline(residuals(linear_model), col = "red")


# Predictions
predictions <- predict(linear_model, newdata = full_df_standardized_TP)



predictions <- predict(linear_model,newdata=full_df_standardized_TP,type='response')
plot(predictions)

gam_model_linear <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                          s(Cropland_500_qt) + s(Forest_500_qt) + 
                          s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=7)+
                          s(Area.t)+s(Depth.t)+s(Hydeoperiod_length.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                        data = full_df_standardized_TP)
summary(gam_model_linear)



gam_model_linear_re <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                      s(Cropland_500_qt) + s(Forest_500_qt) + 
                      s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                      s(Animals_cont.t, k = 7) + s(Area.t) + s(Depth.t) +
                      s(Hydeoperiod_length.t) + s(bio1.t) + s(bio4.t) +
                      s(bio5.t) + s(bio12.t) + 
                      s(Country, bs = "re"),  # Adding Country as a random effect
                    data = full_df_standardized_TP)

summary(gam_model_linear_re)


###poisson model
poisson_model <- gam(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                      Forest_500_qt + Pastures.and.open.nature_500_qt + 
                      Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio1.t+bio4.t+bio5.t+bio12.t, data = full_df_standardized_TP,family=poisson)
summary(poisson_model)


gam_model_poi <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                          s(Cropland_500_qt) + s(Forest_500_qt) + 
                          s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=7)+
                          s(Area.t)+s(Depth.t)+s(Hydeoperiod_length.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                        data = full_df_standardized_TP,family=poisson)
summary(gam_model_poi)


gam_model_poi_re <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                             s(Cropland_500_qt) + s(Forest_500_qt) + 
                             s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                             s(Animals_cont.t, k = 7) + s(Area.t) + s(Depth.t) +
                             s(Hydeoperiod_length.t) + s(bio1.t) + s(bio4.t) +
                             s(bio5.t) + s(bio12.t) + 
                             s(Country, bs = "re"),  # Adding Country as a random effect
                           data = full_df_standardized_TP,family=poisson)

###glm lasso
library("glmnet")
x <- as.matrix(full_df_standardized_TP[, c('Natural_5_qt', 'Aquatic_500_qt', 'Cropland_500_qt', 
                                           'Forest_500_qt', 'Pastures.and.open.nature_500_qt', 
                                           'Urban_500_qt', 'Animals_cont.t', 'Area.t', 'Depth.t', 
                                           'Hydeoperiod_length','bio1.t','bio4.t','bio5.t','bio12.t')])
y <- full_df_standardized_TP$TP

which(is.na(x))
which(is.na(y))

x <- x[-c(212,200),]
y<-y[-c(212,200)]


TP_glm <- glmnet(x = x, y = y,family='poisson',alpha=1)

print(TP_glm)
cvfit<- cv.glmnet(x = x, y = y,family='poisson',alpha=1,measure='c')
plot(cvfit)


cvfit$lambda.min
cfs = coef(TP_glm,s=  cvfit$lambda.min)

meaning_coefs = rownames(cfs)[cfs[,1]!= 0]
meaning_vals = cfs[cfs[,1]!=0,]
meaning_vals


###gamma model(gamma, gamma additive, gamma re)

gamma_model_simple <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                     Forest_500_qt + Pastures.and.open.nature_500_qt + 
                     Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio4.t+bio5.t+
                     bio12.t+bio1.t,
                   data = full_df_standardized_TP, family = Gamma(link = "log"))

summary(gamma_model_simple)

gam_model_gamma <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                          s(Cropland_500_qt) + s(Forest_500_qt) + 
                          s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=7)+
                          s(Area.t)+s(Depth.t)+s(Hydeoperiod_length.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                        data = full_df_standardized_TP, family = Gamma(link = "log"))

summary(gam_model_gamma)
# Diagnostic plots
par(mfrow = c(1, 2), cex = 1.1)

plot(gam_model_gamma$fitted.values, residuals(gam_model_gamma), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

qqnorm(residuals(gam_model_gamma))
qqline(residuals(gam_model_gamma), col = "red")

plot(gam_model_gamma, pages = 1)

# Predictions
predictions <- predict(gam_model_gamma, newdata = full_df_standardized_TP,type='response')
summary(predictions)

plot(predictions)
summary(predictions)
plot(predictions, full_df_standardized_TP$TP,
     xlab = "Predicted Values", ylab = "Observed TP",
     main = "Predicted vs Observed TP")
abline(0, 1, col = "red")  # Add a 45-degree line for reference


gam_model_re <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                      s(Cropland_500_qt) + s(Forest_500_qt) + 
                      s(Pastures.and.open.nature_500_qt) + s(Urban_500_qt) +
                      s(Animals_cont.t, k = 7) + s(Area.t) + s(Depth.t) +
                      s(Hydeoperiod_length.t) + s(bio1.t) + s(bio4.t) +
                      s(bio5.t) + s(bio12.t) + 
                      s(Country, bs = "re"),  # Adding Country as a random effect
                    data = full_df_standardized_TP, family = Gamma(link = "log"))
summary(gam_model_re)


gam.check(gam_model_simple)
plot(gam_model_simple, all.terms = TRUE)
AIC(gam_model_simple)

# Diagnostic plots
plot(gam_model_simple$fitted.values, residuals(gam_model_simple), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

qqnorm(residuals(gam_model_simple))
qqline(residuals(gam_model_simple), col = "red")

plot(gam_model_simple, pages = 1)

# Predictions
predictions <- predict(gam_model_simple, newdata = full_df_standardized_TP)




### inverse.gaussian(simple, additive, re)
gamma_model_igau_simple <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                     Forest_500_qt + Pastures.and.open.nature_500_qt + 
                     Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio4.t+bio5.t+
                     bio12.t+bio1.t,
                   data = full_df_standardized_TP, family = Gamma(link = "log"))

summary(gamma_model_igau_simple)

gam_model_igau <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                          s(Cropland_500_qt) + s(Forest_500_qt) + 
                          s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=7)+
                          s(Area.t)+s(Depth.t)+s(Hydeoperiod_length.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                        data = full_df_standardized_TP, family = inverse.gaussian(link = "log"))
summary(gam_model_igau)


gam_model_igau_re <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                          s(Cropland_500_qt) + s(Forest_500_qt) + 
                          s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=7)+
                          s(Area.t)+s(Depth.t)+s(Hydeoperiod_length.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t)+s(Country, bs = "re"), 
                        data = full_df_standardized_TP, family = inverse.gaussian(link = "log"))


summary(gam_model_igau_re)

AIC(gam_model_invgau)




ps_model_simple <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                          s(Cropland_500_qt) + s(Forest_500_qt) + 
                          s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=7)+
                          s(Area.t)+s(Depth.t)+s(Hydeoperiod_length.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                        data = full_df_standardized_TP, family = poisson)


summary(ps_model_simple)

#inverse gaussion 
igau_model_simple <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                         s(Cropland_500_qt) + s(Forest_500_qt) + 
                         s(Pastures.and.open.nature_500_qt)+s(Urban_500_qt)+s(Animals_cont.t,k=7)+
                         s(Area.t)+s(Depth.t)+s(Hydeoperiod_length.t)+s(bio1.t)+s(bio4.t)+s(bio5.t)+s(bio12.t), 
                       data = full_df_standardized_TP, family = inverse.gaussian())


summary(igau_model_simple)


###gamma model 
gamma_model <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                     Forest_500_qt + Pastures.and.open.nature_500_qt + 
                     Urban_500_qt + Animals_cont + Area.t + Depth.t + Hydeoperiod_length.t+bio4.t+bio5.t+
                     bio12.t+bio1.t,
                   data = full_df_standardized_TP, family = Gamma(link = "log"))

summary(gamma_model)
# Extract the null deviance and residual deviance
null_dev <- gamma_model$null.deviance
res_dev <- gamma_model$deviance

# Calculate deviance explained
deviance_explained <- 1 - (res_dev / null_dev)
deviance_explained

predictions <- predict(gamma_model, type = "response")
plot(gamma_model)

predictions <- predict(gamma_model,newdata=full_df_standardized_TP,type='response')
plot(predictions)

hist(scale(full_df_standardized_TP$TP))
###Mixed model
library(lme4)

# Create a factor variable indicating the segment
full_df_standardized_TP$segment <- ifelse(full_df_standardized_TP$TP <= 1, "low", "high")

# Fit a mixed model with different intercepts for each segment
mixed_model <- lmer(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                      Forest_500_qt + Pastures.and.open.nature_500_qt + 
                      Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio1.t+bio4.t+bio5.t+bio12.t + (1 | segment), data = full_df_standardized_TP)
summary(mixed_model)

null_model <- lmer(TP ~1 + (1|segment),data =full_df_standardized_TP)
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

###Distribution per country

for (i in unique(full_df_standardized_TP$Country)) {
  hist(full_df_standardized_TP[full_df_standardized_TP$Country == i, ]$TP, 
       main = paste("Histogram for", i), 
       xlab = "TP", 
       col = "lightblue")
}

library(fitdistrplus)

TP <-na.omit(full_df_standardized_TP$TP)
descdist(c(TP),discrete=FALSE)

##gamma model selected

tp_gamma_model <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                        Forest_500_qt + Pastures.and.open.nature_500_qt + 
                        Urban_500_qt + Animals_cont.t + Area.t + Depth.t + Hydeoperiod_length.t+bio4.t+bio5.t+
                        bio1.t+ bio12.t,
                      data = full_df_standardized_TP, family = Gamma(link = "log"))

summary(tp_gamma_model)

null_dev <- tp_gamma_model$null.deviance
res_dev <- tp_gamma_model$deviance

# Calculate deviance explained
deviance_explained <- 1 - (res_dev / null_dev)
deviance_explained

#selected

gamma_model_selected <- glm(TP ~ Natural_5_qt + Animals_cont + Area + Depth + bio5.t+bio12.t,
                   data = full_df_standardized_TP, family = Gamma(link = "log"))

plot(gamma_model_selected)



predictions <- predict(gamma_model, type = "response")
plot(gamma_model)

predictions <- predict(gamma_model,newdata=full_df_standardized_TP,type='response')
plot(predictions)
plot(full_df_standardized_TP$TP)




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
predictions <- predict(gam_model, newdata = full_df_standardized_TP)


gam_model_simple <- gam(TP ~ s(Natural_5_qt) + s(Aquatic_500_qt) + 
                          s(Cropland_500_qt) + s(Forest_500_qt) + 
                          s(Pastures.and.open.nature_500_qt), 
                        data = full_df_standardized_TP, family = Gamma(link = "log"))

summary(gam_model_simple)


vif(lm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
         Forest_500_qt + Pastures.and.open.nature_500_qt + 
         Urban_500_qt + Animals_cont + Area + Depth + Hydeoperiod_length, 
       data = full_df_standardized_TP))


#inverse Gaussian Regression
inv_gaussian_model <- glm(TP ~ Natural_5_qt + Aquatic_500_qt + Cropland_500_qt + 
                            Forest_500_qt + Pastures.and.open.nature_500_qt + 
                            Urban_500_qt + Animals_cont + Area + Depth + Hydeoperiod_length, 
                          data = full_df_standardized_TP, 
                          family = inverse.gaussian(link = "log"),
                          control = glm.control(maxit = 100))
par(mfrow = c(2, 2))
plot(inv_gaussian_model)


full_model_TP_arable <- lmer( TP ~  Natural_5_qt+Aquatic_500_qt+Cropland_500_qt+ Forest_500_qt+ Pastures.and.open.nature_500_qt+ Urban_500_qt+ Animals_cont+ Area + Depth+ Hydeoperiod_length +
                                (1 | Country) + 
                                (0 + Natural_5_qt | Country) + 
                                (0 + Aquatic_500_qt | Country) + 
                                (0 + Cropland_500_qt | Country) + 
                                (0 + Forest_500_qt | Country) +
                                (0+Pastures.and.open.nature_500_qt|Country)+
                                (0 + Urban_500_qt | Country)+
                                (0 + Animals_cont | Country)+
                                (0 + Hydeoperiod_length| Country),
                              data = full_df_standardized_TP, na.action = na.omit)


summ(full_model_TP_arable)

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
full_model_TP_ml <- lme(fixed=TP ~ Natural_5_qt+Aquatic_500_qt+Cropland_500_qt+ Forest_500_qt+ Pastures.and.open.nature_500_qt+ Urban_500_qt+ Animals_cont+ Area + Depth+ Hydeoperiod_length+T1+P1,
                        random = list(~1 | Country, ~0 + Natural_5_qt | Country, ~0 + Aquatic_500_qt | Country, ~0 + Cropland_500_qt | Country, 
                                      ~0 + Pastures.and.open.nature_500_qt | Country, ~0 + Forest_500_qt | Country, ~0 + Urban_500_qt | Country, ~0 + Animals_cont | Country, ~0 + Hydeoperiod_length | Country),
                        data = full_df_standardized_TP, method = 'ML', na.action = na.omit,control = lmeControl(opt = "optim")
)

stepAIC(full_model_TP_ml,control=lmeControl(opt = "optim"))

aic_model_TP<- lme(fixed= TP ~ Forest_500_qt + Pastures.and.open.nature_500_qt + Animals_cont +      Area + Depth ,
                   random = list( ~1|Country,~ 0+Open_nature_100|Country, ~0+Forest_100|Country,~0+Animals_cont|Country,  ~0+Area|Country, ~0+Depth|Country),
                   data = full_df_standardized_TP, method='ML',na.action = na.omit)

summary(aic_model_TP)


