#################################
## Description: Principle component analysis
#################################

library(lme4)
library(dplyr)
library(corrplot)
library(devtools)
library(factoextra)
library(tidyverse)
library(ggplot2)


####################################################################################################################################
# Setup data
####################################################################################################################################

final_df <- read.csv('PhyChe_XY.csv')
final_df$unique_id <- paste0(final_df$PondCode, "_",final_df$MeasurementDate)


# Select earliest MeasurementData for each PondCode
earliest_measurements <- final_df %>%
  group_by(PondCode) %>%
  filter(MeasurementDate == min(MeasurementDate)) %>%
  ungroup()

raw_earliest <-earliest_measurements[,c(1:112)]

barplot(table(raw_earliest$Country),las=2)
duplicate_ponds <- raw_earliest %>%
  filter(duplicated(PondCode))
print(duplicate_ponds) #none

# Normalization 
library(bestNormalize)
shapiro.test(raw_earliest$T)

# T transform
Tn <- bestNormalize(raw_earliest$T)
plot(Tn, leg_loc = "bottomright")
raw_earliest$T.t <- sqrt(raw_earliest$T)
hist(raw_earliest$T.t)
shapiro.test(raw_earliest$T.t)

shapiro.test(raw_earliest$T40)
T40n <- bestNormalize(raw_earliest$T40)
plot(T40n, leg_loc = "bottomright")
raw_earliest$T40.t <- predict(orderNorm(raw_earliest$T40))
hist(raw_earliest$T40.t)
shapiro.test(raw_earliest$T40.t)

shapiro.test(raw_earliest$P1)
P1n <- bestNormalize(raw_earliest$P1)
plot(P1n, leg_loc = "bottomright")
raw_earliest$P1.t <- predict(orderNorm(raw_earliest$P1))
hist(raw_earliest$P1.t)
shapiro.test(raw_earliest$P1.t)

shapiro.test(raw_earliest$P40)
P40n <- bestNormalize(raw_earliest$P40)
plot(P40n, leg_loc = "bottomright")
raw_earliest$P40.t <- predict(orderNorm(raw_earliest$P40))
hist(raw_earliest$P40.t)
shapiro.test(raw_earliest$P40.t)

shapiro.test(raw_earliest$Area)
#remove 1 outlier: 55791.8900
raw_earliest=raw_earliest[!raw_earliest$Area==55791.89,]
shapiro.test(raw_earliest$Area)
Arean <- bestNormalize(raw_earliest$Area)
plot(Arean, leg_loc = "bottomright")
raw_earliest$Area.t <- predict(orderNorm(raw_earliest$Area))
hist(raw_earliest$Area.t)
shapiro.test(raw_earliest$Area.t)

shapiro.test(raw_earliest$Depth)
Depthn <- bestNormalize(raw_earliest$Depth)
plot(Depthn, leg_loc = "bottomright")
raw_earliest$Depth.t <- predict(orderNorm(raw_earliest$Depth))
hist(raw_earliest$Depth.t)
shapiro.test(raw_earliest$Depth.t)
# shapiro.test(raw_earliest$Rel_waterlev)
# Rel_waterlevn <- bestNormalize(raw_earliest$Rel_waterlev)
# plot(Rel_waterlevn, leg_loc = "bottomright")
# raw_earliest$Rel_waterlev.t <- predict(orderNorm(raw_earliest$Rel_waterlev))
# hist(raw_earliest$Rel_waterlev.t)

shapiro.test(raw_earliest$Hetog5)
Hetog5n <- bestNormalize(raw_earliest$Hetog5)
plot(Hetog5n, leg_loc = "bottomright")
raw_earliest$Hetog5.t <- predict(orderNorm(raw_earliest$Hetog5))
hist(raw_earliest$Hetog5.t)
shapiro.test(raw_earliest$Hetog5.t)

shapiro.test(raw_earliest$Hetog100)
Hetog100n <- bestNormalize(raw_earliest$Hetog100)
plot(Hetog100n, leg_loc = "bottomright")
raw_earliest$Hetog100.t <- predict(orderNorm(raw_earliest$Hetog100))
hist(raw_earliest$Hetog100.t)
shapiro.test(raw_earliest$Hetog100.t)

# shapiro.test(raw_earliest$LU_PCA)
# LU_PCAn <- bestNormalize(raw_earliest$LU_PCA)
# plot(LU_PCAn, leg_loc = "bottomright")
# raw_earliest$LU_PCA.t <- predict(orderNorm(raw_earliest$LU_PCA))
# hist(raw_earliest$LU_PCA.t)
# shapiro.test(raw_earliest$LU_PCA.t)

shapiro.test(raw_earliest$Animals_cont)
Animals_contn <- bestNormalize(raw_earliest$Animals_cont)
plot(Animals_contn, leg_loc = "bottomright")
raw_earliest$Animals_cont.t <- log(log(raw_earliest$Animals_cont + 6.429306, base = 10), base = 10)
hist(raw_earliest$Animals_cont.t)
shapiro.test(raw_earliest$Animals_cont.t)

shapiro.test(raw_earliest$lifestockQ)
lifestockQn <- bestNormalize(raw_earliest$lifestockQ)
plot(lifestockQn, leg_loc = "bottomright")
raw_earliest$lifestockQ.t <- log(log(raw_earliest$lifestockQ + 4.444039, base = 10), base = 10)
hist(raw_earliest$lifestockQ.t)
shapiro.test(raw_earliest$lifestockQ.t)

shapiro.test(raw_earliest$Pond_dries)
Pond_driesn <- bestNormalize(raw_earliest$Pond_dries)
plot(Pond_driesn, leg_loc = "bottomright")
raw_earliest$Pond_dries.t <- log(log(raw_earliest$Pond_dries + 1.9701941, base = 10), base = 10)
hist(raw_earliest$Pond_dries.t)
shapiro.test(raw_earliest$Pond_dries.t)

shapiro.test(raw_earliest$Hydeoperiod_length)
Hydeoperiod_lengthn <- bestNormalize(raw_earliest$Hydeoperiod_length)
plot(Hydeoperiod_lengthn, leg_loc = "bottomright")
raw_earliest$Hydeoperiod_length.t <- asinh(raw_earliest$Hydeoperiod_length)
hist(raw_earliest$Hydeoperiod_length.t)
shapiro.test(raw_earliest$Hydeoperiod_length.t)

shapiro.test(raw_earliest$ECELS)
ECELSn <- bestNormalize(raw_earliest$ECELS)
plot(ECELSn, leg_loc = "bottomright")
raw_earliest$ECELS.t <- predict(orderNorm(raw_earliest$ECELS))
hist(raw_earliest$ECELS.t)
shapiro.test(raw_earliest$ECELS.t)


####################################################################################################################################
# PCA
####################################################################################################################################

head(raw_earliest[order(raw_earliest$TP,decreasing = TRUE),c('Pond_ID','TP','TN') ])
head(raw_earliest[order(raw_earliest$TOC,decreasing = TRUE),c('Pond_ID','TP','TN','TOC') ])
head(raw_earliest[order(raw_earliest$TN,decreasing = TRUE),c('Pond_ID','TP','TN','TOC') ])

dim(na.omit(raw_earliest[,c("PondCode","Country","year","MeasurementDate","O2_con","Cond","TN","TP","pH")]))

PCA_df <- raw_earliest[,c("PondCode","Country","year","MeasurementDate","O2_con","Cond","TN","TP","pH",
                    "Nat_res","lifestockQ","TSS")]

PCA_vars <- c("O2_con","Cond","TN","TP","pH","TSS")
#213*12 out of 239


PCA_df_nonna <-na.omit(PCA_df)
dim(PCA_df_nonna)
colnames(PCA_df_nonna)

numerical_data <- PCA_df_nonna[,PCA_vars]
head(numerical_data)

data_normalized <- scale(numerical_data)
head(data_normalized)


# Compute correlation matrix
correlation_matrix <- cor(data_normalized)
# Create correlation plot
corrplot(correlation_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,addCoef.col = "black",  diag = FALSE)
          # Color of the coefficients
        )          # Exclude diagonal elements


#run pca
data.pca <- princomp(data_normalized)
summary(data.pca)

data.pca$loadings[, 1:2]

fviz_eig(data.pca, addlabels = TRUE)
# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")

fviz_cos2(data.pca, choice = "var", axes = 1:2)

fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

#Retrieve first 2 components
PCA_df_nonna['nutrients_PC1']<- data.pca$scores[, 1]
PCA_df_nonna['nutrients_PC2']<- data.pca$scores[, 2]