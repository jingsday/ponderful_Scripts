rm(list=ls())
setwd('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Analysis')

#####################################################################################################

Pond <- read.csv('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/D2.4_PondCharacteristics_20240607.csv', header=T, sep = ",")
id <- read.csv('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/D2.4_PondID_20240124.csv', header=T, sep = ",")
PC <- read.csv('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/D2.4_PhysicoChemistry_20240516.csv', header=T, sep = ",")
LU5 <- read.csv('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/D2.4_LandUse_5m_20240516.csv', header=T, sep = ",")
LC <- read.csv('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/LandCover_selectedradii.csv', header=T, sep = ",")
A <- read.csv('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/Animals.csv', header=T, sep = ",")
HA <- read.csv('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/D2.4_HumanActivity_20240205.csv', header=T, sep = ",")
HP <- read.csv('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/Hydroperiod_length_data_sampling_resampling.csv', header=T, sep = ",")
ecels <- read.csv('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/ECELS GEA_2024-03-21.csv', header=T, sep = ",")
climate <- read.csv('C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/Current_climate.csv', header=T, sep = ",")

#notes fitxer PC
# Measurement date=NA: algunes ponds no tenen cap data, eliminar
# Algunes ponds dades gasos i dades de resampling (més d'una data per any)
# "<" límit de detecció com a valor
# Chla Dinamarca = CHLa_RFU (fluorescència)
# dades de nutrients pel mostreig gasos, biodiversitat i resampling. A l'hora de fer el merge amb biodiversitat i altres temes, fer-ho per data més propera

# Pond variables
id <- id[,c("Pond_ID","PondCode","X","Y","Pondscape","Country","Strat_Survey","Resampling")]
id = id[id$Strat_Survey==1,]
Pond <- Pond[,c("PondCode","AssessmentDate","Nat_res","Area","Depth","Pond_dries","Rel_waterlev","PVI")]
Pond$year <-substr(Pond$AssessmentDate, 1, 4)

# Physicochemical variables: TN, TP, O2....
PC[] <- lapply(PC, gsub, pattern='<', replacement='')
PC$NOTES <- NULL
PC$Notes..different.date.O2.spls.taken <- NULL
PC <-PC[!is.na(PC$MeasurementDate),]
PC[,-c(1:3)] <- sapply(PC[,-c(1:3)],as.numeric)
PC$year=as.integer(substr(PC[,'MeasurementDate'], 1, 4))
PC[!is.na(PC$TN) & PC$TN >= 10,]$TN <- NA
PC[!is.na(PC$CHLa_Spectro) & PC$CHLa_Spectro >= 800,]$CHLa_Spectro <- NA

library(dplyr)
PC$MeasurementDate <-as.Date(PC$MeasurementDate,"%Y-%m-%d")

#######select the first date for each PC
# Keep only the row with the first measurement date where TN does not have NA
#PCunique <- PC %>%  group_by(PondCode) %>%
# filter(!is.na(TN)) %>%   filter(MeasurementDate == min(MeasurementDate))

######## Mean of the spring measurements where TN does not have NA
PCunique <- PC[,-2] %>% filter(season_col == "Spring") %>% group_by(PondCode,season_col) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Mean of hydroperiod
HPunique <- HP %>%  group_by(PondCode) %>%
  summarise(Hydeoperiod_length = mean(Hydeoperiod_length, na.rm = TRUE), .groups = "drop")

# Mean of Pond
Pond$PVI = as.numeric(Pond$PVI)
Pondunique <- Pond %>%  group_by(PondCode) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

############################
PC1=merge(id,PCunique,by=c("PondCode"))
PC11=merge(PC1,HPunique,by=c("PondCode"), all.x=T)
PC3=merge(PC11,climate[,c(2,7:14)],by=c("PondCode"), all.x=T) 

#Pond characteristics
PC4=merge(PC3,Pondunique,by=c("PondCode"), all.x=T)

#Lifestock
A$lifestockQ=1
A[A$lifestock == "Moderate",]$lifestockQ<-2
A[A$lifestock == "High",]$lifestockQ<-3

PC5=merge(PC4,A[,c(1,2,5)],by=c("Pond_ID"))

# Land use 5m (field data)
LU5$Natural_5 = LU5$Moorland + LU5$Rank_veg + LU5$Woodland + LU5$Marsh + LU5$Other
LU5$year <-substr(LU5$AssessmentDate, 1, 4)
LU5unique <- LU5 %>%  group_by(PondCode) %>%
  summarise(Natural_5 = mean(Natural_5, na.rm = TRUE), .groups = "drop")

PC5LU=merge(PC5,LU5unique,by=c("PondCode"), all.x=T)

# Land cover
PC6=merge(PC5LU,LC[,c(1,6:10)],by=c("Pond_ID"), all.x=T) 

# ECELS
PC7=merge(PC6,ecels[,c(2,10)],by=c("Pond_ID"), all.x=T)






#################################################################################################################################################
# Data transformation to achieve normality 
#################################################################################################################################################

library(bestNormalize)
shapiro.test(PC7$bio1)
bio1n <- bestNormalize(PC7$bio1)
plot(bio1n, leg_loc = "bottomright")
PC7$bio1.t <- predict(orderNorm(PC7$bio1))
hist(PC7$bio1.t)
shapiro.test(PC7$bio1.t)

shapiro.test(PC7$bio4)
bio4n <- bestNormalize(PC7$bio4)
plot(bio4n, leg_loc = "bottomright")
PC7$bio4.t <- predict(orderNorm(PC7$bio4))
hist(PC7$bio4.t)
shapiro.test(PC7$bio4.t)

shapiro.test(PC7$bio5)
bio5n <- bestNormalize(PC7$bio5)
plot(bio5n, leg_loc = "bottomright")
PC7$bio5.t <- predict(orderNorm(PC7$bio5))
hist(PC7$bio5.t)
shapiro.test(PC7$bio5.t)

shapiro.test(PC7$bio6)
bio6n <- bestNormalize(PC7$bio6)
plot(bio6n, leg_loc = "bottomright")
PC7$bio6.t <- predict(orderNorm(PC7$bio6))
hist(PC7$bio6.t)
shapiro.test(PC7$bio6.t)

shapiro.test(PC7$bio7)
bio7n <- bestNormalize(PC7$bio7)
plot(bio7n, leg_loc = "bottomright")
PC7$bio7.t <- predict(orderNorm(PC7$bio7))
hist(PC7$bio7.t)
shapiro.test(PC7$bio7.t)

shapiro.test(PC7$bio12)
bio12n <- bestNormalize(PC7$bio12)
plot(bio12n, leg_loc = "bottomright")
PC7$bio12.t <- predict(orderNorm(PC7$bio12))
hist(PC7$bio12.t)
shapiro.test(PC7$bio12.t)

shapiro.test(PC7$bio15)
bio15n <- bestNormalize(PC7$bio15)
plot(bio15n, leg_loc = "bottomright")
PC7$bio15.t <- predict(orderNorm(PC7$bio15))
hist(PC7$bio15.t)
shapiro.test(PC7$bio15.t)

shapiro.test(PC7$bio17)
bio17n <- bestNormalize(PC7$bio17)
plot(bio17n, leg_loc = "bottomright")
PC7$bio17.t <- predict(orderNorm(PC7$bio17))
hist(PC7$bio17.t)
shapiro.test(PC7$bio17.t)

shapiro.test(PC7$Area)
Arean <- bestNormalize(PC7$Area)
plot(Arean, leg_loc = "bottomright")
PC7$Area.t <- asinh(PC7$Area)
hist(PC7$Area.t)
shapiro.test(PC7$Area.t)

shapiro.test(PC7$Depth)
Depthn <- bestNormalize(PC7$Depth)
plot(Depthn, leg_loc = "bottomright")
PC7$Depth.t <- predict(orderNorm(PC7$Depth))
hist(PC7$Depth.t)
shapiro.test(PC7$Depth.t)

shapiro.test(PC7$Animals_cont)
Animals_contn <- bestNormalize(PC7$Animals_cont)
plot(Animals_contn, leg_loc = "bottomright")
PC7$Animals_cont.t <- predict(orderNorm(PC7$Animals_cont))
hist(PC7$Animals_cont.t)
shapiro.test(PC7$Animals_cont.t)

shapiro.test(PC7$Hydeoperiod_length)
Hydeoperiod_lengthn <- bestNormalize(PC7$Hydeoperiod_length)
plot(Hydeoperiod_lengthn, leg_loc = "bottomright")
PC7$Hydeoperiod_length.t <- scale(PC7$Hydeoperiod_length)
hist(PC7$Hydeoperiod_length.t)
shapiro.test(PC7$Hydeoperiod_length.t)

shapiro.test(PC7$ECELS)
ECELSn <- bestNormalize(PC7$ECELS)
plot(ECELSn, leg_loc = "bottomright")
PC7$ECELS.t <- predict(orderNorm(PC7$ECELS))
hist(PC7$ECELS.t)
shapiro.test(PC7$ECELS.t)

# Transform with orderNorm all landuse categories
library(dplyr)
PC7 <- PC7 %>% mutate(across(c(62:67), ~predict(orderNorm(.)), .names = "{.col}.t"))

write.csv(PC7,"C:/Users/650475/OneDrive - Universitat de Vic/UVic/Projectes en curs/PONDERFUL/2 BASIC RESEARCH/PhyChe/Data/Unified data - Intranet/PhyChe_XY.csv")

#correlations
library(psych) 
png("correlation_PC.t.png", height=15, width=15, units = "in", res = 300)
pairs.panels(PC7[,c(69:81)], scale=T)
dev.off()

# we remove from the analysis because of high correlation with other variables: bio6, bio7, bio15, bio17