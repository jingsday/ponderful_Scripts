#################################
## Description: Assemble modelling input
#################################

# Input files 
data_dir <- 'ponderful_DATA/'
Pond <- read.csv(paste0(data_dir,'ponderful_DATA_updated/D2.4_PondCharacteristics_20240607.csv'))
id <-  read.csv(paste0(data_dir,'ponderful_DATA_updated/D2.4_PondID_20240124.csv'))
LU5 <- read.csv(paste0(data_dir,'ponderful_DATA_updated/D2.4_LandUse_5m_20240516.csv'), header=T, sep = ",")

LU <- read.csv(paste0(data_dir,'LU_current.csv'), header=T, sep = ",")
LC <- read.csv(paste0(data_dir,'LandCover_selectedradii.csv'), header=T, sep = ",")

A <- read.csv(paste0(data_dir,'Animals.csv'), header=T, sep = ",")
# Hetog100 <- read.csv(paste0(data_dir,'landscape_heterogeneity_100_allponds.csv'), header=T, sep = ",")
# Hetog5 <- read.csv(paste0(data_dir,'landscape_heterogeneity_5_allponds.csv'), header=T, sep = ",")

dom_landcover_nov <- read.csv(paste0(data_dir,'/ponderful_DATA_updated/PhyChe_XY_landcover_nov.txt'))
dom_landcover_nov$PondCode <- toupper(dom_landcover_nov$PondCode)

## Pond variables
id <- id[,c("Pond_ID","PondCode","X","Y","Pondscape","Country","Strat_Survey","Resampling")]
id$PondCode <- toupper(id$PondCode)

id = id[id$Strat_Survey==1,]
Pond <- Pond[,c("PondCode","AssessmentDate","Nat_res","Area","Depth","Pond_dries","Rel_waterlev","PVI","Area")]
Pond$PondCode <- toupper(Pond$PondCode)

Pond$AssessmentDate <- as.Date(Pond$AssessmentDate, format = "%d/%m/%Y")
Pond$year <-substr(Pond$AssessmentDate, 1,4)

# Physicochemical variables: TN, TP
PC <- read.csv(paste0(data_dir,'ponderful_DATA_updated/D2.4_PhysicoChemistry_20240516.csv'))
PC$PondCode <- toupper(PC$PondCode)



PC[] <- lapply(PC, gsub, pattern='<', replacement='')
PC$NOTES <- NULL
PC$Notes..different.date.O2.spls.taken <- NULL
PC <-PC[!is.na(PC$MeasurementDate),]
PC[,-c(1:3)] <- sapply(PC[,-c(1:3)],as.numeric)
PC$MeasurementDate <- as.Date(PC$MeasurementDate, format = "%d/%m/%Y")
PC$year <- format(PC$MeasurementDate, "%Y")

# PC <- merge(PC,Pond[,c("PondCode","AssessmentDate","Pond_dries")],all.x = T,by.x = c('PondCode', 'MeasurementDate'),
#,     by.y=c('PondCode', 'AssessmentDate'))

PC[!is.na(PC$TN) & PC$TN >= 10,]$TN <- NA
#PC[!is.na(PC$CHLa_Spectro) & PC$CHLa_Spectro >= 800,]$CHLa_Spectro <- NA
PC <- merge(PC, id[,c('PondCode','Pond_ID')],by = 'PondCode',all.x=T)

#PC table seasons
PCs=PC[!is.na(PC$TN),c("PondCode","Pond_ID","MeasurementDate","year","season_col","Sludge","TN","TP" )]

PCunique <- PCs %>% 
  group_by(PondCode, year, season_col) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), 
            across(where(is.character), first),  # Keep first occurrence of character columns
            .groups = "drop")

PCunique <- PCunique[PCunique$PondCode %in% dom_landcover_nov$PondCode,]#remove fish ponds etc

#Adding country info
PCunique <- merge(PCunique,id[,c("PondCode","Country")],all.x = T,by = 'PondCode')

length(unique(PCunique$PondCode))#207

# Land use 
LU5$PondCode <- toupper(LU5$PondCode)

LU5$Natural_5 = LU5$Moorland + LU5$Rank_veg + LU5$Woodland + LU5$Marsh + LU5$Other
LU5$AssessmentDate <- as.Date(LU5$AssessmentDate, format = "%d/%m/%Y")
LU5$year <- format(LU5$AssessmentDate, "%Y")

LU5unique <- LU5 %>%  group_by(PondCode,year) %>%
  summarise(Natural_5 = mean(Natural_5, na.rm = TRUE), .groups = "drop")

LC <- read.csv(paste0(data_dir,'ponderful_DATA_updated/LandCover_selectedradii.csv'), header=T, sep = ",")
LC$PondCode <- toupper(LC$PondCode)

model_df <- PCunique[,c("PondCode","Pond_ID","year", "Country","season_col","Sludge","TN","TP")]
model_df <-merge(model_df,LU5unique,on=c('PondCode','year'),all.x=T)

length(unique(model_df$PondCode))
length(unique(model_df[is.na(model_df$Sludge),]$PondCode))
model_df <- merge(model_df,LC[,c(4,6:10)],by="PondCode", all.x=T) 

model_df <- merge(model_df,Pond[,c('PondCode', "year" ,'Area','Depth','Pond_dries')],by=c('PondCode','year'),all.x=T)

# Input 

dom_landcover_nov <- read.csv('/PhyChe_XY_landcover_nov.txt')
season_mean_df <-season_mean_df[,-c(52)]
dom_landcover_nov$PondCode <- toupper(dom_landcover_nov$PondCode)
season_mean_df <-merge(season_mean_df,dom_landcover_nov[,c('PondCode','Country')],by='PondCode',all.x=T)

season_mean_df$Country <- as.factor(season_mean_df$Country)
levels(season_mean_df$Country)

# Bio climatic regions
season_mean_df$Country <- as.character(season_mean_df$Country)

# Initialize altregion as a copy of Country
season_mean_df$altregion <- season_mean_df$Country

season_mean_df[season_mean_df$Country %in% c('Belgium', 'Denmark', 'UK'), "altregion"] <- 'Atlantic'
season_mean_df[season_mean_df$Country %in% c('Switzerland','Germany'), "altregion"] <- 'Temperate'

##Factors and transformation
season_mean_df$altregion <- as.factor(season_mean_df$altregion)
levels(season_mean_df$altregion)

#write.csv(season_mean_df,paste0(data_dir,'FINAL_ponderful_hydro_seasonality_glmm_input.csv'))