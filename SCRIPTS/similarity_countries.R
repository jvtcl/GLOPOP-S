library(dplyr)
library(stringr)
library(tidyr)
library(zoo)
library(reshape2)
library(tibble)
library(data.table)
`%ni%` <- Negate(`%in%`)

library(cepiigeodist)

setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2')

hdi_index <- read.csv('similarities_countries_GDL_indices.csv', sep = ';', header = TRUE)


distances <- dist_cepii

distanceSRB <- distances[distances$iso_d == 'BIH' ,]
distanceSRB$iso_d <- 'SRB'

distanceSRB1 <- distances[distances$iso_o == 'BIH' ,]
distanceSRB1$iso_o <- 'SRB'

distancePSE <- distances[distances$iso_d == 'ISR' ,]
distancePSE$iso_d <- 'PSE'

distancePSE1 <- distances[distances$iso_o == 'ISR' ,]
distancePSE1$iso_o <- 'PSE'

distanceTLS <- distances[distances$iso_d == 'IDN' ,]
distanceTLS$iso_d <- 'TLS'

distanceTLS1 <- distances[distances$iso_o == 'IDN' ,]
distanceTLS1$iso_o <- 'TLS'

distanceCOD <- distances[distances$iso_d == 'COG' ,]
distanceCOD$iso_d <- 'COD'

distanceCOD1 <- distances[distances$iso_o == 'COG' ,]
distanceCOD1$iso_o <- 'COD'

distanceXKO <- distances[distances$iso_d == 'ALB' ,]
distanceXKO$iso_d <- 'XKO'

distanceXKO1 <- distances[distances$iso_o == 'ALB' ,]
distanceXKO1$iso_o <- 'XKO'

distanceMNE <- distances[distances$iso_d == 'ALB' ,]
distanceMNE$iso_d <- 'MNE'

distanceMNE1 <- distances[distances$iso_o == 'ALB' ,]
distanceMNE1$iso_o <- 'MNE'

distanceLIE <- distances[distances$iso_d == 'CHE' ,]
distanceLIE$iso_d <- 'LIE'

distanceLIE1 <- distances[distances$iso_o == 'CHE' ,]
distanceLIE1$iso_o <- 'LIE'

distanceROU <- distances[distances$iso_d == 'BGR' ,]
distanceROU$iso_d <- 'ROU'

distanceROU1 <- distances[distances$iso_o == 'BGR' ,]
distanceROU1$iso_o <- 'ROU'

distanceSSD <- distances[distances$iso_d == 'UGA' ,]
distanceSSD$iso_d <- 'SSD'

distanceSSD1 <- distances[distances$iso_o == 'UGA' ,]
distanceSSD1$iso_o <- 'SSD'

distances <- rbind(distances, distanceSRB, distancePSE, distanceTLS, distanceCOD, 
                   distanceSRB1, distancePSE1, distanceTLS1, distanceCOD1, distanceXKO, distanceXKO1, 
                   distanceMNE, distanceMNE1, distanceLIE, distanceLIE1, distanceROU, distanceROU1, distanceSSD, distanceSSD1)



# dist_cepii
# dist: simple distance
# distcap: distance between capitals
# comlang_off: same official language
# contig: neighbours


LIS_or_DHS = 'DHS'


hdi_index[hdi_index$ISO_Code == "DEU", "Available"] <- 0
hdi_index[hdi_index$ISO_Code == "AUT", "Available"] <- 0
hdi_index[hdi_index$ISO_Code == "CZE", "Available"] <- 0
hdi_index[hdi_index$ISO_Code == "HUN", "Available"] <- 0
hdi_index[hdi_index$ISO_Code == "JPN", "Available"] <- 0
hdi_index[hdi_index$ISO_Code == "RUS", "Available"] <- 0
hdi_index[hdi_index$ISO_Code == "SVK", "Available"] <- 0
hdi_index[hdi_index$ISO_Code == "PSE", "Available"] <- 0
hdi_index[hdi_index$ISO_Code == "KGZ", "Available"] <- 0

similar3countries <- function(countryname, hdi_index){
  df_available <- hdi_index[hdi_index$Available > 0 & hdi_index$Available < 3 & (hdi_index$LISDHS == LIS_or_DHS | hdi_index$LISDHS == 'LIS/DHS'),]
  
  country1 <- hdi_index[hdi_index$Country == countryname ,]
  
  df_available['diff_hdi'] <- abs(df_available$Subnational_HDI - country1$Subnational_HDI)
  
  df_available <- df_available[order(df_available$diff_hdi, decreasing = FALSE), ]
  
  df_available <- df_available[df_available$Country != countryname ,]
  
  distance_o <- distances[distances$iso_o == country1$ISO_Code ,]
  
  distance_o <- distance_o %>% rename(ISO_Code = iso_d)
  
  df_available <- left_join(df_available, distance_o %>% dplyr::select(ISO_Code, dist), by = 'ISO_Code')
  
  df_available <- head(df_available, 10)
  
  df_available <- df_available[order(df_available$dist, decreasing = FALSE), ]
  
  df_available <- head(df_available, 3)
  
  sum_hdi_diff <- sum(df_available$diff_hdi)
  
  sum_distance <- sum(df_available$dist)
  
  similar_countries <- df_available$Country
  
  return(list(similar_countries = similar_countries, sum_hdi_diff = sum_hdi_diff, sum_distance = sum_distance))
  
}



countrieslist <- hdi_index$Country

similardf <- data.frame('Country' = countrieslist)
similardf['Replace1'] <- ''
similardf['Replace2'] <- ''
similardf['Replace3'] <- ''
similardf['LISDHS'] <- LIS_or_DHS
similardf['sum_hdidiff'] <- 0
similardf['sum_distance'] <- 0

for (i in 1:length(countrieslist)){
  countryname <- countrieslist[i]
  listoutput <- similar3countries(countryname, hdi_index)
  similardf[i, 'Replace1'] <- listoutput$similar_countries[1]
  similardf[i, 'Replace2'] <- listoutput$similar_countries[2]
  similardf[i, 'Replace3'] <- listoutput$similar_countries[3]
  similardf[i, 'sum_hdidiff'] <- listoutput$sum_hdi_diff
  similardf[i, 'sum_distance'] <- listoutput$sum_distance

}


#LISDHS <- hdi_index[hdi_index$Available > 0,]

#LISDHS <- LISDHS %>% rename('Replace1' = 'Country', 'Source1' = 'LISDHS')
#similardf <- left_join(similardf, LISDHS %>% select(Replace1, Source1), by = 'Replace1')

#LISDHS <- LISDHS %>% rename('Replace2' = 'Replace1', 'Source2' = 'Source1')
#similardf <- left_join(similardf, LISDHS %>% select(Replace2, Source2), by = 'Replace2')

#LISDHS <- LISDHS %>% rename('Replace3' = 'Replace2', 'Source3' = 'Source2')
#similardf <- left_join(similardf, LISDHS %>% select(Replace3, Source3), by = 'Replace3')



write.csv(similardf, 'similar_countries_DHS_5august24.csv', row.names = FALSE)

LISsimilar <- read.csv('similar_countries_LIS_5august24.csv')
DHSsimilar <- read.csv('similar_countries_DHS_5august24.csv')

# change to isocodes 
setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2')
isocodes_names <- read.csv('Countrynames_to_ISO.csv', sep = ';') 
names(isocodes_names) <- c('ISOcode', 'Country')

LISsimilar <- left_join(LISsimilar, isocodes_names, by = 'Country')

names(isocodes_names) <- c('ISOcode1', 'Replace1')
LISsimilar <- left_join(LISsimilar, isocodes_names, by = 'Replace1')

names(isocodes_names) <- c('ISOcode2', 'Replace2')
LISsimilar <- left_join(LISsimilar, isocodes_names, by = 'Replace2')

names(isocodes_names) <- c('ISOcode3', 'Replace3')
LISsimilar <- left_join(LISsimilar, isocodes_names, by = 'Replace3')

LISsimilar1 <- LISsimilar %>% dplyr::select(ISOcode, ISOcode1, ISOcode2, ISOcode3)
names(LISsimilar1) <- c('ISOcode', 'Replace1', 'Replace2', 'Replace3')
LISsimilar1['LISDHS'] <- LISsimilar$LISDHS
LISsimilar1['sum_hdidiff'] <- LISsimilar$sum_hdidiff
LISsimilar1['sum_distance'] <- LISsimilar$sum_distance

isocodes_names <- read.csv('Countrynames_to_ISO.csv', sep = ';') 
names(isocodes_names) <- c('ISOcode', 'Country')
DHSsimilar <- left_join(DHSsimilar, isocodes_names, by = 'Country')

names(isocodes_names) <- c('ISOcode1', 'Replace1')
DHSsimilar <- left_join(DHSsimilar, isocodes_names, by = 'Replace1')

names(isocodes_names) <- c('ISOcode2', 'Replace2')
DHSsimilar <- left_join(DHSsimilar, isocodes_names, by = 'Replace2')

names(isocodes_names) <- c('ISOcode3', 'Replace3')
DHSsimilar <- left_join(DHSsimilar, isocodes_names, by = 'Replace3')

DHSsimilar1 <- DHSsimilar %>% dplyr::select(ISOcode, ISOcode1, ISOcode2, ISOcode3)
names(DHSsimilar1) <- c('ISOcode', 'Replace1', 'Replace2', 'Replace3')
DHSsimilar1['LISDHS'] <- DHSsimilar$LISDHS
DHSsimilar1['sum_hdidiff'] <- DHSsimilar$sum_hdidiff
DHSsimilar1['sum_distance'] <- DHSsimilar$sum_distance

setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2')
dhscountries <- read.csv('DHSdatafiles.csv', sep = ';')
dhscountries <- dhscountries %>% dplyr::select(Country, isocode)
names(dhscountries) <- c('Country', 'ISOcode')
dhscountries['DHS'] <- 'DHS'

setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
liscountries <- read.csv('LIS_codes_countries2.csv', sep = ';')
names(liscountries) <- c('LISCODE', 'Country', 'ISOcode', 'year')
liscountries['LIS'] <- 'LIS'

similardf2 <- rbind(LISsimilar1, DHSsimilar1)

similardf2 <- left_join(similardf2, dhscountries %>% dplyr::select(ISOcode, DHS), by = 'ISOcode')

similardf2 <- left_join(similardf2, liscountries %>% dplyr::select(ISOcode, LIS), by = 'ISOcode')



similardf2['diff0'] <- similardf2$sum_hdidiff

similardf2$diff0[(similardf2$LIS == 'LIS' & similardf2$LISDHS == 'LIS')] <- 0
similardf2$diff0[(similardf2$DHS == 'DHS' & similardf2$LISDHS == 'DHS')] <- 0

similardf2$diff0[(similardf2$LISDHS == 'LIS' & similardf2$LIS == 'LIS' & similardf2$DHS == 'DHS')] <- 0.1

similardf2_filtered <- similardf2 %>%
  group_by(ISOcode) %>%
  filter(diff0 != max(diff0)) %>%
  ungroup() 

similardf2_filtered <- similardf2_filtered %>% dplyr::select(-DHS, -LIS, -diff0)

setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2')
write.csv(similardf2_filtered, 'similar_countries_5august24.csv', row.names = FALSE)

