library(dplyr)
library(stringr)
library(tidyr)
library(zoo)
library(reshape2)
library(tibble)
library(data.table)
`%ni%` <- Negate(`%in%`)

library(cepiigeodist)

hdi_index <- read.csv('similarities_countries_GDL_indices.csv', sep = ';', header = TRUE)


distances <- dist_cepii

# dist_cepii
# dist: simple distance
# distcap: distance between capitals
# comlang_off: same official language
# contig: neighbours

countryname = 'Austria'


similar3countries <- function(countryname, hdi_index){
  df_available <- hdi_index[hdi_index$Available > 0 ,]
  
  country1 <- hdi_index[hdi_index$Country == countryname ,]
  
  df_available['diff_hdi'] <- abs(df_available$Subnational_HDI - country1$Subnational_HDI)
  
  df_available <- df_available[order(df_available$diff_hdi, decreasing = FALSE), ]
  
  df_available <- df_available[df_available$Country != countryname ,]
  
  distance_o <- distances[distances$iso_o == country1$ISO_Code ,]
  
  #Serbia and Palestina are missing. 
  # So we add the distance of Bosnia and Israel. SRB BIH and PSE ISR 
  
  distanceSRB <- distance_o[distance_o$iso_d == 'BIH' ,]
  distanceSRB$iso_d <- 'SRB'
  
  distancePSE <- distance_o[distance_o$iso_d == 'ISR' ,]
  distancePSE$iso_d <- 'PSE'
  
  distance_o <- rbind(distance_o, distanceSRB, distancePSE)
  
  distance_o <- distance_o %>% rename(ISO_Code = iso_d)
  
  df_available <- left_join(df_available, distance_o %>% select(ISO_Code, dist), by = 'ISO_Code')
  
  df_available <- head(df_available, 15)
  
  df_available <- df_available[order(df_available$dist, decreasing = FALSE), ]
  
  df_available <- head(df_available, 3)
  
  similar_countries <- df_available$Country
  
  return(similar_countries)
  
}



countrieslist <- hdi_index$Country

similardf <- data.frame('Country' = countrieslist)
similardf['Replace1'] <- ''
similardf['Replace2'] <- ''
similardf['Replace3'] <- ''

for (i in 1:length(countrieslist)){
  countryname <- countrieslist[i]
  countries3 <- similar3countries(countryname, hdi_index)
  similardf[i, 'Replace1'] <- countries3[1]
  similardf[i, 'Replace2'] <- countries3[2]
  similardf[i, 'Replace3'] <- countries3[3]

}


LISDHS <- hdi_index[hdi_index$Available > 0,]

LISDHS <- LISDHS %>% rename('Replace1' = 'Country', 'Source1' = 'LISDHS')
similardf <- left_join(similardf, LISDHS %>% select(Replace1, Source1), by = 'Replace1')

LISDHS <- LISDHS %>% rename('Replace2' = 'Replace1', 'Source2' = 'Source1')
similardf <- left_join(similardf, LISDHS %>% select(Replace2, Source2), by = 'Replace2')

LISDHS <- LISDHS %>% rename('Replace3' = 'Replace2', 'Source3' = 'Source2')
similardf <- left_join(similardf, LISDHS %>% select(Replace3, Source3), by = 'Replace3')



write.csv(similardf, 'similar_countries_8nov23.csv', row.names = FALSE)