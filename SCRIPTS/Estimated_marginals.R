#!/usr/bin/env Rscript

library(dplyr)
#library(readxl)
#library(gridExtra)
#library(purrr)
library(tidyr)
library(data.table)
`%ni%` <- Negate(`%in%`)


##########################################################

# DHS is GDLcodes. 
# LIS marginals ook al omzetten naar GDL. 


# CHOOSE VARIABLE: 

choosefrom <- c('HHSIZECAT', 'HHTYPE', 'AGECAT', 'GENDER', 'EDUCAT', 'INCOME', 'ROOFCAT', 'WALLCAT', 'FLOORCAT', 'FARMING')



#choosefrom <- c('HHSIZECAT')


# now it loops through all variables in choosefrom. 



startk <- 50
endk <- 270


#setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2')
similarcountries <- read.csv('similar_countries_5august24.csv')



##########################################################

# DON'T CHANGE ANYTHING HERE BELOW


# function
similar_k_regions <- function(gdlcode, known_marg, unknown_marg, k, columnnames){
  
  keeprow <- known_marg[known_marg$GDLcode == gdlcode,]
  
  known_marg['ISO'] <- substr(known_marg$GDLcode, 1, 3)
  
  
  if (nrow(keeprow) == 1){
    
    isocode <- known_marg[known_marg$GDLcode == gdlcode,]$ISO #hier iets met country doen. 
    
    known_marg <- known_marg[known_marg$ISO != isocode,]
    
  } else {
    
    keeprow <- unknown_marg[unknown_marg$GDLCODE == gdlcode,]
    
  }
  
  known_marg['hdi1'] <- keeprow$shdi
  
  known_marg['diff_hdi'] <- abs(known_marg$shdi - known_marg$hdi1)
  
  known_marg <- known_marg[order(known_marg$diff_hdi, decreasing = FALSE), ]
  
  selected_regions <- head(known_marg, k)
  
  mean_region <- colMeans(selected_regions %>% dplyr::select(`columnnames`))
  
  sum_mean_region <- sum(mean_region)
  
  correction <- (unknown_marg[unknown_marg$GDLcode == gdlcode,]$Population)/sum_mean_region
  
  mean_region <- mean_region*correction
  
  contains_na <- any(is.na(mean_region))
  
  if (contains_na == TRUE){
    print('NA in mean region')
    print(attribute)
    print(gdlcode)
    
  }
  
  return(mean_region)
  
}



bestkarray <- c()

best_k_DF <- data.frame(Attributes = choosefrom)
best_k_DF['bestk'] <- -99
best_k_DF['bestk_LIS'] <- -99
best_k_DF['bestk_DHS'] <- -99

att_count = 0

for (attribute in choosefrom){
  print(attribute)
  att_count = att_count +1
  
  if (attribute == 'HHTYPE'){
    columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8')

    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
    LIS_marginals <- read.csv("LISmarg_hhtype_may24.csv") 
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
             'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
             'FINr102', 'FINr103', 'FINr104', 'FINr105'),]


    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
    DHS_marginals <- read.csv("DHS_observedmarginals_hhtype_aug24.csv") 

    
  } else if (attribute == 'AGECAT'){
    columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8')
    
    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
    LIS_marginals <- read.csv("LISmarg_age_may24.csv")  
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
          'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
        'FINr102', 'FINr103', 'FINr104', 'FINr105'),]
  
    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
    DHS_marginals <- read.csv("DHS_observedmarginals_age_aug24.csv") 
      
  } else if (attribute == 'INCOME'){
    columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5')

    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
    LIS_marginals <- read.csv("LISmarg_income_may24.csv") 
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
            'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
            'FINr102', 'FINr103', 'FINr104', 'FINr105'),]
  
    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
    DHS_marginals <- read.csv("DHS_observedmarginals_wealth_aug24.csv") 
    
  } else if (attribute == 'GENDER'){
    columnnames <- c('X1', 'X2')
    
    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
    LIS_marginals <- read.csv("LISmarg_gender_may24.csv") 
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
            'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
           'FINr102', 'FINr103', 'FINr104', 'FINr105'),]
  
    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
    DHS_marginals <- read.csv("DHS_observedmarginals_gender_aug24.csv") 
    
  } else if (attribute == 'ROOFCAT'){
    columnnames <- c('X1', 'X2', 'X3')
    
    LIS_marginals <- c()
    
    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
    DHS_marginals <- read.csv("DHS_observedmarginals_roofcat_aug24.csv") 
    
  } else if (attribute == 'WALLCAT'){
    columnnames <- c('X1', 'X2', 'X3')
    
    LIS_marginals <- c()
    
    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
    DHS_marginals <- read.csv("DHS_observedmarginals_wallcat_aug24.csv") 
    
  } else if (attribute == 'FLOORCAT'){
    columnnames <- c('X1', 'X2', 'X3')
    
    LIS_marginals <- c()
    
    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
    DHS_marginals <- read.csv("DHS_observedmarginals_floorcat_aug24.csv") 
    
  } else if (attribute == 'FARMING'){
    columnnames <- c('X1', 'X2')
    
    LIS_marginals <- c()
    
    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
    DHS_marginals <- read.csv("DHS_observedmarginals_farming_aug24.csv") 
    
  } else if (attribute == 'EDUCAT'){
    columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5')

    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
    LIS_marginals <- read.csv("LISmarg_edu_may24.csv") 
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
           'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
          'FINr102', 'FINr103', 'FINr104', 'FINr105'),]

  
    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
    DHS_marginals <- read.csv("DHS_observedmarginals_edu_aug24.csv")  
    

    
    LIS_marginals[is.na(LIS_marginals)] <- 0
    
    LIS_marginals['Totalfreq'] <- LIS_marginals$X1 + LIS_marginals$X2 + LIS_marginals$X3 + LIS_marginals$X4 + LIS_marginals$X5 
    
    # take average of countries without missing category. 
    LIS_marginals_1 <- LIS_marginals[LIS_marginals$X1 > 0,]
    LIS_marginals_1['Totalfreq'] <- LIS_marginals_1$X1 + LIS_marginals_1$X2 + LIS_marginals_1$X3 + LIS_marginals_1$X4 + LIS_marginals_1$X5 
    
    LIS_marginals_2 <- LIS_marginals[LIS_marginals$X2 > 0,]
    LIS_marginals_2['Totalfreq'] <- LIS_marginals_2$X1 + LIS_marginals_2$X2 + LIS_marginals_2$X3 + LIS_marginals_2$X4 + LIS_marginals_2$X5 
    
    LIS_marginals_3 <- LIS_marginals[LIS_marginals$X3 > 0,]
    LIS_marginals_3['Totalfreq'] <- LIS_marginals_3$X1 + LIS_marginals_3$X2 + LIS_marginals_3$X3 + LIS_marginals_3$X4 + LIS_marginals_3$X5 
    
    LIS_marginals_4 <- LIS_marginals[LIS_marginals$X4 > 0,]
    LIS_marginals_4['Totalfreq'] <- LIS_marginals_4$X1 + LIS_marginals_4$X2 + LIS_marginals_4$X3 + LIS_marginals_4$X4 + LIS_marginals_4$X5 
    
    LIS_marginals_5 <- LIS_marginals[LIS_marginals$X5 > 0,]
    LIS_marginals_5['Totalfreq'] <- LIS_marginals_5$X1 + LIS_marginals_5$X2 + LIS_marginals_5$X3 + LIS_marginals_5$X4 + LIS_marginals_5$X5 
    
    average_X1 <- sum(LIS_marginals_1$X1)/sum(LIS_marginals_1$Totalfreq)
    average_X2 <- sum(LIS_marginals_2$X2)/sum(LIS_marginals_2$Totalfreq)
    average_X3 <- sum(LIS_marginals_3$X3)/sum(LIS_marginals_3$Totalfreq)
    average_X4 <- sum(LIS_marginals_4$X4)/sum(LIS_marginals_4$Totalfreq)
    average_X5 <- sum(LIS_marginals_5$X5)/sum(LIS_marginals_5$Totalfreq)
    
    sum_averages <- average_X1 + average_X2 + average_X3 + average_X4 + average_X5
    
    average_X1 = average_X1/sum_averages
    average_X2 = average_X2/sum_averages
    average_X3 = average_X3/sum_averages
    average_X4 = average_X4/sum_averages
    average_X5 = average_X5/sum_averages
    
    LIS_marginals$X1[LIS_marginals$X1 == 0] <- average_X1 * LIS_marginals$Totalfreq[LIS_marginals$X1 == 0]
    LIS_marginals$X2[LIS_marginals$X2 == 0] <- average_X2 * LIS_marginals$Totalfreq[LIS_marginals$X2 == 0]
    LIS_marginals$X3[LIS_marginals$X3 == 0] <- average_X3 * LIS_marginals$Totalfreq[LIS_marginals$X3 == 0]
    LIS_marginals$X4[LIS_marginals$X4 == 0] <- average_X4 * LIS_marginals$Totalfreq[LIS_marginals$X4 == 0]
    LIS_marginals$X5[LIS_marginals$X5 == 0] <- average_X5 * LIS_marginals$Totalfreq[LIS_marginals$X5 == 0]
    
    
    LIS_marginals['Totalfreq2'] <- LIS_marginals$X1 + LIS_marginals$X2 + LIS_marginals$X3 + LIS_marginals$X4 + LIS_marginals$X5 
    
    LIS_marginals['Correction'] <- LIS_marginals$Totalfreq/LIS_marginals$Totalfreq2
    
    LIS_marginals$X1 <- LIS_marginals$X1 * LIS_marginals$Correction 
    LIS_marginals$X2 <- LIS_marginals$X2 * LIS_marginals$Correction 
    LIS_marginals$X3 <- LIS_marginals$X3 * LIS_marginals$Correction 
    LIS_marginals$X4 <- LIS_marginals$X4 * LIS_marginals$Correction 
    LIS_marginals$X5 <- LIS_marginals$X5 * LIS_marginals$Correction 
  
    LIS_marginals <- LIS_marginals %>% dplyr::select(GDLcode, X1, X2, X3, X4, X5)
    
    
    DHS_marginals[is.na(DHS_marginals)] <- 0
    
    DHS_marginals['Totalfreq'] <- DHS_marginals$X1 + DHS_marginals$X2 + DHS_marginals$X3 + DHS_marginals$X4 + DHS_marginals$X5 
    
    # take average of countries without missing category. 
    DHS_marginals_1 <- DHS_marginals[DHS_marginals$X1 > 0,]
    DHS_marginals_1['Totalfreq'] <- DHS_marginals_1$X1 + DHS_marginals_1$X2 + DHS_marginals_1$X3 + DHS_marginals_1$X4 + DHS_marginals_1$X5 
    
    DHS_marginals_2 <- DHS_marginals[DHS_marginals$X2 > 0,]
    DHS_marginals_2['Totalfreq'] <- DHS_marginals_2$X1 + DHS_marginals_2$X2 + DHS_marginals_2$X3 + DHS_marginals_2$X4 + DHS_marginals_2$X5 
    
    DHS_marginals_3 <- DHS_marginals[DHS_marginals$X3 > 0,]
    DHS_marginals_3['Totalfreq'] <- DHS_marginals_3$X1 + DHS_marginals_3$X2 + DHS_marginals_3$X3 + DHS_marginals_3$X4 + DHS_marginals_3$X5 
    
    DHS_marginals_4 <- DHS_marginals[DHS_marginals$X4 > 0,]
    DHS_marginals_4['Totalfreq'] <- DHS_marginals_4$X1 + DHS_marginals_4$X2 + DHS_marginals_4$X3 + DHS_marginals_4$X4 + DHS_marginals_4$X5 
    
    DHS_marginals_5 <- DHS_marginals[DHS_marginals$X5 > 0,]
    DHS_marginals_5['Totalfreq'] <- DHS_marginals_5$X1 + DHS_marginals_5$X2 + DHS_marginals_5$X3 + DHS_marginals_5$X4 + DHS_marginals_5$X5 
    
    average_X1 <- sum(DHS_marginals_1$X1)/sum(DHS_marginals_1$Totalfreq)
    average_X2 <- sum(DHS_marginals_2$X2)/sum(DHS_marginals_2$Totalfreq)
    average_X3 <- sum(DHS_marginals_3$X3)/sum(DHS_marginals_3$Totalfreq)
    average_X4 <- sum(DHS_marginals_4$X4)/sum(DHS_marginals_4$Totalfreq)
    average_X5 <- sum(DHS_marginals_5$X5)/sum(DHS_marginals_5$Totalfreq)
    
    sum_averages <- average_X1 + average_X2 + average_X3 + average_X4 + average_X5
    
    average_X1 = average_X1/sum_averages
    average_X2 = average_X2/sum_averages
    average_X3 = average_X3/sum_averages
    average_X4 = average_X4/sum_averages
    average_X5 = average_X5/sum_averages
    
    DHS_marginals$X1[DHS_marginals$X1 == 0] <- average_X1 * DHS_marginals$Totalfreq[DHS_marginals$X1 == 0]
    DHS_marginals$X2[DHS_marginals$X2 == 0] <- average_X2 * DHS_marginals$Totalfreq[DHS_marginals$X2 == 0]
    DHS_marginals$X3[DHS_marginals$X3 == 0] <- average_X3 * DHS_marginals$Totalfreq[DHS_marginals$X3 == 0]
    DHS_marginals$X4[DHS_marginals$X4 == 0] <- average_X4 * DHS_marginals$Totalfreq[DHS_marginals$X4 == 0]
    DHS_marginals$X5[DHS_marginals$X5 == 0] <- average_X5 * DHS_marginals$Totalfreq[DHS_marginals$X5 == 0]
    
    
    DHS_marginals['Totalfreq2'] <- DHS_marginals$X1 + DHS_marginals$X2 + DHS_marginals$X3 + DHS_marginals$X4 + DHS_marginals$X5 
    
    DHS_marginals['Correction'] <- DHS_marginals$Totalfreq/DHS_marginals$Totalfreq2
    
    DHS_marginals$X1 <- DHS_marginals$X1 * DHS_marginals$Correction 
    DHS_marginals$X2 <- DHS_marginals$X2 * DHS_marginals$Correction 
    DHS_marginals$X3 <- DHS_marginals$X3 * DHS_marginals$Correction 
    DHS_marginals$X4 <- DHS_marginals$X4 * DHS_marginals$Correction 
    DHS_marginals$X5 <- DHS_marginals$X5 * DHS_marginals$Correction 
    
    DHS_marginals <- DHS_marginals %>% dplyr::select(GDLcode, X1, X2, X3, X4, X5)
    
  } else if (attribute == 'HHSIZECAT'){
    #hhsizecat 0 invullen. 
    
    columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6')
    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
    DHS_marginals <- read.csv("DHS_observedmarginals_hhsizecat_aug24.csv", sep = ',')

    # LIS
    #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
    LIS_marginals <- read.csv("LISmarg_hhsize_may24.csv") 
    
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
              'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
    LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
              'FINr102', 'FINr103', 'FINr104', 'FINr105'),]
    
    LIS_marginals[is.na(LIS_marginals)] <- 0
    
  } 
  
  columnnames_est <- paste0(columnnames, 'est')
  
  

  # population_data klopt. 
  #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
  popdata_GHSPOP <- read.csv('GHSpopulationGDLregions.csv')
  
  #shdi 
  #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\HHTYPE_files')
  HDI_data <- read.csv("SHDI-SGDI-Total 5.0.csv")
  
  nopop_HDI <- HDI_data[is.na(HDI_data$pop),] #population zit erin, behalve Canada and Tonga. 
  
  # Canada and Tonga have no population under GDL5.0, therefore, these population numbers are taken from GDL4.0
  GDL4.0 <- read.csv("SHDI Complete 4.0 (1).csv") %>% filter(year == 2015) %>% filter(country %in% c('Canada','Tonga')) %>% dplyr::select(GDLCODE, pop) %>% mutate(pop = pop * 1000)
  GDL4.0 <- GDL4.0[GDL4.0$GDLCODE != 'CANt',]
  
  # popdata uit ander bestand ingevuld. 
  HDI_data15 <- HDI_data[HDI_data$year == 2015,]
  
  HDI_data15[HDI_data15$GDLCODE %in% GDL4.0$GDLCODE ,]$pop <- GDL4.0$pop
  
  # Population Tonga only available on national level, so we drop regions Tonga.  
  
  HDI_data15 <- HDI_data15[HDI_data15$GDLCODE %ni% c(HDI_data15[HDI_data15$country == 'Tonga' & HDI_data15$level == 'Subnat',]$GDLCODE),]
  
  HDI_data15 <- HDI_data15 %>% rename(GDLcode = GDLCODE)

  unknown_marg <- popdata_GHSPOP %>% dplyr::select(GDLcode, shdi)
  
  unknown_marg['ISOcode'] <- substr(unknown_marg$GDLcode, 1, 3)
  unknown_marg <- left_join(unknown_marg, similarcountries %>% dplyr::select(ISOcode, LISDHS), by = 'ISOcode')

  
  unknown_marg <- left_join(unknown_marg, popdata_GHSPOP %>% dplyr::select(GDLcode, Population), by = 'GDLcode')
  
  unknown_marg[1754, 'LISDHS'] <- 'LIS'
  
  #for (n in columnnames_est){
  #  unknown_marg[`n`] <- -99
  #}
  
  
  
  
  
  #cor_matrix_subset <- cor(known_marg[c("shdi", columnnames)])
  
  
  # add population size to unknown marg
  
  
  
  
  
  
  # to calculate error
  
  
  
  
  
  
  known_marg <- rbind(DHS_marginals, LIS_marginals)
  
  known_marg <- left_join(known_marg, unknown_marg %>% dplyr::select(GDLcode, shdi), by = 'GDLcode')
  
  unknown_marg2 <- unknown_marg[unknown_marg$GDLcode %in% known_marg$GDLcode,]
  

  
  error_DF_k_LIS <- data.frame(k = startk:endk)
  error_DF_k_DHS <- data.frame(k = startk:endk)
  error_DF_k <- data.frame(k = startk:endk)
  error_DF_k_LIS['sum_abs_error'] <- -99
  error_DF_k_DHS['sum_abs_error'] <- -99
  error_DF_k['sum_abs_error'] <- -99
  
  errornames <- c()
  errornames_percent <- c()
  for (n in 1:length(columnnames)){
    errornames <- append(errornames, paste0('absE', as.character(n)))
    errornames_percent <- append(errornames_percent, paste0('absEpercent', as.character(n)))
  }
  
  for (k in error_DF_k$k){
    
    print(k)  
    for (gdlcode in unknown_marg2$GDLcode){
      
      database = unknown_marg2[unknown_marg2$GDLcode == gdlcode,]$LISDHS
      
      if (attribute %in% c('ROOFCAT', 'WALLCAT', 'FLOORCAT', 'FARMING')){
        database = 'DHS'
      }
      
      if (database == 'LIS'){
        known_marg <- LIS_marginals
        known_marg <- left_join(known_marg, unknown_marg %>% dplyr::select(GDLcode, shdi), by = 'GDLcode')
        
      } 
      
      if (database == 'DHS'){
        known_marg <- DHS_marginals
        known_marg <- left_join(known_marg, unknown_marg %>% dplyr::select(GDLcode, shdi), by = 'GDLcode')
        
      }
      
      #known_marg['ISOcode'] <- substr(known_marg$GDLcode, 1, 3)
      
      #known_marg <- left_join(known_marg, similarcountries %>% dplyr::select(ISOcode, LISDHS), by = 'ISOcode')
      
      known_marg <- na.omit(known_marg)
      mean_region <- similar_k_regions(gdlcode, known_marg, unknown_marg2, k, columnnames) # CHECK DIT
      unknown_marg2[unknown_marg2$GDLcode == gdlcode, c(columnnames_est)] <- mean_region
      
    }
    
    known_marg <- rbind(LIS_marginals, DHS_marginals)
    known_marg <- na.omit(known_marg)
    known_marg <- left_join(known_marg, unknown_marg %>% dplyr::select(GDLcode, shdi), by = 'GDLcode')
    error_calc_df <- left_join(known_marg, unknown_marg2 %>% dplyr::select(-shdi), by = 'GDLcode')
    
    for (n in 1:length(columnnames)){
      error_calc_df[paste0('absE', as.character(n))] <- abs(error_calc_df[paste0('X', as.character(n))] - error_calc_df[paste0('X', as.character(n), 'est')])
      error_calc_df[paste0('absEpercent', as.character(n))] <- (abs(error_calc_df[paste0('X', as.character(n))] - error_calc_df[paste0('X', as.character(n), 'est')]))/error_calc_df$Population
    }
    
    
    error_calc_df['absEsum'] <- rowSums(error_calc_df %>% dplyr::select(all_of(errornames)))
    error_calc_df['absEpercentsum'] <- rowSums(error_calc_df %>% dplyr::select(all_of(errornames_percent)))
    
    if (attribute %ni% c('ROOFCAT', 'WALLCAT', 'FLOORCAT', 'FARMING')){
      error_calc_df_LIS <- error_calc_df[error_calc_df$LISDHS == 'LIS',]
      error_DF_k_LIS[k-(startk-1),'sum_abs_error'] <- sum(error_calc_df_LIS['absEsum'])
      error_DF_k_LIS[k-(startk-1),'sum_abs_percent_error'] <- sum(error_calc_df_LIS['absEpercentsum'])
    }
      
    error_calc_df_DHS <- error_calc_df[error_calc_df$LISDHS == 'DHS',]
    
    error_DF_k[k-(startk-1),'sum_abs_error'] <- sum(error_calc_df['absEsum'])
    error_DF_k_DHS[k-(startk-1),'sum_abs_error'] <- sum(error_calc_df_DHS['absEsum'])
    
    # percent error
    error_DF_k[k-(startk-1),'sum_abs_percent_error'] <- sum(error_calc_df['absEpercentsum'])
    error_DF_k_DHS[k-(startk-1),'sum_abs_percent_error'] <- sum(error_calc_df_DHS['absEpercentsum'])
    
    
  }
  
  error_DF_k <- error_DF_k[order(error_DF_k$sum_abs_percent_error, decreasing = FALSE), ] #change to percent error
  if (attribute %ni% c('ROOFCAT', 'WALLCAT', 'FLOORCAT', 'FARMING')){
    error_DF_k_LIS <- error_DF_k_LIS[order(error_DF_k_LIS$sum_abs_percent_error, decreasing = FALSE), ]
    
    print('error_DF_k_LIS')
    print(error_DF_k_LIS)
    
  }
  
  error_DF_k_DHS <- error_DF_k_DHS[order(error_DF_k_DHS$sum_abs_percent_error, decreasing = FALSE), ]
  print('error_DF_k_DHS')
  print(error_DF_k_DHS)
  #filenameerror <- paste0('Error_perk_marginals_', attribute, '_oct23.csv')
  #write.csv(error_DF_k, filenameerror, row.names = FALSE)
  
  
  bestk <- error_DF_k[1,'k']
  print('bestk') 
  print(bestk)
  
  if (attribute %ni% c('ROOFCAT', 'WALLCAT', 'FLOORCAT', 'FARMING')){
    bestk_LIS <- error_DF_k_LIS[1,'k']
    print('bestk_LIS') 
    print(bestk_LIS)
  } else if (attribute %in% c('ROOFCAT', 'WALLCAT', 'FLOORCAT', 'FARMING')){
    bestk_LIS <- -99
  }
  
  bestk_DHS <- error_DF_k_DHS[1,'k']
  print('bestk_DHS') 
  print(bestk_DHS)
  
  
  best_k_DF[att_count, 'bestk'] <- bestk
  best_k_DF[att_count, 'bestk_LIS'] <- bestk_LIS
  best_k_DF[att_count, 'bestk_DHS'] <- bestk_DHS
  

  
}

print(best_k_DF)

write.table(best_k_DF, 'bestk_june24_percenterror.csv', row.names = FALSE, sep = ',')

setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2')



estimate_unknown_marg <- function(){

  
  bestk_table <- read.csv('bestk_june24_percenterror.csv')
    
  for (attribute in choosefrom){
    
    if (attribute == 'HHTYPE'){
      columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8')
      
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
      LIS_marginals <- read.csv("LISmarg_hhtype_may24.csv") 
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
              'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
              'FINr102', 'FINr103', 'FINr104', 'FINr105'),]
      
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
      DHS_marginals <- read.csv("DHS_observedmarginals_hhtype_aug24.csv") 
      
      
    } else if (attribute == 'AGECAT'){
      columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8')
      
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
      LIS_marginals <- read.csv("LISmarg_age_may24.csv")  
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
                'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
                 'FINr102', 'FINr103', 'FINr104', 'FINr105'),]
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
      DHS_marginals <- read.csv("DHS_observedmarginals_age_aug24.csv") 
      
    } else if (attribute == 'INCOME'){
      columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5')
      
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
      LIS_marginals <- read.csv("LISmarg_income_may24.csv") 
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
               'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
               'FINr102', 'FINr103', 'FINr104', 'FINr105'),]
      
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
      DHS_marginals <- read.csv("DHS_observedmarginals_wealth_aug24.csv") 
      
    } else if (attribute == 'GENDER'){
      columnnames <- c('X1', 'X2')
      
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
      LIS_marginals <- read.csv("LISmarg_gender_may24.csv") 
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
              'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
              'FINr102', 'FINr103', 'FINr104', 'FINr105'),]
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
      DHS_marginals <- read.csv("DHS_observedmarginals_gender_aug24.csv") 
      
    } else if (attribute == 'ROOFCAT'){
      columnnames <- c('X1', 'X2', 'X3')
      
      LIS_marginals <- c()
      
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
      DHS_marginals <- read.csv("DHS_observedmarginals_roofcat_aug24.csv") 
      
    } else if (attribute == 'WALLCAT'){
      columnnames <- c('X1', 'X2', 'X3')
      
      LIS_marginals <- c()
      
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
      DHS_marginals <- read.csv("DHS_observedmarginals_wallcat_aug24.csv") 
      
    } else if (attribute == 'FLOORCAT'){
      columnnames <- c('X1', 'X2', 'X3')
      
      LIS_marginals <- c()
      
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
      DHS_marginals <- read.csv("DHS_observedmarginals_floorcat_aug24.csv") 
      
    } else if (attribute == 'FARMING'){
      columnnames <- c('X1', 'X2')
      
      LIS_marginals <- c()
      
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
      DHS_marginals <- read.csv("DHS_observedmarginals_farming_aug24.csv") 
      
    } else if (attribute == 'EDUCAT'){
      columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5')
      
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
      LIS_marginals <- read.csv("LISmarg_edu_may24.csv") 
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
               'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
              'FINr102', 'FINr103', 'FINr104', 'FINr105'),]
      
      
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
      DHS_marginals <- read.csv("DHS_observedmarginals_edu_aug24.csv")  
      
      
      
      LIS_marginals[is.na(LIS_marginals)] <- 0
      
      LIS_marginals['Totalfreq'] <- LIS_marginals$X1 + LIS_marginals$X2 + LIS_marginals$X3 + LIS_marginals$X4 + LIS_marginals$X5 
      
      # take average of countries without missing category. 
      LIS_marginals_1 <- LIS_marginals[LIS_marginals$X1 > 0,]
      LIS_marginals_1['Totalfreq'] <- LIS_marginals_1$X1 + LIS_marginals_1$X2 + LIS_marginals_1$X3 + LIS_marginals_1$X4 + LIS_marginals_1$X5 
      
      LIS_marginals_2 <- LIS_marginals[LIS_marginals$X2 > 0,]
      LIS_marginals_2['Totalfreq'] <- LIS_marginals_2$X1 + LIS_marginals_2$X2 + LIS_marginals_2$X3 + LIS_marginals_2$X4 + LIS_marginals_2$X5 
      
      LIS_marginals_3 <- LIS_marginals[LIS_marginals$X3 > 0,]
      LIS_marginals_3['Totalfreq'] <- LIS_marginals_3$X1 + LIS_marginals_3$X2 + LIS_marginals_3$X3 + LIS_marginals_3$X4 + LIS_marginals_3$X5 
      
      LIS_marginals_4 <- LIS_marginals[LIS_marginals$X4 > 0,]
      LIS_marginals_4['Totalfreq'] <- LIS_marginals_4$X1 + LIS_marginals_4$X2 + LIS_marginals_4$X3 + LIS_marginals_4$X4 + LIS_marginals_4$X5 
      
      LIS_marginals_5 <- LIS_marginals[LIS_marginals$X5 > 0,]
      LIS_marginals_5['Totalfreq'] <- LIS_marginals_5$X1 + LIS_marginals_5$X2 + LIS_marginals_5$X3 + LIS_marginals_5$X4 + LIS_marginals_5$X5 
      
      average_X1 <- sum(LIS_marginals_1$X1)/sum(LIS_marginals_1$Totalfreq)
      average_X2 <- sum(LIS_marginals_2$X2)/sum(LIS_marginals_2$Totalfreq)
      average_X3 <- sum(LIS_marginals_3$X3)/sum(LIS_marginals_3$Totalfreq)
      average_X4 <- sum(LIS_marginals_4$X4)/sum(LIS_marginals_4$Totalfreq)
      average_X5 <- sum(LIS_marginals_5$X5)/sum(LIS_marginals_5$Totalfreq)
      
      sum_averages <- average_X1 + average_X2 + average_X3 + average_X4 + average_X5
      
      average_X1 = average_X1/sum_averages
      average_X2 = average_X2/sum_averages
      average_X3 = average_X3/sum_averages
      average_X4 = average_X4/sum_averages
      average_X5 = average_X5/sum_averages
      
      LIS_marginals$X1[LIS_marginals$X1 == 0] <- average_X1 * LIS_marginals$Totalfreq[LIS_marginals$X1 == 0]
      LIS_marginals$X2[LIS_marginals$X2 == 0] <- average_X2 * LIS_marginals$Totalfreq[LIS_marginals$X2 == 0]
      LIS_marginals$X3[LIS_marginals$X3 == 0] <- average_X3 * LIS_marginals$Totalfreq[LIS_marginals$X3 == 0]
      LIS_marginals$X4[LIS_marginals$X4 == 0] <- average_X4 * LIS_marginals$Totalfreq[LIS_marginals$X4 == 0]
      LIS_marginals$X5[LIS_marginals$X5 == 0] <- average_X5 * LIS_marginals$Totalfreq[LIS_marginals$X5 == 0]
      
      
      LIS_marginals['Totalfreq2'] <- LIS_marginals$X1 + LIS_marginals$X2 + LIS_marginals$X3 + LIS_marginals$X4 + LIS_marginals$X5 
      
      LIS_marginals['Correction'] <- LIS_marginals$Totalfreq/LIS_marginals$Totalfreq2
      
      LIS_marginals$X1 <- LIS_marginals$X1 * LIS_marginals$Correction 
      LIS_marginals$X2 <- LIS_marginals$X2 * LIS_marginals$Correction 
      LIS_marginals$X3 <- LIS_marginals$X3 * LIS_marginals$Correction 
      LIS_marginals$X4 <- LIS_marginals$X4 * LIS_marginals$Correction 
      LIS_marginals$X5 <- LIS_marginals$X5 * LIS_marginals$Correction 
      
      LIS_marginals <- LIS_marginals %>% dplyr::select(GDLcode, X1, X2, X3, X4, X5)
      
      
      DHS_marginals[is.na(DHS_marginals)] <- 0
      
      DHS_marginals['Totalfreq'] <- DHS_marginals$X1 + DHS_marginals$X2 + DHS_marginals$X3 + DHS_marginals$X4 + DHS_marginals$X5 
      
      # take average of countries without missing category. 
      DHS_marginals_1 <- DHS_marginals[DHS_marginals$X1 > 0,]
      DHS_marginals_1['Totalfreq'] <- DHS_marginals_1$X1 + DHS_marginals_1$X2 + DHS_marginals_1$X3 + DHS_marginals_1$X4 + DHS_marginals_1$X5 
      
      DHS_marginals_2 <- DHS_marginals[DHS_marginals$X2 > 0,]
      DHS_marginals_2['Totalfreq'] <- DHS_marginals_2$X1 + DHS_marginals_2$X2 + DHS_marginals_2$X3 + DHS_marginals_2$X4 + DHS_marginals_2$X5 
      
      DHS_marginals_3 <- DHS_marginals[DHS_marginals$X3 > 0,]
      DHS_marginals_3['Totalfreq'] <- DHS_marginals_3$X1 + DHS_marginals_3$X2 + DHS_marginals_3$X3 + DHS_marginals_3$X4 + DHS_marginals_3$X5 
      
      DHS_marginals_4 <- DHS_marginals[DHS_marginals$X4 > 0,]
      DHS_marginals_4['Totalfreq'] <- DHS_marginals_4$X1 + DHS_marginals_4$X2 + DHS_marginals_4$X3 + DHS_marginals_4$X4 + DHS_marginals_4$X5 
      
      DHS_marginals_5 <- DHS_marginals[DHS_marginals$X5 > 0,]
      DHS_marginals_5['Totalfreq'] <- DHS_marginals_5$X1 + DHS_marginals_5$X2 + DHS_marginals_5$X3 + DHS_marginals_5$X4 + DHS_marginals_5$X5 
      
      average_X1 <- sum(DHS_marginals_1$X1)/sum(DHS_marginals_1$Totalfreq)
      average_X2 <- sum(DHS_marginals_2$X2)/sum(DHS_marginals_2$Totalfreq)
      average_X3 <- sum(DHS_marginals_3$X3)/sum(DHS_marginals_3$Totalfreq)
      average_X4 <- sum(DHS_marginals_4$X4)/sum(DHS_marginals_4$Totalfreq)
      average_X5 <- sum(DHS_marginals_5$X5)/sum(DHS_marginals_5$Totalfreq)
      
      sum_averages <- average_X1 + average_X2 + average_X3 + average_X4 + average_X5
      
      average_X1 = average_X1/sum_averages
      average_X2 = average_X2/sum_averages
      average_X3 = average_X3/sum_averages
      average_X4 = average_X4/sum_averages
      average_X5 = average_X5/sum_averages
      
      DHS_marginals$X1[DHS_marginals$X1 == 0] <- average_X1 * DHS_marginals$Totalfreq[DHS_marginals$X1 == 0]
      DHS_marginals$X2[DHS_marginals$X2 == 0] <- average_X2 * DHS_marginals$Totalfreq[DHS_marginals$X2 == 0]
      DHS_marginals$X3[DHS_marginals$X3 == 0] <- average_X3 * DHS_marginals$Totalfreq[DHS_marginals$X3 == 0]
      DHS_marginals$X4[DHS_marginals$X4 == 0] <- average_X4 * DHS_marginals$Totalfreq[DHS_marginals$X4 == 0]
      DHS_marginals$X5[DHS_marginals$X5 == 0] <- average_X5 * DHS_marginals$Totalfreq[DHS_marginals$X5 == 0]
      
      
      DHS_marginals['Totalfreq2'] <- DHS_marginals$X1 + DHS_marginals$X2 + DHS_marginals$X3 + DHS_marginals$X4 + DHS_marginals$X5 
      
      DHS_marginals['Correction'] <- DHS_marginals$Totalfreq/DHS_marginals$Totalfreq2
      
      DHS_marginals$X1 <- DHS_marginals$X1 * DHS_marginals$Correction 
      DHS_marginals$X2 <- DHS_marginals$X2 * DHS_marginals$Correction 
      DHS_marginals$X3 <- DHS_marginals$X3 * DHS_marginals$Correction 
      DHS_marginals$X4 <- DHS_marginals$X4 * DHS_marginals$Correction 
      DHS_marginals$X5 <- DHS_marginals$X5 * DHS_marginals$Correction 
      
      DHS_marginals <- DHS_marginals %>% dplyr::select(GDLcode, X1, X2, X3, X4, X5)
      
    } else if (attribute == 'HHSIZECAT'){
      #hhsizecat 0 invullen. 
      
      columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6')
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
      DHS_marginals <- read.csv("DHS_observedmarginals_hhsizecat_aug24.csv", sep = ',')
      
      # LIS
      setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
      LIS_marginals <- read.csv("LISmarg_hhsize_may24.csv") 
      
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
           'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
      LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
           'FINr102', 'FINr103', 'FINr104', 'FINr105'),]
      
      LIS_marginals[is.na(LIS_marginals)] <- 0
      
    } 
    
    # population_data klopt. 
    setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
    popdata_GHSPOP <- read.csv('GHSpopulationGDLregions.csv')
    
    #shdi 
    setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\HHTYPE_files')
    HDI_data <- read.csv("SHDI-SGDI-Total 5.0.csv")
    
    nopop_HDI <- HDI_data[is.na(HDI_data$pop),] #population zit erin, behalve Canada and Tonga. 
    
    # Canada and Tonga have no population under GDL5.0, therefore, these population numbers are taken from GDL4.0
    GDL4.0 <- read.csv("SHDI Complete 4.0 (1).csv") %>% filter(year == 2015) %>% filter(country %in% c('Canada','Tonga')) %>% dplyr::select(GDLCODE, pop) %>% mutate(pop = pop * 1000)
    GDL4.0 <- GDL4.0[GDL4.0$GDLCODE != 'CANt',]
    
    # popdata uit ander bestand ingevuld. 
    HDI_data15 <- HDI_data[HDI_data$year == 2015,]
    
    HDI_data15[HDI_data15$GDLCODE %in% GDL4.0$GDLCODE ,]$pop <- GDL4.0$pop
    
    # Population Tonga only available on national level, so we drop regions Tonga.  
    
    HDI_data15 <- HDI_data15[HDI_data15$GDLCODE %ni% c(HDI_data15[HDI_data15$country == 'Tonga' & HDI_data15$level == 'Subnat',]$GDLCODE),]
    
    HDI_data15 <- HDI_data15 %>% rename(GDLcode = GDLCODE)
    
    unknown_marg <- popdata_GHSPOP %>% dplyr::select(GDLcode, shdi)
    
    unknown_marg['ISOcode'] <- substr(unknown_marg$GDLcode, 1, 3)
    unknown_marg <- left_join(unknown_marg, similarcountries %>% dplyr::select(ISOcode, LISDHS), by = 'ISOcode')
    
    
    unknown_marg <- left_join(unknown_marg, popdata_GHSPOP %>% dplyr::select(GDLcode, Population), by = 'GDLcode')
    
    unknown_marg$LISDHS[unknown_marg$GDLcode == 'MLTt'] <- 'LIS'
    
    #for (n in columnnames_est){
    #  unknown_marg[`n`] <- -99
    #}
    
    
    
    
    
    #cor_matrix_subset <- cor(known_marg[c("shdi", columnnames)])
    
    
    # add population size to unknown marg
    
    
    
    
    
    
    # to calculate error
    
    columnnames_est <- paste0(columnnames, 'est')
    
    
    known_marg <- rbind(DHS_marginals, LIS_marginals)
    
    known_marg <- left_join(known_marg, unknown_marg %>% dplyr::select(GDLcode, shdi), by = 'GDLcode')
    
    unknown_marg <- unknown_marg[unknown_marg$GDLcode != "",]
    
    for (gdlcode in unknown_marg$GDLcode){
      
  
      database = unknown_marg[unknown_marg$GDLcode == gdlcode,]$LISDHS #was unknown_marg2
      
      if (attribute %in% c('ROOFCAT', 'WALLCAT', 'FLOORCAT', 'FARMING')){
        database = 'DHS'
      }
      
      if (database == 'LIS'){
        known_marg <- LIS_marginals
        known_marg <- left_join(known_marg, unknown_marg %>% dplyr::select(GDLcode, shdi), by = 'GDLcode')
        
        bestk_row <- bestk_table[bestk_table$Attributes == attribute,]
        bestk <- bestk_row$bestk_LIS
      } 
      
      if (database == 'DHS'){
        known_marg <- DHS_marginals
        known_marg <- left_join(known_marg, unknown_marg %>% dplyr::select(GDLcode, shdi), by = 'GDLcode')
        
        bestk_row <- bestk_table[bestk_table$Attributes == attribute,]
        bestk <- bestk_row$bestk_DHS
      }
      
      
  
      # bestk uit tabel! 
      
      known_marg <- na.omit(known_marg)
      mean_region <- similar_k_regions(gdlcode, known_marg, unknown_marg, bestk, columnnames)
      unknown_marg[unknown_marg$GDLcode == gdlcode, c(columnnames_est)] <- mean_region
      
    }
    
    
  
    setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2')
    filename <- paste0('Est_marginals_', attribute, '_june24.csv') #Tonga
    write.csv(unknown_marg, filename, row.names = FALSE)
  }
}


#na_unknown <- unknown_marg[is.na(unknown_marg$X1est),]



############################################
############ MARGINALS INCOME ##############




# observed income

error_calc_income_LIS <- data.frame(nr_groups = c(5, 6, 7, 8, 9, 10))
error_calc_income_LIS['error'] <- -99

# 6 groups. 

for (group in c(6)){

  setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2')
  est_marg_income <- read.csv('Est_marginals_INCOME_june24.csv')
  
  columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5')
  
  setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
  LIS_marginals <- read.csv("LISmarg_income_may24.csv") 
  LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr117',]
  LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode != 'ESPr118',]
  LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('AUSr101',
                                                              'AUSr102', 'AUSr103', 'AUSr104', 'AUSr105', 'AUSr106', 'AUSr107', 'AUSr108'),]
  LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FINr101',
                                                              'FINr102', 'FINr103', 'FINr104', 'FINr105'),]
  LIS_marginals['iso_code'] <- substr(LIS_marginals$GDLcode, 1,3)
  
  # merge with shdi
  # population_data klopt. 
  setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
  popdata_GHSPOP <- read.csv('GHSpopulationGDLregions.csv')
  
  #shdi 
  setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\HHTYPE_files')
  HDI_data <- read.csv("SHDI-SGDI-Total 5.0.csv")
  
  nopop_HDI <- HDI_data[is.na(HDI_data$pop),] #population zit erin, behalve Canada and Tonga. 
  
  # Canada and Tonga have no population under GDL5.0, therefore, these population numbers are taken from GDL4.0
  GDL4.0 <- read.csv("SHDI Complete 4.0 (1).csv") %>% filter(year == 2015) %>% filter(country %in% c('Canada','Tonga')) %>% dplyr::select(GDLCODE, pop) %>% mutate(pop = pop * 1000)
  GDL4.0 <- GDL4.0[GDL4.0$GDLCODE != 'CANt',]
  
  # popdata uit ander bestand ingevuld. 
  HDI_data15 <- HDI_data[HDI_data$year == 2015,]
  
  HDI_data15[HDI_data15$GDLCODE %in% GDL4.0$GDLCODE ,]$pop <- GDL4.0$pop
  
  # Population Tonga only available on national level, so we drop regions Tonga.  
  
  HDI_data15 <- HDI_data15[HDI_data15$GDLCODE %ni% c(HDI_data15[HDI_data15$country == 'Tonga' & HDI_data15$level == 'Subnat',]$GDLCODE),]
  
  HDI_data15 <- HDI_data15 %>% rename(GDLcode = GDLCODE)
  
  
  #HDI_data15_DHS <- HDI_data15[HDI_data15$iso_code %in% unique(DHS_marginals$iso_code),]
  HDI_data15_LIS <- HDI_data15[HDI_data15$iso_code %in% unique(LIS_marginals$iso_code),]
  
  LIS_marginals <- left_join(HDI_data15_LIS %>% dplyr::select(GDLcode, level, shdi), LIS_marginals, by = 'GDLcode')
  LIS_marginals <- LIS_marginals[LIS_marginals$GDLcode %ni% c('FRAr129', 'FRAr128', 'FRAr127', 'FRAr126', 'FRAr125', 'FRAr126', 'ESPr117', 'ESPr118'),]
  
  
  LIS_marginals['iso_code'] <- substr(LIS_marginals$GDLcode, 1,3)
  LIS_marginals$Population <- LIS_marginals$X1 + LIS_marginals$X2 + LIS_marginals$X3 + LIS_marginals$X4 + LIS_marginals$X5
  LIS_marginals$X1 <- LIS_marginals$X1/LIS_marginals$Population
  LIS_marginals$X2 <- LIS_marginals$X2/LIS_marginals$Population
  LIS_marginals$X3 <- LIS_marginals$X3/LIS_marginals$Population
  LIS_marginals$X4 <- LIS_marginals$X4/LIS_marginals$Population
  LIS_marginals$X5 <- LIS_marginals$X5/LIS_marginals$Population
  
  LIS_marginals[is.na(LIS_marginals)] <- 0.2
  
  LIS_marginals_country <- LIS_marginals[LIS_marginals$level == 'National',]
  LIS_marginals_country <- LIS_marginals_country %>% rename('shdi_country' = 'shdi')
  
  LIS_marginals <- left_join(LIS_marginals, LIS_marginals_country %>% dplyr::select(iso_code, shdi_country), by = 'iso_code')
  
  LIS_marginals['rel_shdi'] <- LIS_marginals['shdi']/LIS_marginals['shdi_country']
  
  LIS_marginals <- LIS_marginals[order(LIS_marginals$rel_shdi), ]
  
  LIS_marginals_nonational <- LIS_marginals[LIS_marginals$level != 'National',]
  
  nr_groups = group
  
  percentiles_LIS <- quantile(LIS_marginals_nonational$rel_shdi, probs = seq(1/nr_groups, 1, by = 1/nr_groups))
  
  group1 <- LIS_marginals_nonational[LIS_marginals_nonational$rel_shdi <= percentiles_LIS[1],]
  mean_group1 <- c(mean(group1$X1), mean(group1$X2), mean(group1$X3), mean(group1$X4), mean(group1$X5))
  
  group2 <- LIS_marginals_nonational[LIS_marginals_nonational$rel_shdi > percentiles_LIS[1] & LIS_marginals_nonational$rel_shdi <= percentiles_LIS[2],]
  mean_group2 <- c(mean(group2$X1), mean(group2$X2), mean(group2$X3), mean(group2$X4), mean(group2$X5))
  
  group3 <- LIS_marginals_nonational[LIS_marginals_nonational$rel_shdi > percentiles_LIS[2] & LIS_marginals_nonational$rel_shdi <= percentiles_LIS[3],]
  mean_group3 <- c(mean(group3$X1), mean(group3$X2), mean(group3$X3), mean(group3$X4), mean(group3$X5))
  
  group4 <- LIS_marginals_nonational[LIS_marginals_nonational$rel_shdi > percentiles_LIS[3] & LIS_marginals_nonational$rel_shdi <= percentiles_LIS[4],]
  mean_group4 <- c(mean(group4$X1), mean(group4$X2), mean(group4$X3), mean(group4$X4), mean(group4$X5))
  
  group5 <- LIS_marginals_nonational[LIS_marginals_nonational$rel_shdi > percentiles_LIS[4] & LIS_marginals_nonational$rel_shdi <= percentiles_LIS[5],]
  mean_group5 <- c(mean(group5$X1), mean(group5$X2), mean(group5$X3), mean(group5$X4), mean(group5$X5))
  
  if (nr_groups >= 6){
    group6 <- LIS_marginals_nonational[LIS_marginals_nonational$rel_shdi > percentiles_LIS[5] & LIS_marginals_nonational$rel_shdi <= percentiles_LIS[6],]
    mean_group6 <- c(mean(group6$X1), mean(group6$X2), mean(group6$X3), mean(group6$X4), mean(group6$X5))
  }
  
  if (nr_groups >= 7){
    group7 <- LIS_marginals_nonational[LIS_marginals_nonational$rel_shdi > percentiles_LIS[6] & LIS_marginals_nonational$rel_shdi <= percentiles_LIS[7],]
    mean_group7 <- c(mean(group7$X1), mean(group7$X2), mean(group7$X3), mean(group7$X4), mean(group7$X5))
  }
  
  if (nr_groups >= 8){
    group8 <- LIS_marginals_nonational[LIS_marginals_nonational$rel_shdi > percentiles_LIS[7] & LIS_marginals_nonational$rel_shdi <= percentiles_LIS[8],]
    mean_group8 <- c(mean(group8$X1), mean(group8$X2), mean(group8$X3), mean(group8$X4), mean(group8$X5))
  }
  
  if (nr_groups >= 9){
    group9 <- LIS_marginals_nonational[LIS_marginals_nonational$rel_shdi > percentiles_LIS[8] & LIS_marginals_nonational$rel_shdi <= percentiles_LIS[9],]
    mean_group9 <- c(mean(group9$X1), mean(group9$X2), mean(group9$X3), mean(group9$X4), mean(group9$X5))
  }
  
  if (nr_groups >= 10){
    group10 <- LIS_marginals_nonational[LIS_marginals_nonational$rel_shdi > percentiles_LIS[9],]
    mean_group10 <- c(mean(group10$X1), mean(group10$X2), mean(group10$X3), mean(group10$X4), mean(group10$X5))
  }
  
  
  HDI_data15_nat <- HDI_data15[HDI_data15$level == 'National',]
  HDI_data15_nat <- HDI_data15_nat %>% rename('ISOcode' = 'iso_code', 'shdi_country' = 'shdi')
  
  est_marg_income <- left_join(est_marg_income, HDI_data15_nat %>% dplyr::select(ISOcode, shdi_country), by = 'ISOcode')
  
  est_marg_income['rel_shdi'] <- est_marg_income['shdi']/est_marg_income['shdi_country']
  est_marg_income$rel_shdi[est_marg_income$GDLcode == 'MLTt'] <- 1
  
  
  est_marg_income_LIS <- est_marg_income[est_marg_income$LISDHS == 'LIS',]
  
  percentiles_LIS[length(percentiles_LIS)] <- 100
  
  est_marg_income_LIS[, c('X1est', 'X2est', 'X3est', 'X4est', 'X5est')] <- t(apply(est_marg_income_LIS, 1, function(row) {
    rel_shdi_value <- row["rel_shdi"]
    print(rel_shdi_value) 
    if (row["rel_shdi"] <= percentiles_LIS[1]) {
      return(mean_group1)
    } else if (row["rel_shdi"] <= percentiles_LIS[2]) {
      return(mean_group2)
    } else if (row["rel_shdi"] <= percentiles_LIS[3]){
      return(mean_group3) 
    } else if (row["rel_shdi"] <= percentiles_LIS[4]){
      return(mean_group4)  
    } else if (row["rel_shdi"] <= percentiles_LIS[5]){
      return(mean_group5)  
    } else if (row["rel_shdi"] <= percentiles_LIS[6]){
      return(mean_group6) 
    } else if (row["rel_shdi"] <= percentiles_LIS[7]){
      return(mean_group7) 
    } else if (row["rel_shdi"] <= percentiles_LIS[8]){
      return(mean_group8) 
    } else if (row["rel_shdi"] <= percentiles_LIS[9]){
      return(mean_group9) 
    } else if (row["rel_shdi"] <= percentiles_LIS[10]){
      return(mean_group10)
    }
  }))
  
  est_marg_income_LIS$X1est <- est_marg_income_LIS$X1est * est_marg_income_LIS$Population 
  est_marg_income_LIS$X2est <- est_marg_income_LIS$X2est * est_marg_income_LIS$Population 
  est_marg_income_LIS$X3est <- est_marg_income_LIS$X3est * est_marg_income_LIS$Population 
  est_marg_income_LIS$X4est <- est_marg_income_LIS$X4est * est_marg_income_LIS$Population 
  est_marg_income_LIS$X5est <- est_marg_income_LIS$X5est * est_marg_income_LIS$Population 
  
  # rechttrekken income
  est_marg_income_LIS_gr <- est_marg_income_LIS %>% group_by(ISOcode) %>% summarize(X1tot = sum(X1est),
            X2tot = sum(X2est), X3tot = sum(X3est), X4tot = sum(X4est), X5tot = sum(X5est))
  
  est_marg_income_LIS_gr['Population'] <- est_marg_income_LIS_gr$X1tot + est_marg_income_LIS_gr$X2tot + 
    est_marg_income_LIS_gr$X3tot + est_marg_income_LIS_gr$X4tot + est_marg_income_LIS_gr$X5tot
  
  est_marg_income_LIS_gr['X1c'] <- (est_marg_income_LIS_gr$Population/5) / est_marg_income_LIS_gr$X1tot
  est_marg_income_LIS_gr['X2c'] <- (est_marg_income_LIS_gr$Population/5) / est_marg_income_LIS_gr$X2tot
  est_marg_income_LIS_gr['X3c'] <- (est_marg_income_LIS_gr$Population/5) / est_marg_income_LIS_gr$X3tot
  est_marg_income_LIS_gr['X4c'] <- (est_marg_income_LIS_gr$Population/5) / est_marg_income_LIS_gr$X4tot
  est_marg_income_LIS_gr['X5c'] <- (est_marg_income_LIS_gr$Population/5) / est_marg_income_LIS_gr$X5tot
  
  
  est_marg_income_LIS <- left_join(est_marg_income_LIS, est_marg_income_LIS_gr %>% dplyr::select(ISOcode, X1c, X2c, X3c, X4c, X5c), by = 'ISOcode')
  
  est_marg_income_LIS$X1est <- est_marg_income_LIS$X1est * est_marg_income_LIS$X1c 
  est_marg_income_LIS$X2est <- est_marg_income_LIS$X2est * est_marg_income_LIS$X2c 
  est_marg_income_LIS$X3est <- est_marg_income_LIS$X3est * est_marg_income_LIS$X3c 
  est_marg_income_LIS$X4est <- est_marg_income_LIS$X4est * est_marg_income_LIS$X4c 
  est_marg_income_LIS$X5est <- est_marg_income_LIS$X5est * est_marg_income_LIS$X5c 
  
  # est_marg_income_LIS 
  # LIS marginals
  
  LIS_marginals2 <- LIS_marginals[LIS_marginals$Population != 0.2,]
  LIS_marginals_error <- left_join(LIS_marginals2, est_marg_income_LIS %>% dplyr::select(GDLcode, X1est, X2est, X3est, X4est, X5est), by = 'GDLcode')
  
  LIS_marginals_error <- na.omit(LIS_marginals_error)
  
  LIS_marginals_error$X1 <- LIS_marginals_error$X1* LIS_marginals_error$Population
  LIS_marginals_error$X2 <- LIS_marginals_error$X2* LIS_marginals_error$Population
  LIS_marginals_error$X3 <- LIS_marginals_error$X3* LIS_marginals_error$Population
  LIS_marginals_error$X4 <- LIS_marginals_error$X4* LIS_marginals_error$Population
  LIS_marginals_error$X5 <- LIS_marginals_error$X5* LIS_marginals_error$Population
  
  for (n in 1:length(columnnames)){
    LIS_marginals_error[paste0('absEpercent', as.character(n))] <- (abs(LIS_marginals_error[paste0('X', as.character(n))] - LIS_marginals_error[paste0('X', as.character(n), 'est')]))/LIS_marginals_error$Population
  }
  
  errornames_percent <- c()
  for (n in 1:length(columnnames)){
    errornames_percent <- append(errornames_percent, paste0('absEpercent', as.character(n)))
  }
  
  
  LIS_marginals_error['absEpercentsum'] <- rowSums(LIS_marginals_error %>% dplyr::select(all_of(errornames_percent)))
  
  
  
  error_calc_income_LIS[(group-4), 'error'] <- sum(LIS_marginals_error$absEpercentsum)

}


# DHS income marginals
error_calc_income_DHS <- data.frame(nr_groups = c(5, 6, 7, 8, 9, 10))
error_calc_income_DHS['error'] <- -99

# 9 groups. 

for (group in c(9)){

  
  setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2')
  est_marg_income <- read.csv('Est_marginals_INCOME_june24.csv')
  
  setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024')
  DHS_marginals <- read.csv("DHS_observedmarginals_wealth_aug24.csv") 
  #DHS_marginals <- DHS_marginals[DHS_marginals$GDLcode %ni% c('PAKr106', 'PAKr107', 'YEMr103'),]
  DHS_marginals['iso_code'] <- substr(DHS_marginals$GDLcode, 1,3)
  
  
  # merge with shdi
  # population_data klopt. 
  setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')
  popdata_GHSPOP <- read.csv('GHSpopulationGDLregions.csv')
  
  #shdi 
  setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\HHTYPE_files')
  HDI_data <- read.csv("SHDI-SGDI-Total 5.0.csv")
  
  nopop_HDI <- HDI_data[is.na(HDI_data$pop),] #population zit erin, behalve Canada and Tonga. 
  
  # Canada and Tonga have no population under GDL5.0, therefore, these population numbers are taken from GDL4.0
  GDL4.0 <- read.csv("SHDI Complete 4.0 (1).csv") %>% filter(year == 2015) %>% filter(country %in% c('Canada','Tonga')) %>% dplyr::select(GDLCODE, pop) %>% mutate(pop = pop * 1000)
  GDL4.0 <- GDL4.0[GDL4.0$GDLCODE != 'CANt',]
  
  # popdata uit ander bestand ingevuld. 
  HDI_data15 <- HDI_data[HDI_data$year == 2015,]
  
  HDI_data15[HDI_data15$GDLCODE %in% GDL4.0$GDLCODE ,]$pop <- GDL4.0$pop
  
  # Population Tonga only available on national level, so we drop regions Tonga.  
  
  HDI_data15 <- HDI_data15[HDI_data15$GDLCODE %ni% c(HDI_data15[HDI_data15$country == 'Tonga' & HDI_data15$level == 'Subnat',]$GDLCODE),]
  
  HDI_data15 <- HDI_data15 %>% rename(GDLcode = GDLCODE)
  
  
  HDI_data15_DHS <- HDI_data15[HDI_data15$iso_code %in% unique(DHS_marginals$iso_code),]
  HDI_data15_LIS <- HDI_data15[HDI_data15$iso_code %in% unique(LIS_marginals$iso_code),]
  
  DHS_marginals <- left_join(HDI_data15_DHS %>% dplyr::select(GDLcode, level, shdi), DHS_marginals, by = 'GDLcode')
  
  DHS_marginals['iso_code'] <- substr(DHS_marginals$GDLcode, 1,3)
  DHS_marginals$Population <- DHS_marginals$X1 + DHS_marginals$X2 + DHS_marginals$X3 + DHS_marginals$X4 + DHS_marginals$X5
  DHS_marginals$X1 <- DHS_marginals$X1/DHS_marginals$Population
  DHS_marginals$X2 <- DHS_marginals$X2/DHS_marginals$Population
  DHS_marginals$X3 <- DHS_marginals$X3/DHS_marginals$Population
  DHS_marginals$X4 <- DHS_marginals$X4/DHS_marginals$Population
  DHS_marginals$X5 <- DHS_marginals$X5/DHS_marginals$Population
  
  DHS_marginals[is.na(DHS_marginals)] <- 0.2
  
  DHS_marginals_country <- DHS_marginals[DHS_marginals$level == 'National',]
  DHS_marginals_country <- DHS_marginals_country %>% rename('shdi_country' = 'shdi')
  
  DHS_marginals <- left_join(DHS_marginals, DHS_marginals_country %>% dplyr::select(iso_code, shdi_country), by = 'iso_code')
  
  DHS_marginals['rel_shdi'] <- DHS_marginals['shdi']/DHS_marginals['shdi_country']
  
  DHS_marginals <- DHS_marginals[order(DHS_marginals$rel_shdi), ]
  
  DHS_marginals_nonational <- DHS_marginals[DHS_marginals$level != 'National',]
  
  nr_groups = group
  
  percentiles_DHS <- quantile(DHS_marginals_nonational$rel_shdi, probs = seq(1/nr_groups, 1, by = 1/nr_groups))
  
  group1 <- DHS_marginals_nonational[DHS_marginals_nonational$rel_shdi <= percentiles_DHS[1],]
  mean_group1 <- c(mean(group1$X1), mean(group1$X2), mean(group1$X3), mean(group1$X4), mean(group1$X5))
  
  group2 <- DHS_marginals_nonational[DHS_marginals_nonational$rel_shdi > percentiles_DHS[1] & DHS_marginals_nonational$rel_shdi <= percentiles_DHS[2],]
  mean_group2 <- c(mean(group2$X1), mean(group2$X2), mean(group2$X3), mean(group2$X4), mean(group2$X5))
  
  group3 <- DHS_marginals_nonational[DHS_marginals_nonational$rel_shdi > percentiles_DHS[2] & DHS_marginals_nonational$rel_shdi <= percentiles_DHS[3],]
  mean_group3 <- c(mean(group3$X1), mean(group3$X2), mean(group3$X3), mean(group3$X4), mean(group3$X5))
  
  group4 <- DHS_marginals_nonational[DHS_marginals_nonational$rel_shdi > percentiles_DHS[3] & DHS_marginals_nonational$rel_shdi <= percentiles_DHS[4],]
  mean_group4 <- c(mean(group4$X1), mean(group4$X2), mean(group4$X3), mean(group4$X4), mean(group4$X5))
  
  group5 <- DHS_marginals_nonational[DHS_marginals_nonational$rel_shdi > percentiles_DHS[4] & DHS_marginals_nonational$rel_shdi <= percentiles_DHS[5],]
  mean_group5 <- c(mean(group5$X1), mean(group5$X2), mean(group5$X3), mean(group5$X4), mean(group5$X5))
  
  if (nr_groups >= 6){
    group6 <- DHS_marginals_nonational[DHS_marginals_nonational$rel_shdi > percentiles_DHS[5] & DHS_marginals_nonational$rel_shdi <= percentiles_DHS[6],]
    mean_group6 <- c(mean(group6$X1), mean(group6$X2), mean(group6$X3), mean(group6$X4), mean(group6$X5))
  }
  
  if (nr_groups >= 7){
    group7 <- DHS_marginals_nonational[DHS_marginals_nonational$rel_shdi > percentiles_DHS[6] & DHS_marginals_nonational$rel_shdi <= percentiles_DHS[7],]
    mean_group7 <- c(mean(group7$X1), mean(group7$X2), mean(group7$X3), mean(group7$X4), mean(group7$X5))
  }
  
  if (nr_groups >= 8){
    group8 <- DHS_marginals_nonational[DHS_marginals_nonational$rel_shdi > percentiles_DHS[7] & DHS_marginals_nonational$rel_shdi <= percentiles_DHS[8],]
    mean_group8 <- c(mean(group8$X1), mean(group8$X2), mean(group8$X3), mean(group8$X4), mean(group8$X5))
  }
  
  if (nr_groups >= 9){
    group9 <- DHS_marginals_nonational[DHS_marginals_nonational$rel_shdi > percentiles_DHS[8] & DHS_marginals_nonational$rel_shdi <= percentiles_DHS[9],]
    mean_group9 <- c(mean(group9$X1), mean(group9$X2), mean(group9$X3), mean(group9$X4), mean(group9$X5))
  }
  
  if (nr_groups >= 10){
    group10 <- DHS_marginals_nonational[DHS_marginals_nonational$rel_shdi > percentiles_DHS[9],]
    mean_group10 <- c(mean(group10$X1), mean(group10$X2), mean(group10$X3), mean(group10$X4), mean(group10$X5))
  }
  
  
  HDI_data15_nat <- HDI_data15[HDI_data15$level == 'National',]
  HDI_data15_nat <- HDI_data15_nat %>% rename('ISOcode' = 'iso_code', 'shdi_country' = 'shdi')
  
  est_marg_income <- left_join(est_marg_income, HDI_data15_nat %>% dplyr::select(ISOcode, shdi_country), by = 'ISOcode')
  
  est_marg_income$shdi_country[est_marg_income$ISOcode == 'SYR'] <- mean(est_marg_income[est_marg_income$ISOcode == 'SYR',]$shdi)
  
  est_marg_income['rel_shdi'] <- est_marg_income['shdi']/est_marg_income['shdi_country']
  est_marg_income$rel_shdi[est_marg_income$GDLcode == 'MLTt'] <- 1
  
  est_marg_income_DHS <- est_marg_income[est_marg_income$LISDHS == 'DHS',]
  
  
  percentiles_DHS[length(percentiles_DHS)] <- 100
  
  est_marg_income_DHS[, c('X1est', 'X2est', 'X3est', 'X4est', 'X5est')] <- t(apply(est_marg_income_DHS, 1, function(row) {
    rel_shdi_value <- row["rel_shdi"]
    print(rel_shdi_value) 
    if (row["rel_shdi"] <= percentiles_DHS[1]) {
      return(mean_group1)
    } else if (row["rel_shdi"] <= percentiles_DHS[2]) {
      return(mean_group2)
    } else if (row["rel_shdi"] <= percentiles_DHS[3]){
      return(mean_group3) 
    } else if (row["rel_shdi"] <= percentiles_DHS[4]){
      return(mean_group4)  
    } else if (row["rel_shdi"] <= percentiles_DHS[5]){
      return(mean_group5)  
    } else if (row["rel_shdi"] <= percentiles_DHS[6]){
      return(mean_group6) 
    } else if (row["rel_shdi"] <= percentiles_DHS[7]){
      return(mean_group7) 
    } else if (row["rel_shdi"] <= percentiles_DHS[8]){
      return(mean_group8) 
    } else if (row["rel_shdi"] <= percentiles_DHS[9]){
      return(mean_group9) 
    } else if (row["rel_shdi"] <= percentiles_DHS[10]){
      return(mean_group10)
    }
  }))
  
  est_marg_income_DHS$X1est <- est_marg_income_DHS$X1est * est_marg_income_DHS$Population 
  est_marg_income_DHS$X2est <- est_marg_income_DHS$X2est * est_marg_income_DHS$Population 
  est_marg_income_DHS$X3est <- est_marg_income_DHS$X3est * est_marg_income_DHS$Population 
  est_marg_income_DHS$X4est <- est_marg_income_DHS$X4est * est_marg_income_DHS$Population 
  est_marg_income_DHS$X5est <- est_marg_income_DHS$X5est * est_marg_income_DHS$Population 
  
  # rechttrekken income
  est_marg_income_DHS_gr <- est_marg_income_DHS %>% group_by(ISOcode) %>% summarize(X1tot = sum(X1est),
                                                                                    X2tot = sum(X2est), X3tot = sum(X3est), X4tot = sum(X4est), X5tot = sum(X5est))
  
  est_marg_income_DHS_gr['Population'] <- est_marg_income_DHS_gr$X1tot + est_marg_income_DHS_gr$X2tot + 
    est_marg_income_DHS_gr$X3tot + est_marg_income_DHS_gr$X4tot + est_marg_income_DHS_gr$X5tot
  
  est_marg_income_DHS_gr['X1c'] <- (est_marg_income_DHS_gr$Population/5) / est_marg_income_DHS_gr$X1tot
  est_marg_income_DHS_gr['X2c'] <- (est_marg_income_DHS_gr$Population/5) / est_marg_income_DHS_gr$X2tot
  est_marg_income_DHS_gr['X3c'] <- (est_marg_income_DHS_gr$Population/5) / est_marg_income_DHS_gr$X3tot
  est_marg_income_DHS_gr['X4c'] <- (est_marg_income_DHS_gr$Population/5) / est_marg_income_DHS_gr$X4tot
  est_marg_income_DHS_gr['X5c'] <- (est_marg_income_DHS_gr$Population/5) / est_marg_income_DHS_gr$X5tot
  
  
  est_marg_income_DHS <- left_join(est_marg_income_DHS, est_marg_income_DHS_gr %>% dplyr::select(ISOcode, X1c, X2c, X3c, X4c, X5c), by = 'ISOcode')
  
  est_marg_income_DHS$X1est <- est_marg_income_DHS$X1est * est_marg_income_DHS$X1c 
  est_marg_income_DHS$X2est <- est_marg_income_DHS$X2est * est_marg_income_DHS$X2c 
  est_marg_income_DHS$X3est <- est_marg_income_DHS$X3est * est_marg_income_DHS$X3c 
  est_marg_income_DHS$X4est <- est_marg_income_DHS$X4est * est_marg_income_DHS$X4c 
  est_marg_income_DHS$X5est <- est_marg_income_DHS$X5est * est_marg_income_DHS$X5c 
  
  DHS_marginals_nonational <- DHS_marginals_nonational[DHS_marginals_nonational$Population != 0.2,]
  DHS_marginals_error <- left_join(DHS_marginals_nonational, est_marg_income_DHS %>% dplyr::select(GDLcode, X1est, X2est, X3est, X4est, X5est), by = 'GDLcode')
  
  DHS_marginals_error <- na.omit(DHS_marginals_error)
  
  DHS_marginals_error$X1 <- DHS_marginals_error$X1* DHS_marginals_error$Population
  DHS_marginals_error$X2 <- DHS_marginals_error$X2* DHS_marginals_error$Population
  DHS_marginals_error$X3 <- DHS_marginals_error$X3* DHS_marginals_error$Population
  DHS_marginals_error$X4 <- DHS_marginals_error$X4* DHS_marginals_error$Population
  DHS_marginals_error$X5 <- DHS_marginals_error$X5* DHS_marginals_error$Population
  
  for (n in 1:length(columnnames)){
    DHS_marginals_error[paste0('absEpercent', as.character(n))] <- (abs(DHS_marginals_error[paste0('X', as.character(n))] - DHS_marginals_error[paste0('X', as.character(n), 'est')]))/DHS_marginals_error$Population
  }
  
  errornames_percent <- c()
  for (n in 1:length(columnnames)){
    errornames_percent <- append(errornames_percent, paste0('absEpercent', as.character(n)))
  }
  
  
  DHS_marginals_error['absEpercentsum'] <- rowSums(DHS_marginals_error %>% dplyr::select(all_of(errornames_percent)))
  
  
  
  DHS_marginals_error[(group-4), 'error'] <- sum(DHS_marginals_error$absEpercentsum)
  
  error_calc_income_DHS[(group-4), 'error'] <- sum(DHS_marginals_error$absEpercentsum)
}



new_income_marginals <- rbind(est_marg_income_LIS, est_marg_income_DHS)

new_income_marginals <- new_income_marginals %>% dplyr::select(GDLcode, shdi, ISOcode, LISDHS, Population, X1est, X2est, X3est, X4est, X5est)

new_income_marginals_na <- na.omit(new_income_marginals)


write.table(new_income_marginals, 'Est_marginals_INCOME_june24.csv', row.names = FALSE, sep = ',')


