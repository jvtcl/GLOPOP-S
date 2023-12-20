library(dplyr)
library(readxl)
library(gridExtra)
library(purrr)
library(tidyr)
library(data.table)
`%ni%` <- Negate(`%in%`)


##########################################################

# CHOOSE VARIABLE: 

choosefrom <- c('HHSIZECAT', 'GENDER', 'EDUCAT', 'AGECAT', 'HHTYPE')
# now it loops through all variables in choosefrom. 



startk <- 40
endk <- 250

correct_marginals_hdiyear <- 'no' #yes or no. 
# no: use the hdi of the year 2015
# yes: use the hdi of the year when the survey was conducted


##########################################################

# DON'T CHANGE ANYTHING HERE BELOW


# function
similar_k_regions <- function(gdlcode, known_marg, unknown_marg, k, columnnames){
  
  keeprow <- known_marg[known_marg$GDLCODE == gdlcode,]
  
  if (nrow(keeprow) == 1){
    
    countryname <- known_marg[known_marg$GDLCODE == gdlcode,]$country
    
    known_marg <- known_marg[known_marg$country != countryname,]
    
  } else {
    
    keeprow <- unknown_marg[unknown_marg$GDLCODE == gdlcode,]
    
  }
  
  known_marg['hdi1'] <- keeprow$shdi
  
  known_marg['diff_hdi'] <- abs(known_marg$shdi - known_marg$hdi1)
  
  known_marg <- known_marg[order(known_marg$diff_hdi, decreasing = FALSE), ]
  
  selected_regions <- head(known_marg, k)
  
  mean_region <- colMeans(selected_regions %>% select(`columnnames`))
  
  return(mean_region)
  
}



correct_hdiyear <- function(HDI_data, HDI_data15){
  
  dhscountries <- read.csv('DHScountries_sept23.csv', sep = ';') #was eerst DHScountries.csv
  liscodecountries <- read.csv('LIScountries_sept23.csv', sep = ';') #dit bestand is zonder spaties. 
  
  dhscountries['yeardhs'] <- dhscountries['year']  
  liscodecountries['yearlis'] <- liscodecountries['year']  
  
  HDI_data <- left_join(HDI_data, dhscountries %>% select(yeardhs, iso_code), by = 'iso_code')  
  HDI_data <- left_join(HDI_data, liscodecountries %>% select(yearlis, iso_code), by = 'iso_code')  
  
  HDI_data$yeardhs <- replace(HDI_data$yeardhs, is.na(HDI_data$yeardhs), 0)
  HDI_data$yearlis <- replace(HDI_data$yearlis, is.na(HDI_data$yearlis), 0)  
  
  HDI_data$yearmarg <- HDI_data$yeardhs + HDI_data$yearlis
  
  HDI_data$yearmarg[HDI_data$yearmarg == 0] <- 2015
  
  HDI_datayear <- HDI_data[HDI_data$year == HDI_data$yearmarg,] 
  
  HDI_datayear <- HDI_datayear %>% rename(shdiyear = shdi)
  
  HDI_data15 <- left_join(HDI_data15, HDI_datayear %>% select(GDLCODE, shdiyear), by = 'GDLCODE')
  
  HDI_data15$shdiyear <- replace(HDI_data15$shdiyear, is.na(HDI_data15$shdiyear), -1) 
  
  HDI_data15_missingshdiyear <- HDI_data15[HDI_data15$shdiyear == -1,]
  
  
  HDI_data15[HDI_data15$shdiyear == -1 ,]$shdiyear <- HDI_data15_missingshdiyear$shdi
  
  HDI_data15 <- HDI_data15 %>% select(-shdi)
  HDI_data15 <- HDI_data15 %>% rename(shdi = shdiyear)
  
  return(HDI_data15)
  
}

bestkarray <- c()

for (attribute in choosefrom){
  print(attribute)
  
  if (attribute == 'HHTYPE'){
    columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8')
    IPUMS_marginals <- read.csv("IPUMSmarg_hhtype_sept23.csv", sep = ',')
    LIS_marginals <- read.csv("LISmarg_hhtype_oct23.csv") 
  } else if (attribute == 'AGECAT'){
    columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7')
    IPUMS_marginals <- read.csv("IPUMSmarg_age_sept23.csv", sep = ',')
    LIS_marginals <- read.csv("LISmarg_age_oct23.csv") 
  } else if (attribute == 'INCOME'){
    columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5')
    LIS_marginals <- read.csv("LISmarg_income_oct23.csv") 
    LIS_marginals <- LIS_marginals %>% rename(INCOME = INCOMEQQ)
  } else if (attribute == 'EDUCAT'){
    columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5')
    
    # IPUMS
    IPUMS_marginals <- read.csv("IPUMSmarg_edu_sept23.csv", sep = ',')

    # Solve missing educats in marginals: 
    IPUMS_country_ZONE <- unique(IPUMS_marginals[, c("country", "ZONE")])
    educats <- unique(IPUMS_marginals$EDUCAT)
    
    # Step 2: Create a template dataframe for each unique combination of "country" and "ZONE"
    IPUMS_marginals_looplist <- lapply(1:nrow(IPUMS_country_ZONE), function(i) {
      country <- IPUMS_country_ZONE[i, "country"]
      ZONE <- IPUMS_country_ZONE[i, "ZONE"]
      EDUCAT <- educats
    
      # Create a data frame with all categories and set Frequency to 0
      IPUMS_marginals_full <- data.frame(
        country = rep(country, length(educats)),
        ZONE = rep(ZONE, length(educats)),
        EDUCAT = educats,
        Freqzero = 0
      )
      
      return(IPUMS_marginals_full)
    })
    
    IPUMS_marginals_full <- do.call(rbind, IPUMS_marginals_looplist)
    
    IPUMS_marginals_full <- left_join(IPUMS_marginals_full, IPUMS_marginals, by = c('country', 'ZONE', 'EDUCAT'))
    IPUMS_marginals_full$Freq[is.na(IPUMS_marginals_full$Freq)] <- 0
    IPUMS_marginals_full <- IPUMS_marginals_full %>% select(-Freqzero)
    
    IPUMS_missingcountries <- unique(IPUMS_marginals_full[IPUMS_marginals_full$Freq == 0,]$country)
    
    IPUMS_marginals_full2 <- IPUMS_marginals_full[IPUMS_marginals_full$country %ni% IPUMS_missingcountries,]
    
    sum_freq_ZONE <- IPUMS_marginals_full %>% group_by(country, ZONE) %>% summarise(Totalfreq = sum(Freq))
    
    IPUMS_marginals_full2 <- left_join(IPUMS_marginals_full2, sum_freq_ZONE, by = c('country', 'ZONE'))
    IPUMS_marginals_full2['Share_freq'] <- IPUMS_marginals_full2['Freq']/IPUMS_marginals_full2['Totalfreq']
    
    average_educats <- IPUMS_marginals_full2 %>% group_by(EDUCAT) %>% summarise(mean_educats = mean(Share_freq))
      
    IPUMS_marginals_full <- left_join(IPUMS_marginals_full, average_educats, by = 'EDUCAT')
    IPUMS_marginals_full <- left_join(IPUMS_marginals_full, sum_freq_ZONE, by = c('country', 'ZONE'))
    
    # fill in average for 0's in Freq
    IPUMS_marginals_full$Freq[IPUMS_marginals_full$Freq == 0] <- IPUMS_marginals_full[IPUMS_marginals_full$Freq == 0,]$mean_educats * IPUMS_marginals_full[IPUMS_marginals_full$Freq == 0,]$Totalfreq
    
    # calculate sum freq and correct freqs: 
    sum_freq_ZONE <- IPUMS_marginals_full %>% group_by(country, ZONE) %>% summarise(Totalfreq_alledu = sum(Freq))
    IPUMS_marginals_full <- left_join(IPUMS_marginals_full, sum_freq_ZONE, by = c('country', 'ZONE'))
    IPUMS_marginals_full$correct <- IPUMS_marginals_full$Totalfreq / IPUMS_marginals_full$Totalfreq_alledu
    IPUMS_marginals_full$Freq <- IPUMS_marginals_full$Freq * IPUMS_marginals_full$correct
    
    IPUMS_marginals_full <- IPUMS_marginals_full %>% select(ZONE, country, EDUCAT, Freq)
    IPUMS_marginals_full <- IPUMS_marginals_full[, c(names(IPUMS_marginals))] #change order column names
    
    IPUMS_marginals <- copy(IPUMS_marginals_full)
    
    
    # DO THE SAME FOR LIS
    LIS_marginals <- read.csv("LISmarg_edu_oct23.csv") #oct includes uk and au. 
    missing_LIS <- LIS_marginals %>% group_by(liscountrycode, EDUCAT) %>% summarise(Frequency = mean(Frequency)) 
    missing_LIS <- missing_LIS %>% count(liscountrycode)
    LIS_missingcountries <- missing_LIS[missing_LIS$n < 5,]$liscountrycode
    
    
    # Solve missing educats in marginals: 
    LIS_country_region_number <- unique(LIS_marginals[, c("liscountrycode", "region_number")])
    educats <- unique(LIS_marginals$EDUCAT)
    
    # Step 2: Create a template dataframe for each unique combination of "liscountrycode" and "region_number"
    LIS_marginals_looplist <- lapply(1:nrow(LIS_country_region_number), function(i) {
      liscountrycode <- LIS_country_region_number[i, "liscountrycode"]
      region_number <- LIS_country_region_number[i, "region_number"]
      EDUCAT <- educats
      
      # Create a data frame with all categories and set Frequency to 0
      LIS_marginals_full <- data.frame(
        liscountrycode = rep(liscountrycode, length(educats)),
        region_number = rep(region_number, length(educats)),
        EDUCAT = educats,
        Freqzero = 0
      )
      
      return(LIS_marginals_full)
    })
    
    LIS_marginals_full <- do.call(rbind, LIS_marginals_looplist)
    
    LIS_marginals_full <- left_join(LIS_marginals_full, LIS_marginals, by = c('liscountrycode', 'region_number', 'EDUCAT'))
    LIS_marginals_full$Frequency[is.na(LIS_marginals_full$Frequency)] <- 0
    LIS_marginals_full <- LIS_marginals_full %>% select(-Freqzero)
    
    LIS_missingcountries <- unique(LIS_marginals_full[LIS_marginals_full$Frequency == 0,]$liscountrycode)
    
    LIS_marginals_full2 <- LIS_marginals_full[LIS_marginals_full$liscountrycode %ni% LIS_missingcountries,]
    
    sum_freq_ZONE <- LIS_marginals_full %>% group_by(liscountrycode, region_number) %>% summarise(Totalfreq = sum(Frequency))
    
    LIS_marginals_full2 <- left_join(LIS_marginals_full2, sum_freq_ZONE, by = c('liscountrycode', 'region_number'))
    LIS_marginals_full2['Share_freq'] <- LIS_marginals_full2['Frequency']/LIS_marginals_full2['Totalfreq']
    
    average_educats <- LIS_marginals_full2 %>% group_by(EDUCAT) %>% summarise(mean_educats = mean(Share_freq))
    
    LIS_marginals_full <- left_join(LIS_marginals_full, average_educats, by = 'EDUCAT')
    LIS_marginals_full <- left_join(LIS_marginals_full, sum_freq_ZONE, by = c('liscountrycode', 'region_number'))
    
    # fill in average for 0's in Freq
    LIS_marginals_full$Frequency[LIS_marginals_full$Frequency == 0] <- LIS_marginals_full[LIS_marginals_full$Frequency == 0,]$mean_educats * LIS_marginals_full[LIS_marginals_full$Frequency == 0,]$Totalfreq
    
    # calculate sum freq and correct freqs: 
    sum_freq_ZONE <- LIS_marginals_full %>% group_by(liscountrycode, region_number) %>% summarise(Totalfreq_alledu = sum(Frequency))
    LIS_marginals_full <- left_join(LIS_marginals_full, sum_freq_ZONE, by = c('liscountrycode', 'region_number'))
    LIS_marginals_full$correct <- LIS_marginals_full$Totalfreq / LIS_marginals_full$Totalfreq_alledu
    LIS_marginals_full$Frequency <- LIS_marginals_full$Frequency * LIS_marginals_full$correct
    
    LIS_marginals_full <- LIS_marginals_full %>% select(region_number, liscountrycode, EDUCAT, Frequency)
    LIS_marginals_full <- LIS_marginals_full[, c(names(LIS_marginals))] #change order column names
    
    LIS_marginals <- copy(LIS_marginals_full)
    
    
  } else if (attribute == 'HHSIZECAT'){
    #hhsizecat 0 invullen. 
    
    columnnames <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6')
    IPUMS_marginals <- read.csv("IPUMSmarg_hhsize_sept23.csv", sep = ',')
    
    # Solve missing hhsizecats in marginals: 
    IPUMS_country_ZONE <- unique(IPUMS_marginals[, c("country", "ZONE")])
    hhsizecatcats <- unique(IPUMS_marginals$HHSIZECAT)
    
    # Step 2: Create a template dataframe for each unique combination of "country" and "ZONE"
    IPUMS_marginals_looplist <- lapply(1:nrow(IPUMS_country_ZONE), function(i) {
      country <- IPUMS_country_ZONE[i, "country"]
      ZONE <- IPUMS_country_ZONE[i, "ZONE"]
      HHSIZECAT <- hhsizecatcats
      
      # Create a data frame with all categories and set Frequency to 0
      IPUMS_marginals_full <- data.frame(
        country = rep(country, length(hhsizecatcats)),
        ZONE = rep(ZONE, length(hhsizecatcats)),
        HHSIZECAT = hhsizecatcats,
        Freqzero = 0
      )
      
      return(IPUMS_marginals_full)
    })
    
    IPUMS_marginals_full <- do.call(rbind, IPUMS_marginals_looplist)
    
    IPUMS_marginals_full <- left_join(IPUMS_marginals_full, IPUMS_marginals, by = c('country', 'ZONE', 'HHSIZECAT'))
    IPUMS_marginals_full$Freq[is.na(IPUMS_marginals_full$Freq)] <- 0
    IPUMS_marginals_full <- IPUMS_marginals_full %>% select(-Freqzero)
    
    IPUMS_marginals_full <- IPUMS_marginals_full[, c(names(IPUMS_marginals))] #change order column names
    
    IPUMS_marginals <- copy(IPUMS_marginals_full)
    
    # LIS
    LIS_marginals <- read.csv("LISmarg_hhsize_oct23.csv") 
    
    LIS_country_region_number <- unique(LIS_marginals[, c("liscountrycode", "region_number")])
    hhsizecatcats <- unique(LIS_marginals$HHSIZECAT)
    
    # Step 2: Create a template dataframe for each unique combination of "liscountrycode" and "region_number"
    LIS_marginals_looplist <- lapply(1:nrow(LIS_country_region_number), function(i) {
      liscountrycode <- LIS_country_region_number[i, "liscountrycode"]
      region_number <- LIS_country_region_number[i, "region_number"]
      HHSIZECAT <- hhsizecatcats
      
      # Create a data frame with all categories and set Frequency to 0
      LIS_marginals_full <- data.frame(
        liscountrycode = rep(liscountrycode, length(hhsizecatcats)),
        region_number = rep(region_number, length(hhsizecatcats)),
        HHSIZECAT = hhsizecatcats,
        Freqzero = 0
      )
      
      return(LIS_marginals_full)
    })
    
    LIS_marginals_full <- do.call(rbind, LIS_marginals_looplist)
    
    LIS_marginals_full <- left_join(LIS_marginals_full, LIS_marginals, by = c('liscountrycode', 'region_number', 'HHSIZECAT'))
    LIS_marginals_full$Frequency[is.na(LIS_marginals_full$Frequency)] <- 0
    LIS_marginals_full <- LIS_marginals_full %>% select(-Freqzero)
    
    LIS_marginals_full <- LIS_marginals_full[, c(names(LIS_marginals))] #change order column names
    
    LIS_marginals <- copy(LIS_marginals_full)
    
    
    
  } else if (attribute == 'GENDER'){
    columnnames <- c('X1', 'X2')
    IPUMS_marginals <- read.csv("IPUMSmarg_gender_sept23.csv", sep = ',')
    LIS_marginals <- read.csv("LISmarg_gender_oct23.csv") 
  }
    
  columnnames_est <- paste0(columnnames, 'est')
  
  
  
  
  
  HDI_data <- read.csv("SHDI-SGDI-Total 5.0.csv")
  
  nopop_HDI <- HDI_data[is.na(HDI_data$pop),] #population zit erin, behalve Canada and Tonga. 
  
  # Canada and Tonga have no population under GDL5.0, therefore, these population numbers are taken from GDL4.0
  GDL4.0 <- read.csv("SHDI Complete 4.0 (1).csv") %>% filter(year == 2015) %>% filter(country %in% c('Canada','Tonga')) %>% select(GDLCODE, pop) %>% mutate(pop = pop * 1000)
  GDL4.0 <- GDL4.0[GDL4.0$GDLCODE != 'CANt',]
  
  # popdata uit ander bestand ingevuld. 
  HDI_data15 <- HDI_data[HDI_data$year == 2015,]
  
  HDI_data15[HDI_data15$GDLCODE %in% GDL4.0$GDLCODE ,]$pop <- GDL4.0$pop
  
  # Population Tonga only available on national level, so we drop regions Tonga.  
  
  HDI_data15 <- HDI_data15[HDI_data15$GDLCODE %ni% c(HDI_data15[HDI_data15$country == 'Tonga' & HDI_data15$level == 'Subnat',]$GDLCODE),]
  
  
  
  # correct hdi index 6 october so that it matches the year the marginals are from: 
  
  
  if (correct_marginals_hdiyear == 'yes'){
    HDI_data15 <- correct_hdiyear(HDI_data, HDI_data15)
  }

  
  
  # GDLregio's aan vast maken: 
  
  countrieslist <- unique(IPUMS_marginals$country)
  
  GDL_IPUMS_match <- read.csv("GDL_IPUMS_match.csv", sep = ';')
  GDL_IPUMS_match <- GDL_IPUMS_match %>% rename('ZONE' = 'GEOLEV1_IPUMS', 'country' = 'Country')
  IPUMS_marginals <- left_join(IPUMS_marginals, GDL_IPUMS_match %>% select(ZONE, country, GDLCODE), by = c('ZONE', 'country'), multiple = 'all')
  
  
  IPUMS_marginals <- IPUMS_marginals %>% pivot_wider(names_from = `attribute`, values_from = Freq)
  colnames(IPUMS_marginals) <- c('ZONE', 'country', 'GDLCODE', columnnames)
  
  IPUMS_marginals <- IPUMS_marginals %>% select(-ZONE)
  
  
  #write.csv(IPUMS_marginals, 'IPUMS_age_knownmarginals_sept23.csv', row.names = FALSE)
  
  
  # GDL aan LIS maken
  liscodecountries <- read.csv('LIS_codes_countries2.csv', sep = ';') #dit bestand is zonder spaties.
  liscodecountries <- liscodecountries %>% select(-iso_code, -year)
  liscodecountries <- liscodecountries %>% rename('liscountrycode' = 'LISCODE', 'country' = 'Country')
  
  LIS_marginals <- left_join(LIS_marginals, liscodecountries, by = 'liscountrycode')
  
  # merge with GDLCODE file
  GDL_LIS_match <- read.csv("GDL_match_population_all_LIS.csv", sep = ';') 
  GDL_LIS_match <- GDL_LIS_match %>% rename('country' = 'Country', 'GDLCODE' = 'GDLcode')
  
  LIS_marginals <- left_join(LIS_marginals, GDL_LIS_match %>% select(region_number, country, GDLCODE), by = c('region_number', 'country'), multiple = 'all')
  
  LIS_marginals <- LIS_marginals %>% pivot_wider(names_from = `attribute`, values_from = Frequency)
  colnames(LIS_marginals) <- c('region_number', 'liscountrycode', 'country', 'GDLCODE', columnnames)
  
  #LIS_marginals <- LIS_marginals[LIS_marginals$country %ni% c('Belgium', 'Greece', 'Ireland', 'China'),]
  
  LIS_marginals <- LIS_marginals %>% select(-region_number, -liscountrycode)
  
  #write.csv(LIS_marginals, 'LIS_income_knownmarginals_sept23.csv', row.names = FALSE)
  
  
  
  
  known_marg <- rbind.data.frame(LIS_marginals, IPUMS_marginals)
  known_marg['sumpop'] <- as.numeric(apply(known_marg[,3:(length(columnnames)+2)], 1, sum))
  
  known_marg <- left_join(known_marg, HDI_data15 %>% select(GDLCODE, shdi, pop), by = 'GDLCODE')
  known_marg$pop <- known_marg$pop * 1000
  
  known_marg <- known_marg %>% rename('gdlpop' = 'pop')
  
  
  known_marg$pop_ratio <- known_marg$gdlpop / known_marg$sumpop
  
  known_marg <- known_marg[!is.na(known_marg$GDLCODE),]
  
  # eruit: filter(popratio > 0.6 & popratio < 1.4)
  
  # take the shares (percentages)
  for (n in columnnames){
    known_marg[`n`] <- known_marg[`n`]/known_marg['sumpop']
  }
  
  
  
  unknown_marg <- HDI_data15 %>% select(GDLCODE, country, shdi, level)
  unknown_marg['count1'] <- 1
  
  noregions <- unknown_marg %>% group_by(country) %>% summarise(count1region = sum(count1))
  
  unknown_marg <- left_join(unknown_marg, noregions, by = 'country')
  
  unknown_marg <- unknown_marg[(unknown_marg$level == 'Subnat' | unknown_marg$count1region == 1),]
  
  unknown_marg <- unknown_marg %>% select(GDLCODE, shdi)
  
  
  for (n in columnnames_est){
    unknown_marg[`n`] <- -99
  }
  
  
  cor_matrix_subset <- cor(known_marg[c("shdi", columnnames)])
  
  
  

  
  # to calculate error
  unknown_marg2 <- unknown_marg[unknown_marg$GDLCODE %in% known_marg$GDLCODE ,]
  
  
  error_DF_k <- data.frame(k = startk:endk)
  error_DF_k['sum_abs_error'] <- -99
  
  
  
  errornames <- c()
  for (n in 1:length(columnnames)){
    errornames <- append(errornames, paste0('absE', as.character(n)))
  }
  
  for (k in error_DF_k$k){
  
    print(k)  
    for (gdlcode in unknown_marg2$GDLCODE){
      mean_region <- similar_k_regions(gdlcode, known_marg, unknown_marg2, k, columnnames)
      unknown_marg2[unknown_marg2$GDLCODE == gdlcode, c(columnnames_est)] <- mean_region
    }
    
    error_calc_df <- left_join(known_marg, unknown_marg2 %>% select(-shdi), by = 'GDLCODE')
    
    for (n in 1:length(columnnames)){
      error_calc_df[paste0('absE', as.character(n))] <- abs(error_calc_df[paste0('X', as.character(n))] - error_calc_df[paste0('X', as.character(n), 'est')])
    }
    
    
    error_calc_df['absEsum'] <- rowSums(error_calc_df %>% select(all_of(errornames)))
    
    error_DF_k[k-(startk+1),'sum_abs_error'] <- sum(error_calc_df['absEsum'])
    
    
  }
  
  error_DF_k <- error_DF_k[order(error_DF_k$sum_abs_error, decreasing = FALSE), ]
  
  #filenameerror <- paste0('Error_perk_marginals_', attribute, '_oct23.csv')
  #write.csv(error_DF_k, filenameerror, row.names = FALSE)
  
  
  bestk <- error_DF_k[1,'k']
  print('bestk') 
  print(bestk)
  
  bestkarray <- append(bestkarray, bestk)
  
  for (gdlcode in unknown_marg$GDLCODE){
    mean_region <- similar_k_regions(gdlcode, known_marg, unknown_marg, bestk, columnnames)
    unknown_marg[unknown_marg$GDLCODE == gdlcode, c(columnnames_est)] <- mean_region
  }
  
  
  unknown_marg <- left_join(unknown_marg, HDI_data15 %>% select(GDLCODE, pop), by = 'GDLCODE')
  
  for (n in columnnames_est){
    unknown_marg[`n`] <- unknown_marg[`n`] * unknown_marg$pop * 1000
  }
  
  unknown_marg <- unknown_marg %>% select(-shdi, -pop)
  
  filename <- paste0('Est_marginals_', attribute, '_oct23.csv') #Tonga
  write.csv(unknown_marg, filename, row.names = FALSE)

}









