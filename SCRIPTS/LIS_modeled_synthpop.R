#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)


isocode = as.character(args[1])

uselisdata = as.character(args[2])

uselismarginals = as.character(args[3])


if (uselisdata != 'yes' || uselisdata != 'no'){
  
  print(uselisdata)
}



library(dplyr)
#library(readtext)
library(stringr)
library(tidyr)
library(zoo)
library(reshape2)
library(tibble)
library(wrswoR)
library(data.table)
`%ni%` <- Negate(`%in%`)
library(mlfit)


# No error calculation



if (uselisdata == 'yes'){
  
  filename_length <- paste0('length_LIS_survey_', isocode, '.csv')
  rowlength_LIS <- read.csv(filename_length, sep = ',', header = TRUE)
  
  l = rowlength_LIS$x
  w = 11
  
  filenamelisdata <- paste0(isocode, "_LISdata_may23.dat")
  
  con = file(filenamelisdata, "rb")
  
  jointhead100small = readBin(con, integer(), n = l*w)
  
  # reshape the bindata
  jointhead100small <- array(jointhead100small, dim=c(l, w))
  jointhead100small <-  as.data.frame(jointhead100small) #was frame
  
  colnames(jointhead100small) <- c('INCOME', 'RURAL', 'FARMING', 'AGECAT', 'GENDER', 
                                   'EDUCAT', 'HHTYPE', 'HID', 'RELATE', 'HHSIZECAT', 'WEIGHTS')
  
  close(con)
  
  print('imported LIS data')
  
  
  jointhead100small <- as.data.table(jointhead100small)
  
  print(nrow(jointhead100small))
  
  
  
  
} else if (uselisdata == 'replace'){
  
  #setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2')
  similarcountries <- read.csv('similar_countries_5august24.csv')
  
  similarcountries_row <- similarcountries[similarcountries$ISOcode == isocode,]
  
  isocode1 <- similarcountries_row$Replace1
  isocode2 <- similarcountries_row$Replace2
  isocode3 <- similarcountries_row$Replace3
  
  # replace 1
  filename_length <- paste0('length_LIS_survey_', isocode1, '.csv')
  rowlength_LIS <- read.csv(filename_length, sep = ',', header = TRUE)
  
  l = rowlength_LIS$x
  w = 11
  
  filenamelisdata <- paste0(isocode1, "_LISdata_may23.dat")
  
  con = file(filenamelisdata, "rb")
  
  jointhead100small = readBin(con, integer(), n = l*w)
  
  # reshape the bindata
  jointhead100small <- array(jointhead100small, dim=c(l, w))
  jointhead100small <-  as.data.frame(jointhead100small) #was frame
  
  colnames(jointhead100small) <- c('INCOME', 'RURAL', 'FARMING', 'AGECAT', 'GENDER', 
                                   'EDUCAT', 'HHTYPE', 'HID', 'RELATE', 'HHSIZECAT', 'WEIGHTS')
  
  close(con)
  
  print('imported LIS data 1')
  
  if (isocode1 == 'CHN'){
  
    jointhead100smallhhsize5 <- jointhead100small[jointhead100small$HHSIZECAT < 6,]
    
    jointhead100smallhhsize6 <- jointhead100small[jointhead100small$HHSIZECAT == 6,]
    jointhead100smallhhsize6$WEIGHTS <- jointhead100smallhhsize6$WEIGHTS * 10
    
    jointhead100small <- rbind.data.frame(jointhead100smallhhsize5, jointhead100smallhhsize6)
    
    jointhead100small <- jointhead100small[jointhead100small$WEIGHTS > 2,]
    
  }
  
  jointhead100small_1 <- as.data.table(jointhead100small)
  
  
  jointhead100small_1[, Replacecountry := 1]
  
  # replace 2
  filename_length <- paste0('length_LIS_survey_', isocode2, '.csv')
  rowlength_LIS <- read.csv(filename_length, sep = ',', header = TRUE)
  
  l = rowlength_LIS$x
  w = 11
  
  filenamelisdata <- paste0(isocode2, "_LISdata_may23.dat")
  
  con = file(filenamelisdata, "rb")
  
  jointhead100small = readBin(con, integer(), n = l*w)
  
  # reshape the bindata
  jointhead100small <- array(jointhead100small, dim=c(l, w))
  jointhead100small <-  as.data.frame(jointhead100small) #was frame
  
  colnames(jointhead100small) <- c('INCOME', 'RURAL', 'FARMING', 'AGECAT', 'GENDER', 
                                   'EDUCAT', 'HHTYPE', 'HID', 'RELATE', 'HHSIZECAT', 'WEIGHTS')
  
  close(con)
  
  print('imported LIS data 2')
  
  if (isocode2 == 'CHN'){
    
    jointhead100smallhhsize5 <- jointhead100small[jointhead100small$HHSIZECAT < 6,]
    
    jointhead100smallhhsize6 <- jointhead100small[jointhead100small$HHSIZECAT == 6,]
    jointhead100smallhhsize6$WEIGHTS <- jointhead100smallhhsize6$WEIGHTS * 10
    
    jointhead100small <- rbind.data.frame(jointhead100smallhhsize5, jointhead100smallhhsize6)
    
    jointhead100small <- jointhead100small[jointhead100small$WEIGHTS > 2,]
    
  }
  
  jointhead100small_2 <- as.data.table(jointhead100small)
  jointhead100small_2[, HID := HID+max(jointhead100small_1$HID)]
  print(colnames(jointhead100small_2))
  jointhead100small_2[, Replacecountry := 2]
  
  # replace 3
  filename_length <- paste0('length_LIS_survey_', isocode3, '.csv')
  rowlength_LIS <- read.csv(filename_length, sep = ',', header = TRUE)
  
  l = rowlength_LIS$x
  w = 11
  
  filenamelisdata <- paste0(isocode3, "_LISdata_may23.dat")
  
  con = file(filenamelisdata, "rb")
  
  jointhead100small = readBin(con, integer(), n = l*w)
  
  # reshape the bindata
  jointhead100small <- array(jointhead100small, dim=c(l, w))
  jointhead100small <-  as.data.frame(jointhead100small) #was frame
  
  colnames(jointhead100small) <- c('INCOME', 'RURAL', 'FARMING', 'AGECAT', 'GENDER', 
                                   'EDUCAT', 'HHTYPE', 'HID', 'RELATE', 'HHSIZECAT', 'WEIGHTS')
  
  close(con)
  
  print('imported LIS data 3')
  
  if (isocode3 == 'CHN'){
    
    jointhead100smallhhsize5 <- jointhead100small[jointhead100small$HHSIZECAT < 6,]
    
    jointhead100smallhhsize6 <- jointhead100small[jointhead100small$HHSIZECAT == 6,]
    jointhead100smallhhsize6$WEIGHTS <- jointhead100smallhhsize6$WEIGHTS * 10
    
    jointhead100small <- rbind.data.frame(jointhead100smallhhsize5, jointhead100smallhhsize6)
    
    jointhead100small <- jointhead100small[jointhead100small$WEIGHTS > 2,]
    
  }
  
  jointhead100small_3 <- as.data.table(jointhead100small)
  jointhead100small_3[, HID := HID+max(jointhead100small_2$HID)]
  print(colnames(jointhead100small_3))
  jointhead100small_3[, Replacecountry := 3]
  
  #jointhead100small <- rbind(jointhead100small_1, jointhead100small_2)
  #jointhead100small <- rbind(jointhead100small, jointhead100small_3)
  
  jointhead100small <- rbindlist(list(jointhead100small_1, jointhead100small_2), use.names=TRUE)
  jointhead100small <- rbindlist(list(jointhead100small, jointhead100small_3), use.names=TRUE)
  
  rm(jointhead100small_1)
  rm(jointhead100small_2)
  rm(jointhead100small_3)
  gc()
  
  #correct weights
  
  weights1 <- sum(jointhead100small[jointhead100small$Replacecountry == 1,]$WEIGHTS)
  weights2 <- sum(jointhead100small[jointhead100small$Replacecountry == 2,]$WEIGHTS)
  weights3 <- sum(jointhead100small[jointhead100small$Replacecountry == 3,]$WEIGHTS)
  
  maxweight <- max(weights1, weights2, weights3)
  
  
  jointhead100small$Replacecountry[jointhead100small$Replacecountry == 1] <- weights1
  jointhead100small$Replacecountry[jointhead100small$Replacecountry == 2] <- weights2
  jointhead100small$Replacecountry[jointhead100small$Replacecountry == 3] <- weights3
  
  jointhead100small$Maxweight <- maxweight
  
  jointhead100small$Weightcorrection <- jointhead100small$Maxweight / jointhead100small$Replacecountry
  
  jointhead100small$WEIGHTS <- jointhead100small$Weightcorrection * jointhead100small$WEIGHTS
  
  jointhead100small$WEIGHTS <- round(jointhead100small$WEIGHTS)
  
  
  jointhead100small <- as.data.table(jointhead100small)
  
  
  print('income ref')
  print(table(jointhead100small$INCOME))
  
  print('length jointhead100small 1')
  print(nrow(jointhead100small))
  
}




########################################################################################################
################ SYNTHETIC POPULATION #############################

# estimated marginals: 

# when there is a 0 in the education category, then we need to alter the estimated marginal accordingly. 

#setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2')

population <- read.csv('GHSpopulationGDLregions.csv')
population['Urban_percent'] <- population$Urbanpop/population$Population
population['Rural_percent'] <- population$Ruralpop/population$Population

if (uselismarginals == 'no'){

  # income
  est_marg_income <- read.csv('Est_marginals_INCOME_june24.csv')
  est_marg_income <- est_marg_income[est_marg_income$ISOcode == isocode,]
                                                                        
  
  est_marg_income_gr <- est_marg_income %>% 
    pivot_longer(
      cols = X1est:X5est,   
      names_to = "INCOME",    
      values_to = "Frequency"       
    )
  
  est_marg_income_gr['INCOME'] <- as.integer(as.factor(est_marg_income_gr$INCOME))
  
  est_marg_income_gr <- est_marg_income_gr %>% select(GDLcode, INCOME, Frequency)
  
  print('est_marg_income_gr')
  print(est_marg_income_gr)
  
  # hhtype
  est_marg_hhtype <- read.csv('Est_marginals_HHTYPE_june24.csv')
  est_marg_hhtype <- est_marg_hhtype[est_marg_hhtype$ISOcode == isocode,]
  
  
  est_marg_hhtype_gr <- est_marg_hhtype %>% 
    pivot_longer(
      cols = X1est:X8est,   
      names_to = "HHTYPE",    
      values_to = "Frequency"       
    )
  
  est_marg_hhtype_gr['HHTYPE'] <- as.integer(as.factor(est_marg_hhtype_gr$HHTYPE))
  
  est_marg_hhtype_gr <- est_marg_hhtype_gr %>% select(GDLcode, HHTYPE, Frequency)
  
  # hhsize
  est_marg_hhsize <- read.csv('Est_marginals_HHSIZECAT_june24.csv')
  est_marg_hhsize <- est_marg_hhsize[est_marg_hhsize$ISOcode == isocode,]
  
  
  est_marg_hhsize_gr <- est_marg_hhsize %>% 
    pivot_longer(
      cols = X1est:X6est,   
      names_to = "HHSIZECAT",    
      values_to = "Frequency"       
    )
  
  est_marg_hhsize_gr['HHSIZECAT'] <- as.integer(as.factor(est_marg_hhsize_gr$HHSIZECAT))
  
  est_marg_hhsize_gr <- est_marg_hhsize_gr %>% select(GDLcode, HHSIZECAT, Frequency)
  
  
  
  # agecat
  est_marg_age <- read.csv('Est_marginals_AGECAT_june24.csv')
  est_marg_age <- est_marg_age[est_marg_age$ISOcode == isocode,]
  
  
  
  est_marg_age_gr <- est_marg_age %>% 
    pivot_longer(
      cols = X1est:X8est,   
      names_to = "AGECAT",    
      values_to = "Frequency"       
    )
  
  est_marg_age_gr['AGECAT'] <- as.integer(as.factor(est_marg_age_gr$AGECAT))
  
  est_marg_age_gr <- est_marg_age_gr %>% select(GDLcode, AGECAT, Frequency)
  
  # educat
  est_marg_edu <- read.csv('Est_marginals_EDUCAT_june24.csv')
  est_marg_edu <- est_marg_edu[est_marg_edu$ISOcode == isocode,]
  
  
  est_marg_edu_gr <- est_marg_edu %>% 
    pivot_longer(
      cols = X1est:X5est,   
      names_to = "EDUCAT",    
      values_to = "Frequency"       
    )
  
  est_marg_edu_gr['EDUCAT'] <- as.integer(as.factor(est_marg_edu_gr$EDUCAT))
  
  est_marg_edu_gr <- est_marg_edu_gr %>% select(GDLcode, EDUCAT, Frequency)
  
  
  
  # gender
  est_marg_gender <- read.csv('Est_marginals_GENDER_june24.csv')
  est_marg_gender <- est_marg_gender[est_marg_gender$ISOcode == isocode,]
  
  
  est_marg_gender_gr <- est_marg_gender %>% 
    pivot_longer(
      cols = X1est:X2est,   
      names_to = "GENDER",    
      values_to = "Frequency"       
    )
  
  est_marg_gender_gr['GENDER'] <- as.integer(as.factor(est_marg_gender_gr$GENDER))-1
  
  est_marg_gender_gr <- est_marg_gender_gr %>% select(GDLcode, GENDER, Frequency)

} else if (uselismarginals == 'yes'){
  
  # income
  est_marg_income <- read.csv('LISmarg_income_may24.csv')
  est_marg_income['ISOcode'] <- substr(est_marg_income$GDLcode, 1, 3)
  est_marg_income <- est_marg_income[est_marg_income$ISOcode == isocode,]
  
  
  est_marg_income_gr <- est_marg_income %>% 
    pivot_longer(
      cols = X1:X5,   
      names_to = "INCOME",    
      values_to = "Frequency"       
    )
  
  est_marg_income_gr['INCOME'] <- as.integer(as.factor(est_marg_income_gr$INCOME))
  
  est_marg_income_gr <- est_marg_income_gr %>% select(GDLcode, INCOME, Frequency)
  
  print('est_marg_income_gr')
  print(est_marg_income_gr)
  
  # hhtype
  est_marg_hhtype <- read.csv('LISmarg_hhtype_may24.csv')
  est_marg_hhtype['ISOcode'] <- substr(est_marg_hhtype$GDLcode, 1, 3)
  est_marg_hhtype <- est_marg_hhtype[est_marg_hhtype$ISOcode == isocode,]
  
  
  est_marg_hhtype_gr <- est_marg_hhtype %>% 
    pivot_longer(
      cols = X1:X8,   
      names_to = "HHTYPE",    
      values_to = "Frequency"       
    )
  
  est_marg_hhtype_gr['HHTYPE'] <- as.integer(as.factor(est_marg_hhtype_gr$HHTYPE))
  
  est_marg_hhtype_gr <- est_marg_hhtype_gr %>% select(GDLcode, HHTYPE, Frequency)
  
  # hhsize
  est_marg_hhsize <- read.csv('LISmarg_hhsize_may24.csv')
  est_marg_hhsize['ISOcode'] <- substr(est_marg_hhsize$GDLcode, 1, 3)
  est_marg_hhsize <- est_marg_hhsize[est_marg_hhsize$ISOcode == isocode,]
  
  
  est_marg_hhsize_gr <- est_marg_hhsize %>% 
    pivot_longer(
      cols = X1:X6,   
      names_to = "HHSIZECAT",    
      values_to = "Frequency"       
    )
  
  est_marg_hhsize_gr['HHSIZECAT'] <- as.integer(as.factor(est_marg_hhsize_gr$HHSIZECAT))
  
  est_marg_hhsize_gr <- est_marg_hhsize_gr %>% select(GDLcode, HHSIZECAT, Frequency)

  
  # agecat
  est_marg_age <- read.csv('LISmarg_age_may24.csv')
  est_marg_age['ISOcode'] <- substr(est_marg_age$GDLcode, 1, 3)
  est_marg_age <- est_marg_age[est_marg_age$ISOcode == isocode,]
  
  
  
  est_marg_age_gr <- est_marg_age %>% 
    pivot_longer(
      cols = X1:X8,   
      names_to = "AGECAT",    
      values_to = "Frequency"       
    )
  
  est_marg_age_gr['AGECAT'] <- as.integer(as.factor(est_marg_age_gr$AGECAT))
  
  est_marg_age_gr <- est_marg_age_gr %>% select(GDLcode, AGECAT, Frequency)
  
  # educat
  est_marg_edu <- read.csv('LISmarg_edu_may24.csv')
  est_marg_edu['ISOcode'] <- substr(est_marg_edu$GDLcode, 1, 3)
  est_marg_edu <- est_marg_edu[est_marg_edu$ISOcode == isocode,]
  
  
  est_marg_edu_gr <- est_marg_edu %>% 
    pivot_longer(
      cols = X1:X5,   
      names_to = "EDUCAT",    
      values_to = "Frequency"       
    )
  
  est_marg_edu_gr['EDUCAT'] <- as.integer(as.factor(est_marg_edu_gr$EDUCAT))
  
  est_marg_edu_gr <- est_marg_edu_gr %>% select(GDLcode, EDUCAT, Frequency)
  
  
  
  # gender
  est_marg_gender <- read.csv('LISmarg_gender_may24.csv')
  est_marg_gender['ISOcode'] <- substr(est_marg_gender$GDLcode, 1, 3)
  est_marg_gender <- est_marg_gender[est_marg_gender$ISOcode == isocode,]
  
  
  est_marg_gender_gr <- est_marg_gender %>% 
    pivot_longer(
      cols = X1:X2,   
      names_to = "GENDER",    
      values_to = "Frequency"       
    )
  
  est_marg_gender_gr['GENDER'] <- as.integer(as.factor(est_marg_gender_gr$GENDER))-1
  
  est_marg_gender_gr <- est_marg_gender_gr %>% select(GDLcode, GENDER, Frequency)
  
}

# NEW RURAL MARGINAL
#SMODmarg <- read.csv('individual_marginals_UrbanRural.csv', sep = ';', header = TRUE)
SMODmarg <- read.csv('rural_urban_marginals_march24.csv', sep = ',', header = TRUE)
GDL_pop <- read.csv('GDL_match_population_all_LIS.csv', sep = ';', header = TRUE)
population <- read.csv('GHSpopulationGDLregions.csv', sep = ',', header = TRUE)

SMODmarg <- read.csv('GHSpopulationGDLregions.csv', sep = ',', header = TRUE)

GDL_pop <- left_join(GDL_pop, population %>% select(GDLcode, Population), by = 'GDLcode')




# rural
rural_marg <- population[population$iso_code == isocode,]
rural_marg <- rural_marg %>% dplyr::select(GDLcode, Ruralpop, Urbanpop)
rural_marg <- pivot_longer(rural_marg, cols = c(Ruralpop, Urbanpop), names_to = "RURAL", values_to = "Frequency")
rural_marg$RURAL <- abs(as.integer(as.factor(rural_marg$RURAL))-2) #1 is rural and 0 is urban

rural_tib <- as_tibble(rural_marg)


# remove mismatch marginals (control) and survey data (reference)

survey_hhsize_cats <- unique(jointhead100small$HHSIZECAT)
marg_hhsize_cats <- unique(est_marg_hhsize_gr$HHSIZECAT)

print

if (length(marg_hhsize_cats) < length(survey_hhsize_cats)){
  print('hhsizecat survey removed')
  missingcat <- setdiff(survey_hhsize_cats, marg_hhsize_cats)
  missingcat <- c(missingcat)
  jointhead100small_missingcat <- jointhead100small[jointhead100small$HHSIZECAT == missingcat ,]
  missingcat_hid <- unique(jointhead100small_missingcat$HID)
  
  jointhead100small <- jointhead100small[jointhead100small$HID %ni% missingcat_hid]
}


if (length(marg_hhsize_cats) > length(survey_hhsize_cats)){
  print('hhsizecat marginal removed')
  missingcat <- setdiff(marg_hhsize_cats, survey_hhsize_cats)
  missingcat <- c(missingcat)
  OG_weights <- est_marg_hhsize_gr %>% group_by(GEOLEV1) %>% summarise(OG_Frequency = sum(Frequency))
  est_marg_hhsize_gr <- est_marg_hhsize_gr[est_marg_hhsize_gr$HHSIZECAT %ni% missingcat,]
  NEW_weights <- est_marg_hhsize_gr %>% group_by(GEOLEV1) %>% summarise(NEW_Frequency = sum(Frequency))
  est_marg_hhsize_gr <- left_join(est_marg_hhtype_gr, OG_weights, by = 'GEOLEV1')
  est_marg_hhsize_gr <- left_join(est_marg_hhtype_gr, NEW_weights, by = 'GEOLEV1')
  est_marg_hhsize_gr$correction_factor <- est_marg_hhtype_gr$OG_Frequency / est_marg_hhtype_gr$NEW_Frequency
  est_marg_hhsize_gr$Frequency <- est_marg_hhtype_gr$Frequency * est_marg_hhtype_gr$correction_factor
  
  est_marg_hhsize_gr <- est_marg_hhsize_gr %>% select(-OG_Frequency, -NEW_Frequency, -correction_factor)
}



survey_edu_cats <- unique(jointhead100small$EDUCAT)
marg_edu_cats <- unique(est_marg_edu_gr$EDUCAT)

print('survey_edu_cats')
print(survey_edu_cats)
print('marg_edu_cats')
print(marg_edu_cats)

if (length(marg_edu_cats) < length(survey_edu_cats)){
  print('educat survey removed')

  missingcat <- setdiff(survey_edu_cats, marg_edu_cats)
  missingcat <- c(missingcat)
  jointhead100small_missingcat <- jointhead100small[jointhead100small$EDUCAT %in% missingcat ,]
  missingcat_hid <- unique(jointhead100small_missingcat$HID)
  
  jointhead100small <- jointhead100small[!jointhead100small$HID %in% missingcat_hid ,]
}


if (length(marg_edu_cats) > length(survey_edu_cats)){
  print('educat marginal removed')
  missingcat <- setdiff(marg_edu_cats, survey_edu_cats)
  missingcat <- c(missingcat)
  OG_weights <- est_marg_edu_gr %>% group_by(GEOLEV1) %>% summarise(OG_Frequency = sum(Frequency))
  est_marg_edu_gr <- est_marg_edu_gr[est_marg_edu_gr$EDUCAT %ni% missingcat,]
  NEW_weights <- est_marg_edu_gr %>% group_by(GEOLEV1) %>% summarise(NEW_Frequency = sum(Frequency))
  est_marg_edu_gr <- left_join(est_marg_edu_gr, OG_weights, by = 'GEOLEV1')
  est_marg_edu_gr <- left_join(est_marg_edu_gr, NEW_weights, by = 'GEOLEV1')
  est_marg_edu_gr$correction_factor <- est_marg_edu_gr$OG_Frequency / est_marg_edu_gr$NEW_Frequency
  est_marg_edu_gr$Frequency <- est_marg_edu_gr$Frequency * est_marg_edu_gr$correction_factor
  
  est_marg_edu_gr <- est_marg_edu_gr %>% select(-OG_Frequency, -NEW_Frequency, -correction_factor)
}



survey_hhtype_cats <- unique(jointhead100small$HHTYPE)
marg_hhtype_cats <- unique(est_marg_hhtype_gr$HHTYPE)

if (length(marg_hhtype_cats) < length(survey_hhtype_cats)){
  print('hhtype survey removed')
  missingcat <- setdiff(survey_hhtype_cats, marg_hhtype_cats)
  missingcat <- c(missingcat)
  print('missing cat hhtype survey')
  print(missingcat)
  jointhead100small_missingcat <- jointhead100small[jointhead100small$HHTYPE %in% missingcat ,]
  missingcat_hid <- unique(jointhead100small_missingcat$HID)
  
  jointhead100small <- jointhead100small[!jointhead100small$HID %in% missingcat_hid ,]
}


if (length(marg_hhtype_cats) > length(survey_hhtype_cats)){
  print('hhtype marginal removed')
  missingcat <- setdiff(marg_hhtype_cats, survey_hhtype_cats)
  missingcat <- c(missingcat)
  OG_weights <- est_marg_hhtype_gr %>% group_by(GDLcode) %>% summarise(OG_Frequency = sum(Frequency))
  est_marg_hhtype_gr <- est_marg_hhtype_gr[est_marg_hhtype_gr$HHTYPE %ni% missingcat,]
  NEW_weights <- est_marg_hhtype_gr %>% group_by(GDLcode) %>% summarise(NEW_Frequency = sum(Frequency))
  est_marg_hhtype_gr <- left_join(est_marg_hhtype_gr, OG_weights, by = 'GDLcode')
  est_marg_hhtype_gr <- left_join(est_marg_hhtype_gr, NEW_weights, by = 'GDLcode')
  est_marg_hhtype_gr$correction_factor <- est_marg_hhtype_gr$OG_Frequency / est_marg_hhtype_gr$NEW_Frequency
  est_marg_hhtype_gr$Frequency <- est_marg_hhtype_gr$Frequency * est_marg_hhtype_gr$correction_factor
  
  est_marg_hhtype_gr <- est_marg_hhtype_gr %>% select(-OG_Frequency, -NEW_Frequency, -correction_factor)
}


regionnumbers <- unique(rural_tib$GDLcode) #not integers, otherwise we can't match them with GDL.  


jointhead100small[, PID := 1:.N] 

# print length 

print('length jointhead100small 2')
print(nrow(jointhead100small))


nr_individuals_per_regio <- c()
nr_individuals_per_regio_GDL <- c()

est_marg_income_gr <- na.omit(est_marg_income_gr)
est_marg_hhtype_gr <- na.omit(est_marg_hhtype_gr)
est_marg_hhsize_gr <- na.omit(est_marg_hhsize_gr)
est_marg_age_gr <- na.omit(est_marg_age_gr)
est_marg_edu_gr <- na.omit(est_marg_edu_gr)
est_marg_gender_gr <- na.omit(est_marg_gender_gr)


for (regnr in regionnumbers){
  
  print('regnr')
  print(regnr)
  
  income_tib1reg <- est_marg_income_gr[est_marg_income_gr$GDLcode == regnr,]
  income_tib1reg <- income_tib1reg %>% select(-GDLcode)
  
  print('income_tib1reg')
  print(income_tib1reg)
  
  rural_tib1reg <- rural_tib[rural_tib$GDLcode == regnr,]
  rural_tib1reg <- rural_tib1reg %>% select(-GDLcode)
  
  hhtype_tib1reg <- est_marg_hhtype_gr[est_marg_hhtype_gr$GDLcode == regnr,]
  hhtype_tib1reg <- hhtype_tib1reg %>% select(-GDLcode)
  
  print('hhtype_tib1reg')
  print(hhtype_tib1reg)
  
  hhsize_tib1reg <- est_marg_hhsize_gr[est_marg_hhsize_gr$GDLcode == regnr,]
  hhsize_tib1reg <- hhsize_tib1reg %>% select(-GDLcode)
  
  print('hhsize_tib1reg')
  print(hhsize_tib1reg)
  
  age_tib1reg <- est_marg_age_gr[est_marg_age_gr$GDLcode == regnr,]
  age_tib1reg <- age_tib1reg %>% select(-GDLcode)
  
  print('age_tib1reg')
  print(age_tib1reg)
  
  edu_tib1reg <- est_marg_edu_gr[est_marg_edu_gr$GDLcode == regnr,]
  edu_tib1reg <- edu_tib1reg %>% select(-GDLcode)
  
  print('edu_tib1reg')
  print(edu_tib1reg)
  
  sex_tib1reg <- est_marg_gender_gr[est_marg_gender_gr$GDLcode == regnr,]
  sex_tib1reg <- sex_tib1reg %>% select(-GDLcode)
  
  print('sex_tib1reg')
  print(sex_tib1reg)
  
  
  group_control <- list()
  individual_control <- list(income_tib1reg, hhtype_tib1reg, hhsize_tib1reg, rural_tib1reg, age_tib1reg, edu_tib1reg, sex_tib1reg)  
  
  names(group_control) <- c() 
  names(individual_control) <- c('INCOME', 'HHTYPE', 'HHSIZECAT', 'RURAL', 'AGECAT','EDUCAT', 'GENDER')
  
  ml_fitWEIGHTS <- jointhead100small$WEIGHTS
  
  fitting_problem <- ml_problem(
    ref_sample = jointhead100small, 
    controls = list(
      individual = individual_control,
      group = group_control), 
    prior_weights = ml_fitWEIGHTS,
    field_names = special_field_names(
      groupId = "HID", 
      individualId = "PID", 
      count = "Frequency"
    )
  )
  
  
  fit <- ml_fit(ml_problem = fitting_problem, algorithm = "ipu", maxiter = 150) #was 50
  
  
  syn_pop_reg <- ml_replicate(fit, algorithm = "trs")
  
  syn_pop_reg$HHSIZECAT <- as.integer(syn_pop_reg$HHSIZECAT)
  syn_pop_reg$RURAL <- as.integer(syn_pop_reg$RURAL)
  syn_pop_reg$GENDER <- as.integer(syn_pop_reg$GENDER)
  syn_pop_reg$AGECAT <- as.integer(syn_pop_reg$AGECAT)
  syn_pop_reg$EDUCAT <- as.integer(syn_pop_reg$EDUCAT)
  syn_pop_reg$FARMING <- -1
  syn_pop_reg$WEALTH <- -1
  syn_pop_reg$FLOORCAT <- -1
  syn_pop_reg$WALLCAT <- -1
  syn_pop_reg$ROOFCAT <- -1
  syn_pop_reg$SOURCE <- 4
  syn_pop_reg$INCOME <- as.integer(syn_pop_reg$INCOME)
  syn_pop_reg$HID <- as.integer(syn_pop_reg$HID)
  syn_pop_reg$RELATE <- as.integer(syn_pop_reg$RELATE)
  syn_pop_reg$HHTYPE <- as.integer(syn_pop_reg$HHTYPE)
  
  if (uselisdata == 'yes'){
    syn_pop_reg$SOURCE <- 2
  }
  
  if (uselismarginals == 'yes'){
    syn_pop_reg$SOURCE <- 3
  }
  
  
  syn_pop_reg$FARMING <- as.integer(syn_pop_reg$FARMING)
  syn_pop_reg$WEALTH <- as.integer(syn_pop_reg$WEALTH)
  syn_pop_reg$FLOORCAT <- as.integer(syn_pop_reg$FLOORCAT)
  syn_pop_reg$WALLCAT <- as.integer(syn_pop_reg$WALLCAT)
  syn_pop_reg$ROOFCAT <- as.integer(syn_pop_reg$ROOFCAT)
  syn_pop_reg$SOURCE <- as.integer(syn_pop_reg$SOURCE)
  
  name = paste0('synthpop_unknown2_LIS_', regnr, '.dat') 
  
  con = file(name, "wb")
  
  writeBin(c(syn_pop_reg$HID, syn_pop_reg$RELATE, syn_pop_reg$INCOME, syn_pop_reg$WEALTH, syn_pop_reg$RURAL, syn_pop_reg$AGECAT, syn_pop_reg$GENDER, 
             syn_pop_reg$EDUCAT, syn_pop_reg$HHTYPE, syn_pop_reg$HHSIZECAT, syn_pop_reg$FARMING, syn_pop_reg$FLOORCAT, syn_pop_reg$WALLCAT, syn_pop_reg$ROOFCAT, syn_pop_reg$SOURCE), con)
  
  
  close(con)
  
  
  nr_individuals_per_regio <- c(nr_individuals_per_regio, nrow(syn_pop_reg))
  
  
  gc()
  
  
  rm(syn_pop_reg)
  gc()
  
  
  
}


DF_nr_individuals_per_region <- data.frame('GDLcode' = regionnumbers, 'Nr_individuals' = nr_individuals_per_regio)

filename_indiperregion <- paste0("Individuals_per_region_", isocode, ".csv")
write.table(DF_nr_individuals_per_region, filename_indiperregion, row.names = FALSE, sep = ',')

#########################################################################
#########################################################################
############################### END #####################################
#########################################################################
#########################################################################
############################### END #####################################
#########################################################################
#########################################################################
############################### END #####################################
#########################################################################
#########################################################################


