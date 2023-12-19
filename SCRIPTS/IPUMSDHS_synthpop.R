#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)



isocode = as.character(args[1])
print(isocode)

joindhswithipums = as.character(args[2])

library(dplyr)
library(stringr)
library(tidyr)
library(zoo)
library(reshape2)
library(tibble)
library(wrswoR)
library(data.table)
`%ni%` <- Negate(`%in%`)
library(mlfit)
library(readstata13)


#setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2') 



dhscountries <- read.csv('DHScountries_july23.csv', sep = ';')
print('dhscountries imported')


GDL_population <- read.csv('GDL_subnational_population_world.csv', head = TRUE, sep = ',')

GDL_pop2015 <- GDL_population[GDL_population$year == 2015,]

GDLcountry <- GDL_pop2015[GDL_pop2015$iso_code == isocode,]

countryname <- GDLcountry$country[1]



GDLPUMSmatch <- read.csv('GDL_IPUMS_match.csv', sep = ';')

GDLPUMSmatch <- GDLPUMSmatch %>% select(Country, ISO_Code, GDLCODE, Region, GEOLEV1_IPUMS, Label)

GDLPUMSmatch$GDLCODEnr <- substr(GDLPUMSmatch$GDLCODE, nchar(GDLPUMSmatch$GDLCODE) - 2, nchar(GDLPUMSmatch$GDLCODE))




DF_nr_individuals_per_region <- data.frame()

DF_errors <- data.frame('iso_code' = isocode, 'Synth_sq_error' = 0, 'Marginal_sq_error' = 0, 'National_sq_error' = 0)

trs_frequency <- function(df){  
  
  truncated <- trunc(df$Frequency)
  remainders <- (df$Frequency - truncated)
  deficit <- round(sum(df$Frequency) - sum(truncated))
  if (deficit != 0) {
    sampled_indexes <-
      sample_int_crank(length(df$Frequency),
                       size = deficit,
                       prob = remainders
      )
    truncated[sampled_indexes] <-
      truncated[sampled_indexes] + 1L
  }
  #truncated
  
  df$Truncated <- truncated
  
  return(df)  
  
} 

targets_PUMS <- function(pums, marg_or_dataframe){
  
  
  
  dataindi <- pums %>% dplyr::select(SERIAL, PERSONS, HHWT, PERWT, URBAN, SEX, AGE, RELATE, EDATTAIND, GEOLEV1) #EDATTAIND is more detailed education. 
  
  missage <- dataindi[dataindi$AGE == 999,]
  missgender <- dataindi[dataindi$SEX == 9,]
  
  to_remove_ids <- unique(missage$SERIAL) 
  to_remove_ids2 <- unique(missgender$SERIAL)
  
  length1 <- nrow(dataindi)
  
  dataindi <- dataindi[! dataindi$SERIAL %in% to_remove_ids,]
  dataindi <- dataindi[! dataindi$SERIAL %in% to_remove_ids2,]
  
  length2 <- nrow(dataindi)
  
  
  if (length2 < length1) {
    dataindi$HHWT <- dataindi$HHWT * (length1/length2)
    dataindi$PERWT <- dataindi$PERWT * (length1/length2)
  } 
  
  # ADD HHTYPE
  dataindi["RELATE_2"] <- 0
  dataindi$RELATE_2[dataindi$RELATE == 2] <- 1
  
  dataindi["RELATE_3"] <- 0
  dataindi$RELATE_3[dataindi$RELATE == 3] <- 1
  
  dataindi["RELATE_4"] <- 0
  dataindi$RELATE_4[dataindi$RELATE == 4] <- 1
  
  dataindi["RELATE_5"] <- 0
  dataindi$RELATE_5[dataindi$RELATE == 5] <- 1
  dataindi$RELATE_5[dataindi$RELATE == 9] <- 1
  
  Partnerdummy <- dataindi %>%
    group_by(SERIAL) %>%
    summarise(Partnerdummy = sum(RELATE_2))
  
  dataindi <- left_join(dataindi, Partnerdummy, by = 'SERIAL')
  
  Childdummy <- dataindi %>%
    group_by(SERIAL) %>%
    summarise(Childdummy = sum(RELATE_3))
  
  dataindi <- left_join(dataindi, Childdummy, by = 'SERIAL')
  
  Relativedummy <- dataindi %>%
    group_by(SERIAL) %>%
    summarise(Relativedummy = sum(RELATE_4))
  
  dataindi <- left_join(dataindi, Relativedummy, by = 'SERIAL')
  
  Nonrelativedummy <- dataindi %>%
    group_by(SERIAL) %>%
    summarise(Nonrelativedummy = sum(RELATE_5))
  
  dataindi <- left_join(dataindi, Nonrelativedummy, by = 'SERIAL')
  
  dataindi$Otherdummy <- dataindi$Relativedummy + dataindi$Nonrelativedummy 
  
  
  dataindi <- dataindi %>% select(-RELATE_2, -RELATE_3, -RELATE_4, -RELATE_5)
  
  
  dataindi['HHTYPE'] <- 8
  dataindi$HHTYPE[dataindi$PERSONS == 1] <- 1
  dataindi$HHTYPE[(dataindi$PERSONS == 2 & dataindi$Partnerdummy >= 1)] <- 2
  dataindi$HHTYPE[(dataindi$PERSONS >= 3 & dataindi$Partnerdummy >= 1 & dataindi$Childdummy >= 1 & dataindi$Otherdummy == 0)] <- 3
  dataindi$HHTYPE[(dataindi$PERSONS >= 2 & dataindi$Partnerdummy == 0 & dataindi$Childdummy >= 1 & dataindi$Otherdummy == 0)] <- 4
  dataindi$HHTYPE[(dataindi$PERSONS >= 3 & dataindi$Partnerdummy >= 1 & dataindi$Childdummy == 0 & dataindi$Otherdummy >= 1)] <- 5
  dataindi$HHTYPE[(dataindi$PERSONS >= 3 & dataindi$Partnerdummy >= 1 & dataindi$Childdummy >= 1 & dataindi$Otherdummy >= 1)] <- 6
  dataindi$HHTYPE[(dataindi$PERSONS >= 2 & dataindi$Partnerdummy == 0 & dataindi$Childdummy >= 1 & dataindi$Otherdummy >= 1)] <- 7 #no partner in ipums hhtype 7. marginals opnieuw doen. 
  
  
  
  #PERSONS = HHSIZECAT
  dataindi['HHSIZECAT'] <- 0
  dataindi$HHSIZECAT[dataindi$PERSONS == 1] <- 1
  dataindi$HHSIZECAT[dataindi$PERSONS == 2] <- 2
  dataindi$HHSIZECAT[dataindi$PERSONS == 3] <- 3
  dataindi$HHSIZECAT[dataindi$PERSONS == 4] <- 3
  dataindi$HHSIZECAT[dataindi$PERSONS == 5] <- 4
  dataindi$HHSIZECAT[dataindi$PERSONS == 6] <- 4
  dataindi$HHSIZECAT[dataindi$PERSONS %in% c(7,8,9,10)] <- 5
  dataindi$HHSIZECAT[dataindi$PERSONS > 10] <- 6
  
  #URBAN = urban rural
  dataindi['RURAL'] <- 0 #URBAN = 0. LET OP WE ZIJN GESWITCHT! NU KOMT HET OVEREEN MET LIS. 
  dataindi$RURAL[dataindi$URBAN == 1] <- 1 #RURAL = 1
  
  #SEX =  sex of head of household
  dataindi['GENDER'] <- 1 #MALE
  dataindi$GENDER[dataindi$SEX == 2] <- 0 #female
  
  #AGE = age 
  dataindi['AGECAT'] <- 0
  dataindi$AGECAT[dataindi$AGE %in% (0:15)] <- 1
  dataindi$AGECAT[dataindi$AGE %in% (16:25)] <- 2
  dataindi$AGECAT[dataindi$AGE %in% (26:35)] <- 3
  dataindi$AGECAT[dataindi$AGE %in% (36:45)] <- 4
  dataindi$AGECAT[dataindi$AGE %in% (46:55)] <- 5
  dataindi$AGECAT[dataindi$AGE %in% (56:65)] <- 6
  dataindi$AGECAT[dataindi$AGE > 65] <- 7 #97 = 97+
  
  # EDATTAIND: 0: NUI, 110: no schooling, 120 & 130 some primary, 211 212 primary completed, 
  # 221, 222 some secondary. 311, 312, 321, 322 secondary completed, 400 university. 999 unknown or missing. 
  dataindi['EDUCAT'] <- 1 # not in universe zit hier ook bij. No education. 
  dataindi$EDUCAT[dataindi$EDATTAIND == 110] <- 1 #no schooling or preschool
  dataindi$EDUCAT[dataindi$EDATTAIND > 110 & dataindi$EDATTAIND < 200] <- 1 #some primary
  dataindi$EDUCAT[dataindi$EDATTAIND > 199 & dataindi$EDATTAIND < 213] <- 2 # primary completed
  dataindi$EDUCAT[dataindi$EDATTAIND > 212 & dataindi$EDATTAIND < 300] <- 3 # some secondary
  dataindi$EDUCAT[dataindi$EDATTAIND > 299 & dataindi$EDATTAIND < 400] <- 4 # secondary completed
  dataindi$EDUCAT[dataindi$EDATTAIND > 399 & dataindi$EDATTAIND < 995] <- 5 # higher
  
  #dataindi$HHSIZECAT <- as.integer(as.factor(dataindi$HHSIZECAT))
  #dataindi$RURAL <- as.integer(as.factor(dataindi$RURAL))
  dataindi$HHTYPE <- as.integer(as.factor(dataindi$HHTYPE))
  
  
  #dataindi$GENDER <- as.integer(as.factor(dataindi$GENDER))
  #dataindi$AGECAT <- as.integer(as.factor(dataindi$AGECAT))
  #dataindi$EDUCAT <- as.integer(as.factor(dataindi$EDUCAT))
  
  datahead <- dataindi %>% filter(RELATE == 1)
  
  datahead2 <- dataindi %>% filter(RELATE == 1)
  datahead2 <- datahead2 %>% rename(GENDERHEAD = GENDER, AGECATHEAD = AGECAT, EDUCATHEAD = EDUCAT)
  dataindi <- merge(dataindi, datahead2 %>% select(SERIAL, GENDERHEAD, AGECATHEAD, EDUCATHEAD), on = 'SERIAL', how = 'left')
  
  GEOLEV1regions <- sort(unique(dataindi$GEOLEV1), decreasing = FALSE)
  targets <- list()
  
  for (region in GEOLEV1regions){
    #print(region)
    
    dataheadreg <- datahead[datahead$GEOLEV1 == region,]
    dataindireg <- dataindi[dataindi$GEOLEV1 == region,]
    
    
    marginals = list()
    marginals[[1]] = xtabs(HHWT ~ HHSIZECAT, data = dataheadreg) #/ sum(data$HHWT)
    marginals[[2]] = xtabs(HHWT ~ RURAL, data = dataheadreg) #/ sum(data$HHWT)
    #marginals[[3]] = xtabs(HHWT ~ GENDER, data = datahead) #/ sum(data$HHWT)
    #marginals[[4]] = xtabs(HHWT ~ AGECAT, data = datahead) #/ sum(data$HHWT)
    #marginals[[5]] = xtabs(HHWT ~ EDUCAT, data = datahead) #/ sum(data$HHWT)
    marginals[[3]] = xtabs(HHWT ~ HHTYPE, data = dataheadreg) #/ sum(data$HHWT)
    #marginals[[6]] = xtabs(HHWT ~ CHILDCAT, data = data) #/ sum(data$HHWT)
    #marginals[[7]] = xtabs(HHWT ~ ELDERCAT, data = data) #/ sum(data$HHWT)
    
    marginals[[4]] = xtabs(PERWT ~ GENDER, data = dataindireg) #/ sum(data$HHWT)
    marginals[[5]] = xtabs(PERWT ~ AGECAT, data = dataindireg) #/ sum(data$HHWT)
    marginals[[6]] = xtabs(PERWT ~ EDUCAT, data = dataindireg) #/ sum(data$HHWT)
    
    marginals[[7]] = xtabs(HHWT ~ GENDER, data = dataheadreg) #/ sum(data$HHWT)
    marginals[[8]] = xtabs(HHWT ~ AGECAT, data = dataheadreg) #/ sum(data$HHWT)
    marginals[[9]] = xtabs(HHWT ~ EDUCAT, data = dataheadreg) #/ sum(data$HHWT)
    
    
    #put the marginals in tibble form
    #create target list using the marginals
    
    #MLFIT PACKAGE !!!!
    
    for (i in 1:length(marginals)){
      x <- as.data.frame(marginals[[i]])
      tibble <- as_tibble(x)
      
      if (region == GEOLEV1regions[1]){
        
        targets[[i]] <- tibble
        columnnames <- colnames(targets[[i]])
        targets[[i]] <- targets[[i]] %>% add_column(ZONE = region, .before = columnnames[1])
      } else {
        
        tibble_addregion <- tibble
        columnnames <- colnames(tibble_addregion)
        tibble_addregion <- tibble_addregion %>% add_column(ZONE = region, .before = columnnames[1])
        
        targets[[i]] <- bind_rows(targets[[i]], tibble_addregion)
        
      }
    }
    
  }
  #names(targets) <- c('HHSIZECAT', 'RURAL', 'GENDER', 'AGECAT', 'EDUCAT', 'CHILDCAT', 'ELDERCAT')
  names(targets) <- c('HHSIZECAT', 'RURAL','HHTYPE','GENDER','AGECAT','EDUCAT', 'GENDERHEAD','AGECATHEAD','EDUCATHEAD')
  
  
  
  
  
  
  # THIS IS FOR IPFR PACKAGE
  
  #for (i in 1:length(marginals)){
  #  x <- as.data.frame(marginals[[i]])
  #  colnames(x) <- c('name', 'Count')
  #  z <- t(x)
  #  tibble <- as_tibble_row(as.integer(z[2,]), .name_repair = 'unique')
  #  names(tibble) <- z[1,]
  #  targets[[i]] <- tibble
  #}
  #names(targets) <- c('HHSIZECAT', 'RURAL', 'GENDER', 'AGECAT', 'EDUCAT', 'CHILDCAT', 'ELDERCAT')
  #names(targets) <- c('HHSIZECAT', 'RURAL','HHTYPE','GENDER','AGECAT','EDUCAT')
  
  
  if (marg_or_dataframe == "marg"){
    
    return(targets)
    
  } 
  
  if (marg_or_dataframe != "marg"){
    
    return(dataindi)
  }
  
}

dhs_survey2 <- function(dhs, countryname){
  dataindi <- dhs %>% select(HID, URBANHH, SEX, HHAGE, EDSUMM, HHRELATE, HHMEMBERS, WEALTHQHH, AGLANDYN) #changed hv106 to hv109, because more categories in hv109.
  
  # hv001 cluster number for location
  # hv009 number of household members
  # hv025 urban or rural
  # HV101 relationship to HH head
  # HV104 sex household member
  # hv105 age household member
  # hv109 education
  # hv219 sex head
  # hv220 age head
  # HV244 owns land for agriculture
  # HV270 wealth index. 
  # hv213 floor
  # hv214 wall
  # hv215 roof
  
  missage <- dataindi[dataindi$HHAGE >= 97,]
  missgender <- dataindi[dataindi$SEX >= 8,]
  
  missage2 <- dataindi[is.na(dataindi$HHAGE),]
  missgender2 <- dataindi[is.na(dataindi$SEX),]
  
  to_remove_ids <- unique(missage$HID) 
  to_remove_ids2 <- unique(missgender$HID)
  to_remove_ids3 <- unique(missage2$HID) 
  to_remove_ids4 <- unique(missgender2$HID)
  
  
  dataindi <- dataindi[! dataindi$HID %in% to_remove_ids,]
  dataindi <- dataindi[! dataindi$HID %in% to_remove_ids2,]
  dataindi <- dataindi[! dataindi$HID %in% to_remove_ids3,]
  dataindi <- dataindi[! dataindi$HID %in% to_remove_ids4,]
  
  
  
  
  #hv009 = HHSIZECAT
  dataindi['HHSIZECAT'] <- 0
  dataindi$HHSIZECAT[dataindi$HHMEMBERS == 1] <- 1
  dataindi$HHSIZECAT[dataindi$HHMEMBERS == 2] <- 2
  dataindi$HHSIZECAT[dataindi$HHMEMBERS == 3] <- 3
  dataindi$HHSIZECAT[dataindi$HHMEMBERS == 4] <- 3
  dataindi$HHSIZECAT[dataindi$HHMEMBERS == 5] <- 4
  dataindi$HHSIZECAT[dataindi$HHMEMBERS == 6] <- 4
  dataindi$HHSIZECAT[dataindi$HHMEMBERS %in% c(7,8,9,10)] <- 5
  dataindi$HHSIZECAT[dataindi$HHMEMBERS > 10] <- 6
  
  #URBANHH: 1 URBAN, 2 RURAL
  dataindi['RURAL'] <- 0 #URBAN
  dataindi$URBANHH <- as.character(dataindi$URBANHH)
  dataindi$RURAL[dataindi$URBANHH == 'rural'] <- 1 #RURAL
  dataindi$RURAL[dataindi$URBANHH == 'Rural'] <- 1 #RURAL
  dataindi$RURAL[dataindi$URBANHH == '2'] <- 1 #RURAL
  
  #hv219 = sex of hh member
  dataindi['GENDER'] <- 1 #Male
  dataindi$SEX <- as.character(dataindi$SEX)
  dataindi$GENDER[dataindi$SEX == 'female'] <- 0 #Female
  dataindi$GENDER[dataindi$SEX == 'Female'] <- 0 #Female
  dataindi$GENDER[dataindi$SEX == '2'] <- 0 #Female
  
  #hv105 = age hh member
  dataindi['AGECAT'] <- 0
  dataindi$AGECAT[dataindi$HHAGE %in% (0:15)] <- 1
  dataindi$AGECAT[dataindi$HHAGE %in% (16:25)] <- 2
  dataindi$AGECAT[dataindi$HHAGE %in% (26:35)] <- 3
  dataindi$AGECAT[dataindi$HHAGE %in% (36:45)] <- 4
  dataindi$AGECAT[dataindi$HHAGE %in% (46:55)] <- 5
  dataindi$AGECAT[dataindi$HHAGE %in% (56:65)] <- 6
  dataindi$AGECAT[dataindi$HHAGE > 65] <- 7 #missing and don't know eruit. 
  
  #hv109 = highest attained education
  dataindi['EDUCAT'] <- 1
  dataindi$EDUCAT[dataindi$EDSUMM == 'no education'] <- 1 #no edu or preschool
  dataindi$EDUCAT[dataindi$EDSUMM == 'incomplete primary'] <- 1 #incomplete primary
  dataindi$EDUCAT[dataindi$EDSUMM == 'complete primary'] <- 2 #primary
  dataindi$EDUCAT[dataindi$EDSUMM == 'incomplete secondary'] <- 3 #incomplete primary
  dataindi$EDUCAT[dataindi$EDSUMM == 'complete secondary'] <- 4 #secondary
  dataindi$EDUCAT[dataindi$EDSUMM == 'higher'] <- 5 #higher
  
  dataindi$EDUCAT[dataindi$EDSUMM == 'No education'] <- 1 #no edu or preschool
  dataindi$EDUCAT[dataindi$EDSUMM == 'Incomplete primary'] <- 1 #incomplete primary
  dataindi$EDUCAT[dataindi$EDSUMM == 'Complete primary'] <- 2 #primary
  dataindi$EDUCAT[dataindi$EDSUMM == 'Incomplete secondary'] <- 3 #incomplete secondary
  dataindi$EDUCAT[dataindi$EDSUMM == 'Complete secondary'] <- 4 #secondary
  dataindi$EDUCAT[dataindi$EDSUMM == 'Higher'] <- 5 #higher
  
  dataindi$EDUCAT[dataindi$EDSUMM == 0] <- 1 #no edu or preschool
  dataindi$EDUCAT[dataindi$EDSUMM == 1] <- 1 #incomplete primary
  dataindi$EDUCAT[dataindi$EDSUMM == 2] <- 2 #primary
  dataindi$EDUCAT[dataindi$EDSUMM == 3] <- 3 #incomplete secondary
  dataindi$EDUCAT[dataindi$EDSUMM == 4] <- 4 #secondary
  dataindi$EDUCAT[dataindi$EDSUMM == 5] <- 5 #higher
  
  if (countryname %in% c('Armenia', 'KyrgyzRepublic') == TRUE){
    dataindi$EDUCAT[dataindi$EDUCAT == 2] <- 1
  }
  
  data_EDUCAT <- dataindi[dataindi$EDUCAT == 0,]
  
  if (nrow(dataindi) == nrow(data_EDUCAT)){
    print(paste0("Check DHS EDUCAT for potential error ", countryname))
  }
  
  dataindi['WEALTH'] <- 0
  dataindi$WEALTH[dataindi$WEALTHQHH == 'Poorest'] <- 1 
  dataindi$WEALTH[dataindi$WEALTHQHH == 'Poorer'] <- 2 
  dataindi$WEALTH[dataindi$WEALTHQHH == 'Middle'] <- 3 
  dataindi$WEALTH[dataindi$WEALTHQHH == 'Richer'] <- 4  
  dataindi$WEALTH[dataindi$WEALTHQHH == 'Richest'] <- 5
  
  dataindi$WEALTH[dataindi$WEALTHQHH == 'poorest'] <- 1 
  dataindi$WEALTH[dataindi$WEALTHQHH == 'poorer'] <- 2 
  dataindi$WEALTH[dataindi$WEALTHQHH == 'middle'] <- 3 
  dataindi$WEALTH[dataindi$WEALTHQHH == 'richer'] <- 4  
  dataindi$WEALTH[dataindi$WEALTHQHH == 'richest'] <- 5
  
  dataindi$WEALTH[dataindi$WEALTHQHH == 'lowest'] <- 1 
  dataindi$WEALTH[dataindi$WEALTHQHH == 'second'] <- 2 
  dataindi$WEALTH[dataindi$WEALTHQHH == 'fourth'] <- 4  
  dataindi$WEALTH[dataindi$WEALTHQHH == 'highest'] <- 5
  
  #dataindi$WEALTH <- as.integer(dataindi$WEALTH)
  
  dataindi$WEALTH[dataindi$WEALTHQHH < 1] <- 1
  dataindi$WEALTH[dataindi$WEALTHQHH == 1] <- 1 
  dataindi$WEALTH[dataindi$WEALTHQHH == 2] <- 2 
  dataindi$WEALTH[dataindi$WEALTHQHH == 3] <- 3 
  dataindi$WEALTH[dataindi$WEALTHQHH == 4] <- 4  
  dataindi$WEALTH[dataindi$WEALTHQHH == 5] <- 5
  dataindi$WEALTH[dataindi$WEALTHQHH > 5] <- 5
  
  data_WEALTH <- dataindi[dataindi$WEALTH == 0,]
  
  
  if ((nrow(data_WEALTH)/nrow(dataindi) > 0 & nrow(data_WEALTH)/nrow(dataindi) <= 0.005)){
    print(paste0("Minimal DHS wealth error ", countryname))
    dataindi <- dataindi[dataindi$WEALTH != 0,]
  }
  
  if ((nrow(data_WEALTH)/nrow(dataindi) > 0.005 & nrow(data_WEALTH)/nrow(dataindi) <= 0.01)){
    print(paste0("Less than 1% DHS wealth missing", countryname))
    dataindi <- dataindi[dataindi$WEALTH != 0,]
  }
  
  if ((nrow(data_WEALTH)/nrow(dataindi) > 0.01 & nrow(data_WEALTH)/nrow(dataindi) <= 0.1)){
    print(paste0("Substantial DHS wealth error", countryname))
    dataindi <- dataindi[dataindi$WEALTH != 0,]
  }
  
  if ((nrow(data_WEALTH)/nrow(dataindi) > 0.1)){
    print(paste0("DHS wealth error too big", countryname))
    dataindi <- dataindi[dataindi$WEALTH != 0,]
  }
  
  dataindi['FARMING'] <- 0
  dataindi$FARMING[dataindi$AGLANDYN == 'Yes'] <- 1
  dataindi$FARMING[dataindi$AGLANDYN == 'yes'] <- 1
  dataindi$FARMING[dataindi$AGLANDYN == 1] <- 1
  
  data_FARM <- dataindi[dataindi$FARMING == 0,]
  
  if (nrow(data_FARM) == nrow(dataindi)){
    print(paste0("Check DHS farming for error ", countryname))
  }
  
  #relationship to HH head
  dataindi["RELATE"] <- 4
  
  dataindi$RELATE[dataindi$HHRELATE == "Wife or husband"] <- 2
  dataindi$RELATE[dataindi$HHRELATE == "wife or husband"] <- 2
  dataindi$RELATE[dataindi$HHRELATE == 2] <- 2
  dataindi$RELATE[dataindi$HHRELATE == "Co-spouse"] <- 2
  dataindi$RELATE[dataindi$HHRELATE == "co-spouse"] <- 2
  dataindi$RELATE[dataindi$HHRELATE == 9] <- 2
  
  dataindi$RELATE[dataindi$HHRELATE == "Head"] <- 1
  dataindi$RELATE[dataindi$HHRELATE == 1] <- 1
  dataindi$RELATE[dataindi$HHRELATE == "head"] <- 1
  
  
  dataindi$RELATE[dataindi$HHRELATE == "Son/daughter"] <- 3
  dataindi$RELATE[dataindi$HHRELATE == "son/daughter"] <- 3
  dataindi$RELATE[dataindi$HHRELATE == 3] <- 3
  dataindi$RELATE[dataindi$HHRELATE == "Son/daughter-in-law"] <- 3
  dataindi$RELATE[dataindi$HHRELATE == "son/daughter-in-law"] <- 3
  dataindi$RELATE[dataindi$HHRELATE == 4] <- 3
  dataindi$RELATE[dataindi$HHRELATE == "adopted/foster child/stepchild"] <- 3
  dataindi$RELATE[dataindi$HHRELATE == "adopted/foster/step child"] <- 3
  
  dataindi$RELATE[dataindi$HHRELATE == "Adopted/foster child"] <- 3
  dataindi$RELATE[dataindi$HHRELATE == "adopted/foster child"] <- 3
  dataindi$RELATE[dataindi$HHRELATE == 11] <- 3
  dataindi$RELATE[dataindi$HHRELATE == "Stepson/daughter"] <- 3
  dataindi$RELATE[dataindi$HHRELATE == "stepson/daughter"] <- 3
  
  
  dataindi$RELATE[dataindi$HHRELATE == "Not related"] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "not related"] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "domestic employee (cs)"] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "herdboy (cs)"] <- 5
  dataindi$RELATE[dataindi$HHRELATE %in% c(12, 15,40,41, 42, 97, 98, 99)] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "Don't know"] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "don't know"] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "DK"] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "dk"] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "Missing"] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "missing"] <- 5
  
  
  
  #for hhtypes
  dataindi["RELATE_2"] <- 0
  dataindi$RELATE_2[dataindi$RELATE == 2] <- 1
  dataindi["RELATE_3"] <- 0
  dataindi$RELATE_3[dataindi$RELATE == 3] <- 1
  dataindi["RELATE_4"] <- 0
  dataindi$RELATE_4[dataindi$RELATE == 4] <- 1
  dataindi["RELATE_5"] <- 0
  dataindi$RELATE_5[dataindi$RELATE == 5] <- 1
  
  # household head
  dhshead <- dataindi[dataindi$RELATE == 1,]
  dhshead <- dhshead %>% rename(GENDERHEAD = GENDER, EDUCATHEAD = EDUCAT, AGECATHEAD = AGECAT)
  dataindi <- merge(dataindi, dhshead[c('HID', 'GENDERHEAD', 'EDUCATHEAD', 'AGECATHEAD')], on = 'HID', how = 'left')
  
  
  Partnerdummy <- dataindi %>%
    group_by(HID) %>%
    summarise(Partnerdummy = sum(RELATE_2))
  
  dataindi <- left_join(dataindi, Partnerdummy, by = 'HID')
  
  Childdummy <- dataindi %>%
    group_by(HID) %>%
    summarise(Childdummy = sum(RELATE_3))
  
  dataindi <- left_join(dataindi, Childdummy, by = 'HID')
  
  Relativedummy <- dataindi %>%
    group_by(HID) %>%
    summarise(Relativedummy = sum(RELATE_4))
  
  dataindi <- left_join(dataindi, Relativedummy, by = 'HID')
  
  Nonrelativedummy <- dataindi %>%
    group_by(HID) %>%
    summarise(Nonrelativedummy = sum(RELATE_5))
  
  dataindi <- left_join(dataindi, Nonrelativedummy, by = 'HID')
  
  dataindi$Otherdummy <- dataindi$Relativedummy + dataindi$Nonrelativedummy 
  
  
  dataindi <- dataindi %>% select(-RELATE_2, -RELATE_3, -RELATE_4, -RELATE_5)
  
  dataindi['HHTYPE'] <- 8
  dataindi$HHTYPE[dataindi$HHMEMBERS == 1] <- 1
  dataindi$HHTYPE[(dataindi$HHMEMBERS == 2 & dataindi$Partnerdummy >= 1)] <- 2
  dataindi$HHTYPE[(dataindi$HHMEMBERS >= 3 & dataindi$Partnerdummy >= 1 & dataindi$Childdummy >= 1 & dataindi$Otherdummy == 0)] <- 3
  dataindi$HHTYPE[(dataindi$HHMEMBERS >= 2 & dataindi$Partnerdummy == 0 & dataindi$Childdummy >= 1 & dataindi$Otherdummy == 0)] <- 4
  dataindi$HHTYPE[(dataindi$HHMEMBERS >= 3 & dataindi$Partnerdummy >= 1 & dataindi$Childdummy == 0 & dataindi$Otherdummy >= 1)] <- 5
  dataindi$HHTYPE[(dataindi$HHMEMBERS >= 3 & dataindi$Partnerdummy >= 1 & dataindi$Childdummy >= 1 & dataindi$Otherdummy >= 1)] <- 6
  dataindi$HHTYPE[(dataindi$HHMEMBERS >= 2 & dataindi$Partnerdummy == 0 & dataindi$Childdummy >= 1 & dataindi$Otherdummy >= 1)] <- 7 #moet partnerdummy niet 0 zijn?? 
  
  
  
  dataindi['PID'] = (1:nrow(dataindi))
  
  datadhs <- dataindi %>% select(HID, PID, HHSIZECAT, RURAL, HHTYPE, GENDER, AGECAT, EDUCAT, GENDERHEAD, AGECATHEAD, EDUCATHEAD, WEALTH, FARMING, RELATE)
  
  return(datadhs)
  
}



i <- which(dhscountries$countries == countryname)


countrynumber <- dhscountries[i,4]
countryyear <- dhscountries[i,2]


pumsfilename <- paste0(isocode, "_margpums.csv")
pums <- read.csv(pumsfilename)
print('margpums imported')

if (countryname == 'Ethiopia'){
  pums <- pums[pums$GEOLEV1 != 231017,]
}

if (countryname == 'Benin'){
  pums$GEOLEV1[pums$GEOLEV1 %in% c(204002, 204007)] <- 101
  pums$GEOLEV1[pums$GEOLEV1 %in% c(204003, 204008)] <- 102
  pums$GEOLEV1[pums$GEOLEV1 %in% c(204001, 204004)] <- 103
  pums$GEOLEV1[pums$GEOLEV1 %in% c(204006, 204009)] <- 104
  pums$GEOLEV1[pums$GEOLEV1 %in% c(204010, 204011)] <- 105
  pums$GEOLEV1[pums$GEOLEV1 %in% c(204005, 204012)] <- 106
}

if (countryname == 'Egypt'){
  pums$GEOLEV1[pums$GEOLEV1 %in% c(818031, 818032, 818033, 818034, 818035)] <- 818031
}

if (countryname == 'Guinea'){
  pums$GEOLEV1[pums$GEOLEV1 %in% c(324011, 324012, 324013, 324014, 324015)] <- 101
  pums$GEOLEV1[pums$GEOLEV1 %in% c(324021)] <- 102
  pums$GEOLEV1[pums$GEOLEV1 %in% c(324031, 324032, 324033, 324034)] <- 103
  pums$GEOLEV1[pums$GEOLEV1 %in% c(324041, 324042, 324043, 324044, 324045)] <- 104
  pums$GEOLEV1[pums$GEOLEV1 %in% c(324051, 324052, 324053, 324054, 324055)] <- 105
  pums$GEOLEV1[pums$GEOLEV1 %in% c(324061, 324062, 324063, 324064, 324065)] <- 106
  pums$GEOLEV1[pums$GEOLEV1 %in% c(324071, 324072, 324073)] <- 107
  pums$GEOLEV1[pums$GEOLEV1 %in% c(324081, 324082, 324083, 324084, 324085, 324086)] <- 108
}


if (countryname == 'Cambodia'){
  pums$GEOLEV1[pums$GEOLEV1 %in% c(116002, 116024)] <- 116002
  pums$GEOLEV1[pums$GEOLEV1 %in% c(116007, 116023)] <- 116007
  pums$GEOLEV1[pums$GEOLEV1 %in% c(116010, 116013, 116019)] <- 116010
  pums$GEOLEV1[pums$GEOLEV1 %in% c(116011, 116016)] <- 116011
}

if (countryname == 'Myanmar'){
  pums$GEOLEV1[pums$GEOLEV1 %in% c(104015)] <- 104009
}

if (countryname == 'Malawi'){
  pums$GEOLEV1[pums$GEOLEV1 %in% c(454101, 454102, 454103, 454104)] <- 454101
  pums$GEOLEV1[pums$GEOLEV1 %in% c(454202, 454203, 454204, 454207, 454208, 454209)] <- 454202
  pums$GEOLEV1[pums$GEOLEV1 %in% c(454304, 454309, 454310, 454311, 454312, 454313)] <- 454304
}

if (countryname == 'Nepal'){
  pums$GEOLEV1[pums$GEOLEV1 %in% c(524001, 524002, 524003)] <- 101
  pums$GEOLEV1[pums$GEOLEV1 %in% c(524004, 524005, 524006)] <- 102
  pums$GEOLEV1[pums$GEOLEV1 %in% c(524007, 524008, 524009)] <- 103
  pums$GEOLEV1[pums$GEOLEV1 %in% c(524010, 524011, 524012)] <- 104
  pums$GEOLEV1[pums$GEOLEV1 %in% c(524013, 524014)] <- 105
}

if (countryname == 'Uganda'){
  pums$GEOLEV1[pums$GEOLEV1 %in% c(800101, 800105, 800106, 800110)] <- 101
  pums$GEOLEV1[pums$GEOLEV1 %in% c(800103, 800104, 800107, 800108)] <- 102
  pums$GEOLEV1[pums$GEOLEV1 %in% c(800102)] <- 103
  pums$GEOLEV1[pums$GEOLEV1 %in% c(800201, 800202, 800204, 800205)] <- 104
  pums$GEOLEV1[pums$GEOLEV1 %in% c(800206, 800207, 800208, 800209, 800210)] <- 105
  pums$GEOLEV1[pums$GEOLEV1 %in% c(800302, 800304, 800305, 800306, 800307)] <- 106
  pums$GEOLEV1[pums$GEOLEV1 %in% c(800301, 800303, 800310)] <- 107
  pums$GEOLEV1[pums$GEOLEV1 %in% c(800401, 800403, 800405, 800406, 800407, 800409)] <- 108
  pums$GEOLEV1[pums$GEOLEV1 %in% c(800402, 800404, 800408, 800412)] <- 109
}

if (countryname == 'Ethiopia'){
  pums <- pums[pums$GEOLEV1 != 231017 ,]
}

if (countryname == 'Philippines'){
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608074, 608075, 608076, 608039)] <- 101
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608001, 608011, 608015, 608027, 608032, 608044)] <- 102
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608014, 608028, 608029, 608033, 608055)] <- 103
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608031, 608050, 608057)] <- 104
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608008, 608049, 608054, 608069, 608071)] <- 105
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608010, 608021, 608034, 608056, 608058)] <- 106
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608040, 608051, 608052, 608053, 608059)] <- 107
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608005, 608016, 608017, 608020, 608041, 608062)] <- 108
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608004, 608006, 608019, 608030, 608045)] <- 109
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608012, 608022, 608046, 608061)] <- 110
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608026, 608037, 608048, 608060, 608064, 608067, 608068)] <- 111
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608072, 608073)] <- 112
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608013, 608018, 608035, 608042, 608043)] <- 113
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608023, 608024, 608025)] <- 114
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608047, 608065, 608063)] <- 115
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608002, 608003)] <- 116
  pums$GEOLEV1[pums$GEOLEV1 %in% c(608007, 608036, 608038, 608066, 608070)] <- 117
}


if (countryname == 'Ecuador'){
  pums$GEOLEV1[pums$GEOLEV1 %in% c(218009, 218007)] <- 101
  pums$GEOLEV1[pums$GEOLEV1 %in% c(218001, 218002, 218004, 218005, 218006, 218010, 218011, 218018)] <- 102
  pums$GEOLEV1[pums$GEOLEV1 %in% c(218014, 218016, 218019, 218021)] <- 103
}

if (countryname == 'Mauritius'){
  pums$GEOLEV1[pums$GEOLEV1 %in% c(480011, 480012, 480013, 480014, 480018)] <- 101
  pums$GEOLEV1[pums$GEOLEV1 %in% c(480015, 480016, 480017, 480019)] <- 102
  pums$GEOLEV1[pums$GEOLEV1 %in% c(480020)] <- 103
}

if (countryname == 'Romania'){
  pums$GEOLEV1[pums$GEOLEV1 %in% c(642004, 642007, 642022, 642027, 642033, 642037)] <- 101
  pums$GEOLEV1[pums$GEOLEV1 %in% c(642009, 642010, 642013, 642017, 642036, 642039)] <- 102
  pums$GEOLEV1[pums$GEOLEV1 %in% c(642003, 642051, 642015, 642029, 642034)] <- 103
  pums$GEOLEV1[pums$GEOLEV1 %in% c(642016, 642018, 642025, 642028, 642038)] <- 104
  pums$GEOLEV1[pums$GEOLEV1 %in% c(642002, 642011, 642020, 642035)] <- 105
  pums$GEOLEV1[pums$GEOLEV1 %in% c(642005, 642006, 642012, 642024, 642030, 64031)] <- 106
  pums$GEOLEV1[pums$GEOLEV1 %in% c(642001, 642008, 642014, 642019, 642026, 642032)] <- 107
  pums$GEOLEV1[pums$GEOLEV1 %in% c(642043)] <- 108
}


dhsfilename <- paste0(isocode, countryyear, 'DHS.DTA')

dhs <- read.dta13(dhsfilename)

print('DHSdata imported')

dhs <- dhs %>% select(hhid, hv009, hv024, hv025, hv101, hv104, hv105, hv109, hv244, hv270, hv213, hv214, hv215)


table(dhs$hv024)

dhs <- dhs %>% rename(HID = hhid, HHMEMBERS = hv009, URBANHH = hv025, HHRELATE = hv101, 
                      SEX = hv104, HHAGE = hv105, EDSUMM = hv109, AGLANDYN = hv244,
                      WEALTHQHH = hv270, FLOOR = hv213, WALL = hv214, ROOF = hv215)



GEOLEV1regions <- sort(unique(pums$GEOLEV1), decreasing = FALSE)


test <- dhs_survey2(dhs, countryname)



pumsdf <- targets_PUMS(pums, "df")


#####################################################################
# Joint distributie dhs data



fill_zero_cells <- function(jointhead1){
  
  colnames <- names(jointhead1)
  
  colnames <- colnames[-length(colnames)]
  
  comb4 <- combn(colnames, length(colnames)-1)
  
  
  #jointhead0 <- jointhead1
  
  
  if ('HHTYPE' %in% colnames(jointhead1)){
    
    jointhead1$Frequency <- ifelse(jointhead1$HHSIZECAT == 1 & jointhead1$HHTYPE > 1, -1, jointhead1$Frequency)
    jointhead1$Frequency <- ifelse(jointhead1$HHSIZECAT > 1 & jointhead1$HHTYPE == 1, -1, jointhead1$Frequency)
    jointhead1$Frequency <- ifelse(jointhead1$HHSIZECAT > 2 & jointhead1$HHTYPE == 2, -1, jointhead1$Frequency)
    
    jointheadmin1 <- jointhead1[jointhead1$Frequency == -1 ,]
    
  }
  
  
  
  
  
  jointhead0 <- jointhead1[jointhead1$Frequency == 0,]
  
  jointhead1[jointhead1 == -1] <- 0
  
  
  impute1 = c()
  impute2 = c()
  impute3 = c()
  impute4 = c()
  impute5 = c()
  impute6 = c()
  impute7 = c()
  
  
  jh4_1 <- jointhead1 %>% group_by(across(all_of(comb4[,1]))) %>% summarise(Frequency = sum(Frequency))
  jh4_2 <- jointhead1 %>% group_by(across(all_of(comb4[,2]))) %>% summarise(Frequency = sum(Frequency))  
  jh4_3 <- jointhead1 %>% group_by(across(all_of(comb4[,3]))) %>% summarise(Frequency = sum(Frequency))
  jh4_4 <- jointhead1 %>% group_by(across(all_of(comb4[,4]))) %>% summarise(Frequency = sum(Frequency))  
  jh4_5 <- jointhead1 %>% group_by(across(all_of(comb4[,5]))) %>% summarise(Frequency = sum(Frequency)) 
  jh4_6 <- jointhead1 %>% group_by(across(all_of(comb4[,6]))) %>% summarise(Frequency = sum(Frequency)) 
  jh4_7 <- jointhead1 %>% group_by(across(all_of(comb4[,7]))) %>% summarise(Frequency = sum(Frequency)) 
  
  
  sumfreq = sum(jointhead1$Frequency)
  
  margname1 = colnames[!grepl(paste(comb4[,1], collapse = "|"), colnames)]
  marg1 = jointhead1 %>% group_by(across(all_of(margname1))) %>% summarise(Frequency = sum(Frequency))
  marg1$Frequency = marg1$Frequency/sumfreq
  
  margname2 = colnames[!grepl(paste(comb4[,2], collapse = "|"), colnames)]
  marg2 = jointhead1 %>% group_by(across(all_of(margname2))) %>% summarise(Frequency = sum(Frequency))
  marg2$Frequency = marg2$Frequency/sumfreq
  
  margname3 = colnames[!grepl(paste(comb4[,3], collapse = "|"), colnames)]
  marg3 = jointhead1 %>% group_by(across(all_of(margname3))) %>% summarise(Frequency = sum(Frequency))
  marg3$Frequency = marg3$Frequency/sumfreq
  
  margname4 = colnames[!grepl(paste(comb4[,4], collapse = "|"), colnames)]
  marg4 = jointhead1 %>% group_by(across(all_of(margname4))) %>% summarise(Frequency = sum(Frequency))
  marg4$Frequency = marg4$Frequency/sumfreq
  
  margname5 = colnames[!grepl(paste(comb4[,5], collapse = "|"), colnames)]
  marg5 = jointhead1 %>% group_by(across(all_of(margname5))) %>% summarise(Frequency = sum(Frequency))
  marg5$Frequency = marg5$Frequency/sumfreq
  
  margname6 = colnames[!grepl(paste(comb4[,6], collapse = "|"), colnames)]
  marg6 = jointhead1 %>% group_by(across(all_of(margname6))) %>% summarise(Frequency = sum(Frequency))
  marg6$Frequency = marg6$Frequency/sumfreq
  
  margname7 = colnames[!grepl(paste(comb4[,7], collapse = "|"), colnames)]
  marg7 = jointhead1 %>% group_by(across(all_of(margname7))) %>% summarise(Frequency = sum(Frequency))
  marg7$Frequency = marg7$Frequency/sumfreq
  
  impute1 <- numeric(nrow(jointhead0))
  impute2 <- numeric(nrow(jointhead0))
  impute3 <- numeric(nrow(jointhead0))
  impute4 <- numeric(nrow(jointhead0))
  impute5 <- numeric(nrow(jointhead0))
  impute6 <- numeric(nrow(jointhead0))
  impute7 <- numeric(nrow(jointhead0))
  
  for (row in 1:nrow(jointhead0)){
    rowjh <- jointhead0[row,]
    
    jh4_1s <- jh4_1[jh4_1[comb4[1,1]] == rowjh[comb4[1,1]][[1]] & jh4_1[comb4[2,1]] == rowjh[comb4[2,1]][[1]] & jh4_1[comb4[3,1]] == rowjh[comb4[3,1]][[1]] & jh4_1[comb4[4,1]] == rowjh[comb4[4,1]][[1]] & jh4_1[comb4[5,1]] == rowjh[comb4[5,1]][[1]] & jh4_1[comb4[6,1]] == rowjh[comb4[6,1]][[1]],]
    jh4_2s <- jh4_2[jh4_2[comb4[1,2]] == rowjh[comb4[1,2]][[1]] & jh4_2[comb4[2,2]] == rowjh[comb4[2,2]][[1]] & jh4_2[comb4[3,2]] == rowjh[comb4[3,2]][[1]] & jh4_2[comb4[4,2]] == rowjh[comb4[4,2]][[1]] & jh4_2[comb4[5,2]] == rowjh[comb4[5,2]][[1]] & jh4_2[comb4[6,2]] == rowjh[comb4[6,2]][[1]],]
    jh4_3s <- jh4_3[jh4_3[comb4[1,3]] == rowjh[comb4[1,3]][[1]] & jh4_3[comb4[2,3]] == rowjh[comb4[2,3]][[1]] & jh4_3[comb4[3,3]] == rowjh[comb4[3,3]][[1]] & jh4_3[comb4[4,3]] == rowjh[comb4[4,3]][[1]] & jh4_3[comb4[5,3]] == rowjh[comb4[5,3]][[1]] & jh4_3[comb4[6,3]] == rowjh[comb4[6,3]][[1]],]
    jh4_4s <- jh4_4[jh4_4[comb4[1,4]] == rowjh[comb4[1,4]][[1]] & jh4_4[comb4[2,4]] == rowjh[comb4[2,4]][[1]] & jh4_4[comb4[3,4]] == rowjh[comb4[3,4]][[1]] & jh4_4[comb4[4,4]] == rowjh[comb4[4,4]][[1]] & jh4_4[comb4[5,4]] == rowjh[comb4[5,4]][[1]] & jh4_4[comb4[6,4]] == rowjh[comb4[6,4]][[1]],]
    jh4_5s <- jh4_5[jh4_5[comb4[1,5]] == rowjh[comb4[1,5]][[1]] & jh4_5[comb4[2,5]] == rowjh[comb4[2,5]][[1]] & jh4_5[comb4[3,5]] == rowjh[comb4[3,5]][[1]] & jh4_5[comb4[4,5]] == rowjh[comb4[4,5]][[1]] & jh4_5[comb4[5,5]] == rowjh[comb4[5,5]][[1]] & jh4_5[comb4[6,5]] == rowjh[comb4[6,5]][[1]],]
    jh4_6s <- jh4_6[jh4_6[comb4[1,6]] == rowjh[comb4[1,6]][[1]] & jh4_6[comb4[2,6]] == rowjh[comb4[2,6]][[1]] & jh4_6[comb4[3,6]] == rowjh[comb4[3,6]][[1]] & jh4_6[comb4[4,6]] == rowjh[comb4[4,6]][[1]] & jh4_6[comb4[5,6]] == rowjh[comb4[5,6]][[1]] & jh4_6[comb4[6,6]] == rowjh[comb4[6,6]][[1]],]
    jh4_7s <- jh4_7[jh4_7[comb4[1,7]] == rowjh[comb4[1,7]][[1]] & jh4_7[comb4[2,7]] == rowjh[comb4[2,7]][[1]] & jh4_7[comb4[3,7]] == rowjh[comb4[3,7]][[1]] & jh4_7[comb4[4,7]] == rowjh[comb4[4,7]][[1]] & jh4_7[comb4[5,7]] == rowjh[comb4[5,7]][[1]] & jh4_7[comb4[6,7]] == rowjh[comb4[6,7]][[1]],]
    
    impute1[row] = jh4_1s$Frequency * marg1[marg1[margname1] == rowjh[margname1][[1]],]$Frequency
    
    impute2[row] = jh4_2s$Frequency * marg2[marg2[margname2] == rowjh[margname2][[1]],]$Frequency
    
    impute3[row] = jh4_3s$Frequency * marg3[marg3[margname3] == rowjh[margname3][[1]],]$Frequency
    
    impute4[row] = jh4_4s$Frequency * marg4[marg4[margname4] == rowjh[margname4][[1]],]$Frequency
    
    impute5[row] = jh4_5s$Frequency * marg5[marg5[margname5] == rowjh[margname5][[1]],]$Frequency
    
    impute6[row] = jh4_6s$Frequency * marg6[marg6[margname6] == rowjh[margname6][[1]],]$Frequency
    
    impute7[row] = jh4_7s$Frequency * marg7[marg7[margname7] == rowjh[margname7][[1]],]$Frequency
    
    if (row %% 1000 == 0) {
      print(row)
    }
    
  }
  
  
  imputations <- data.frame(impute1 = impute1, impute2 = impute2, impute3 = impute3, impute4 = impute4, impute5 = impute5, impute6 = impute6, impute7 = impute7)
  imputations <- imputations %>% 
    mutate(mean_impute = (impute1 + impute2 + impute3 + impute4 + impute5 + impute6 + impute7) / 7)
  
  
  imputations0 <- imputations[imputations$mean_impute == 0,]
  
  
  zero_indices <- which(jointhead1$Frequency == 0)
  jointhead1$imputations <- jointhead1$Frequency
  jointhead1$imputations[zero_indices] <- imputations$mean_impute
  
  sumfreq = sum(jointhead1$Frequency)
  sumimpu = sum(jointhead1$imputations)
  
  jointhead1$imputations = jointhead1$imputations * (sumfreq/sumimpu)
  
  return(jointhead1)
  
  
}


join_DHS_with_IPUMS <- function(){
  
  
  dhshead <- test[test$RELATE == 1,]
  
  jointdhswealth.table <- xtabs( ~ RURAL + HHSIZECAT + HHTYPE + GENDERHEAD + AGECATHEAD + EDUCATHEAD + WEALTH, data = dhshead)
  
  jointdhswealth <- as.data.frame(ftable(jointdhswealth.table))
  
  jointdhswealth <- jointdhswealth[(order(jointdhswealth$RURAL, jointdhswealth$HHSIZECAT, jointdhswealth$HHTYPE, jointdhswealth$GENDERHEAD, jointdhswealth$AGECATHEAD, jointdhswealth$EDUCATHEAD, jointdhswealth$WEALTH)),]
  
  
  jointdhsfarming.table <- xtabs( ~ RURAL + HHSIZECAT + GENDERHEAD + AGECATHEAD + EDUCATHEAD + WEALTH + FARMING, data = dhshead)
  
  jointdhsfarm <-as.data.frame(ftable(jointdhsfarming.table))
  
  jointdhsfarm <- jointdhsfarm[(order(jointdhsfarm$RURAL, jointdhsfarm$HHSIZECAT, jointdhsfarm$GENDERHEAD, jointdhsfarm$AGECATHEAD, jointdhsfarm$EDUCATHEAD, jointdhsfarm$WEALTH, jointdhsfarm$FARMING)),]
  
  
  # fill in the gaps: 
  
  jointheadwealth <- copy(jointdhswealth)
  
  jointheadwealth <- jointheadwealth %>% rename(Frequency = Freq)
  
  jointheadwealth$RURAL <- as.integer(jointheadwealth$RURAL)-1
  jointheadwealth$HHSIZECAT <- as.integer(jointheadwealth$HHSIZECAT)
  jointheadwealth$HHTYPE <- as.integer(jointheadwealth$HHTYPE)
  jointheadwealth$GENDERHEAD <- as.integer(jointheadwealth$GENDERHEAD)-1
  jointheadwealth$AGECATHEAD <- as.integer(jointheadwealth$AGECATHEAD)
  jointheadwealth$EDUCATHEAD <- as.integer(jointheadwealth$EDUCATHEAD)
  jointheadwealth$WEALTH <- as.integer(jointheadwealth$WEALTH)
  
  
  jointheadfarm <- copy(jointdhsfarm)
  
  jointheadfarm <- jointheadfarm %>% rename(Frequency = Freq)
  
  jointheadfarm$RURAL <- as.integer(jointheadfarm$RURAL)-1
  jointheadfarm$HHSIZECAT <- as.integer(jointheadfarm$HHSIZECAT)
  jointheadfarm$GENDERHEAD <- as.integer(jointheadfarm$GENDERHEAD)-1
  jointheadfarm$AGECATHEAD <- as.integer(jointheadfarm$AGECATHEAD)
  jointheadfarm$EDUCATHEAD <- as.integer(jointheadfarm$EDUCATHEAD)
  jointheadfarm$WEALTH <- as.integer(jointheadfarm$WEALTH)
  jointheadfarm$FARMING <- as.integer(jointheadfarm$FARMING)-1
  
  
  
  jointhead1 <- as.data.frame(jointheadwealth)
  print(nrow(jointhead1))
  
  
  
  
  
  # stond onder fill zero cells function. 
  
  jointhead1 <- fill_zero_cells(jointhead1)
  print(nrow(jointhead1))
  
  
  jointhead1_sumwealth <- jointhead1 %>% group_by(RURAL, HHSIZECAT, HHTYPE, GENDERHEAD, AGECATHEAD, EDUCATHEAD) %>% summarise(sum_imp = sum(imputations))
  
  jointhead1 <- left_join(jointhead1, jointhead1_sumwealth, by = c('RURAL', 'HHSIZECAT', 'HHTYPE', 'GENDERHEAD', 'AGECATHEAD', 'EDUCATHEAD'))
  
  jointhead1$Probs <- jointhead1$imputations / jointhead1$sum_imp
  
  jointhead1 <- jointhead1 %>% select(-imputations, -sum_imp, -Frequency)
  
  jointhead1_wide <- pivot_wider(jointhead1, names_from = WEALTH, values_from = Probs) 
  
  jointhead1_wide <- jointhead1_wide[(order(jointhead1_wide$RURAL, jointhead1_wide$HHSIZECAT, jointhead1_wide$HHTYPE, jointhead1_wide$GENDERHEAD, jointhead1_wide$AGECATHEAD, jointhead1_wide$EDUCATHEAD)),]
  
  
  # 
  # jointdist ipums
  pumshead <- pumsdf[pumsdf$RELATE == 1,]
  
  #jointipums.table <- xtabs( ~ RURAL + HHSIZECAT + HHTYPE + GENDERHEAD + AGECATHEAD + EDUCATHEAD, data = pumshead)
  
  #jointdist_pums <- as.data.frame(ftable(jointipums.table))
  
  #jointdist_pums <- jointdist_pums[(order(jointdist_pums$RURAL, jointdist_pums$HHSIZECAT, jointdist_pums$HHTYPE, jointdist_pums$GENDERHEAD, jointdist_pums$AGECATHEAD, jointdist_pums$EDUCATHEAD)),]
  
  pumssmall <- pumshead %>% select(RURAL, HHSIZECAT, HHTYPE, GENDERHEAD, AGECATHEAD, EDUCATHEAD)
  
  
  pumssmall$WEIGHTS <- 1
  pumssmall_weights <- pumssmall %>% group_by(RURAL, HHSIZECAT, HHTYPE, GENDERHEAD, AGECATHEAD, EDUCATHEAD) %>% summarise(WEIGHTS = sum(WEIGHTS))
  
  
  
  pumssmall_weights <- as.data.frame(pumssmall_weights)
  jointhead1_wide <- as.data.frame(jointhead1_wide)
  pumssmall_weights <- left_join(pumssmall_weights, jointhead1_wide, by = c('RURAL', 'HHSIZECAT', 'HHTYPE', 'GENDERHEAD', 'AGECATHEAD', 'EDUCATHEAD'))
  
  pumssmall_weights <- pumssmall_weights %>% rename('WEALTH1' = '1')
  pumssmall_weights <- pumssmall_weights %>% rename('WEALTH2' = '2')
  pumssmall_weights <- pumssmall_weights %>% rename('WEALTH3' = '3')
  pumssmall_weights <- pumssmall_weights %>% rename('WEALTH4' = '4')
  pumssmall_weights <- pumssmall_weights %>% rename('WEALTH5' = '5')
  
  pumssmall_weights$sum_wealth <- pumssmall_weights$WEALTH1 + pumssmall_weights$WEALTH2 + pumssmall_weights$WEALTH3 + pumssmall_weights$WEALTH4 + pumssmall_weights$WEALTH5
  
  pumssmall_weights[is.na(pumssmall_weights)] <- 0.2
  
  sum(pumssmall_weights$sum_wealth)
  
  
  pumssmall_weights$WEALTH1 <- pumssmall_weights$WEALTH1 * pumssmall_weights$WEIGHTS
  pumssmall_weights$WEALTH2 <- pumssmall_weights$WEALTH2 * pumssmall_weights$WEIGHTS
  pumssmall_weights$WEALTH3 <- pumssmall_weights$WEALTH3 * pumssmall_weights$WEIGHTS
  pumssmall_weights$WEALTH4 <- pumssmall_weights$WEALTH4 * pumssmall_weights$WEIGHTS
  pumssmall_weights$WEALTH5 <- pumssmall_weights$WEALTH5 * pumssmall_weights$WEIGHTS
  
  
  
  pumssmall_weights_long <- pivot_longer(pumssmall_weights, cols = c(WEALTH1, WEALTH2, WEALTH3, WEALTH4, WEALTH5), names_to = "WEALTH", values_to = "Frequency")
  pumssmall_weights_long$WEALTH <- as.integer(as.factor(pumssmall_weights_long$WEALTH))
  
  
  filename_wealth <- paste0("ipumswealth_aug23_", isocode, ".csv")
  
  write.csv(pumssmall_weights_long, filename_wealth, sep = ';', row.names = FALSE, quote = FALSE)
  
  
  
  
  # FARMING
  
  
  jointhead1 <- as.data.frame(jointheadfarm)
  
  jointhead1 <- fill_zero_cells(jointhead1)
  
  
  #jointhead1 <- copy(jointheadfarm)
  #jointhead1$imputations <- 1
  
  
  jointhead1_sumwealth <- jointhead1 %>% group_by(RURAL, HHSIZECAT, GENDERHEAD, AGECATHEAD, EDUCATHEAD, WEALTH) %>% summarise(sum_imp = sum(imputations))
  
  jointhead1 <- left_join(jointhead1, jointhead1_sumwealth, by = c('RURAL', 'HHSIZECAT', 'GENDERHEAD', 'AGECATHEAD', 'EDUCATHEAD','WEALTH'))
  
  jointhead1$Probs <- jointhead1$imputations / jointhead1$sum_imp
  
  jointhead1 <- jointhead1 %>% select(-imputations, -sum_imp, -Frequency)
  
  jointhead1_wide <- pivot_wider(jointhead1, names_from = FARMING, values_from = Probs) 
  
  print('farming')
  print(names(jointhead1_wide))
  print(table(jointhead1$FARMING))
  
  
  jointhead1_wide <- jointhead1_wide[(order(jointhead1_wide$RURAL, jointhead1_wide$HHSIZECAT, jointhead1_wide$GENDERHEAD, jointhead1_wide$AGECATHEAD, jointhead1_wide$EDUCATHEAD, jointhead1_wide$WEALTH)),]
  
  
  # 
  # jointdist ipums
  #pumshead <- pumsdf[pumsdf$RELATE == 1,]
  
  #jointipums.table <- xtabs( ~ RURAL + HHSIZECAT + HHTYPE + GENDERHEAD + AGECATHEAD + EDUCATHEAD, data = pumshead)
  
  #jointdist_pums <- as.data.frame(ftable(jointipums.table))
  
  #jointdist_pums <- jointdist_pums[(order(jointdist_pums$RURAL, jointdist_pums$HHSIZECAT, jointdist_pums$HHTYPE, jointdist_pums$GENDERHEAD, jointdist_pums$AGECATHEAD, jointdist_pums$EDUCATHEAD)),]
  
  #pumssmall <- pumshead %>% select(RURAL, HHSIZECAT, HHTYPE, GENDERHEAD, AGECATHEAD, EDUCATHEAD)
  
  
  #pumssmall$WEIGHTS <- 1
  #pumssmall_weights <- pumssmall %>% group_by(RURAL, HHSIZECAT, HHTYPE, GENDERHEAD, AGECATHEAD, EDUCATHEAD) %>% summarise(WEIGHTS = sum(WEIGHTS))
  
  
  pumssmall_weights_long <- read.csv(paste0("ipumswealth_aug23_", isocode, ".csv"))
  
  #pumssmall_weights_long <- read.csv("ipumswealth_aug23_Uganda.csv")
  
  pumssmall_weights <- copy(pumssmall_weights_long)
  
  
  pumssmall_weights <- as.data.frame(pumssmall_weights)
  jointhead1_wide <- as.data.frame(jointhead1_wide)
  pumssmall_weights <- left_join(pumssmall_weights, jointhead1_wide, by = c('RURAL', 'HHSIZECAT', 'GENDERHEAD', 'AGECATHEAD', 'EDUCATHEAD', 'WEALTH'))
  
  pumssmall_weights <- pumssmall_weights %>% rename('FARMING0' = '0')
  pumssmall_weights <- pumssmall_weights %>% rename('FARMING1' = '1')
  
  
  pumssmall_weights$sum_wealth <- pumssmall_weights$FARMING0 + pumssmall_weights$FARMING1
  
  pumssmall_weights[is.na(pumssmall_weights)] <- 0.5
  
  sum(pumssmall_weights$sum_wealth)
  
  
  pumssmall_weights$FARMING0 <- pumssmall_weights$FARMING0 * pumssmall_weights$Frequency
  pumssmall_weights$FARMING1 <- pumssmall_weights$FARMING1 * pumssmall_weights$Frequency
  
  
  pumssmall_weights <- pumssmall_weights %>% select(-Frequency)
  
  pumssmall_weights_long <- pivot_longer(pumssmall_weights, cols = c(FARMING0, FARMING1), names_to = "FARMING", values_to = "Frequency")
  pumssmall_weights_long$FARMING <- as.integer(as.factor(pumssmall_weights_long$FARMING))
  
  
  filename_farming <- paste0("ipumswealthfarm_aug23_", isocode, ".csv")
  
  write.csv(pumssmall_weights_long, filename_farming, row.names = FALSE, quote = FALSE)

}
    
#################################################### 
############### TEST MERGE TO IPUMS ################

if (joindhswithipums == 'yes'){
  join_DHS_with_IPUMS()
}


filename_farming <- paste0("ipumswealthfarm_aug23_", isocode, ".csv")

ipumswealth <- read.csv(filename_farming)
ipumswealth <- ipumswealth %>% select(-sum_wealth)

wealthfarmingdf <- data.frame()


ipumswealth <- ipumswealth %>% rename(EDUCAT = EDUCATHEAD, AGECAT = AGECATHEAD, GENDER = GENDERHEAD)

pumsdf <- pumsdf %>% select(SERIAL, HHTYPE, HHSIZECAT, RURAL, GENDER, AGECAT, EDUCAT, RELATE, GEOLEV1, HHWT, PERWT)
pumsdf$HID <- as.integer(as.factor(pumsdf$SERIAL)) #dit gefixt. 

pumsdf <- pumsdf %>% select(-SERIAL)

pumshead <- pumsdf[pumsdf$RELATE == 1,]


for (i in 1:(nrow(ipumswealth)/10)){
  row = (i*10)-9
  ipumswealthrow <- ipumswealth[row,]
  
  
  pumsdfselect <- pumshead[(pumshead$RURAL == ipumswealthrow$RURAL & pumshead$HHSIZECAT == ipumswealthrow$HHSIZECAT & pumshead$HHTYPE == ipumswealthrow$HHTYPE & pumshead$GENDER == ipumswealthrow$GENDER & pumshead$AGECAT == ipumswealthrow$AGECAT & pumshead$EDUCAT == ipumswealthrow$EDUCAT),]
  ipumswealthselect <- ipumswealth[(ipumswealth$RURAL == ipumswealthrow$RURAL & ipumswealth$HHSIZECAT == ipumswealthrow$HHSIZECAT & ipumswealth$HHTYPE == ipumswealthrow$HHTYPE & ipumswealth$GENDER == ipumswealthrow$GENDER & ipumswealth$AGECAT == ipumswealthrow$AGECAT & ipumswealth$EDUCAT == ipumswealthrow$EDUCAT),]
  
  ipumswealthselect <- trs_frequency(ipumswealthselect)
  
  ipumswealthselect <- ipumswealthselect %>% filter(Truncated > 0)
  ipumswealthselect <- ipumswealthselect %>% uncount(Truncated)
  
  pumsdfselect <- pumsdfselect[sample(nrow(pumsdfselect)), ]
  
  forloopdf <- data.frame(pumsdfselect$HID, ipumswealthselect$WEALTH, ipumswealthselect$FARMING)
  
  colnames(forloopdf) <- c("HID", "WEALTH", "FARMING")
  
  wealthfarmingdf <- rbind(wealthfarmingdf, forloopdf)
  
  if (i %% 100 == 0) {
    print(i)
  }
  
}


reference_sample <- left_join(pumsdf, wealthfarmingdf, by = 'HID')
reference_sample$FARMING <- reference_sample$FARMING -1




reference_sample$PID <- 1:nrow(reference_sample)



reference_sample <- reference_sample %>% select(-GEOLEV1, -HHWT, -PERWT)




########################################################################
##################### TEST SYNTHESIS COUNTRY LEVEL #####################
targets <- targets_PUMS(pums, "marg")



rural_tib <- targets$RURAL
hhtype_tib <- targets$HHTYPE
hhsize_tib <- targets$HHSIZECAT
age_tib <- targets$AGECAT
edu_tib <- targets$EDUCAT
sex_tib <- targets$GENDER


ruralcountrydf <- as.data.frame(rural_tib) %>% group_by(RURAL) %>% summarize(Freq = sum(Freq))
ruralcountry_tib <- as_tibble(ruralcountrydf)

hhtypecountrydf <- as.data.frame(hhtype_tib) %>% group_by(HHTYPE) %>% summarize(Freq = sum(Freq))
hhtypecountry_tib <- as_tibble(hhtypecountrydf)

hhsizecountrydf <- as.data.frame(hhsize_tib) %>% group_by(HHSIZECAT) %>% summarize(Freq = sum(Freq))
hhsizecountry_tib <- as_tibble(hhsizecountrydf)

agecountrydf <- as.data.frame(age_tib) %>% group_by(AGECAT) %>% summarize(Freq = sum(Freq))
agecountry_tib <- as_tibble(agecountrydf)

educountrydf <- as.data.frame(edu_tib) %>% group_by(EDUCAT) %>% summarize(Freq = sum(Freq))
educountry_tib <- as_tibble(educountrydf)

sexcountrydf <- as.data.frame(sex_tib) %>% group_by(GENDER) %>% summarize(Freq = sum(Freq))
sexcountry_tib <- as_tibble(sexcountrydf)

#hier klopt rural wel want op household niveau. 
sumfreq5 <- (sum(ruralcountry_tib$Freq))/5
incomecountrymargdf <- data.frame('WEALTH' = c(1,2,3,4,5), 'Freq' = c(sumfreq5, sumfreq5, sumfreq5, sumfreq5, sumfreq5))
incomecountrymargdf$WEALTH <- as.factor(incomecountrymargdf$WEALTH)

incomecountry_tib <- as_tibble(incomecountrymargdf)


group_control <- list(ruralcountry_tib, hhtypecountry_tib, hhsizecountry_tib, incomecountry_tib)
individual_control <- list(agecountry_tib, educountry_tib, sexcountry_tib)

names(group_control) <- c('RURAL','HHTYPE', 'HHSIZECAT', 'WEALTH') 
names(individual_control) <- c('AGECAT','EDUCAT', 'GENDER')


ml_fitWEIGHTS <- numeric(nrow(reference_sample)) + 1

fitting_problem <- ml_problem(
  ref_sample = reference_sample, 
  controls = list(
    individual = individual_control,
    group = group_control
  ), prior_weights = ml_fitWEIGHTS,
  field_names = special_field_names(
    groupId = "HID", 
    individualId = "PID", 
    count = "Freq"
  )
)

fitcountry <- ml_fit(ml_problem = fitting_problem, algorithm = "ipu", maxiter = 500) #was 500

syn_pop_country <- ml_replicate(fitcountry, algorithm = "trs")

popsize_syn <- nrow(syn_pop_country)

GDLnational <- GDLcountry[GDLcountry$level == 'National',]

popsize_GDL <- as.numeric(GDLnational$pop) * 1000 

mp_syn_GDL <- popsize_GDL/popsize_syn

maxHID <- max(syn_pop_country$HID)

if (mp_syn_GDL > 1.001){
  extraHID <- sample(1:maxHID, round(maxHID*(mp_syn_GDL-1)), replace = TRUE)
  
  extraHIDdt <- syn_pop_country[syn_pop_country$HID %in% extraHID,]
  extraHIDdt$HID <- extraHIDdt$HID + maxHID
  
  syn_pop_country <- bind_rows(syn_pop_country, extraHIDdt)
  
  popsize_syn_new <- nrow(syn_pop_country)
  
} 

if (mp_syn_GDL < 0.999){
  cancelHID <- sample(1:maxHID, round(maxHID*(1-mp_syn_GDL)))
  
  syn_pop_country <- syn_pop_country[syn_pop_country$HID %ni% cancelHID,]
}

print(nrow(syn_pop_country))


# Save synthetic population


DHSdata <- copy(reference_sample)
DHSdata$Frequency <- fitcountry$weights

DHSdatahead <- DHSdata[DHSdata$RELATE == 1,]

DHSdatahead <- trs_frequency(DHSdatahead)


DHSdata <- left_join(DHSdata, DHSdatahead %>% select(HID, Truncated), by = 'HID')

DHSdata <- DHSdata %>% select(-Frequency)

DHSdata <- DHSdata %>% rename(WEIGHT = Truncated)

DHSdata$HHSIZECAT <- as.integer(DHSdata$HHSIZECAT)
DHSdata$RURAL <- as.integer(DHSdata$RURAL)
DHSdata$GENDER <- as.integer(DHSdata$GENDER)
DHSdata$AGECAT <- as.integer(DHSdata$AGECAT)
DHSdata$EDUCAT <- as.integer(DHSdata$EDUCAT)
DHSdata$FARMING <- as.integer(DHSdata$FARMING)
DHSdata$WEIGHT <- as.integer(DHSdata$WEIGHT)
DHSdata$WEALTH <- as.integer(DHSdata$WEALTH)
DHSdata$HID <- as.integer(DHSdata$HID)
DHSdata$RELATE <- as.integer(DHSdata$RELATE)
DHSdata$HHTYPE <- as.integer(DHSdata$HHTYPE)

name = paste0(countryname, '_DHS_oct23_synth_country.dat')

#con = file(name, "wb")

#writeBin(c(DHSdata$WEALTH, DHSdata$RURAL, DHSdata$FARMING, DHSdata$AGECAT, DHSdata$GENDER, 
#           DHSdata$EDUCAT, DHSdata$HHTYPE, DHSdata$HID, DHSdata$RELATE, DHSdata$HHSIZECAT, DHSdata$WEIGHT), con)

#writeBin(c(syn_pop$INCOME, syn_pop$PID), con)


#close(con)



# save nr of individuals: 
#length_DHS_data <- data.frame('length' = nrow(DHSdata))

#filename_DHS_length <- paste0('length_DHSdata_', countryname, '_oct23.csv')

#write.csv(length_DHS_data, filename_DHS_length, row.names = FALSE)








weightscountry <- fitcountry$weights
weightscountry <- weightscountry[weightscountry > 0]



###############################################################
######## CHANGE IPUMS REGIONS IN MARGINALS TO GDL REGIONS


marginals_IPUMS_to_GDL <- function(GDLPUMSmatch_sel, hhtype_tib){
  
  hhtype_tib_GDL <- left_join(GDLPUMSmatch_sel, hhtype_tib, by = 'ZONE', multiple = 'all')
  
  hhtype_tib_GDL['GDLPUMSfactor'] <- hhtype_tib_GDL['pop']/hhtype_tib_GDL['sum_pop_IPUMS']
  
  hhtype_tib_GDL['Freq'] <- hhtype_tib_GDL['Freq'] * hhtype_tib_GDL['GDLPUMSfactor']
  
  hhtype_tib_GDL <- hhtype_tib_GDL %>% select(-GDLPUMSfactor, -sum_pop_IPUMS, -ZONE, -pop)
  
  hhtype_tib_GDL <- hhtype_tib_GDL %>% rename(ZONE = GDLCODEnr)
  
  return(hhtype_tib_GDL)
  
}



if (countryname %ni% c('Benin', 'Guinea', 'Nepal', 'Uganda', 'Philippines')){

  GDL_population <- read.csv('GDL_subnational_population_world.csv', head = TRUE, sep = ',')
  
  
  GDL_pop2015 <- GDL_population[GDL_population$year == 2015,]
  
  GDLpopcountry <- GDL_pop2015[GDL_pop2015$country == countryname,]
  
  GDLpopcountry$GDLCODEnr <- substr(GDLpopcountry$GDLCODE, nchar(GDLpopcountry$GDLCODE) - 2, nchar(GDLpopcountry$GDLCODE))
  
  
  
  GDLPUMSmatch <- read.csv('GDL_IPUMS_match.csv', sep = ';')
  
  #GDLPUMSmatch <- GDLPUMSmatch[GDLPUMSmatch$Country == countryname,]
  
  #GDLPUMSmatch <- left_join(GDLPUMSmatch, GDLcountry %>% select(GDLCODE, pop), by = 'GDLCODE')
  
  GDLPUMSmatch <- GDLPUMSmatch %>% select(Country, ISO_Code, GDLCODE, Region, GEOLEV1_IPUMS, Label)
  
  GDLPUMSmatch$GDLCODEnr <- substr(GDLPUMSmatch$GDLCODE, nchar(GDLPUMSmatch$GDLCODE) - 2, nchar(GDLPUMSmatch$GDLCODE))
  
  GDLPUMSmatch_sel <- GDLPUMSmatch[GDLPUMSmatch['Country'] == countryname,]
  GDLPUMSmatch_sel <- GDLPUMSmatch_sel %>% select(GEOLEV1_IPUMS, GDLCODEnr)
  GDLPUMSmatch_sel <- GDLPUMSmatch_sel %>% rename(ZONE = GEOLEV1_IPUMS)
  
  GDLPUMSmatch_sel <- left_join(GDLPUMSmatch_sel, GDLpopcountry %>% select(GDLCODEnr, pop), by = 'GDLCODEnr')
  
  sum_popIPUMS <- GDLPUMSmatch_sel %>% group_by(ZONE) %>% summarise(sum_pop_IPUMS = sum(pop))
  GDLPUMSmatch_sel <- left_join(GDLPUMSmatch_sel, sum_popIPUMS, by = 'ZONE')
  
  
  hhtype_tib <- marginals_IPUMS_to_GDL(GDLPUMSmatch_sel, hhtype_tib)
  hhsize_tib <- marginals_IPUMS_to_GDL(GDLPUMSmatch_sel, hhsize_tib)
  age_tib <- marginals_IPUMS_to_GDL(GDLPUMSmatch_sel, age_tib)
  sex_tib <- marginals_IPUMS_to_GDL(GDLPUMSmatch_sel, sex_tib)
  edu_tib <- marginals_IPUMS_to_GDL(GDLPUMSmatch_sel, edu_tib)

}  


if (countryname %in% c('Benin', 'Guinea', 'Nepal', 'Uganda', 'Philippines')){
  
  GDL_population <- read.csv('GDL_subnational_population_world.csv', head = TRUE, sep = ',')
  
  
  GDL_pop2015 <- GDL_population[GDL_population$year == 2015,]
  
  GDLpopcountry <- GDL_pop2015[GDL_pop2015$country == countryname,]
  
  GDLpopcountry <- GDLpopcountry[GDLpopcountry$level == 'Subnat',]
  
  GDLpopcountry$GDLCODEnr <- substr(GDLpopcountry$GDLCODE, nchar(GDLpopcountry$GDLCODE) - 2, nchar(GDLpopcountry$GDLCODE))
  
  GDLPUMSmatch_sel <- copy(GDLpopcountry)
  
  GDLPUMSmatch_sel$ZONE <- as.numeric(GDLPUMSmatch_sel$GDLCODEnr)
  
}  
##############################################################
################### RURAL MARGINAL SMOD ######################

SMODmarg <- read.csv('individual_marginals_UrbanRural.csv', sep = ';', header = TRUE)
GDL_pop <- read.csv('GDL_subnational_population_world.csv', head = TRUE, sep = ',')


  
SMODmarg <- SMODmarg %>% rename('GDLCODE' = 'GDLcode')

SMODmarg_country <- SMODmarg[SMODmarg$country == countryname,]

SMODmarg_country['ZONE'] <- substr(SMODmarg_country$GDLCODE, nchar(SMODmarg_country$GDLCODE) - 2, nchar(SMODmarg_country$GDLCODE)) 
SMODmarg_country$ZONE <- as.numeric(SMODmarg_country$ZONE)

ruralmargSMOD0 <- SMODmarg_country %>% select(ZONE, Urban_percent)
ruralmargSMOD1 <- SMODmarg_country %>% select(ZONE, Rural_percent)

sum_indi <- sex_tib %>% group_by(ZONE) %>% summarise(Population = sum(Freq))

sum_indi$ZONE <- as.numeric(sum_indi$ZONE)

ruralmargSMOD0 <- left_join(ruralmargSMOD0, sum_indi, by = 'ZONE')
ruralmargSMOD0['New_freq'] <- ruralmargSMOD0$Urban_percent * ruralmargSMOD0$Population

ruralmargSMOD1 <- left_join(ruralmargSMOD1, sum_indi, by = 'ZONE')
ruralmargSMOD1['New_freq'] <- ruralmargSMOD1$Rural_percent * ruralmargSMOD1$Population

ruralmargSMOD0$RURAL <- 0
ruralmargSMOD1$RURAL <- 1

ruralmargSMOD0 <- ruralmargSMOD0 %>% select(ZONE, RURAL, New_freq)
ruralmargSMOD1 <- ruralmargSMOD1 %>% select(ZONE, RURAL, New_freq)

ruralmargSMOD <- rbind.data.frame(ruralmargSMOD0, ruralmargSMOD1)

ruralmargSMOD <- ruralmargSMOD %>% rename('Freq' = 'New_freq')

ruralmargSMOD$ZONE <- as.numeric(ruralmargSMOD$ZONE)
  
rural_tib <- as_tibble(ruralmargSMOD)




GEOLEV1regions <- unique(rural_tib$ZONE)



nr_individuals_per_regio = c()

syn_pop <- data.table()

ml_fitWEIGHTS <- fitcountry$weights

for (regnr in GEOLEV1regions) {
  
  print(regnr)
  
  rural_tib1reg <- rural_tib[rural_tib$ZONE == regnr,]
  rural_tib1reg <- rural_tib1reg %>% select(-ZONE)
  
  print('rural')
  print(rural_tib1reg)
  
  hhtype_tib1reg <- hhtype_tib[hhtype_tib$ZONE == regnr,]
  hhtype_tib1reg <- hhtype_tib1reg %>% select(-ZONE)
  
  print('hhtype')
  print(hhtype_tib1reg)
  
  hhsize_tib1reg <- hhsize_tib[hhsize_tib$ZONE == regnr,]
  hhsize_tib1reg <- hhsize_tib1reg %>% select(-ZONE)
  
  print('hhsize')
  print(hhsize_tib1reg)
  
  age_tib1reg <- age_tib[age_tib$ZONE == regnr,]
  age_tib1reg <- age_tib1reg %>% select(-ZONE)
  
  print('age')
  print(age_tib1reg)
  
  edu_tib1reg <- edu_tib[edu_tib$ZONE == regnr,]
  edu_tib1reg <- edu_tib1reg %>% select(-ZONE)
  
  print('edu')
  print(edu_tib1reg)
  
  sex_tib1reg <- sex_tib[sex_tib$ZONE == regnr,]
  sex_tib1reg <- sex_tib1reg %>% select(-ZONE)
  
  print('gender')
  print(sex_tib1reg)
  
  group_control <- list(hhtype_tib1reg, hhsize_tib1reg)
  individual_control <- list(rural_tib1reg, age_tib1reg, edu_tib1reg, sex_tib1reg)
  
  
  names(group_control) <- c('HHTYPE', 'HHSIZECAT') 
  names(individual_control) <- c('RURAL','AGECAT','EDUCAT', 'GENDER')
  
  
  fitting_problem <- ml_problem(
    ref_sample = reference_sample, 
    controls = list(
      individual = individual_control,
      group = group_control
    ), prior_weights = ml_fitWEIGHTS,
    field_names = special_field_names(
      groupId = "HID", 
      individualId = "PID", 
      count = "Freq"
    )
  )
  
  fit <- ml_fit(ml_problem = fitting_problem, algorithm = "ipu", maxiter = 50) #was 50
  
  syn_pop_reg <- ml_replicate(fit, algorithm = "trs")
  
  
  print(nrow(syn_pop_reg))
  
  nr_individuals_per_regio <- append(nr_individuals_per_regio, nrow(syn_pop_reg))
  
  syn_pop_reg <- syn_pop_reg %>% add_column(GEOLEV1 = as.integer(regnr))
  gc()
  syn_pop <- rbindlist(list(syn_pop, syn_pop_reg), use.names=TRUE)
  rm(syn_pop_reg)
  
  
  gc()
    
}



synpophead <- syn_pop[syn_pop$RELATE == 1,]
tableinc <- table(synpophead$WEALTH)
tableinc / sum(tableinc)


# per region, calculate percentages of wealth groups
wealthfreq_regio <- data.frame(xtabs(~WEALTH + GEOLEV1, data = synpophead))

mp_w1 <- 1-((tableinc[1]/sum(tableinc))-0.2)/(tableinc[1]/sum(tableinc))
mp_w2 <- 1-((tableinc[2]/sum(tableinc))-0.2)/(tableinc[2]/sum(tableinc))
mp_w3 <- 1-((tableinc[3]/sum(tableinc))-0.2)/(tableinc[3]/sum(tableinc))
mp_w4 <- 1-((tableinc[4]/sum(tableinc))-0.2)/(tableinc[4]/sum(tableinc))
mp_w5 <- 1-((tableinc[5]/sum(tableinc))-0.2)/(tableinc[5]/sum(tableinc))



mp_df <- data.frame('WEALTH' = c(1,2,3,4,5), 'mp' = c(mp_w1,mp_w2,mp_w3,mp_w4,mp_w5))

wealthfreq_regio <- merge(wealthfreq_regio, mp_df, on = 'WEALTH', how = 'left')
wealthfreq_regio['Freq'] <- wealthfreq_regio['Freq']*wealthfreq_regio['mp']
wealthfreq_regio['ZONE'] <- wealthfreq_regio['GEOLEV1']

wealth_tib <- as_tibble(wealthfreq_regio %>% select(ZONE, WEALTH, Freq))


# for loop with regions. Now with wealth marginal.  
nr_individuals_per_regio = c()

syn_pop <- data.table()


for (regnr in GEOLEV1regions) {
  
  print(regnr)
  wealth_tib1reg <- wealth_tib[wealth_tib$ZONE == regnr,]
  wealth_tib1reg <- wealth_tib1reg %>% select(-ZONE)
  
  rural_tib1reg <- rural_tib[rural_tib$ZONE == regnr,]
  rural_tib1reg <- rural_tib1reg %>% select(-ZONE)
  
  hhsize_tib1reg <- hhsize_tib[hhsize_tib$ZONE == regnr,]
  hhsize_tib1reg <- hhsize_tib1reg %>% select(-ZONE)
  
  #correction wealth
  hhsizesumfreq <- sum(hhsize_tib1reg$Freq)
  wealthsumfreq <- sum(wealth_tib1reg$Freq)
  
  correction_wealth <- hhsizesumfreq/wealthsumfreq
  
  wealth_tib1reg$Freq <- wealth_tib1reg$Freq*correction_wealth
  
  hhtype_tib1reg <- hhtype_tib[hhtype_tib$ZONE == regnr,]
  hhtype_tib1reg <- hhtype_tib1reg %>% select(-ZONE)
  
  
  age_tib1reg <- age_tib[age_tib$ZONE == regnr,]
  age_tib1reg <- age_tib1reg %>% select(-ZONE)
  
  edu_tib1reg <- edu_tib[edu_tib$ZONE == regnr,]
  edu_tib1reg <- edu_tib1reg %>% select(-ZONE)
  
  sex_tib1reg <- sex_tib[sex_tib$ZONE == regnr,]
  sex_tib1reg <- sex_tib1reg %>% select(-ZONE)
  
  group_control <- list(wealth_tib1reg, hhtype_tib1reg, hhsize_tib1reg)
  individual_control <- list(rural_tib1reg, age_tib1reg, edu_tib1reg, sex_tib1reg)
  
  names(group_control) <- c('WEALTH', 'HHTYPE', 'HHSIZECAT') 
  names(individual_control) <- c('RURAL','AGECAT','EDUCAT', 'GENDER')
  
    
  ml_fitWEIGHTS <- fitcountry$weights
  
  fitting_problem <- ml_problem(
    ref_sample = reference_sample, 
    controls = list(
      individual = individual_control,
      group = group_control
    ), prior_weights = ml_fitWEIGHTS,
    field_names = special_field_names(
      groupId = "HID", 
      individualId = "PID", 
      count = "Freq"
    )
  )
  
  fit <- ml_fit(ml_problem = fitting_problem, algorithm = "ipu", maxiter = 50)

  
  syn_pop_reg <- ml_replicate(fit, algorithm = "trs")
  
  popsize_syn <- nrow(syn_pop_reg)
  
  if (countryname %ni% c('Bangladesh', 'Egypt', 'Ethiopia', 'Indonesia', 'Cambodia', 'Liberia', 'Senegal', 'Tanzania', 'Zambia')){
    
    GDL_region <- GDLPUMSmatch_sel[GDLPUMSmatch_sel$GDLCODEnr == regnr,]
    
    
  } else if (countryname %in% c('Bangladesh', 'Egypt', 'Ethiopia', 'Indonesia', 'Cambodia', 'Liberia', 'Senegal', 'Tanzania', 'Zambia')){
    
    GDL_region <- GDLPUMSmatch_sel[GDLPUMSmatch_sel$GDLCODEnr == regnr,]
    
  }
  
  popsize_GDL <- as.double(GDL_region$pop) * 1000
  
  mp_syn_GDL <- popsize_GDL/popsize_syn
  
  maxHID <- max(syn_pop_reg$HID)
  
  if (mp_syn_GDL > 1.001){
    extraHID <- sample(1:maxHID, round(maxHID*(mp_syn_GDL-1)), replace = TRUE)
    
    extraHIDdt <- syn_pop_reg[syn_pop_reg$HID %in% extraHID,]
    extraHIDdt$HID <- extraHIDdt$HID + maxHID
    
    syn_pop_reg <- bind_rows(syn_pop_reg, extraHIDdt)
    
    popsize_syn_new <- nrow(syn_pop_reg)
    
  } 
  
  if (mp_syn_GDL < 0.999){
    cancelHID <- sample(1:maxHID, round(maxHID*(1-mp_syn_GDL)))
    
    syn_pop_reg <- syn_pop_reg[syn_pop_reg$HID %ni% cancelHID,]
  }
  
  print(nrow(syn_pop_reg))
  
  nr_individuals_per_regio <- append(nr_individuals_per_regio, nrow(syn_pop_reg))
  
  
  
  # Save synthetic population
  
  
  syn_pop_reg$HHSIZECAT <- as.integer(syn_pop_reg$HHSIZECAT)
  syn_pop_reg$RURAL <- as.integer(syn_pop_reg$RURAL)
  syn_pop_reg$GENDER <- as.integer(syn_pop_reg$GENDER)
  syn_pop_reg$AGECAT <- as.integer(syn_pop_reg$AGECAT)
  syn_pop_reg$EDUCAT <- as.integer(syn_pop_reg$EDUCAT)
  syn_pop_reg$FARMING <- as.integer(syn_pop_reg$FARMING)
  syn_pop_reg$WEALTH <- as.integer(syn_pop_reg$WEALTH)
  syn_pop_reg$HID <- as.integer(syn_pop_reg$HID)
  syn_pop_reg$RELATE <- as.integer(syn_pop_reg$RELATE)
  syn_pop_reg$HHTYPE <- as.integer(syn_pop_reg$HHTYPE)
  
  syn_pop_reg <- syn_pop_reg %>% add_column(GEOLEV1 = as.integer(regnr))
  
  
  name = paste0(isocode, '_DHS_oct23_synthpop_', isocode, 'r',  as.character(regnr), '.dat')
  
  con = file(name, "wb")
  
  writeBin(c(syn_pop_reg$WEALTH, syn_pop_reg$RURAL, syn_pop_reg$FARMING, syn_pop_reg$AGECAT, syn_pop_reg$GENDER, 
             syn_pop_reg$EDUCAT, syn_pop_reg$HHTYPE, syn_pop_reg$HID, syn_pop_reg$RELATE, syn_pop_reg$HHSIZECAT), con)
  
  
  close(con)
  
   
  gc()
  syn_pop <- rbindlist(list(syn_pop, syn_pop_reg), use.names=TRUE)
  rm(syn_pop_reg)
  
  
  gc()
    
  
}



df_nr_individuals_per_region <- data.frame(GDLCODEnr = GEOLEV1regions, Nr_individuals = nr_individuals_per_regio)
DF_nr_individuals_per_region <- bind_rows(DF_nr_individuals_per_region, df_nr_individuals_per_region)



if (countryname %ni% c('Benin', 'Guinea', 'Nepal', 'Uganda', 'Philippines')){
  
  GDLPUMSmatch_sel$GDLCODEnr <- as.numeric(GDLPUMSmatch_sel$GDLCODEnr)
  DF_nr_individuals_per_region <- left_join(DF_nr_individuals_per_region, GDLPUMSmatch_sel %>% select(ZONE, GDLCODEnr), by = 'GDLCODEnr')
  DF_nr_individuals_per_region_PUMS <- DF_nr_individuals_per_region %>% group_by(ZONE) %>% summarise(Nr_individuals = sum(Nr_individuals))
  nr_individuals_per_regio <- DF_nr_individuals_per_region_PUMS$Nr_individuals
}

nr_individuals_per_regio_share <- nr_individuals_per_regio/sum(nr_individuals_per_regio) 



gc()

pumshead <- pumsdf[pumsdf$RELATE == 1,]

# SPATIAL ERROR


freq6 <- data.frame(xtabs(HHWT ~ HHTYPE + HHSIZECAT + GEOLEV1, data = pumshead)/sum(pumshead$HHWT))
freq7 <- data.frame(xtabs(PERWT ~ HHTYPE + AGECAT + GEOLEV1, data = pumsdf)/sum(pumsdf$PERWT))
freq8 <- data.frame(xtabs(PERWT ~ HHTYPE + EDUCAT + GEOLEV1, data = pumsdf)/sum(pumsdf$PERWT))
freq9 <- data.frame(xtabs(PERWT ~ HHTYPE + GENDER + GEOLEV1, data = pumsdf)/sum(pumsdf$PERWT))
freq10 <- data.frame(xtabs(PERWT ~ HHSIZECAT + AGECAT + GEOLEV1, data = pumsdf)/sum(pumsdf$PERWT))
freq11 <- data.frame(xtabs(PERWT ~ HHSIZECAT + EDUCAT + GEOLEV1, data = pumsdf)/sum(pumsdf$PERWT))
freq12 <- data.frame(xtabs(PERWT ~ HHSIZECAT + GENDER + GEOLEV1, data = pumsdf)/sum(pumsdf$PERWT))
freq13 <- data.frame(xtabs(PERWT ~ AGECAT + EDUCAT + GEOLEV1, data = pumsdf)/sum(pumsdf$PERWT))
freq14 <- data.frame(xtabs(PERWT ~ AGECAT + GENDER + GEOLEV1, data = pumsdf)/sum(pumsdf$PERWT))
freq15 <- data.frame(xtabs(PERWT ~ EDUCAT + GENDER + GEOLEV1, data = pumsdf)/sum(pumsdf$PERWT))


freq6tot <- data.frame(xtabs(HHWT ~ HHTYPE + HHSIZECAT, data = pumshead))$Freq/sum(pumshead$HHWT)
freq7tot <- data.frame(xtabs(PERWT ~ HHTYPE + AGECAT, data = pumsdf))$Freq/sum(pumsdf$PERWT)
freq8tot <- data.frame(xtabs(PERWT ~ HHTYPE + EDUCAT, data = pumsdf))$Freq/sum(pumsdf$PERWT)
freq9tot <- data.frame(xtabs(PERWT ~ HHTYPE + GENDER, data = pumsdf))$Freq/sum(pumsdf$PERWT)
freq10tot <- data.frame(xtabs(PERWT ~ HHSIZECAT + AGECAT, data = pumsdf))$Freq/sum(pumsdf$PERWT)
freq11tot <- data.frame(xtabs(PERWT ~ HHSIZECAT + EDUCAT, data = pumsdf))$Freq/sum(pumsdf$PERWT)
freq12tot <- data.frame(xtabs(PERWT ~ HHSIZECAT + GENDER, data = pumsdf))$Freq/sum(pumsdf$PERWT)
freq13tot <- data.frame(xtabs(PERWT ~ AGECAT + EDUCAT, data = pumsdf))$Freq/sum(pumsdf$PERWT)
freq14tot <- data.frame(xtabs(PERWT ~ AGECAT + GENDER, data = pumsdf))$Freq/sum(pumsdf$PERWT)
freq15tot <- data.frame(xtabs(PERWT ~ EDUCAT + GENDER, data = pumsdf))$Freq/sum(pumsdf$PERWT)


freq6tot <- rep(freq6tot, length(nr_individuals_per_regio))
freq6tot <- freq6tot * rep(nr_individuals_per_regio_share, each = length(freq6tot)/length(nr_individuals_per_regio))
freq7tot <- rep(freq7tot, length(nr_individuals_per_regio))
freq7tot <- freq7tot * rep(nr_individuals_per_regio_share, each = length(freq7tot)/length(nr_individuals_per_regio))
freq8tot <- rep(freq8tot, length(nr_individuals_per_regio))
freq8tot <- freq8tot * rep(nr_individuals_per_regio_share, each = length(freq8tot)/length(nr_individuals_per_regio))
freq9tot <- rep(freq9tot, length(nr_individuals_per_regio))
freq9tot <- freq9tot * rep(nr_individuals_per_regio_share, each = length(freq9tot)/length(nr_individuals_per_regio))
freq10tot <- rep(freq10tot, length(nr_individuals_per_regio))
freq10tot <- freq10tot * rep(nr_individuals_per_regio_share, each = length(freq10tot)/length(nr_individuals_per_regio))
freq11tot <- rep(freq11tot, length(nr_individuals_per_regio))
freq11tot <- freq11tot * rep(nr_individuals_per_regio_share, each = length(freq11tot)/length(nr_individuals_per_regio))
freq12tot <- rep(freq12tot, length(nr_individuals_per_regio))
freq12tot <- freq12tot * rep(nr_individuals_per_regio_share, each = length(freq12tot)/length(nr_individuals_per_regio))
freq13tot <- rep(freq13tot, length(nr_individuals_per_regio))
freq13tot <- freq13tot * rep(nr_individuals_per_regio_share, each = length(freq13tot)/length(nr_individuals_per_regio))
freq14tot <- rep(freq14tot, length(nr_individuals_per_regio))
freq14tot <- freq14tot * rep(nr_individuals_per_regio_share, each = length(freq14tot)/length(nr_individuals_per_regio))
freq15tot <- rep(freq15tot, length(nr_individuals_per_regio))
freq15tot <- freq15tot * rep(nr_individuals_per_regio_share, each = length(freq15tot)/length(nr_individuals_per_regio))


# MARGINAL ERROR
marg2 <- data.frame(xtabs(HHWT ~ HHSIZECAT + GEOLEV1, data = pumshead)/sum(pumshead$HHWT))
names(marg2)[names(marg2) == 'Freq'] <- 'Prob_HHSIZECAT'

marg3 <- data.frame(xtabs(PERWT ~ AGECAT + GEOLEV1, data = pumsdf)/sum(pumsdf$PERWT))
names(marg3)[names(marg3) == 'Freq'] <- 'Prob_AGECAT'

marg4 <- data.frame(xtabs(PERWT ~ EDUCAT + GEOLEV1, data = pumsdf)/sum(pumsdf$PERWT))
names(marg4)[names(marg4) == 'Freq'] <- 'Prob_EDUCAT'

marg5 <- data.frame(xtabs(PERWT ~ GENDER + GEOLEV1, data = pumsdf)/sum(pumsdf$PERWT))
names(marg5)[names(marg5) == 'Freq'] <- 'Prob_GENDER'

marg6 <- data.frame(xtabs(HHWT ~ HHTYPE + GEOLEV1, data = pumshead)/sum(pumshead$HHWT))
names(marg6)[names(marg6) == 'Freq'] <- 'Prob_HHTYPE'




freq6 <- left_join(freq6, marg6, by = c('HHTYPE', 'GEOLEV1'))
freq6 <- left_join(freq6, marg2, by = c("HHSIZECAT", "GEOLEV1"))
freq6[is.na(freq6)] <- 0
freq6$Marg_freq <- freq6$Prob_HHTYPE * freq6$Prob_HHSIZECAT
sumfreq_perregio <- (freq6 %>% group_by(GEOLEV1) %>% summarize(sumfreq_perregio = sum(Marg_freq)))$sumfreq_perregio
correctfactor <- nr_individuals_per_regio_share/sumfreq_perregio
freq6$Marg_freq <- freq6$Marg_freq * rep(correctfactor, each = nrow(freq6)/length(nr_individuals_per_regio_share))

freq7 <- left_join(freq7, marg6, by = c('HHTYPE', 'GEOLEV1'))
freq7 <- left_join(freq7, marg3, by = c('AGECAT', 'GEOLEV1'))
freq7[is.na(freq7)] <- 0
freq7$Marg_freq <- freq7$Prob_HHTYPE * freq7$Prob_AGECAT
sumfreq_perregio <- (freq7 %>% group_by(GEOLEV1) %>% summarize(sumfreq_perregio = sum(Marg_freq)))$sumfreq_perregio
correctfactor <- nr_individuals_per_regio_share/sumfreq_perregio
freq7$Marg_freq <- freq7$Marg_freq * rep(correctfactor, each = nrow(freq7)/length(nr_individuals_per_regio_share))

freq8 <- left_join(freq8, marg6, by = c('HHTYPE', 'GEOLEV1'))
freq8 <- left_join(freq8, marg4, by = c('EDUCAT', 'GEOLEV1'))
freq8[is.na(freq8)] <- 0
freq8$Marg_freq <- freq8$Prob_HHTYPE * freq8$Prob_EDUCAT
sumfreq_perregio <- (freq8 %>% group_by(GEOLEV1) %>% summarize(sumfreq_perregio = sum(Marg_freq)))$sumfreq_perregio
correctfactor <- nr_individuals_per_regio_share/sumfreq_perregio
freq8$Marg_freq <- freq8$Marg_freq * rep(correctfactor, each = nrow(freq8)/length(nr_individuals_per_regio_share))

freq9 <- left_join(freq9, marg6, by = c('HHTYPE', 'GEOLEV1'))
freq9 <- left_join(freq9, marg5, by = c('GENDER', 'GEOLEV1'))
freq9[is.na(freq9)] <- 0
freq9$Marg_freq <- freq9$Prob_HHTYPE * freq9$Prob_GENDER
sumfreq_perregio <- (freq9 %>% group_by(GEOLEV1) %>% summarize(sumfreq_perregio = sum(Marg_freq)))$sumfreq_perregio
correctfactor <- nr_individuals_per_regio_share/sumfreq_perregio
freq9$Marg_freq <- freq9$Marg_freq * rep(correctfactor, each = nrow(freq9)/length(nr_individuals_per_regio_share))

freq10 <- left_join(freq10, marg2, by = c('HHSIZECAT', 'GEOLEV1'))
freq10 <- left_join(freq10, marg3, by = c('AGECAT', 'GEOLEV1'))
freq10[is.na(freq10)] <- 0
freq10$Marg_freq <- freq10$Prob_HHSIZECAT * freq10$Prob_AGECAT
sumfreq_perregio <- (freq10 %>% group_by(GEOLEV1) %>% summarize(sumfreq_perregio = sum(Marg_freq)))$sumfreq_perregio
correctfactor <- nr_individuals_per_regio_share/sumfreq_perregio
freq10$Marg_freq <- freq10$Marg_freq * rep(correctfactor, each = nrow(freq10)/length(nr_individuals_per_regio_share))

freq11 <- left_join(freq11, marg2, by = c('HHSIZECAT', 'GEOLEV1'))
freq11 <- left_join(freq11, marg4, by = c('EDUCAT', 'GEOLEV1'))
freq11[is.na(freq11)] <- 0
freq11$Marg_freq <- freq11$Prob_HHSIZECAT * freq11$Prob_EDUCAT
sumfreq_perregio <- (freq11 %>% group_by(GEOLEV1) %>% summarize(sumfreq_perregio = sum(Marg_freq)))$sumfreq_perregio
correctfactor <- nr_individuals_per_regio_share/sumfreq_perregio
freq11$Marg_freq <- freq11$Marg_freq * rep(correctfactor, each = nrow(freq11)/length(nr_individuals_per_regio_share))

freq12 <- left_join(freq12, marg2, by = c('HHSIZECAT', 'GEOLEV1'))
freq12 <- left_join(freq12, marg5, by = c('GENDER', 'GEOLEV1'))
freq12[is.na(freq12)] <- 0
freq12$Marg_freq <- freq12$Prob_HHSIZECAT * freq12$Prob_GENDER
sumfreq_perregio <- (freq12 %>% group_by(GEOLEV1) %>% summarize(sumfreq_perregio = sum(Marg_freq)))$sumfreq_perregio
correctfactor <- nr_individuals_per_regio_share/sumfreq_perregio
freq12$Marg_freq <- freq12$Marg_freq * rep(correctfactor, each = nrow(freq12)/length(nr_individuals_per_regio_share))

freq13 <- left_join(freq13, marg3, by = c('AGECAT', 'GEOLEV1'))
freq13 <- left_join(freq13, marg4, by = c('EDUCAT', 'GEOLEV1'))
freq13[is.na(freq13)] <- 0
freq13$Marg_freq <- freq13$Prob_AGECAT * freq13$Prob_EDUCAT
sumfreq_perregio <- (freq13 %>% group_by(GEOLEV1) %>% summarize(sumfreq_perregio = sum(Marg_freq)))$sumfreq_perregio
correctfactor <- nr_individuals_per_regio_share/sumfreq_perregio
freq13$Marg_freq <- freq13$Marg_freq * rep(correctfactor, each = nrow(freq13)/length(nr_individuals_per_regio_share))

freq14 <- left_join(freq14, marg3, by = c('AGECAT', 'GEOLEV1'))
freq14 <- left_join(freq14, marg5, by = c('GENDER', 'GEOLEV1'))
freq14[is.na(freq14)] <- 0
freq14$Marg_freq <- freq14$Prob_AGECAT * freq14$Prob_GENDER
sumfreq_perregio <- (freq14 %>% group_by(GEOLEV1) %>% summarize(sumfreq_perregio = sum(Marg_freq)))$sumfreq_perregio
correctfactor <- nr_individuals_per_regio_share/sumfreq_perregio
freq14$Marg_freq <- freq14$Marg_freq * rep(correctfactor, each = nrow(freq14)/length(nr_individuals_per_regio_share))

freq15 <- left_join(freq15, marg4, by = c('EDUCAT', 'GEOLEV1'))
freq15 <- left_join(freq15, marg5, by = c('GENDER', 'GEOLEV1'))
freq15[is.na(freq15)] <- 0
freq15$Marg_freq <- freq15$Prob_EDUCAT * freq15$Prob_GENDER
sumfreq_perregio <- (freq15 %>% group_by(GEOLEV1) %>% summarize(sumfreq_perregio = sum(Marg_freq)))$sumfreq_perregio
correctfactor <- nr_individuals_per_regio_share/sumfreq_perregio
freq15$Marg_freq <- freq15$Marg_freq * rep(correctfactor, each = nrow(freq15)/length(nr_individuals_per_regio_share))



freq6['National'] <- freq6tot
freq7['National'] <- freq7tot
freq8['National'] <- freq8tot
freq9['National'] <- freq9tot
freq10['National'] <- freq10tot
freq11['National'] <- freq11tot
freq12['National'] <- freq12tot
freq13['National'] <- freq13tot
freq14['National'] <- freq14tot
freq15['National'] <- freq15tot


freq6 <- freq6 %>% select(Freq, Marg_freq, National)
freq7 <- freq7 %>% select(Freq, Marg_freq, National)
freq8 <- freq8 %>% select(Freq, Marg_freq, National)
freq9 <- freq9 %>% select(Freq, Marg_freq, National)
freq10 <- freq10 %>% select(Freq, Marg_freq, National)
freq11 <- freq11 %>% select(Freq, Marg_freq, National)
freq12 <- freq12 %>% select(Freq, Marg_freq, National)
freq13 <- freq13 %>% select(Freq, Marg_freq, National)
freq14 <- freq14 %>% select(Freq, Marg_freq, National)
freq15 <- freq15 %>% select(Freq, Marg_freq, National)



df_allfreqs <- bind_rows(freq6, freq7, freq8, freq9,
                         freq10, freq11, freq12, freq13, freq14, freq15)


names(df_allfreqs)[names(df_allfreqs) == 'Freq'] <- 'Data_freq'

df_allfreqs['Sq_error_survey_national'] <- (df_allfreqs$Data_freq - df_allfreqs$National)^2
df_allfreqs['Sq_error_survey_marginal'] <- (df_allfreqs$Data_freq - df_allfreqs$Marg_freq)^2

# Synthetic combinations. 

# change regions in syn_pop here: 
if (countryname %ni% c('Benin', 'Guinea', 'Nepal', 'Uganda', 'Philippines')){

  syn_pop <- syn_pop %>% rename(GDLCODEnr = GEOLEV1)

  syn_pop <- left_join(syn_pop, GDLPUMSmatch_sel %>% select(ZONE, GDLCODEnr), by = 'GDLCODEnr') 

  syn_pop <- syn_pop %>% select(-GDLCODEnr)
  
  syn_pop <- syn_pop %>% rename(GEOLEV1 = ZONE)

}


syn_pop_head <- syn_pop[RELATE==1,]



freq6synth <- data.frame(xtabs( ~ HHTYPE + HHSIZECAT + GEOLEV1, data = syn_pop_head)/nrow(syn_pop_head))
freq7synth <- data.frame(xtabs( ~ HHTYPE + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
freq8synth <- data.frame(xtabs( ~ HHTYPE + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
freq9synth <- data.frame(xtabs( ~ HHTYPE + GENDER + GEOLEV1, data = syn_pop)/nrow(syn_pop))
freq10synth <- data.frame(xtabs( ~ HHSIZECAT + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
freq11synth <- data.frame(xtabs( ~ HHSIZECAT + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
freq12synth <- data.frame(xtabs( ~ HHSIZECAT + GENDER + GEOLEV1, data = syn_pop)/nrow(syn_pop))
freq13synth <- data.frame(xtabs( ~ AGECAT + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
freq14synth <- data.frame(xtabs( ~ AGECAT + GENDER + GEOLEV1, data = syn_pop)/nrow(syn_pop))
freq15synth <- data.frame(xtabs( ~ EDUCAT + GENDER + GEOLEV1, data = syn_pop)/nrow(syn_pop))



names(freq6synth)[names(freq6synth) == 'Freq'] <- 'Freq_synth'
names(freq7synth)[names(freq7synth) == 'Freq'] <- 'Freq_synth'
names(freq8synth)[names(freq8synth) == 'Freq'] <- 'Freq_synth'
names(freq9synth)[names(freq9synth) == 'Freq'] <- 'Freq_synth'
names(freq10synth)[names(freq10synth) == 'Freq'] <- 'Freq_synth'
names(freq11synth)[names(freq11synth) == 'Freq'] <- 'Freq_synth'
names(freq12synth)[names(freq12synth) == 'Freq'] <- 'Freq_synth'
names(freq13synth)[names(freq13synth) == 'Freq'] <- 'Freq_synth'
names(freq14synth)[names(freq14synth) == 'Freq'] <- 'Freq_synth'
names(freq15synth)[names(freq15synth) == 'Freq'] <- 'Freq_synth'



df_synthfreqs <- bind_rows(freq6synth, freq7synth, freq8synth, freq9synth,
                           freq10synth, freq11synth, freq12synth, freq13synth, freq14synth, freq15synth)

df_allfreqs['Synth_freq'] <- df_synthfreqs$Freq_synth

check_sums_survey <- sum(df_allfreqs$Data_freq)
check_sums_synth <- sum(df_allfreqs$Synth_freq)
check_sums_national <- sum(df_allfreqs$National)
check_sums_marginal <- sum(df_allfreqs$Marg_freq)

if ((check_sums_survey - check_sums_national) + (check_sums_synth - check_sums_marginal) <= 0.01){
  
  standardized_error_factor <- 100/sum(df_allfreqs$Synth_freq)
  standardized_error_factor <- 1
  df_allfreqs$Data_freq <- df_allfreqs$Data_freq * standardized_error_factor
  df_allfreqs$Synth_freq <- df_allfreqs$Synth_freq * standardized_error_factor
  df_allfreqs$National <- df_allfreqs$National * standardized_error_factor
  df_allfreqs$Marg_freq <- df_allfreqs$Marg_freq * standardized_error_factor
  
  
  df_allfreqs['Sq_error_survey_national'] <- (df_allfreqs$Data_freq - df_allfreqs$National)^2
  df_allfreqs['Sq_error_survey_marginal'] <- (df_allfreqs$Data_freq - df_allfreqs$Marg_freq)^2
  
  
  df_allfreqs['Sq_error_survey_synthetic'] <- (df_allfreqs$Data_freq - df_allfreqs$Synth_freq)^2
  
  
  DF_errors[1,2] <- sum(df_allfreqs$Sq_error_survey_synthetic)
  DF_errors[1,3] <- sum(df_allfreqs$Sq_error_survey_marginal)
  DF_errors[1,4] <- sum(df_allfreqs$Sq_error_survey_national)
  
} else if ((check_sums_survey - check_sums_national) + (check_sums_synth - check_sums_marginal) > 0.01){
  DF_errors[1,2] <- -99
  DF_errors[1,3] <- -99
  DF_errors[1,4] <- -99
  
}


rm(syn_pop)
gc()



filename_dferrors <- paste0("synth_errors_DHS_nov23_", isocode, ".csv") #was oct
filename_indiperregion <- paste0("Individuals_per_region_DHS_oct23_", isocode, ".csv") #was oct

write.csv(DF_errors, filename_dferrors, row.names = FALSE)

write.csv(DF_nr_individuals_per_region, filename_indiperregion, row.names = FALSE)

print('saved output')














