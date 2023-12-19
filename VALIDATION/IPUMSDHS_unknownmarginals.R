#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)



isocode = as.character(args[1])
print(isocode)


# countryname = 'Philippines' 

# countryname = 'Zambia'

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
#library(haven)
#library(sjlabelled)
library(readstata13)


#setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2') 



countryname_to_iso <- read.csv('Countrynames_to_ISO.csv', sep = ';', header = TRUE)

countryname <- countryname_to_iso[countryname_to_iso$iso_code == isocode,]$country


GDL_population <- read.csv('GDL_subnational_population_world.csv', head = TRUE, sep = ',')


GDL_pop2015 <- GDL_population[GDL_population$year == 2015,]

GDLcountry <- GDL_pop2015[GDL_pop2015$country == countryname,]





GDLPUMSmatch <- read.csv('GDL_IPUMS_match.csv', sep = ';')

#GDLPUMSmatch <- GDLPUMSmatch[GDLPUMSmatch$Country == countryname,]

#GDLPUMSmatch <- left_join(GDLPUMSmatch, GDLcountry %>% select(GDLCODE, pop), by = 'GDLCODE')

GDLPUMSmatch <- GDLPUMSmatch %>% select(Country, ISO_Code, GDLCODE, Region, GEOLEV1_IPUMS, Label)

GDLPUMSmatch$GDLCODEnr <- substr(GDLPUMSmatch$GDLCODE, nchar(GDLPUMSmatch$GDLCODE) - 2, nchar(GDLPUMSmatch$GDLCODE))




DF_nr_individuals_per_region <- data.frame()

DF_errors <- data.frame('Country' = countryname, 'Synth_sq_error' = 0)

ALL_region_errors_missing_marginals <- c()

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



GEOLEV1regions <- sort(unique(pums$GEOLEV1), decreasing = FALSE)


pumsdf <- targets_PUMS(pums, "df")



################################################################
################### IMPORT SURVEY DATA

reference_sample = data.frame()
maxhid = 0


filename_length <- paste0('length_DHSdata_', isocode, '_sept23.csv')


rowlength_DHS <- read.csv(filename_length, sep = ',', header = TRUE)

l = rowlength_DHS$length
w = 11

filenamelisdata <- paste0(isocode, "_DHS_sept23_synth_country.dat")

con = file(filenamelisdata, "rb")

reference_sample = readBin(con, integer(), n = l*w)

# reshape the bindata
reference_sample <- array(reference_sample, dim=c(l, w))
reference_sample <-  as.data.frame(reference_sample) #was frame

colnames(reference_sample) <- c('WEALTH', 'RURAL', 'FARMING', 'AGECAT', 'GENDER', 
                                  'EDUCAT', 'HHTYPE', 'HID', 'RELATE', 'HHSIZECAT', 'WEIGHTS')


close(con)

print('imported DHS reference sample')


reference_sample <- as.data.table(reference_sample)

reference_sample[, PID := 1:.N]



#############################################################
############ IMPORT ESTIMATED MARGINALS



#hhsize
hhsize_marg_est <- read.csv('Est_marginals_HHSIZECAT_sept23.csv', sep = ',', header = TRUE)
hhsize_marg_est <- left_join(hhsize_marg_est, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
hhsize_marg_est <- hhsize_marg_est[hhsize_marg_est$country == countryname,]
hhsize_marg_est <- hhsize_marg_est %>% select(-country)

hhsize_marg_est <- pivot_longer(hhsize_marg_est, cols = c(X1est, X2est, X3est, X4est, X5est, X6est), names_to = "HHSIZECAT", values_to = "Frequency")
hhsize_marg_est$HHSIZECAT <- as.integer(as.factor(hhsize_marg_est$HHSIZECAT))


hhsize_marg_est <- left_join(hhsize_marg_est, GDLPUMSmatch %>% select(GDLCODE, GEOLEV1_IPUMS), by = 'GDLCODE')
hhsize_marg_est_pums <- hhsize_marg_est %>% group_by(HHSIZECAT, GEOLEV1_IPUMS) %>% summarise(Frequency = sum(Frequency))

hhsize_marg_est_tib <- as_tibble(hhsize_marg_est_pums)


#hhtype
hhtype_marg_est <- read.csv('Est_marginals_HHTYPE_sept23.csv', sep = ',', header = TRUE)
hhtype_marg_est <- left_join(hhtype_marg_est, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
hhtype_marg_est <- hhtype_marg_est[hhtype_marg_est$country == countryname,]
hhtype_marg_est <- hhtype_marg_est %>% select(-country)

hhtype_marg_est <- pivot_longer(hhtype_marg_est, cols = c(X1est, X2est, X3est, X4est, X5est, X6est, X7est, X8est), names_to = "HHTYPE", values_to = "Frequency")
hhtype_marg_est$HHTYPE <- as.integer(as.factor(hhtype_marg_est$HHTYPE))

hhtype_marg_est <- left_join(hhtype_marg_est, GDLPUMSmatch %>% select(GDLCODE, GEOLEV1_IPUMS), by = 'GDLCODE')
hhtype_marg_est_pums <- hhtype_marg_est %>% group_by(HHTYPE, GEOLEV1_IPUMS) %>% summarise(Frequency = sum(Frequency))

hhtype_marg_est_tib <- as_tibble(hhtype_marg_est_pums)


# age
age_marg_est <- read.csv('Est_marginals_AGECAT_sept23.csv', sep = ',', header = TRUE)
age_marg_est <- left_join(age_marg_est, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
age_marg_est <- age_marg_est[age_marg_est$country == countryname,]
age_marg_est <- age_marg_est %>% select(-country)

age_marg_est <- pivot_longer(age_marg_est, cols = c(X1est, X2est, X3est, X4est, X5est, X6est, X7est), names_to = "AGECAT", values_to = "Frequency")
age_marg_est$AGECAT <- as.integer(as.factor(age_marg_est$AGECAT))

age_marg_est <- left_join(age_marg_est, GDLPUMSmatch %>% select(GDLCODE, GEOLEV1_IPUMS), by = 'GDLCODE')
age_marg_est_pums <- age_marg_est %>% group_by(AGECAT, GEOLEV1_IPUMS) %>% summarise(Frequency = sum(Frequency))

age_marg_est_tib <- as_tibble(age_marg_est_pums)


# gender
gender_marg_est <- read.csv('Est_marginals_GENDER_sept23.csv', sep = ',', header = TRUE)
gender_marg_est <- left_join(gender_marg_est, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
gender_marg_est <- gender_marg_est[gender_marg_est$country == countryname,]
gender_marg_est <- gender_marg_est %>% select(-country)

gender_marg_est <- pivot_longer(gender_marg_est, cols = c(X1est, X2est), names_to = "GENDER", values_to = "Frequency")
gender_marg_est$GENDER <- as.integer(as.factor(gender_marg_est$GENDER))
gender_marg_est$GENDER <- gender_marg_est$GENDER -1

gender_marg_est <- left_join(gender_marg_est, GDLPUMSmatch %>% select(GDLCODE, GEOLEV1_IPUMS), by = 'GDLCODE')
gender_marg_est_pums <- gender_marg_est %>% group_by(GENDER, GEOLEV1_IPUMS) %>% summarise(Frequency = sum(Frequency))

gender_marg_est_tib <- as_tibble(gender_marg_est_pums)


# educat
educat_marg_est <- read.csv('Est_marginals_EDUCAT_sept23.csv', sep = ',', header = TRUE)
educat_marg_est <- left_join(educat_marg_est, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
educat_marg_est <- educat_marg_est[educat_marg_est$country == countryname,]
educat_marg_est <- educat_marg_est %>% select(-country)

educat_marg_est <- pivot_longer(educat_marg_est, cols = c(X1est, X2est, X3est, X4est, X5est), names_to = "EDUCAT", values_to = "Frequency")
educat_marg_est$EDUCAT <- as.integer(as.factor(educat_marg_est$EDUCAT))

educat_marg_est <- left_join(educat_marg_est, GDLPUMSmatch %>% select(GDLCODE, GEOLEV1_IPUMS), by = 'GDLCODE')
educat_marg_est_pums <- educat_marg_est %>% group_by(EDUCAT, GEOLEV1_IPUMS) %>% summarise(Frequency = sum(Frequency))

educat_marg_est_tib <- as_tibble(educat_marg_est_pums)






##############################################################
################### RURAL MARGINAL SMOD ######################

SMODmarg <- read.csv('individual_marginals_UrbanRural.csv', sep = ';', header = TRUE)
GDL_pop <- read.csv('GDL_subnational_population_world.csv', head = TRUE, sep = ',')


SMODmarg <- SMODmarg %>% rename('GDLCODE' = 'GDLcode')

SMODmarg_country <- SMODmarg[SMODmarg$country == countryname,]

SMODmarg_country['ZONE'] <- substr(SMODmarg_country$GDLCODE, nchar(SMODmarg_country$GDLCODE) - 2, nchar(SMODmarg_country$GDLCODE)) 
SMODmarg_country$ZONE <- as.numeric(SMODmarg_country$ZONE)

ruralmargSMOD0 <- SMODmarg_country %>% select(GDLCODE, Urban_percent)
ruralmargSMOD1 <- SMODmarg_country %>% select(GDLCODE, Rural_percent)

sum_indi <- gender_marg_est %>% group_by(GDLCODE) %>% summarise(Population = sum(Frequency))


ruralmargSMOD0 <- left_join(ruralmargSMOD0, sum_indi, by = 'GDLCODE')
ruralmargSMOD0['New_freq'] <- ruralmargSMOD0$Urban_percent * ruralmargSMOD0$Population

ruralmargSMOD1 <- left_join(ruralmargSMOD1, sum_indi, by = 'GDLCODE')
ruralmargSMOD1['New_freq'] <- ruralmargSMOD1$Rural_percent * ruralmargSMOD1$Population

ruralmargSMOD0$RURAL <- 0
ruralmargSMOD1$RURAL <- 1

ruralmargSMOD0 <- ruralmargSMOD0 %>% select(GDLCODE, RURAL, New_freq)
ruralmargSMOD1 <- ruralmargSMOD1 %>% select(GDLCODE, RURAL, New_freq)

ruralmargSMOD <- rbind.data.frame(ruralmargSMOD0, ruralmargSMOD1)

ruralmargSMOD <- ruralmargSMOD %>% rename('Frequency' = 'New_freq')

ruralmargSMOD <- left_join(ruralmargSMOD, GDLPUMSmatch %>% select(GDLCODE, GEOLEV1_IPUMS), by = 'GDLCODE')
ruralmargSMOD_pums <- ruralmargSMOD %>% group_by(RURAL, GEOLEV1_IPUMS) %>% summarise(Frequency = sum(Frequency))


rural_tib <- as_tibble(ruralmargSMOD_pums)


GEOLEV1regions <- unique(rural_tib$GEOLEV1_IPUMS) 



nr_individuals_per_regio = c()

syn_pop <- data.table()


for (regnr in GEOLEV1regions) {
  
  print(regnr)
  
  rural_tib1reg <- rural_tib[rural_tib$GEOLEV1_IPUMS == regnr,]
  rural_tib1reg <- rural_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  hhtype_tib1reg <- hhtype_marg_est_tib[hhtype_marg_est_tib$GEOLEV1_IPUMS == regnr,]
  hhtype_tib1reg <- hhtype_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  hhsize_tib1reg <- hhsize_marg_est_tib[hhsize_marg_est_tib$GEOLEV1_IPUMS == regnr,]
  hhsize_tib1reg <- hhsize_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  age_tib1reg <- age_marg_est_tib[age_marg_est_tib$GEOLEV1_IPUMS == regnr,]
  age_tib1reg <- age_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  edu_tib1reg <- educat_marg_est_tib[educat_marg_est_tib$GEOLEV1_IPUMS == regnr,]
  edu_tib1reg <- edu_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  sex_tib1reg <- gender_marg_est_tib[gender_marg_est_tib$GEOLEV1_IPUMS == regnr,]
  sex_tib1reg <- sex_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  
  group_control <- list()
  individual_control <- list(hhtype_tib1reg, hhsize_tib1reg, rural_tib1reg, age_tib1reg, edu_tib1reg, sex_tib1reg)
  
  
  
  names(group_control) <- c() 
  names(individual_control) <- c('HHTYPE', 'HHSIZECAT','RURAL','AGECAT','EDUCAT', 'GENDER')
  
  
  ml_fitWEIGHTS <- reference_sample$WEIGHTS 
  
  fitting_problem <- ml_problem(
    ref_sample = reference_sample, 
    controls = list(
      individual = individual_control,
      group = group_control
    ), prior_weights = ml_fitWEIGHTS,
    field_names = special_field_names(
      groupId = "HID", 
      individualId = "PID", 
      count = "Frequency"
    )
  )
  
  fit <- ml_fit(ml_problem = fitting_problem, algorithm = "ipu", maxiter = 50)
  
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


# per region, calculate percentages wealth groups 
wealthfreq_regio <- data.frame(xtabs(~WEALTH + GEOLEV1, data = synpophead))

mp_w1 <- 1-((tableinc[1]/sum(tableinc))-0.2)/(tableinc[1]/sum(tableinc))
mp_w2 <- 1-((tableinc[2]/sum(tableinc))-0.2)/(tableinc[2]/sum(tableinc))
mp_w3 <- 1-((tableinc[3]/sum(tableinc))-0.2)/(tableinc[3]/sum(tableinc))
mp_w4 <- 1-((tableinc[4]/sum(tableinc))-0.2)/(tableinc[4]/sum(tableinc))
mp_w5 <- 1-((tableinc[5]/sum(tableinc))-0.2)/(tableinc[5]/sum(tableinc))



mp_df <- data.frame('WEALTH' = c(1,2,3,4,5), 'mp' = c(mp_w1,mp_w2,mp_w3,mp_w4,mp_w5))

wealthfreq_regio <- merge(wealthfreq_regio, mp_df, on = 'WEALTH', how = 'left')
wealthfreq_regio['Freq'] <- wealthfreq_regio['Freq']*wealthfreq_regio['mp']
wealthfreq_regio['GEOLEV1_IPUMS'] <- wealthfreq_regio['GEOLEV1']

wealthfreq_regio <- wealthfreq_regio %>% rename(Frequency = Freq)

wealth_tib <- as_tibble(wealthfreq_regio %>% select(GEOLEV1_IPUMS, WEALTH, Frequency))



# for loop with regions. Now with wealth marginal.  
nr_individuals_per_regio = c()

syn_pop <- data.table()



for (regnr in GEOLEV1regions) {
  
  print(regnr)
  wealth_tib1reg <- wealth_tib[wealth_tib$GEOLEV1_IPUMS == regnr,]
  wealth_tib1reg <- wealth_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  rural_tib1reg <- rural_tib[rural_tib$GEOLEV1_IPUMS == regnr,]
  rural_tib1reg <- rural_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  hhsize_tib1reg <- hhsize_marg_est_tib[hhsize_marg_est_tib$GEOLEV1_IPUMS == regnr,]
  hhsize_tib1reg <- hhsize_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  hhtype_tib1reg <- hhtype_marg_est_tib[hhtype_marg_est_tib$GEOLEV1_IPUMS == regnr,]
  hhtype_tib1reg <- hhtype_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  age_tib1reg <- age_marg_est_tib[age_marg_est_tib$GEOLEV1_IPUMS == regnr,]
  age_tib1reg <- age_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  edu_tib1reg <- educat_marg_est_tib[educat_marg_est_tib$GEOLEV1_IPUMS == regnr,]
  edu_tib1reg <- edu_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  sex_tib1reg <- gender_marg_est_tib[gender_marg_est_tib$GEOLEV1_IPUMS == regnr,]
  sex_tib1reg <- sex_tib1reg %>% select(-GEOLEV1_IPUMS)
  
  group_control <- list(wealth_tib1reg)
  individual_control <- list(hhtype_tib1reg, hhsize_tib1reg, rural_tib1reg, age_tib1reg, edu_tib1reg, sex_tib1reg)
  
  # rural is indivodual here. 
  
  names(group_control) <- c('WEALTH') 
  names(individual_control) <- c('HHTYPE', 'HHSIZECAT', 'RURAL', 'AGECAT','EDUCAT', 'GENDER')
  
  
  ml_fitWEIGHTS <- reference_sample$WEIGHTS
  
  fitting_problem <- ml_problem(
    ref_sample = reference_sample, 
    controls = list(
      individual = individual_control,
      group = group_control
    ), prior_weights = ml_fitWEIGHTS,
    field_names = special_field_names(
      groupId = "HID", 
      individualId = "PID", 
      count = "Frequency"
    )
  )
  
  fit <- ml_fit(ml_problem = fitting_problem, algorithm = "ipu", maxiter = 50)
  
  syn_pop_reg <- ml_replicate(fit, algorithm = "trs")
  
  popsize_syn <- nrow(syn_pop_reg)
  
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
  
  
  name = paste0(countryname, '_DHS_aug23_synthpop_region', as.character(regnr), '.dat')
  
  #con = file(name, "wb")
  
  #writeBin(c(syn_pop_reg$WEALTH, syn_pop_reg$RURAL, syn_pop_reg$FARMING, syn_pop_reg$AGECAT, syn_pop_reg$GENDER, 
  #           syn_pop_reg$EDUCAT, syn_pop_reg$HHTYPE, syn_pop_reg$HID, syn_pop_reg$RELATE, syn_pop_reg$HHSIZECAT, syn_pop_reg$GEOLEV1), con)
  
  
  #close(con)

  
  gc()
  syn_pop <- rbindlist(list(syn_pop, syn_pop_reg), use.names=TRUE)
  rm(syn_pop_reg)
  
  
  gc()
  
  
}


df_nr_individuals_per_region <- data.frame(GEOLEV1_IPUMS = GEOLEV1regions, Nr_individuals = nr_individuals_per_regio)
DF_nr_individuals_per_region <- bind_rows(DF_nr_individuals_per_region, df_nr_individuals_per_region)


nr_individuals_per_regio_share <- nr_individuals_per_regio/sum(nr_individuals_per_regio) 



gc()

pumshead <- pumsdf[pumsdf$RELATE == 1,]


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

freq6 <- freq6 %>% select(Freq)
freq7 <- freq7 %>% select(Freq)
freq8 <- freq8 %>% select(Freq)
freq9 <- freq9 %>% select(Freq)
freq10 <- freq10 %>% select(Freq)
freq11 <- freq11 %>% select(Freq)
freq12 <- freq12 %>% select(Freq)
freq13 <- freq13 %>% select(Freq)
freq14 <- freq14 %>% select(Freq)
freq15 <- freq15 %>% select(Freq)



df_allfreqs <- bind_rows(freq6, freq7, freq8, freq9,
                         freq10, freq11, freq12, freq13, freq14, freq15)


names(df_allfreqs)[names(df_allfreqs) == 'Freq'] <- 'Data_freq'


# Synthetic combinations.

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


if ((check_sums_survey - check_sums_synth) <= 0.01){
  
  standardized_error_factor <- 100/sum(df_allfreqs$Synth_freq)
  standardized_error_factor <- 1
  df_allfreqs$Data_freq <- df_allfreqs$Data_freq * standardized_error_factor
  df_allfreqs$Synth_freq <- df_allfreqs$Synth_freq * standardized_error_factor
  
  
  df_allfreqs['Sq_error_survey_synthetic'] <- (df_allfreqs$Data_freq - df_allfreqs$Synth_freq)^2
  
  
  DF_errors[1,2] <- sum(df_allfreqs$Sq_error_survey_synthetic)
  
} else if ((check_sums_survey - check_sums_synth) > 0.01){
  DF_errors[1,2] <- -99
  
  
}


rm(syn_pop)
gc()



filename_dferrors <- paste0("synth_errors_estmarginals_DHS_nov23_", isocode, ".csv") #was oct23

write.csv(DF_errors, filename_dferrors, row.names = FALSE)

print('saved output')

