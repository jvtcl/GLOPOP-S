#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)



countryname = as.character(args[1])


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



################################################
######## NO error calculation in this file!!! 


liscodecountries <- read.csv('LIS_codes_countries2.csv', sep = ';')

GDL_population <- read.csv('GDL_subnational_population_world.csv', head = TRUE, sep = ',')
GDL_pop2015 <- GDL_population[GDL_population$year == 2015,]
GDLcountry <- GDL_pop2015[GDL_pop2015$country == countryname,]



syn_pop <- data.table()
nosynth_regions <- c()



##################################################
###### IMPORT SURVEY DATA ######

replace_countries_file <- read.csv('similar_countries_8nov23.csv', sep = ',')

replace_countries_file <- replace_countries_file[replace_countries_file['Country'] == countryname ,]

replace1 <- replace_countries_file$Replace1 
replace2 <- replace_countries_file$Replace2
replace3 <- replace_countries_file$Replace3

replace_lisdhs1 <- replace_countries_file$Source1 
replace_lisdhs2 <- replace_countries_file$Source2
replace_lisdhs3 <- replace_countries_file$Source3

replacecountries <- c(replace1, replace2, replace3)
replace_lisdhs <- c(replace_lisdhs1, replace_lisdhs2, replace_lisdhs3)

print(replacecountries)
print(replace_lisdhs)


jointhead100small = data.frame()
maxhid = 0

dhscountries <- read.csv('DHScountries_july23.csv', sep = ';')

countryname_to_iso <- read.csv('Countrynames_to_ISO.csv', sep = ';', header = TRUE)


for (repl in c(1,2,3)){
  
  replacecountry <- replacecountries[repl]
  print(replacecountry) 
  
  #if (liscode != 'nolis'){
  #  replacecode = liscode
  #}
  
  
  if (replace_lisdhs[repl] == "LIS"){
    print(liscodecountries[liscodecountries['Country'] == replacecountry ,])
    replacecode <- liscodecountries[liscodecountries['Country'] == replacecountry ,]$iso_code
    print(replacecode)
    #read the file with agents. Loop through the region number.
    filename_length <- paste0('length_LIS_survey_may23', replacecode, '.csv')
    print(filename_length)
    rowlength_LIS <- read.csv(filename_length, sep = ',', header = TRUE)
    
    l = rowlength_LIS$x
    w = 11
    
    filenamelisdata <- paste0(replacecode, "_LISdata_may23.dat")
    
    con = file(filenamelisdata, "rb")
    
    jointhead100small_1 = readBin(con, integer(), n = l*w)
    
    # reshape the bindata
    jointhead100small_1 <- array(jointhead100small_1, dim=c(l, w))
    jointhead100small_1 <-  as.data.frame(jointhead100small_1) #was frame
    
    colnames(jointhead100small_1) <- c('INCOME', 'RURAL', 'FARMING', 'AGECAT', 'GENDER', 
                                       'EDUCAT', 'HHTYPE', 'HID', 'RELATE', 'HHSIZECAT', 'WEIGHTS')
    
    jointhead100small_1['Replacecountry'] <- repl
    
    
    close(con)
    
    print('imported LIS data')
    
    if (repl > 1){
      maxhid <- max(jointhead100small$HID)
      jointhead100small_1$HID <- jointhead100small_1$HID + maxhid
    }
    
    
    
    jointhead100small <- rbind(jointhead100small, jointhead100small_1)
    
    
    
    # LIS farming available: 
    # Brazil, Dominican Republic, Georgia, Guatemala, Hungary, India, Italy, Mexico, Poland, Russia, Serbia, South Africa, Vietnam. 
    
    
    
    
    
    
  } else if(replace_lisdhs[repl] == "DHS"){
    
    # Hier tzt even naar kijken. Namen kolommen hetzelfde maken. 
    # 15 sept 2023. check LIS farming. 
    # DHS met farming!!
    
    replacecountryname = replacecountries[repl]
    
    replace_iso = countryname_to_iso[countryname_to_iso$country == replacecountryname,]$iso_code
    
    
    filename_length <- paste0('length_DHSdata_', replace_iso, '_sept23.csv')
    
    
    rowlength_DHS <- read.csv(filename_length, sep = ',', header = TRUE)
    
    l = rowlength_DHS$length
    w = 11
    
    filenamelisdata <- paste0(replace_iso, "_DHS_sept23_synth_country.dat")
    
    con = file(filenamelisdata, "rb")
    
    jointhead100small_1 = readBin(con, integer(), n = l*w)
    
    # reshape the bindata
    jointhead100small_1 <- array(jointhead100small_1, dim=c(l, w))
    jointhead100small_1 <-  as.data.frame(jointhead100small_1) #was frame
    
    colnames(jointhead100small_1) <- c('INCOME', 'RURAL', 'FARMING', 'AGECAT', 'GENDER', 
                                       'EDUCAT', 'HHTYPE', 'HID', 'RELATE', 'HHSIZECAT', 'WEIGHTS')
    
    jointhead100small_1['Replacecountry'] <- repl
    
    
    close(con)
    
    print('imported DHS data')
    
    if (repl > 1){
      maxhid <- max(jointhead100small$HID)
      jointhead100small_1$HID <- jointhead100small_1$HID + maxhid
    }
    
    
    
    jointhead100small <- rbind(jointhead100small, jointhead100small_1)
    
  }
  
}


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


########################################################################################################
################ SYNTHETIC POPULATION #############################

GDL_pop <- read.csv('GDL_subnational_population_world.csv', head = TRUE, sep = ',')

GDL_pop_country <- GDL_pop[GDL_pop$country == countryname,]




#hhsize
hhsize_marg_est <- read.csv('Est_marginals_HHSIZECAT_sept23.csv', sep = ',', header = TRUE)
hhsize_marg_known <- read.csv('IPUMS_hhsize_observedmarginals_sept23.csv', sep = ',', header = TRUE)
hhsize_marg_known <- hhsize_marg_known %>% select(-country)
hhsize_marg_known <- left_join(hhsize_marg_known, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
hhsize_marg_known <- hhsize_marg_known[hhsize_marg_known$country == countryname,]
hhsize_marg_known <- hhsize_marg_known %>% select(-country)

hhsize_marg_known <- pivot_longer(hhsize_marg_known, cols = c(X1, X2, X3, X4, X5, X6), names_to = "HHSIZECAT", values_to = "Frequency")
hhsize_marg_known$HHSIZECAT <- as.integer(as.factor(hhsize_marg_known$HHSIZECAT))

hhsize_marg_est_tib <- as_tibble(hhsize_marg_known)


#hhtype
hhtype_marg_est <- read.csv('Est_marginals_HHTYPE_sept23.csv', sep = ',', header = TRUE)
hhtype_marg_known <- read.csv('IPUMS_hhtype_observedmarginals_sept23.csv', sep = ',', header = TRUE)
hhtype_marg_known <- hhtype_marg_known %>% select(-country)
hhtype_marg_known <- left_join(hhtype_marg_known, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
hhtype_marg_known <- hhtype_marg_known[hhtype_marg_known$country == countryname,]
hhtype_marg_known <- hhtype_marg_known %>% select(-country)

hhtype_marg_known <- pivot_longer(hhtype_marg_known, cols = c(X1, X2, X3, X4, X5, X6, X7, X8), names_to = "HHTYPE", values_to = "Frequency")
hhtype_marg_known$HHTYPE <- as.integer(as.factor(hhtype_marg_known$HHTYPE))

hhtype_marg_est_tib <- as_tibble(hhtype_marg_known)


# age
age_marg_est <- read.csv('Est_marginals_AGECAT_sept23.csv', sep = ',', header = TRUE)
age_marg_known <- read.csv('IPUMS_age_observedmarginals_sept23.csv', sep = ',', header = TRUE)
age_marg_known <- age_marg_known %>% select(-country)
age_marg_known <- left_join(age_marg_known, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
age_marg_known <- age_marg_known[age_marg_known$country == countryname,]
age_marg_known <- age_marg_known %>% select(-country)

age_marg_known <- pivot_longer(age_marg_known, cols = c(X1, X2, X3, X4, X5, X6, X7), names_to = "AGECAT", values_to = "Frequency")
age_marg_known$AGECAT <- as.integer(as.factor(age_marg_known$AGECAT))

age_marg_est_tib <- as_tibble(age_marg_known)


# gender
gender_marg_est <- read.csv('Est_marginals_GENDER_sept23.csv', sep = ',', header = TRUE)
gender_marg_known <- read.csv('IPUMS_gender_observedmarginals_sept23.csv', sep = ',', header = TRUE)
gender_marg_known <- gender_marg_known %>% select(-country)
gender_marg_known <- left_join(gender_marg_known, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
gender_marg_known <- gender_marg_known[gender_marg_known$country == countryname,]
gender_marg_known <- gender_marg_known %>% select(-country)

gender_marg_known <- pivot_longer(gender_marg_known, cols = c(X1, X2), names_to = "GENDER", values_to = "Frequency")
gender_marg_known$GENDER <- as.integer(as.factor(gender_marg_known$GENDER))
gender_marg_known$GENDER <- gender_marg_known$GENDER -1

gender_marg_est_tib <- as_tibble(gender_marg_known)


# educat
educat_marg_est <- read.csv('Est_marginals_EDUCAT_sept23.csv', sep = ',', header = TRUE)
educat_marg_known <- read.csv('IPUMS_edu_observedmarginals_sept23.csv', sep = ',', header = TRUE)
educat_marg_known <- educat_marg_known %>% select(-country)
educat_marg_known <- left_join(educat_marg_known, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
educat_marg_known <- educat_marg_known[educat_marg_known$country == countryname,]
educat_marg_known <- educat_marg_known %>% select(-country)

educat_marg_known <- pivot_longer(educat_marg_known, cols = c(X1, X2, X3, X4, X5), names_to = "EDUCAT", values_to = "Frequency")
educat_marg_known$EDUCAT <- as.integer(as.factor(educat_marg_known$EDUCAT))

educat_marg_est_tib <- as_tibble(educat_marg_known)




# NEW RURAL MARGINAL
SMODmarg <- read.csv('individual_marginals_UrbanRural.csv', sep = ';', header = TRUE)
GDL_pop <- read.csv('GDL_subnational_population_world.csv', head = TRUE, sep = ',')


SMODmarg <- SMODmarg %>% rename('GDLCODE' = 'GDLcode')

SMODmarg_country <- SMODmarg[SMODmarg$country == countryname,]

ruralmargSMOD0 <- SMODmarg_country %>% select(GDLCODE, Urban_percent)
ruralmargSMOD1 <- SMODmarg_country %>% select(GDLCODE, Rural_percent)

sum_indi <- gender_marg_est_tib %>% group_by(GDLCODE) %>% summarise(Population = sum(Frequency))

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




###################################################
rural_tib <- as_tibble(ruralmargSMOD)
#rural_tib <- rural_tib[rural_tib$GEOLEV1 %ni% remove_regions ,]


print('ruraltib')
print(unique(rural_tib$GDLCODE))


regionnumbers <- unique(rural_tib$GDLCODE) #not integers, otherwise we can't match them with GDL.  


jointhead100small[, PID := 1:.N] #was jointhead10. 

nr_individuals_per_regio <- c()

error_region <- c()



##########################################################
# Remove mismatch reference sample (survey) and control sample (marginals)
#hhsize, hhtype and educat. 


survey_hhsize_cats <- unique(jointhead100small$HHSIZECAT)
marg_hhsize_cats <- unique(hhsize_marg_est_tib$HHSIZECAT)

if (length(marg_hhsize_cats) < length(survey_hhsize_cats)){
  missingcat <- setdiff(survey_hhsize_cats, marg_hhsize_cats)
  missingcat <- c(missingcat)
  jointhead100small_missingcat <- jointhead100small[jointhead100small$HHSIZECAT == missingcat ,]
  missingcat_hid <- unique(jointhead100small_missingcat$HID)
  
  jointhead100small <- jointhead100small[jointhead100small$HID %ni% missingcat_hid]
}


if (length(marg_hhsize_cats) > length(survey_hhsize_cats)){
  missingcat <- setdiff(marg_hhsize_cats, survey_hhsize_cats)
  missingcat <- c(missingcat)
  OG_weights <- hhsize_marg_est_tib %>% group_by(GEOLEV1) %>% summarise(OG_Frequency = sum(Frequency))
  hhsize_marg_est_tib <- hhsize_marg_est_tib[hhsize_marg_est_tib$HHSIZECAT %ni% missingcat,]
  NEW_weights <- hhsize_marg_est_tib %>% group_by(GEOLEV1) %>% summarise(NEW_Frequency = sum(Frequency))
  hhsize_marg_est_tib <- left_join(hhsize_marg_est_tib, OG_weights, by = 'GEOLEV1')
  hhsize_marg_est_tib <- left_join(hhsize_marg_est_tib, NEW_weights, by = 'GEOLEV1')
  hhsize_marg_est_tib$correction_factor <- hhsize_marg_est_tib$OG_Frequency / hhsize_marg_est_tib$NEW_Frequency
  hhsize_marg_est_tib$Frequency <- hhsize_marg_est_tib$Frequency * hhsize_marg_est_tib$correction_factor
  
  hhsize_marg_est_tib <- hhsize_marg_est_tib %>% select(-OG_Frequency, -NEW_Frequency, -correction_factor)
}



survey_edu_cats <- unique(jointhead100small$EDUCAT)
marg_edu_cats <- unique(educat_marg_est_tib$EDUCAT)

if (length(marg_edu_cats) < length(survey_edu_cats)){
  missingcat <- setdiff(survey_edu_cats, marg_edu_cats)
  missingcat <- c(missingcat)
  jointhead100small_missingcat <- jointhead100small[jointhead100small$EDUCAT %in% missingcat ,]
  missingcat_hid <- unique(jointhead100small_missingcat$HID)
  
  jointhead100small <- jointhead100small[!jointhead100small$HID %in% missingcat_hid ,]
}


if (length(marg_edu_cats) > length(survey_edu_cats)){
  missingcat <- setdiff(marg_edu_cats, survey_edu_cats)
  missingcat <- c(missingcat)
  OG_weights <- educat_marg_est_tib %>% group_by(GEOLEV1) %>% summarise(OG_Frequency = sum(Frequency))
  educat_marg_est_tib <- educat_marg_est_tib[educat_marg_est_tib$EDUCAT %ni% missingcat,]
  NEW_weights <- educat_marg_est_tib %>% group_by(GEOLEV1) %>% summarise(NEW_Frequency = sum(Frequency))
  educat_marg_est_tib <- left_join(educat_marg_est_tib, OG_weights, by = 'GEOLEV1')
  educat_marg_est_tib <- left_join(educat_marg_est_tib, NEW_weights, by = 'GEOLEV1')
  educat_marg_est_tib$correction_factor <- educat_marg_est_tib$OG_Frequency / educat_marg_est_tib$NEW_Frequency
  educat_marg_est_tib$Frequency <- educat_marg_est_tib$Frequency * educat_marg_est_tib$correction_factor
  
  educat_marg_est_tib <- educat_marg_est_tib %>% select(-OG_Frequency, -NEW_Frequency, -correction_factor)
}



survey_hhtype_cats <- unique(jointhead100small$HHTYPE)
marg_hhtype_cats <- unique(hhtype_marg_est_tib$HHTYPE)

if (length(marg_hhtype_cats) < length(survey_hhtype_cats)){
  missingcat <- setdiff(survey_hhtype_cats, marg_hhtype_cats)
  missingcat <- c(missingcat)
  jointhead100small_missingcat <- jointhead100small[jointhead100small$HHTYPE %in% missingcat ,]
  missingcat_hid <- unique(jointhead100small_missingcat$HID)
  
  jointhead100small <- jointhead100small[!jointhead100small$HID %in% missingcat_hid ,]
}


if (length(marg_hhtype_cats) > length(survey_hhtype_cats)){
  missingcat <- setdiff(marg_hhtype_cats, survey_hhtype_cats)
  missingcat <- c(missingcat)
  OG_weights <- hhtype_marg_est_tib %>% group_by(GEOLEV1) %>% summarise(OG_Frequency = sum(Frequency))
  hhtype_marg_est_tib <- hhtype_marg_est_tib[hhtype_marg_est_tib$HHTYPE %ni% missingcat,]
  NEW_weights <- hhtype_marg_est_tib %>% group_by(GEOLEV1) %>% summarise(NEW_Frequency = sum(Frequency))
  hhtype_marg_est_tib <- left_join(hhtype_marg_est_tib, OG_weights, by = 'GEOLEV1')
  hhtype_marg_est_tib <- left_join(hhtype_marg_est_tib, NEW_weights, by = 'GEOLEV1')
  hhtype_marg_est_tib$correction_factor <- hhtype_marg_est_tib$OG_Frequency / hhtype_marg_est_tib$NEW_Frequency
  hhtype_marg_est_tib$Frequency <- hhtype_marg_est_tib$Frequency * hhtype_marg_est_tib$correction_factor
  
  hhtype_marg_est_tib <- hhtype_marg_est_tib %>% select(-OG_Frequency, -NEW_Frequency, -correction_factor)
}


ruralcountrydf <- as.data.frame(rural_tib) %>% group_by(RURAL) %>% summarize(Frequency = sum(Frequency))
ruralcountry_tib <- as_tibble(ruralcountrydf)

hhtypecountrydf <- as.data.frame(hhtype_marg_est_tib) %>% group_by(HHTYPE) %>% summarize(Frequency = sum(Frequency))
hhtypecountry_tib <- as_tibble(hhtypecountrydf)

hhsizecountrydf <- as.data.frame(hhsize_marg_est_tib) %>% group_by(HHSIZECAT) %>% summarize(Frequency = sum(Frequency))
hhsizecountry_tib <- as_tibble(hhsizecountrydf)

agecountrydf <- as.data.frame(age_marg_est_tib) %>% group_by(AGECAT) %>% summarize(Frequency = sum(Frequency))
agecountry_tib <- as_tibble(agecountrydf)

educountrydf <- as.data.frame(educat_marg_est_tib) %>% group_by(EDUCAT) %>% summarize(Frequency = sum(Frequency))
educountry_tib <- as_tibble(educountrydf)

sexcountrydf <- as.data.frame(gender_marg_est_tib) %>% group_by(GENDER) %>% summarize(Frequency = sum(Frequency))
sexcountry_tib <- as_tibble(sexcountrydf)

sumfreq5 <- (sum(ruralcountry_tib$Frequency))/5
incomecountrymargdf <- data.frame('INCOME' = c(1,2,3,4,5), 'Frequency' = c(sumfreq5, sumfreq5, sumfreq5, sumfreq5, sumfreq5))
incomecountrymargdf$INCOME <- as.factor(incomecountrymargdf$INCOME)

incomecountry_tib <- as_tibble(incomecountrymargdf)


group_control <- list( incomecountry_tib)
individual_control <- list(hhtypecountry_tib, hhsizecountry_tib, ruralcountry_tib, agecountry_tib, educountry_tib, sexcountry_tib)

names(group_control) <- c('INCOME') 
names(individual_control) <- c('HHTYPE', 'HHSIZECAT', 'RURAL','AGECAT','EDUCAT', 'GENDER')


ml_fitWEIGHTS <- numeric(nrow(jointhead100small)) + 1

fitting_problem <- ml_problem(
  ref_sample = jointhead100small, 
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



GEOLEV1regions <- unique(rural_tib$GDLCODE)


nr_individuals_per_regio = c()

syn_pop <- data.table()



for (regnr in GEOLEV1regions) {
  
  print(regnr)
  
  rural_tib1reg <- rural_tib[rural_tib$GDLCODE == regnr,]
  rural_tib1reg <- rural_tib1reg %>% select(-GDLCODE)
  
  hhtype_tib1reg <- hhtype_marg_est_tib[hhtype_marg_est_tib$GDLCODE == regnr,]
  hhtype_tib1reg <- hhtype_tib1reg %>% select(-GDLCODE)
  
  hhsize_tib1reg <- hhsize_marg_est_tib[hhsize_marg_est_tib$GDLCODE == regnr,]
  hhsize_tib1reg <- hhsize_tib1reg %>% select(-GDLCODE)
  
  age_tib1reg <- age_marg_est_tib[age_marg_est_tib$GDLCODE == regnr,]
  age_tib1reg <- age_tib1reg %>% select(-GDLCODE)
  
  edu_tib1reg <- educat_marg_est_tib[educat_marg_est_tib$GDLCODE == regnr,]
  edu_tib1reg <- edu_tib1reg %>% select(-GDLCODE)
  
  sex_tib1reg <- gender_marg_est_tib[gender_marg_est_tib$GDLCODE == regnr,]
  sex_tib1reg <- sex_tib1reg %>% select(-GDLCODE)
  
  
  group_control <- list()
  individual_control <- list(hhtype_tib1reg, hhsize_tib1reg, rural_tib1reg, age_tib1reg, edu_tib1reg, sex_tib1reg)
  
  
  
  names(group_control) <- c() 
  names(individual_control) <- c('HHTYPE', 'HHSIZECAT', 'RURAL','AGECAT','EDUCAT', 'GENDER')
  
  
  ml_fitWEIGHTS <- fitcountry$weights
  
  fitting_problem <- ml_problem(
    ref_sample = jointhead100small, 
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
  
  fit <- ml_fit(ml_problem = fitting_problem, algorithm = "ipu", maxiter = 50) #was 50
  
  syn_pop_reg <- ml_replicate(fit, algorithm = "trs")
  
  
  print(nrow(syn_pop_reg))
  
  #regnrnr <- substr(regnr, nchar(regnr) - 2, nchar(regnr))
  
  syn_pop_reg <- syn_pop_reg %>% add_column(GDLCODE = regnr)
  gc()
  syn_pop <- rbindlist(list(syn_pop, syn_pop_reg), use.names=TRUE)
  rm(syn_pop_reg)
  
  
  gc()
  
}





synpophead <- syn_pop[syn_pop$RELATE == 1,]
tableinc <- table(synpophead$INCOME)
tableinc / sum(tableinc)


# per regio, percentages INCOME groepen uitrekenen. 
INCOMEfreq_regio <- data.frame(xtabs(~INCOME + GDLCODE, data = synpophead))
INCOMEfreq_regio$INCOME <- as.numeric(INCOMEfreq_regio$INCOME)
print(INCOMEfreq_regio)

mp_w1 <- 1-((tableinc[1]/sum(tableinc))-0.2)/(tableinc[1]/sum(tableinc))
mp_w2 <- 1-((tableinc[2]/sum(tableinc))-0.2)/(tableinc[2]/sum(tableinc))
mp_w3 <- 1-((tableinc[3]/sum(tableinc))-0.2)/(tableinc[3]/sum(tableinc))
mp_w4 <- 1-((tableinc[4]/sum(tableinc))-0.2)/(tableinc[4]/sum(tableinc))
mp_w5 <- 1-((tableinc[5]/sum(tableinc))-0.2)/(tableinc[5]/sum(tableinc))
print(mp_w1)


mp_df <- data.frame('INCOME' = c(1,2,3,4,5), 'mp' = c(mp_w1,mp_w2,mp_w3,mp_w4,mp_w5))
mp_df$INCOME <- as.numeric(mp_df$INCOME)

print(mp_df)

#INCOMEfreq_regio <- merge(INCOMEfreq_regio, mp_df, on = 'INCOME', how = 'left')

INCOMEfreq_regio <- left_join(INCOMEfreq_regio, mp_df, by = 'INCOME', multiple = 'all')
INCOMEfreq_regio['Frequency'] <- INCOMEfreq_regio$Freq*INCOMEfreq_regio$mp
INCOMEfreq_regio <- INCOMEfreq_regio %>% select(-Freq)
INCOMEfreq_regio <- INCOMEfreq_regio %>% select(GDLCODE, INCOME, Frequency)
INCOME_tib <- as_tibble(INCOMEfreq_regio)



INCOME_tib <- as_tibble(INCOMEfreq_regio %>% select(GDLCODE, INCOME, Frequency))


# for loop with regions. Now with INCOME marginal.  
nr_individuals_per_regio = c()

syn_pop <- data.table()


# rural is individual geworden. 


for (regnr in GEOLEV1regions) {
  
  print(regnr)
  INCOME_tib1reg <- INCOME_tib[INCOME_tib$GDLCODE == regnr,]
  INCOME_tib1reg <- INCOME_tib1reg %>% select(-GDLCODE)
  
  rural_tib1reg <- rural_tib[rural_tib$GDLCODE == regnr,]
  rural_tib1reg <- rural_tib1reg %>% select(-GDLCODE)
  
  hhsize_tib1reg <- hhsize_marg_est_tib[hhsize_marg_est_tib$GDLCODE == regnr,]
  hhsize_tib1reg <- hhsize_tib1reg %>% select(-GDLCODE)
  
  #correction INCOME
  #hhsizesumfreq <- sum(hhsize_tib1reg$Frequency)
  #INCOMEsumfreq <- sum(INCOME_tib1reg$Frequency)
  
  #correction_INCOME <- hhsizesumfreq/INCOMEsumfreq
  
  #INCOME_tib1reg$Frequency <- INCOME_tib1reg$Frequency*correction_INCOME
  
  hhtype_tib1reg <- hhtype_marg_est_tib[hhtype_marg_est_tib$GDLCODE == regnr,]
  hhtype_tib1reg <- hhtype_tib1reg %>% select(-GDLCODE)
  
  
  age_tib1reg <- age_marg_est_tib[age_marg_est_tib$GDLCODE == regnr,]
  age_tib1reg <- age_tib1reg %>% select(-GDLCODE)
  
  edu_tib1reg <- educat_marg_est_tib[educat_marg_est_tib$GDLCODE == regnr,]
  edu_tib1reg <- edu_tib1reg %>% select(-GDLCODE)
  
  sex_tib1reg <- gender_marg_est_tib[gender_marg_est_tib$GDLCODE == regnr,]
  sex_tib1reg <- sex_tib1reg %>% select(-GDLCODE)
  
  group_control <- list(INCOME_tib1reg)
  individual_control <- list(hhtype_tib1reg, hhsize_tib1reg, rural_tib1reg, age_tib1reg, edu_tib1reg, sex_tib1reg)
  
  # rural is indivodual here. 
  
  names(group_control) <- c('INCOME') 
  names(individual_control) <- c('HHTYPE', 'HHSIZECAT', 'RURAL','AGECAT','EDUCAT', 'GENDER')
  
  
  ml_fitWEIGHTS <- fitcountry$weights
  
  fitting_problem <- ml_problem(
    ref_sample = jointhead100small, 
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
  
  fitweights <- fit$weights
  
  fitweights <- fitweights[fitweights>0]
  
  syn_pop_reg <- ml_replicate(fit, algorithm = "trs")
  
  popsize_syn <- nrow(syn_pop_reg)
  
  GDL_region <- GDLcountry[GDLcountry$GDLCODE == regnr,]
  
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
  
  
  syn_pop_reg$INCOME <- as.integer(syn_pop_reg$INCOME)
  syn_pop_reg$RURAL <- as.integer(syn_pop_reg$RURAL)
  syn_pop_reg$FARMING <- as.integer(syn_pop_reg$FARMING)
  syn_pop_reg$AGECAT <- as.integer(syn_pop_reg$AGECAT)
  syn_pop_reg$GENDER <- as.integer(syn_pop_reg$GENDER)
  syn_pop_reg$EDUCAT <- as.integer(syn_pop_reg$EDUCAT)
  syn_pop_reg$HHTYPE <- as.integer(syn_pop_reg$HHTYPE)
  syn_pop_reg$HID <- as.integer(syn_pop_reg$HID)
  syn_pop_reg$RELATE <- as.integer(syn_pop_reg$RELATE)
  syn_pop_reg$HHSIZECAT <- as.integer(syn_pop_reg$HHSIZECAT)
  
  
  # Save synthetic population
  
  name = paste0(isocode, '_knownmarg_oct23_synthpop_', as.character(regnr), '.dat')
  
  con = file(name, "wb")
  
  writeBin(c(syn_pop_reg$INCOME, syn_pop_reg$RURAL, syn_pop_reg$FARMING, syn_pop_reg$AGECAT, syn_pop_reg$GENDER, 
             syn_pop_reg$EDUCAT, syn_pop_reg$HHTYPE, syn_pop_reg$HID, syn_pop_reg$RELATE, syn_pop_reg$HHSIZECAT), con)
  
  #writeBin(c(syn_pop$INCOME, syn_pop$PID), con)
  
  
  close(con)
  
  
  syn_pop_reg <- syn_pop_reg %>% add_column(GDLCODE = regnr)
  gc()
  syn_pop <- rbindlist(list(syn_pop, syn_pop_reg), use.names=TRUE)
  rm(syn_pop_reg)
  
  
  gc()
  
  
}





name_individualfile <- paste0('length_knownmarg_', isocode, '.csv') 
DF_nr_individuals_per_region <- data.frame('Country' = rep(isocode, length(GEOLEV1regions)), 'GDLCODE' = GEOLEV1regions, 'Nr_individuals' = nr_individuals_per_regio)

write.csv(DF_nr_individuals_per_region, name_individualfile, row.names = FALSE)




