library(dplyr)
dfh <- read.LIS('dk16h')
dfp <- read.LIS('dk16p')
print(colnames(dfh))
print(colnames(dfp))
dfh_s <- dfh %>% select(hid, dhi, hpopwgt, region_c, rural, farming, hhtype, nhhmem, nhhmem17, nhhmem65)
dfp_s <- dfp %>% select(hid, pid, ppopwgt, educlev, relation, sex, age, enroll)
dfh_s['dhipos'] <- dfh_s$dhi
dfh_s <- dfh_s[!is.na(dfh_s$hhtype),]
print(nrow(dfh_s))
dfh_s <- dfh_s[!is.na(dfh_s$hpopwgt),]
print(nrow(dfh_s))
data_subset <- dfh_s[ , c('hpopwgt')] 
dfh_s <- dfh_s[complete.cases(data_subset), ]
print(nrow(dfh_s))
dfh_s$hpopwgt[is.na(dfh_s$hpopwgt)] <- 0
print(nrow(dfh_s))
dfh_s <- dfh_s[! dfh_s$hpopwgt <= 0,]
print(nrow(dfh_s))
print(str(dfh_s$hpopwgt))
dfp_s <- dfp_s[!is.na(dfp_s$ppopwgt),]
dfh_s$dhipos[is.na(dfh_s$dhipos)] <- 0
print(nrow(dfh_s))
dfh_s <- dfh_s %>% 
  mutate(hpopwgt = ifelse(hpopwgt == "NA",NA,hpopwgt )) %>%
  filter(!is.na(hpopwgt))
print(nrow(dfh_s))
print(str(dfh_s$dhipos))
print(max(dfh_s$dhipos))
print(min(dfh_s$dhipos))
print(median(dfh_s$dhipos))
print(max(dfh_s$hpopwgt, na.rm = TRUE))
print(min(dfh_s$hpopwgt))
print(median(dfh_s$hpopwgt))
dfp_s <- dfp_s %>%
  filter_at(vars(age, sex), all_vars(!is.na(.)))
dfp_s$educlev_string <- as.character(dfp_s$educlev)
dfp_s$educlev_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfp_s$educlev_string)) 
dfp_s$educlev_string <- as.character(dfp_s$educlev)
dfp_s$educlev_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfp_s$educlev_string)) 
dfp_s$education <- 1
dfp_s$education[dfp_s$educlev_number == 100] <- 3 #vague category, 3. 
dfp_s$education[dfp_s$educlev_number == 111] <- 1 #no education
dfp_s$education[dfp_s$educlev_number == 110] <- 1 #incomplete primary
dfp_s$education[dfp_s$educlev_number == 120] <- 2 #complete primary
dfp_s$education[dfp_s$educlev_number == 130] <- 3 #incomplete secondary
dfp_s$education[dfp_s$educlev_number >= 200 & dfp_s$educlev_number < 300] <- 4 #complete secondary. complete middelbare school en MBO. 
dfp_s$education[dfp_s$educlev_number >= 300] <- 5 #higher. hbo en wo verschillen maken. 
dfp_s$relation_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfp_s$relation)) #1000 is head
dfp_s$sex <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfp_s$sex)) #1 is male, 2 is female. 
dfp_s$sex_number <- 1
dfp_s$sex_number[dfp_s$sex == 2] <- 0
dfp_s$enroll <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfp_s$enroll)) 
dfh_s$hhtype_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfh_s$hhtype)) #hhtypes 
dfh_s$nhhmem_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfh_s$nhhmem)) #hh size 
dfh_s$nhhmem17_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfh_s$nhhmem17)) #nr of children 
dfh_s$nhhmem65_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfh_s$nhhmem65)) #nr of elderly
dfh_s$rural_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfh_s$rural)) #rural = 1
#dfh_s$farming_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfh_s$farming)) #farming = 1
dfh_s['farming_number'] <- 0
dfh_s$farming <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfh_s$farming)) #farming = 1
dfh_s$farming_number[dfh_s$farming == 1] <- 1
print(table(dfh_s$farming_number))
farmunique <- unique(dfh_s$farming_number)
print(farmunique)
if (length(farmunique) <= 1){
  farmavailable = 'NO'
} else{
  farmavailable = 'YES'
}
print(farmavailable)
#FARMSTATEMENT
if (farmavailable == "NO"){
  dfh_s$farming_number <- sample(c(0,1), size = nrow(dfh_s), prob=c(0.5, 0.5), replace = T) 
}
dfh_s$region_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfh_s$region_c))
dfh_s$region_number[dfh_s$region_number %in% c(101,147,151,153,155,157,159,161,163,165,167,169,173,175,183,185,187,190,201,210,217,219,223,230,240,250,260,270,400)] <- 101
dfh_s$region_number[dfh_s$region_number %in% c(253,259,265,269,306,316,320,326,329,330,336,340,350,360,370,376,390)] <- 253
dfh_s$region_number[dfh_s$region_number %in% c(410,420,430,440,450,461,479,480,482,492,510,530,540,550,561,563,573,575,580,607,621,630)] <- 410
dfh_s$region_number[dfh_s$region_number %in% c(615,657,661,665,671,706,707,710,727,730,740,741,746,751,756,760,766,779,791)] <- 615
dfh_s$region_number[dfh_s$region_number %in% c(773,787,810,813,820,825,840,846,849,851,860)] <- 773
print(unique(dfh_s$region_number))
print(unique(dfh_s$region_c))
# einde regionamen
dfh_s$hhtype_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfh_s$hhtype))
dfp_s['AGECAT'] <- 0
dfp_s$AGECAT[dfp_s$age %in% (0:4)] <- 1
dfp_s$AGECAT[dfp_s$age %in% (5:14)] <- 2
dfp_s$AGECAT[dfp_s$age %in% (15:24)] <- 3
dfp_s$AGECAT[dfp_s$age %in% (25:34)] <- 4
dfp_s$AGECAT[dfp_s$age %in% (35:44)] <- 5
dfp_s$AGECAT[dfp_s$age %in% (45:54)] <- 6
dfp_s$AGECAT[dfp_s$age %in% (55:64)] <- 7
dfp_s$AGECAT[dfp_s$age > 64] <- 8 
dfmerg <- left_join(dfp_s, dfh_s, by = 'hid')
dfmerg$relation_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfmerg$relation)) #1000 is head
print(nrow(dfmerg))
newhhmembers <- dfmerg %>% count(hid)  
dfmerg <- left_join(dfmerg, newhhmembers, by = 'hid')
dfmerg$hpopwgt[is.na(dfmerg$hpopwgt)] <- 0
incomeweightedvector <- rep(dfmerg$dhipos, dfmerg$hpopwgt, na.rm = TRUE)
groups5_income <- quantile(incomeweightedvector, probs = c(0.2, 0.4, 0.6, 0.8))
dfmerg$dhipos <- as.integer(dfmerg$dhipos)
dfmerg["INCOMEQQ"] <- 0
dfmerg$INCOMEQQ[dfmerg$dhipos <=  groups5_income[1]] <- 1
dfmerg$INCOMEQQ[dfmerg$dhipos > groups5_income[1] & dfmerg$dhipos <= groups5_income[2]] <- 2
dfmerg$INCOMEQQ[dfmerg$dhipos > groups5_income[2] & dfmerg$dhipos <= groups5_income[3]] <- 3
dfmerg$INCOMEQQ[dfmerg$dhipos > groups5_income[3] & dfmerg$dhipos <= groups5_income[4]] <- 4
dfmerg$INCOMEQQ[dfmerg$dhipos >  groups5_income[4]] <- 5 
dfmerg<- dfmerg[dfmerg$INCOMEQQ > 0,]
dfmerg$n <- as.numeric(dfmerg$n) 
dfmerg_l1 <- nrow(dfmerg)
sum_hpop_l1 <- sum(dfmerg$hpopwgt, na.rm = TRUE)
sum_ppop_l1 <- sum(dfmerg$ppopwgt, na.rm = TRUE)
dfmerg <- dfmerg %>% filter(nhhmem_number == n) #hiermee halen we hele households weg die een missing age of gender hebben.
print(nrow(dfmerg))
dfmerg_l2 <- nrow(dfmerg)
sum_hpop_l2 <- sum(dfmerg$hpopwgt, na.rm = TRUE)
sum_ppop_l2 <- sum(dfmerg$ppopwgt, na.rm = TRUE)
if (dfmerg_l2 < dfmerg_l1){
  
  dfmerg$hpopwgt <- dfmerg$hpopwgt * (sum_hpop_l1/sum_hpop_l2)
  dfmerg$ppopwgt <- dfmerg$ppopwgt * (sum_ppop_l1/sum_ppop_l2)
}
# Hsizecat. 
dfmerg['HHSIZECAT'] <- 0
dfmerg$HHSIZECAT[dfmerg$n == 1] <- 1
dfmerg$HHSIZECAT[dfmerg$n == 2] <- 2
dfmerg$HHSIZECAT[dfmerg$n == 3] <- 3
dfmerg$HHSIZECAT[dfmerg$n == 4] <- 3
dfmerg$HHSIZECAT[dfmerg$n == 5] <- 4
dfmerg$HHSIZECAT[dfmerg$n == 6] <- 4
dfmerg$HHSIZECAT[dfmerg$n %in% c(7,8,9,10)] <- 5
dfmerg$HHSIZECAT[dfmerg$n > 10] <- 6
dfmerg$hhtype_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfmerg$hhtype))
dfmerghead <- dfmerg %>% filter(relation_number == 1000)
dfmerghead <- dfmerghead %>%
  rename(age_head = AGECAT, sex_head = sex_number, educ_head = education) 
dfmerghead_s <- dfmerghead %>% select(hid, age_head, sex_head, educ_head)
dfmerg <- left_join(dfmerg, dfmerghead_s, by = 'hid')
dfmergpartner <- dfmerg %>% filter(relation_number >= 2000 & relation_number < 3000)
dfmergpartner <- dfmergpartner %>%
  rename(age_part = AGECAT, sex_part = sex_number, educ_part = education) 
dfmergpartner_s <- dfmergpartner %>% select(hid, age_part, sex_part, educ_part)
dfmerg <- left_join(dfmerg, dfmergpartner_s, by = 'hid')
dfmerg$childdummy <- 0 
dfmerg$childdummy[dfmerg$relation_number >= 3000 & dfmerg$relation_number < 4000] <- 1
#dfmerg$childdummy[dfmerg$relation_number == 4110] <- 1
#dfmerg$childdummy[dfmerg$relation_number == 4120] <- 1
#dfmerg$childdummy[dfmerg$relation_number == 4160] <- 1
dfmerg$relativedummy <- 0 #other is nu relative
dfmerg$relativedummy[dfmerg$relation_number >= 4000 & dfmerg$relation_number <= 4170] <- 1
#dfmerg$relativedummy[dfmerg$relation_number >= 4170] <- 1
#dfmerg$relativedummy[dfmerg$relation_number == 4000] <- 1
#dfmerg$relativedummy[dfmerg$relation_number == 4100] <- 1
dfmerg$nonrelativedummy <- 0 
dfmerg$nonrelativedummy[dfmerg$relation_number >= 4200] <- 1
if (length(unique(dfmerg$nonrelativedummy)) <= 1){
  print('nononrelatives')
} else{
  print('yesnonrelatives')
} #presence nonrelatives
uniquenonreldum <- unique(dfmerg$nonrelativedummy)
print(length(uniquenonreldum))
nr_children <- aggregate(dfmerg$childdummy, by = list(dfmerg$hid), sum)
nr_relative <- aggregate(dfmerg$relativedummy, by = list(dfmerg$hid), sum)
nr_nonrelative <- aggregate(dfmerg$nonrelativedummy, by = list(dfmerg$hid), sum)
nr_children <- nr_children %>%
  rename(hid = Group.1, nr_children = x)
nr_relative <- nr_relative %>%
  rename(hid = Group.1, nr_relative = x)
nr_nonrelative <- nr_nonrelative %>%
  rename(hid = Group.1, nr_nonrelative = x)
dfmerg <- left_join(dfmerg, nr_children, by = 'hid')
dfmerg <- left_join(dfmerg, nr_relative, by = 'hid')
dfmerg <- left_join(dfmerg, nr_nonrelative, by = 'hid')
dfmerg_hh <- dfmerg[dfmerg$relation_number == 1000,]
dfmerg_hh_child4 <- dfmerg_hh[dfmerg_hh$nr_children > 3,]
print(nrow(dfmerg_hh_child4))
print(xtabs(hpopwgt~nr_children, data = dfmerg_hh_child4)/sum(dfmerg_hh_child4$hpopwgt, na.rm = TRUE))
dfmerg_hh_relative3 <- dfmerg_hh[dfmerg_hh$nr_relative > 2,]
print(xtabs(hpopwgt~nr_relative, data = dfmerg_hh_relative3)/sum(dfmerg_hh_relative3$hpopwgt, na.rm = TRUE))
dfmerg_hh_nonrelative2 <- dfmerg_hh[dfmerg_hh$nr_nonrelative > 1,]
if (nrow(dfmerg_hh_nonrelative2) > 0){
  print(xtabs(hpopwgt~nr_nonrelative, data = dfmerg_hh_nonrelative2)/sum(dfmerg_hh_nonrelative2$hpopwgt, na.rm = TRUE))
} #FORNONRELATIVESSMALLTABLE
dfmerg$CHILD_CAT <- 0
dfmerg$CHILD_CAT[dfmerg$nr_children == 1] <- 1
dfmerg$CHILD_CAT[dfmerg$nr_children == 2] <- 2
dfmerg$CHILD_CAT[dfmerg$nr_children == 3] <- 3
dfmerg$CHILD_CAT[dfmerg$nr_children > 3] <- 4
dfmerg$RELATIVE_CAT <- 0
dfmerg$RELATIVE_CAT[dfmerg$nr_relative == 1] <- 1
dfmerg$RELATIVE_CAT[dfmerg$nr_relative == 2] <- 2
dfmerg$RELATIVE_CAT[dfmerg$nr_relative > 2] <- 3
dfmerg$NONRELATIVE_CAT <- 0
dfmerg$NONRELATIVE_CAT[dfmerg$nr_nonrelative == 1] <- 1
dfmerg$NONRELATIVE_CAT[dfmerg$nr_nonrelative > 1] <- 2
dfmerg$HHTYPE_CAT <- 0
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 100] <- 1
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 210] <- 2
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 220] <- 3
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 230] <- 4
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 320] <- 6
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 520] <- 6
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 620] <- 6
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 920] <- 6
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 310] <- 5
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 510] <- 5
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 610] <- 5
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 910] <- 5
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 330] <- 7
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 530] <- 7
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 630] <- 7
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 930] <- 7
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 400] <- 8
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 700] <- 8
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 800] <- 8
dfmerg$HHTYPE_CAT[dfmerg$hhtype_number == 900] <- 8
dfmerg <- dfmerg[dfmerg$HHTYPE_CAT > 0,]
dfmerghhtypecat0 <- dfmerg %>% filter(HHTYPE_CAT == 0)
print(nrow(dfmerghhtypecat0))
dfmerghhtypecat0 <- dfmerg %>% filter(HHTYPE_CAT == 0)
print(nrow(dfmerghhtypecat0))
print(table(dfmerg$rural_number))
print(table(dfmerg$HHTYPE_CAT))
print(table(dfmerg$farming_number))
print(table(dfmerg$HHSIZECAT))
print(table(dfmerg$INCOMEQQ))
print(table(dfmerg$AGECAT))
print(table(dfmerg$education))
print(table(dfmerg$sex_number))
print(table(dfmerg$region_number))

print(table(dfmerg$rural_number))
print(table(dfmerg$HHTYPE_CAT))
print(table(dfmerg$farming_number))
print(table(dfmerg$HHSIZECAT))
print(table(dfmerg$INCOMEQQ))
print(table(dfmerg$AGECAT))
print(table(dfmerg$education))
print(table(dfmerg$sex_number))
print(table(dfmerg$region_number))
dfhead <- dfmerg[dfmerg$relation_number==1000,]
print((data.frame(xtabs(hpopwgt ~ rural_number + HHTYPE_CAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))
print((data.frame(xtabs(hpopwgt ~ rural_number + HHSIZECAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))
print((data.frame(xtabs(hpopwgt ~ rural_number + farming_number + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))
print((data.frame(xtabs(ppopwgt ~ rural_number + INCOMEQQ + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ rural_number + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ rural_number + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ rural_number + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(hpopwgt ~ farming_number + HHTYPE_CAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))
print((data.frame(xtabs(hpopwgt ~ farming_number + HHSIZECAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))
print((data.frame(xtabs(ppopwgt ~ farming_number + INCOMEQQ + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ farming_number + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ farming_number + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ farming_number + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + HHTYPE_CAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + HHSIZECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(hpopwgt ~ HHSIZECAT + HHTYPE_CAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))
print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ sex_number + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ sex_number + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ education + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
# Without regions
print((data.frame(xtabs(hpopwgt ~ rural_number + HHTYPE_CAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))
print((data.frame(xtabs(hpopwgt ~ rural_number + HHSIZECAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))
print((data.frame(xtabs(hpopwgt ~ rural_number + farming_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))
print((data.frame(xtabs(ppopwgt ~ rural_number + INCOMEQQ, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ rural_number + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ rural_number + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ rural_number + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(hpopwgt ~ farming_number + HHTYPE_CAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))
print((data.frame(xtabs(hpopwgt ~ farming_number + HHSIZECAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))
print((data.frame(xtabs(ppopwgt ~ farming_number + INCOMEQQ, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ farming_number + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ farming_number + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ farming_number + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + HHTYPE_CAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + HHSIZECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(hpopwgt ~ HHSIZECAT + HHTYPE_CAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))
print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ sex_number + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ sex_number + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
print((data.frame(xtabs(ppopwgt ~ education + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))
# EINDE