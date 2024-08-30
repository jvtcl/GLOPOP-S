library(dplyr)
dfh <- read.LIS('dk16h')
dfp <- read.LIS('dk16p')
print(colnames(dfh))
print(colnames(dfp))
dfh_s <- dfh %>% select(hid, dhi, hpopwgt, region_c, rural, farming, hhtype, nhhmem, nhhmem17, nhhmem65)
dfp_s <- dfp %>% select(hid, pid, ppopwgt, educlev, relation, sex, age, enroll)
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
dfh_s['dhipos'] <- dfh_s$dhi
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
dfp_s$AGECAT[dfp_s$age > 64] <- 8 #97 =
dfmerg <- left_join(dfp_s, dfh_s, by = 'hid')
dfmerg$hpopwgt[is.na(dfmerg$hpopwgt)] <- 0 
incomeweightedvector <- rep(dfmerg$dhipos, dfmerg$hpopwgt, na.rm = TRUE)
#incomeweightedvector_hh <- rep(dfmerg$dhipos, dfh_s$ppopwgt, na.rm = TRUE) #test dit
groups5_income <- quantile(incomeweightedvector, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
print(groups5_income)
#groups5_income_hh <- quantile(incomeweightedvector_hh, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
#print(groups5_income_hh)
dfmerg$dhipos <- as.integer(dfmerg$dhipos)
dfmerg["INCOMEQQ"] <- 0
dfmerg$INCOMEQQ[dfmerg$dhipos <=  groups5_income[1]] <- 1
dfmerg$INCOMEQQ[dfmerg$dhipos > groups5_income[1] & dfmerg$dhipos <= groups5_income[2]] <- 2
dfmerg$INCOMEQQ[dfmerg$dhipos > groups5_income[2] & dfmerg$dhipos <= groups5_income[3]] <- 3
dfmerg$INCOMEQQ[dfmerg$dhipos > groups5_income[3] & dfmerg$dhipos <= groups5_income[4]] <- 4
dfmerg$INCOMEQQ[dfmerg$dhipos >  groups5_income[4]] <- 5 
dfmerg<- dfmerg[dfmerg$INCOMEQQ > 0,]
dfmerg$relation_number <- as.integer(gsub(".*?([0-9]+).*", "\\1", dfmerg$relation)) #1000 is head
print(nrow(dfmerg))
newhhmembers <- dfmerg %>% count(hid)  
dfmerg <- left_join(dfmerg, newhhmembers, by = 'hid')
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
## SINGLE HOUSEHOLDS 100
df100 <- dfmerg %>% filter(HHTYPE_CAT == 1)
joint100.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + farming_number + age_head + sex_head + educ_head, data = df100 )
print(ftable(joint100.table))
df100sumwgt <- sum(df100$hpopwgt)
print(df100sumwgt)
## COUPLE HOUSEHOLDS 210
df210 <- dfmerg %>% filter(HHTYPE_CAT == 2)
df210head <- df210 %>% filter(relation_number == 1000)
df210part <- df210 %>% filter(relation_number != 1000)
print(nrow(df210part))
print(nrow(df210head))
print(nrow(df210))
df210multpart <- df210part[duplicated(df210part$hid),]
df210multpart_hid <- unique(df210multpart$hid)
df210head <- df210head[! df210head$hid %in% df210multpart_hid,]
df210part <- df210part[! df210part$hid %in% df210multpart_hid,]
df210 <- df210[! df210$hid %in% df210multpart_hid,]
joint210head.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + farming_number + age_head + sex_head + educ_head, data = df210head)
print(ftable(joint210head.table))
joint210partage.table <- xtabs(hpopwgt ~ educ_head + age_head + sex_head + age_part, data = df210part)
print(ftable(joint210partage.table))
joint210partsex.table <- xtabs(hpopwgt ~ educ_head + sex_head + age_part + sex_part, data = df210part)
print(ftable(joint210partsex.table))
joint210parteduc.table <- xtabs(hpopwgt ~ educ_head + sex_part + age_part + educ_part, data = df210part)
print(ftable(joint210parteduc.table))
## COUPLE WITH CHILDREN HOUSEHOLDS 220. Nu loopt ie vast. Loshalen hhtypes.  
df220 <- dfmerg %>% filter(HHTYPE_CAT == 3)
df220head <- df220 %>% filter(relation_number == 1000)
df220part <- df220 %>% filter(relation_number > 1000 & relation_number < 3000)
print(nrow(df220part))
print(nrow(df220head))
print(nrow(df220))
df220multpart <- df220part[duplicated(df220part$hid),]
df220multpart_hid <- unique(df220multpart$hid)
df220head <- df220head[! df220head$hid %in% df220multpart_hid,]
df220part <- df220part[! df220part$hid %in% df220multpart_hid,]
df220 <- df220[! df220$hid %in% df220multpart_hid,]
print(nrow(df220part))
print(nrow(df220head))
print(nrow(df220))
joint220head.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + farming_number + age_head + sex_head + educ_head, data = df220head)
print(ftable(joint220head.table))
joint220hhsize1.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + age_head + HHSIZECAT, data = df220head)
print(ftable(joint220hhsize1.table))
joint220partage.table <- xtabs(hpopwgt ~ educ_head + age_head + sex_head + age_part, data = df220part)
print(ftable(joint220partage.table))
joint220partsex.table <- xtabs(hpopwgt ~ educ_head + sex_head + age_part + sex_part, data = df220part)
print(ftable(joint220partsex.table))
joint220parteduc.table <- xtabs(hpopwgt ~ educ_head + sex_part + age_part + educ_part, data = df220part)
print(ftable(joint220parteduc.table))
joint220hhsize2.table <- xtabs(hpopwgt ~  CHILD_CAT + HHSIZECAT, data = df220head)
print(ftable(joint220hhsize2.table))
## ONE PARENT WITH CHILDREN 230
df230 <- dfmerg %>% filter(HHTYPE_CAT == 4)
df230head <- df230 %>% filter(relation_number == 1000)
joint230head.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + farming_number + age_head + sex_head + educ_head, data = df230head)
print(ftable(joint230head.table))
joint230hhsize1.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + age_head + HHSIZECAT, data = df230head)
print(ftable(joint230hhsize1.table))
joint230hhsize2.table <- xtabs(hpopwgt ~  CHILD_CAT + HHSIZECAT, data = df230head)
print(ftable(joint230hhsize2.table))
## COUPLE WITHOUT CHILDREN but with RELATIVES, NONRELATIVES AND OTHERS 310, 510, 610, 910
df310 <- dfmerg %>% filter(HHTYPE_CAT == 5)
df310head <- df310 %>% filter(relation_number == 1000)
df310part <- df310 %>% filter(relation_number > 1000 & relation_number < 3000)
print(nrow(df310part))
print(nrow(df310head))
print(nrow(df310))
df310multpart <- df310part[duplicated(df310part$hid),]
df310multpart_hid <- unique(df310multpart$hid)
df310head <- df310head[! df310head$hid %in% df310multpart_hid,]
df310part <- df310part[! df310part$hid %in% df310multpart_hid,]
df310 <- df310[! df310$hid %in% df310multpart_hid,]
joint310head.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + farming_number + age_head + sex_head + educ_head, data = df310head)
print(ftable(joint310head.table))
joint310hhsize1.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + age_head + HHSIZECAT, data = df310head)
print(ftable(joint310hhsize1.table))
joint310partage.table <- xtabs(hpopwgt ~ educ_head + age_head + sex_head + age_part, data = df310part)
print(ftable(joint310partage.table))
joint310partsex.table <- xtabs(hpopwgt ~ educ_head + sex_head + age_part + sex_part, data = df310part)
print(ftable(joint310partsex.table))
joint310parteduc.table <- xtabs(hpopwgt ~ educ_head + sex_part + age_part + educ_part, data = df310part)
print(ftable(joint310parteduc.table))
joint310hhsize2.table <- xtabs(hpopwgt ~  RELATIVE_CAT + NONRELATIVE_CAT + HHSIZECAT, data = df310head)
print(ftable(joint310hhsize2.table))
## COUPLE WITH CHILDREN, RELATIVES, NONRELATIVES AND OTHERS 320, 520, 620, 920
df320 <- dfmerg %>% filter(HHTYPE_CAT == 6)
df320head <- df320 %>% filter(relation_number == 1000)
df320part <- df320 %>% filter(relation_number > 1000 & relation_number < 3000)
print(nrow(df320part))
print(nrow(df320head))
print(nrow(df320))
df320multpart <- df320part[duplicated(df320part$hid),]
df320multpart_hid <- unique(df320multpart$hid)
df320head <- df320head[! df320head$hid %in% df320multpart_hid,]
df320part <- df320part[! df320part$hid %in% df320multpart_hid,]
df320 <- df320[! df320$hid %in% df320multpart_hid,]
print(nrow(df320part))
print(nrow(df320head))
print(nrow(df320))
df320part_sel <- df320part %>% select(hid, age_part, sex_part, educ_part)
df320head <- left_join(df320head, df320part_sel, by = 'hid')
joint320head.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + farming_number + age_head + sex_head + educ_head, data = df320head)
print(ftable(joint320head.table))
joint320hhsize1.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + age_head + HHSIZECAT, data = df320head)
print(ftable(joint320hhsize1.table))
joint320partage.table <- xtabs(hpopwgt ~ educ_head + age_head + sex_head + age_part, data = df320part)
print(ftable(joint320partage.table))
joint320partsex.table <- xtabs(hpopwgt ~ educ_head + sex_head + age_part + sex_part, data = df320part)
print(ftable(joint320partsex.table))
joint320parteduc.table <- xtabs(hpopwgt ~ educ_head + sex_part + age_part + educ_part, data = df320part)
print(ftable(joint320parteduc.table))
joint320hhsize2.table <- xtabs(hpopwgt ~ CHILD_CAT + RELATIVE_CAT + NONRELATIVE_CAT + HHSIZECAT, data = df320head)
print(ftable(joint320hhsize2.table))
## ONE PERSON WITH CHILDREN, RELATIVES, NONRELATIVES AND OTHERS 330, 530, 630, 930
df330 <- dfmerg %>% filter(HHTYPE_CAT == 7)
df330head <- df330 %>% filter(relation_number == 1000)
joint330head.table <- xtabs(hpopwgt ~  INCOMEQQ + rural_number + farming_number + age_head + sex_head + educ_head, data = df330head)
print(ftable(joint330head.table))
joint330hhsize1.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + age_head + HHSIZECAT, data = df330head)
print(ftable(joint330hhsize1.table))
joint330hhsize2.table <- xtabs(hpopwgt ~ CHILD_CAT + RELATIVE_CAT + NONRELATIVE_CAT + HHSIZECAT, data = df330head)
print(ftable(joint330hhsize2.table))
## RELATIVES, NONRELATIVES LIVING TOGETHER 400, 700, 800, 910 
df400 <- dfmerg %>% filter(HHTYPE_CAT == 8)
df400head <- df400 %>% filter(relation_number == 1000)
joint400head.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + farming_number + sex_head + age_head + educ_head, data = df400head)
print(ftable(joint400head.table))
joint400hhsize1.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + age_head + HHSIZECAT, data = df400head)
print(ftable(joint400hhsize1.table))
joint400hhsize2.table <- xtabs(hpopwgt ~ RELATIVE_CAT + NONRELATIVE_CAT + HHSIZECAT, data = df400head)
print(ftable(joint400hhsize2.table))
# general child.
dfchild <- dfmerg %>% filter(childdummy == 1) 
jointchildsex.table <- xtabs(ppopwgt ~ age_head + educ_head + sex_head + AGECAT + sex_number, data = dfchild)
print(ftable(jointchildsex.table))
jointchildeduc.table <- xtabs(ppopwgt ~ educ_head + AGECAT + sex_number + education, data = dfchild)
print(ftable(jointchildeduc.table))
# general relative
dfrelative <- dfmerg %>% filter(relativedummy == 1) 
jointrelativesex.table <- xtabs(ppopwgt ~ age_head + educ_head + AGECAT + sex_number, data = dfrelative)
print(ftable(jointrelativesex.table))
jointrelativeeduc.table <- xtabs(ppopwgt ~ educ_head + AGECAT + sex_number + education, data = dfrelative)
print(ftable(jointrelativeeduc.table))
# general nonrelative
if (length(uniquenonreldum) > 1){
  dfnonrelative <- dfmerg %>% filter(nonrelativedummy == 1) 
  
  jointnonrelativesex.table <- xtabs(ppopwgt ~ age_head + educ_head + AGECAT + sex_number, data = dfnonrelative)
  print(ftable(jointnonrelativesex.table))
  print('startnonrelative_educ')
  jointnonrelativeeduc.table <- xtabs(ppopwgt ~ educ_head + AGECAT + sex_number + education, data = dfnonrelative)
  print(ftable(jointnonrelativeeduc.table))
} else {
  print('NO NONRELATIVES')
} #printstatement nonrelatives
# MARGINALS NOG
##################### THE MARGINALS PER REGION ###########################
# households
dfhead <- dfmerg %>% filter(relation_number == 1000)
incomemarg.table =  xtabs(ppopwgt ~ region_number + INCOMEQQ, data = dfmerg)
ruralmarg.table = xtabs(hpopwgt ~ region_number + rural_number , data = dfhead)
farmingmarg.table = xtabs(hpopwgt ~ region_number + farming_number , data = dfhead)
hhtypemarg.table = xtabs(hpopwgt ~ region_number + HHTYPE_CAT, data = dfhead)
hhsizecatmarg.table = xtabs(hpopwgt ~ region_number + HHSIZECAT, data = dfhead)
ageheadmarg.table = xtabs(hpopwgt ~ region_number + age_head , data = dfhead)
educheadmarg.table = xtabs(hpopwgt ~ region_number + educ_head , data = dfhead)
sexheadmarg.table = xtabs(hpopwgt ~ region_number + sex_head , data = dfhead)
#hhsizemarg.table = xtabs(hpopwgt ~ nhhmem_number, data = dfhead)
# deze hhsize werkt niet.. 
# individuals
ageindimarg.table = xtabs(ppopwgt ~ region_number + AGECAT, data = dfmerg)
educindimarg.table = xtabs(ppopwgt ~ region_number + education, data = dfmerg)
sexindimarg.table = xtabs(ppopwgt ~ region_number + sex_number , data = dfmerg)
print(ftable(incomemarg.table))
print(ftable(ruralmarg.table))
print(ftable(farmingmarg.table))
print(ftable(hhtypemarg.table))
print(ftable(hhsizecatmarg.table))
print(ftable(ageheadmarg.table))
print(ftable(educheadmarg.table))
print(ftable(sexheadmarg.table))
print(ftable(ageindimarg.table))
print(ftable(educindimarg.table))
print(ftable(sexindimarg.table))
# BIJNA EINDE