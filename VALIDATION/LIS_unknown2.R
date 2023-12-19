#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)



isocode = as.character(args[1])

print(isocode)


####################################################################
## TWO UNKNOWNS (SURVEY AND MARGINAL), LIS COUNTRIES


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


countryname_to_iso <- read.csv('Countrynames_to_ISO.csv', sep = ';', header = TRUE)

countryname <- countryname_to_iso[countryname_to_iso$iso_code == isocode,]$country


liscodecountries <- read.csv('LIS_codes_countries2.csv', sep = ';')


if (countryname %in% liscodecountries$Country){
  liscodecountries1 <- liscodecountries[liscodecountries$Country == countryname ,]
  liscode <- liscodecountries1$LISCODE
  liscodeletters <- unlist(strsplit(liscode, ""))
  liscodeletters <- paste0(liscodeletters[1], liscodeletters[2])
}





# FUNCTIONS
region_names <- function(txtpath){
  x <- readLines(txtpath)
  #x <- iconv(x,to="ASCII//TRANSLIT")
  
  begin_pos <- grep('> print(unique(dfh_s$region_c))', x, fixed = TRUE)
  end_pos <- grep('# einde regionamen', x, fixed = TRUE)
  
  text <- x[begin_pos:end_pos]
  
  
  text <- text[- 1] 
  text <- text[- length(text)]  
  text <- text[- length(text)]
  
  region_numbers <- as.integer(gsub(".*?([0-9]+).*", "\\1", text))
  characters_inline1test = strsplit(text, split = " \\[")
  characters_inline1test = unlist(strsplit(unlist(characters_inline1test), split = "\\]"))
  characters_inline1test = gsub("\\[|\\]", "", characters_inline1test)
  
  onlynumbers_inline1test = is.na(as.numeric(characters_inline1test))
  
  characters_inline1test <- characters_inline1test[onlynumbers_inline1test]
  
  characters_inline1test = unlist(lapply(characters_inline1test, function(z){ z[!is.na(z) & z != ""]})) #remove empty strings.
  
  
  x <- readLines(txtpath)
  begin_pos <- grep("> print(unique(dfh_s$region_number))", x, fixed = TRUE)
  end_pos <- grep("> print(unique(dfh_s$region_c))", x, fixed = TRUE)
  
  text <- x[begin_pos:end_pos]
  
  
  text <- text[- 1] 
  text <- text[- length(text)]  
  
  region_numbers_string = gsub("\\[.*?\\]", "", text) #remove square brackets and everything within the brackets. 
  region_numbers_string = unlist(strsplit(region_numbers_string, split = " "))
  region_numbers_string = unlist(lapply(region_numbers_string, function(z){ z[!is.na(z) & z != ""]})) #remove empty strings.
  region_numbers = as.numeric(region_numbers_string)
  
  
  if (length(region_numbers) == length(characters_inline1test)){
    
    regiondf <- data.frame('region_number' = region_numbers, 'region_name' = characters_inline1test)
    
    regiondf['region_integer'] <- as.integer(as.factor(regiondf$region_number))
  } else {
    
    regiondf <- data.frame('region_number' = region_numbers, 'region_name' = region_numbers)
    
    regiondf['region_integer'] <- as.integer(as.factor(regiondf$region_number))
  }
  
  
  
  return(regiondf)
  
}

region_numbers <- function(txtpath){
  
  
  
  x <- readLines(txtpath)
  begin_pos <- grep("> print(unique(dfh_s$region_number))", x, fixed = TRUE)
  end_pos <- grep("> print(unique(dfh_s$region_c))", x, fixed = TRUE)
  
  text <- x[begin_pos:end_pos]
  
  
  text <- text[- 1] #eerste rij eraf. print statement
  text <- text[- length(text)]  #laatste rij eraf. print statement.
  
  region_numbers_string = gsub("\\[.*?\\]", "", text) #remove square brackets and everything within the brackets. 
  region_numbers_string = unlist(strsplit(region_numbers_string, split = " "))
  region_numbers_string = unlist(lapply(region_numbers_string, function(z){ z[!is.na(z) & z != ""]})) #remove empty strings.
  region_numbers = as.numeric(region_numbers_string)
  
  regiondf <- data.frame('region_number' = region_numbers, 'region_name' = region_numbers)
  
  return(regiondf)
  
}


text_to_df <- function(txtpath, beginline, endline, hhtype) {
  x <- readLines(txtpath)
  
  begin_pos <- grep(beginline, x, fixed = TRUE)
  end_pos <- grep(endline, x, fixed = TRUE)
  
  text <- x[begin_pos:end_pos]
  
  text <- text[- 1] #eerste rij eraf. print statement
  text <- text[- length(text)]  #laatste rij eraf. print statement. 
  
  line1 = text[1]
  line2 = text[2]
  line3 = text[3]
  
  charn1 = nchar(line1)
  
  #split line at each point. 
  characters_inline1test = strsplit(line1, split = " ")
  characters_inline1test = unlist(lapply(characters_inline1test, function(z){ z[!is.na(z) & z != ""]})) #remove empty strings. 
  
  characters_inline1 = strsplit(line1, split = NULL)
  characters_inline2 = strsplit(line2, split = NULL)
  characters_inline3 = strsplit(line3, split = NULL)
  
  characters_inline1 = unlist(characters_inline1)
  characters_inline2 = unlist(characters_inline2)
  characters_inline3 = unlist(characters_inline3)
  
  # dit kan eruit. extract variablename of column categories. 
  
  # tot hier. 
  #variablename <- paste(characters_inline1[(firstrow[1]:firstrow[counter])], collapse = "")
  
  variablename <- characters_inline1test[1]
  
  #variable_position_values <- firstrow[((counter+1):length(firstrow) )] 
  
  #variable_values <- as.integer(characters_inline1[variable_position_values])
  
  variable_values <- as.integer(characters_inline1test[2:length(characters_inline1test)])
  
  delimiterplace1 = list()
  counter = 1
  for (c in characters_inline2){
    if (c == " "){
      delimiterplace1 <- append(delimiterplace1, counter)
    }
    
    counter = counter+1
    
  }
  delimiterplace1 = unlist(delimiterplace1)
  
  
  delimiterplace2 = list()
  counter = 1
  for (c in characters_inline3){
    if (c == " "){
      delimiterplace2 <- append(delimiterplace2, counter)
    }
    
    counter = counter+1
    
  }
  delimiterplace2 = unlist(delimiterplace2)
  
  counter = 1
  placesline1 = list()
  
  while ((delimiterplace1[counter+1] - delimiterplace1[counter]) > 1){
    placesline1 <- append(placesline1, delimiterplace1[counter+1])
    counter = counter+1
  }
  
  
  placesline1 = unlist(placesline1)
  
  
  
  
  
  delimiterplace_part1 = append(delimiterplace1[1], placesline1)
  laatsteplek = delimiterplace_part1[length(delimiterplace_part1)]
  
  delimiterplace2_s <- delimiterplace2[delimiterplace2 > laatsteplek]
  
  
  delimiterplace2part2 = list()
  counter = 1
  for (c in delimiterplace2_s[- length(delimiterplace2_s)]){
    if ((delimiterplace2_s[counter + 1] - delimiterplace2_s[counter]) > 1){
      delimiterplace2part2 <- append(delimiterplace2part2, delimiterplace2_s[counter+1])
    }
    
    counter = counter+1
    
  }
  
  delimiterplace2part2 = unlist(delimiterplace2part2)
  
  
  delimiter_indices = append(delimiterplace_part1, delimiterplace2part2)
  delimiter_indices = append(delimiter_indices, charn1)
  
  
  delimiter_places <- unlist(append(delimiter_indices[1], diff(delimiter_indices)))
  
  df_final <- read.fwf(textConnection(text), header = F, skip = 2,   #hier even skip 2 om de zooi op te vullen. 
                       widths=c(delimiter_places))
  
  df_final <- df_final %>%
    na.locf()
  
  
  
  idvars <- colnames(df_final)[1:length(delimiterplace_part1)]
  
  colnames(df_final)[(length(delimiterplace_part1)+1):length(colnames(df_final))] <- variable_values
  
  measurevars <- colnames(df_final)[(length(delimiterplace_part1)+1):length(colnames(df_final))]
  
  
  df_final <- reshape2::melt(df_final, id.vars = idvars, measure_vars = measurevars)
  
  othercolumnnames = list()
  
  delimimiterplace_part1_fornames <- append(0, delimiterplace_part1)
  
  
  for (counter in (1:length(delimiterplace_part1))){
    othername <- paste(characters_inline2[(delimimiterplace_part1_fornames[counter]+1):(delimimiterplace_part1_fornames[counter+1]-1)], collapse = "")
    othercolumnnames <- append(othercolumnnames, othername)
  }
  
  othercolumnnames = unlist(othercolumnnames)  
  
  colnames(df_final)[1:length(delimiterplace_part1)] <- othercolumnnames
  colnames(df_final)[1+length(delimiterplace_part1)] <- variablename
  colnames(df_final)[2+length(delimiterplace_part1)] <- "Frequency"  
  
  #this is the right way: from factors to numeric! 
  df_final[[variablename]] <- as.numeric(levels(df_final[[variablename]]))[df_final[[variablename]]]
  
  if (hhtype == TRUE){
    hhtype_nr <- as.integer(gsub(".*?([0-9]+).*", "\\1", beginline))
    df_final <- add_column(df_final, hhtype = hhtype_nr, .before = "Frequency")
  }
  
  return(df_final)
  
}


beginlines_list = list("> print(ftable(joint100.table))",
                       "> print(ftable(joint210head.table))",
                       "> print(ftable(joint210partage.table))",
                       "> print(ftable(joint210partsex.table))",
                       "> print(ftable(joint210parteduc.table))",
                       "> print(ftable(joint220head.table))",
                       "> print(ftable(joint220hhsize1.table))",
                       "> print(ftable(joint220partage.table))",
                       "> print(ftable(joint220partsex.table))",
                       "> print(ftable(joint220parteduc.table))",
                       "> print(ftable(joint220hhsize2.table))",
                       "> print(ftable(joint230head.table))",
                       "> print(ftable(joint230hhsize1.table))",
                       "> print(ftable(joint230hhsize2.table))",
                       "> print(ftable(joint310head.table))",
                       "> print(ftable(joint310hhsize1.table))",
                       "> print(ftable(joint310partage.table))",
                       "> print(ftable(joint310partsex.table))",
                       "> print(ftable(joint310parteduc.table))",
                       "> print(ftable(joint310hhsize2.table))",
                       "> print(ftable(joint320head.table))", 
                       "> print(ftable(joint320hhsize1.table))", 
                       "> print(ftable(joint320partage.table))", 
                       "> print(ftable(joint320partsex.table))", 
                       "> print(ftable(joint320parteduc.table))", 
                       "> print(ftable(joint320hhsize2.table))", 
                       "> print(ftable(joint330head.table))",
                       "> print(ftable(joint330hhsize1.table))",
                       "> print(ftable(joint330hhsize2.table))",
                       "> print(ftable(joint400head.table))",
                       "> print(ftable(joint400hhsize1.table))",
                       "> print(ftable(joint400hhsize2.table))",
                       "> print(ftable(jointchildsex.table))",
                       "> print(ftable(jointchildeduc.table))",
                       "> print(ftable(jointrelativesex.table))",
                       "> print(ftable(jointrelativeeduc.table))",
                       "+ } #printstatement nonrelatives",
                       '[1] "startnonrelative_educ"')



endlines_list = list("> df100sumwgt <- sum(df100$hpopwgt)",
                     "> joint210partage.table <- xtabs(hpopwgt ~ educ_head + age_head + sex_head + age_part, data = df210part)",
                     "> joint210partsex.table <- xtabs(hpopwgt ~ educ_head + sex_head + age_part + sex_part, data = df210part)",
                     "> joint210parteduc.table <- xtabs(hpopwgt ~ educ_head + sex_part + age_part + educ_part, data = df210part)",
                     "> ## COUPLE WITH CHILDREN HOUSEHOLDS 220. Nu loopt ie vast. Loshalen hhtypes.",
                     "> joint220hhsize1.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + age_head + HHSIZECAT, data = df220head)",
                     "> joint220partage.table <- xtabs(hpopwgt ~ educ_head + age_head + sex_head + age_part, data = df220part)",
                     "> joint220partsex.table <- xtabs(hpopwgt ~ educ_head + sex_head + age_part + sex_part, data = df220part)",
                     "> joint220parteduc.table <- xtabs(hpopwgt ~ educ_head + sex_part + age_part + educ_part, data = df220part)",
                     "> joint220hhsize2.table <- xtabs(hpopwgt ~  CHILD_CAT + HHSIZECAT, data = df220head)",
                     "> ## ONE PARENT WITH CHILDREN 230",
                     "> joint230hhsize1.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + age_head + HHSIZECAT, data = df230head)",
                     "> joint230hhsize2.table <- xtabs(hpopwgt ~  CHILD_CAT + HHSIZECAT, data = df230head)",
                     "> ## COUPLE WITHOUT CHILDREN but with RELATIVES, NONRELATIVES AND OTHERS 310, 510, 610, 910",
                     "> joint310hhsize1.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + age_head + HHSIZECAT, data = df310head)",
                     "> joint310partage.table <- xtabs(hpopwgt ~ educ_head + age_head + sex_head + age_part, data = df310part)",
                     "> joint310partsex.table <- xtabs(hpopwgt ~ educ_head + sex_head + age_part + sex_part, data = df310part)",
                     "> joint310parteduc.table <- xtabs(hpopwgt ~ educ_head + sex_part + age_part + educ_part, data = df310part)",
                     "> joint310hhsize2.table <- xtabs(hpopwgt ~  RELATIVE_CAT + NONRELATIVE_CAT + HHSIZECAT, data = df310head)",
                     "> ## COUPLE WITH CHILDREN, RELATIVES, NONRELATIVES AND OTHERS 320, 520, 620, 920",
                     "> joint320hhsize1.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + age_head + HHSIZECAT, data = df320head)", 
                     "> joint320partage.table <- xtabs(hpopwgt ~ educ_head + age_head + sex_head + age_part, data = df320part)", 
                     "> joint320partsex.table <- xtabs(hpopwgt ~ educ_head + sex_head + age_part + sex_part, data = df320part)",
                     "> joint320parteduc.table <- xtabs(hpopwgt ~ educ_head + sex_part + age_part + educ_part, data = df320part)", 
                     "> joint320hhsize2.table <- xtabs(hpopwgt ~ CHILD_CAT + RELATIVE_CAT + NONRELATIVE_CAT + HHSIZECAT, data = df320head)",
                     "> ## ONE PERSON WITH CHILDREN, RELATIVES, NONRELATIVES AND OTHERS 330, 530, 630, 930",
                     "> joint330hhsize1.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + age_head + HHSIZECAT, data = df330head)",
                     "> joint330hhsize2.table <- xtabs(hpopwgt ~ CHILD_CAT + RELATIVE_CAT + NONRELATIVE_CAT + HHSIZECAT, data = df330head)",
                     "> ## RELATIVES, NONRELATIVES LIVING TOGETHER 400, 700, 800, 910",
                     "> joint400hhsize1.table <- xtabs(hpopwgt ~ INCOMEQQ + rural_number + age_head + HHSIZECAT, data = df400head)",
                     "> joint400hhsize2.table <- xtabs(hpopwgt ~ RELATIVE_CAT + NONRELATIVE_CAT + HHSIZECAT, data = df400head)",
                     "> # general child.",
                     "> jointchildeduc.table <- xtabs(ppopwgt ~ educ_head + AGECAT + sex_number + education, data = dfchild)",
                     "> # general relative",
                     "> jointrelativeeduc.table <- xtabs(ppopwgt ~ educ_head + AGECAT + sex_number + education, data = dfrelative)",
                     "> # general nonrelative",
                     '[1] "startnonrelative_educ"',
                     "> # MARGINALS NOG")







##### availability farming 
txtpath <- paste0(isocode, "_compl_margfix.txt")
print(txtpath)

farm_presence <- 0 #set farming to 0, because not needed in error calculation. 



#############################
######### THE ERROR #########



txtpathfreq <- paste0(isocode, "_freq_margfix.txt") 

beginlist_freq = list('> print((data.frame(xtabs(hpopwgt ~ farming_number + HHTYPE_CAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                      '> print((data.frame(xtabs(hpopwgt ~ farming_number + HHSIZECAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                      '> print((data.frame(xtabs(hpopwgt ~ farming_number + INCOMEQQ + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ farming_number + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ farming_number + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ farming_number + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(hpopwgt ~ INCOMEQQ + HHTYPE_CAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                      '> print((data.frame(xtabs(hpopwgt ~ INCOMEQQ + HHSIZECAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(hpopwgt ~ HHSIZECAT + HHTYPE_CAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ sex_number + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ sex_number + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ education + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))')



endlist_freq = list('> print((data.frame(xtabs(hpopwgt ~ farming_number + HHSIZECAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                    '> print((data.frame(xtabs(hpopwgt ~ farming_number + INCOMEQQ + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ farming_number + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ farming_number + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ farming_number + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(hpopwgt ~ INCOMEQQ + HHTYPE_CAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                    '> print((data.frame(xtabs(hpopwgt ~ INCOMEQQ + HHSIZECAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(hpopwgt ~ HHSIZECAT + HHTYPE_CAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ sex_number + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ sex_number + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ education + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> # Without regions')


beginlist_freqtotal = list('> print((data.frame(xtabs(hpopwgt ~ farming_number + HHTYPE_CAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                           '> print((data.frame(xtabs(hpopwgt ~ farming_number + HHSIZECAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                           '> print((data.frame(xtabs(hpopwgt ~ farming_number + INCOMEQQ, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ farming_number + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ farming_number + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ farming_number + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(hpopwgt ~ INCOMEQQ + HHTYPE_CAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                           '> print((data.frame(xtabs(hpopwgt ~ INCOMEQQ + HHSIZECAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(hpopwgt ~ HHSIZECAT + HHTYPE_CAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ sex_number + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ sex_number + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ education + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))')



endlist_freqtotal = list('> print((data.frame(xtabs(hpopwgt ~ farming_number + HHSIZECAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                         '> print((data.frame(xtabs(hpopwgt ~ farming_number + INCOMEQQ, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ farming_number + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ farming_number + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ farming_number + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(hpopwgt ~ INCOMEQQ + HHTYPE_CAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                         '> print((data.frame(xtabs(hpopwgt ~ INCOMEQQ + HHSIZECAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(hpopwgt ~ HHSIZECAT + HHTYPE_CAT, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ HHSIZECAT + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ HHTYPE_CAT + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ sex_number + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ sex_number + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ education + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> # EINDE')






read_freqs_LIS <- function(txtpathfreq, beginline, endline){
  
  missingcat <- c()
  missingcat2 <- c()
  
  if (grepl('HHSIZECAT', beginline)){
    
    x <- readLines(txtpathfreq)
    hhsizetable <- x[grep('> print(table(dfmerg$HHSIZECAT))', x, fixed = TRUE):grep('> print(table(dfmerg$INCOMEQQ))', x, fixed = TRUE)]
    
    hhsizetable <- hhsizetable[- 1] #eerste rij eraf. print statement
    hhsizetable <- hhsizetable[- length(hhsizetable)]  #laatste rij eraf. print statement.
    
    hhsizetable = gsub("\\[.*?\\]", "", hhsizetable) #remove square brackets and everything within the brackets. 
    hhsizetable = unlist(strsplit(hhsizetable, split = " "))
    hhsizetable = unlist(lapply(hhsizetable, function(z){ z[!is.na(z) & z != ""]})) #remove empty strings.
    categories = hhsizetable[1:(length(hhsizetable)/2)]
    freqs = hhsizetable[((length(hhsizetable)/2)+1):(length(hhsizetable))]
    freqs = as.integer(freqs)
    missingcat <- setdiff(c('1', '2', '3', '4', '5', '6'), categories)
    missingcat <- as.integer(missingcat)
  
  }
  
  
  if (grepl('education', beginline)){
    
    x <- readLines(txtpathfreq)
    hhsizetable <- x[grep('> print(table(dfmerg$education))', x, fixed = TRUE):grep('> print(table(dfmerg$sex_number))', x, fixed = TRUE)]
    
    hhsizetable <- hhsizetable[- 1] #eerste rij eraf. print statement
    hhsizetable <- hhsizetable[- length(hhsizetable)]  #laatste rij eraf. print statement.
    
    hhsizetable = gsub("\\[.*?\\]", "", hhsizetable) #remove square brackets and everything within the brackets. 
    hhsizetable = unlist(strsplit(hhsizetable, split = " "))
    hhsizetable = unlist(lapply(hhsizetable, function(z){ z[!is.na(z) & z != ""]})) #remove empty strings.
    categories = hhsizetable[1:(length(hhsizetable)/2)]
    freqs = hhsizetable[((length(hhsizetable)/2)+1):(length(hhsizetable))]
    freqs = as.integer(freqs)
    missingcat2 <- setdiff(c('1', '2', '3', '4', '5'), categories)
    missingcat2 <- as.integer(missingcat2)
    
  }
  
  x <- readLines(txtpathfreq)
  begin_pos <- grep(beginline, x, fixed = TRUE)
  end_pos <- grep(endline, x, fixed = TRUE)
  
  text <- x[begin_pos:end_pos]
  
  
  text <- text[- 1] #eerste rij eraf. print statement
  text <- text[- length(text)]  #laatste rij eraf. print statement.
  
  text2 = gsub("\\[.*?\\]", "", text) #remove square brackets and everything within the brackets. 
  text2 = unlist(strsplit(text2, split = " "))
  text2 = unlist(lapply(text2, function(z){ z[!is.na(z) & z != ""]})) #remove empty strings.
  vector_freqtotal = as.numeric(text2)
  
  if ((grepl('HHSIZECAT', beginline)) & (length(missingcat) > 0)){
    if (grepl('INCOMEQQ', beginline)){
      vector_freqtotal <- append(vector_freqtotal, c(0,0,0,0,0), ((missingcat*5)-5))
    } else if((grepl('HHTYPE_CAT', beginline))){
      endloop = 8
    } else if((grepl('AGECAT', beginline))){
      endloop = 7
    } else if((grepl('education', beginline))){
      if (length(missingcat2) > 0){ 
        endloop = 4
      } else {
        endloop = 5
      } 
      
    } else if((grepl('sex_number', beginline))){
      endloop = 2
    } else if((grepl('farming_number', beginline))){
      endloop = 2
    }
    
    if (!grepl('INCOMEQQ', beginline)){
      for (i in 1:endloop){
        vector_freqtotal <- append(vector_freqtotal, 0, (missingcat-1)+(6*(i-1))) # length(categories)
      } 
    }

  }
  
  if ((grepl('education', beginline)) & (length(missingcat2) > 0)){
    if (grepl('farming', beginline)){
      vector_freqtotal <- append(vector_freqtotal, c(0,0), ((missingcat2*2)-2))
    } else if((grepl('HHTYPE_CAT', beginline))){
      vector_freqtotal <- append(vector_freqtotal, c(0,0,0,0,0,0,0,0), ((missingcat2*8)-8))
    } else if((grepl('sex_number', beginline))){
      vector_freqtotal <- append(vector_freqtotal, c(0,0), ((missingcat2*2)-2))
    } else if((grepl('INCOMEQQ', beginline))){
      vector_freqtotal <- append(vector_freqtotal, c(0,0,0,0,0), ((missingcat2*5)-5))
    } else if((grepl('HHSIZECAT', beginline))){
      vector_freqtotal <- append(vector_freqtotal, c(0,0,0,0,0,0), ((missingcat2*6)-6))
    } else if((grepl('AGECAT', beginline))){
      endloop = 7
    }
    
    if (grepl('AGECAT', beginline)){
      for (i in 1:endloop){
        vector_freqtotal <- append(vector_freqtotal, 0, (missingcat2-1)+(5*(i-1))) # length(categories)
      } 
    }
    
  }
  

  dffreqtotal <- data.frame('National' = vector_freqtotal)
  
  return(dffreqtotal)
}


#hier ook if statement voor farming.
#changed to 7: 


if (farm_presence == 1){
  for (i in 1:length(beginlist_freqtotal)){
    if (i == 1){
      #changed to 7, was 1. 
      df1tot <- read_freqs_LIS(txtpathfreq, beginlist_freqtotal[i], endlist_freqtotal[i])
    } else {
        df2tot <- read_freqs_LIS(txtpathfreq, beginlist_freqtotal[i], endlist_freqtotal[i])
        
        df1tot <- rbind.data.frame(df1tot, df2tot)
      
    }
    
  }

}

if (farm_presence == 0){
  for (i in 7:length(beginlist_freqtotal)){
    print(i)
    if (i == 7){
      #changed to 7, was 1. 
      df1tot <- read_freqs_LIS(txtpathfreq, beginlist_freqtotal[i], endlist_freqtotal[i])
    } else {
      df2tot <- read_freqs_LIS(txtpathfreq, beginlist_freqtotal[i], endlist_freqtotal[i])
      df1tot <- rbind.data.frame(df1tot, df2tot)
      
    }
    
  }
  
}




##########################################



DF_errors <- data.frame('iso_code' = isocode, 'Synth_sq_error' = 0)


txtpath <- paste0(isocode, "_compl_margfix.txt")

regiondf <- region_numbers(txtpath)

if (liscodeletters == 'us'){
  regiondf <- regiondf[regiondf$region_number != 95,] #remove hawaii US, no data available.  
}



syn_pop <- data.table()



##################################################
###### IMPORT LIS DATA WHEN COUNTRY UNKNOWN ######

replace_countries_file <- read.csv('similar_countries_8nov23.csv', sep = ',')

replace_countries_file <- replace_countries_file[replace_countries_file['iso_code'] == isocode,]

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
  
  replacecode <- replacecountries[repl]
  print(replacecode) 
  
  #if (liscode != 'nolis'){
  #  replacecode = liscode
  #}
  
  
  if (replace_lisdhs[repl] == "LIS"){
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
    
    filename_length <- paste0('length_DHSdata_', replacecode, '_sept23.csv')
    
    
    rowlength_DHS <- read.csv(filename_length, sep = ',', header = TRUE)
    
    l = rowlength_DHS$length
    w = 11
    
    filenamelisdata <- paste0(replacecode, "_DHS_sept23_synth_country.dat")
    
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



#hhsize
hhsize_marg_est <- read.csv('Est_marginals_HHSIZECAT_sept23.csv', sep = ',', header = TRUE)

print(hhsize_marg_est)

hhsize_marg_est$iso_code <- substr(hhsize_marg_est$GDLCODE, 1,3)
hhsize_marg_est <- hhsize_marg_est[hhsize_marg_est$iso_code == isocode,]
hhsize_marg_est <- hhsize_marg_est %>% select(-iso_code)

hhsize_marg_est <- pivot_longer(hhsize_marg_est, cols = c(X1est, X2est, X3est, X4est, X5est, X6est), names_to = "HHSIZECAT", values_to = "Frequency")
hhsize_marg_est$HHSIZECAT <- as.integer(as.factor(hhsize_marg_est$HHSIZECAT))

hhsize_marg_est_tib <- as_tibble(hhsize_marg_est)


#hhtype
hhtype_marg_est <- read.csv('Est_marginals_HHTYPE_sept23.csv', sep = ',', header = TRUE)
hhtype_marg_est$iso_code <- substr(hhtype_marg_est$GDLCODE, 1,3)
hhtype_marg_est <- hhtype_marg_est[hhtype_marg_est$iso_code == isocode,]
hhtype_marg_est <- hhtype_marg_est %>% select(-iso_code)

hhtype_marg_est <- pivot_longer(hhtype_marg_est, cols = c(X1est, X2est, X3est, X4est, X5est, X6est, X7est, X8est), names_to = "HHTYPE", values_to = "Frequency")
hhtype_marg_est$HHTYPE <- as.integer(as.factor(hhtype_marg_est$HHTYPE))

hhtype_marg_est_tib <- as_tibble(hhtype_marg_est)


# age
age_marg_est <- read.csv('Est_marginals_AGECAT_sept23.csv', sep = ',', header = TRUE)
age_marg_est$iso_code <- substr(age_marg_est$GDLCODE, 1,3)
age_marg_est <- age_marg_est[age_marg_est$iso_code == isocode,]
age_marg_est <- age_marg_est %>% select(-iso_code)

age_marg_est <- pivot_longer(age_marg_est, cols = c(X1est, X2est, X3est, X4est, X5est, X6est, X7est), names_to = "AGECAT", values_to = "Frequency")
age_marg_est$AGECAT <- as.integer(as.factor(age_marg_est$AGECAT))

age_marg_est_tib <- as_tibble(age_marg_est)


# gender
gender_marg_est <- read.csv('Est_marginals_GENDER_sept23.csv', sep = ',', header = TRUE)
gender_marg_est$iso_code <- substr(gender_marg_est$GDLCODE, 1,3)
gender_marg_est <- gender_marg_est[gender_marg_est$iso_code == isocode,]
gender_marg_est <- gender_marg_est %>% select(-iso_code)

gender_marg_est <- pivot_longer(gender_marg_est, cols = c(X1est, X2est), names_to = "GENDER", values_to = "Frequency")
gender_marg_est$GENDER <- as.integer(as.factor(gender_marg_est$GENDER))
gender_marg_est$GENDER <- gender_marg_est$GENDER -1

gender_marg_est_tib <- as_tibble(gender_marg_est)


# educat
educat_marg_est <- read.csv('Est_marginals_EDUCAT_sept23.csv', sep = ',', header = TRUE)
educat_marg_est$iso_code <- substr(educat_marg_est$GDLCODE, 1,3)
educat_marg_est <- educat_marg_est[educat_marg_est$iso_code == isocode,]
educat_marg_est <- educat_marg_est %>% select(-iso_code)

educat_marg_est <- pivot_longer(educat_marg_est, cols = c(X1est, X2est, X3est, X4est, X5est), names_to = "EDUCAT", values_to = "Frequency")
educat_marg_est$EDUCAT <- as.integer(as.factor(educat_marg_est$EDUCAT))

educat_marg_est_tib <- as_tibble(educat_marg_est)





# Observed marginals
marginals_beginlist = list('> print(ftable(incomemarg.table))', '> print(ftable(ruralmarg.table))', 
                           '> print(ftable(farmingmarg.table))', '> print(ftable(hhtypemarg.table))',
                           '> print(ftable(hhsizecatmarg.table))', '> print(ftable(ageindimarg.table))',
                           '> print(ftable(educindimarg.table))', '> print(ftable(sexindimarg.table))')
marginals_endlist = list('> print(ftable(ruralmarg.table))', '> print(ftable(farmingmarg.table))', 
                         '> print(ftable(hhtypemarg.table))', '> print(ftable(hhsizecatmarg.table))',
                         '> print(ftable(ageheadmarg.table))', '> print(ftable(educindimarg.table))',
                         '> print(ftable(sexindimarg.table))', '> # BIJNA EINDE')




ruralmarg <- text_to_df(txtpath, marginals_beginlist[[2]], marginals_endlist[[2]], F)
ruralmarg <- ruralmarg %>% rename(RURAL = rural_number, GEOLEV1 = region_number)
rural_tib <- as_tibble(ruralmarg)
#rural_tib <- rural_tib[rural_tib$GEOLEV1 %ni% remove_regions ,]
rural_tib <- rural_tib %>% arrange(GEOLEV1)
rural_tib$GEOLEV1 <- as.integer(rural_tib$GEOLEV1)
rural_tib$RURAL <- as.integer(rural_tib$RURAL)


sexmarg <- text_to_df(txtpath, marginals_beginlist[[8]], marginals_endlist[[8]], F)
sexmarg <- sexmarg %>% rename(GENDER = sex_number, GEOLEV1 = region_number)
sex_tib <- as_tibble(sexmarg)
#sex_tib <- sex_tib[sex_tib$GEOLEV1 %ni% remove_regions ,]
sex_tib <- sex_tib %>% arrange(GEOLEV1)
sex_tib$GEOLEV1 <- as.integer(sex_tib$GEOLEV1)
sex_tib$GENDER <- as.integer(sex_tib$GENDER)



# NEW RURAL MARGINAL
SMODmarg <- read.csv('individual_marginals_UrbanRural.csv', sep = ';', header = TRUE)
GDL_pop <- read.csv('GDL_match_population_all_LIS.csv', sep = ';', header = TRUE)
GDL_pop <- GDL_pop %>% rename('GDLCODE' = 'GDLcode')
SMODmarg <- SMODmarg %>% rename('GDLCODE' = 'GDLcode')


if (liscodeletters %in% c('gr', 'be', 'ie', 'cn', 'in', 'jp', 'it')){
  GDL_SMOD <- read.csv('fix_regions_griebe.csv', sep = ';', header = TRUE)
  GDL_SMOD['Rural_pop'] <- GDL_SMOD['year2015'] * GDL_SMOD['Rural_percent'] * 1000
  GDL_SMOD['Urban_pop'] <- GDL_SMOD['year2015'] * GDL_SMOD['Urban_percent'] * 1000
  
  sum_GDL_POP <- GDL_SMOD %>% group_by(liscountry, region_number) %>% summarise(Sum_rural_pop = sum(Rural_pop), Sum_urban_pop = sum(Urban_pop), Sum_year2015 = sum(year2015))
  
  sum_GDL_POP <- sum_GDL_POP[sum_GDL_POP$liscountry == liscodeletters,]
  sum_GDL_POP <- sum_GDL_POP %>% ungroup() %>% select(-liscountry)
  
  sum_GDL_POP <- sum_GDL_POP %>% rename('GEOLEV1' = 'region_number')
  
  sum_indi <- sexmarg %>% group_by(GEOLEV1) %>% summarise(Population = sum(Frequency))
  
  sum_GDL_POP <- left_join(sum_GDL_POP, sum_indi, by = 'GEOLEV1')
  
  sum_GDL_POP['adjust'] <- sum_GDL_POP['Population'] / (1000*sum_GDL_POP['Sum_year2015'])
  
  sum_GDL_POP['Sum_rural_pop'] <- sum_GDL_POP['Sum_rural_pop'] * sum_GDL_POP['adjust']
  sum_GDL_POP['Sum_urban_pop'] <- sum_GDL_POP['Sum_urban_pop'] * sum_GDL_POP['adjust']
  
  
  ruralmargSMOD0 <- sum_GDL_POP
  ruralmargSMOD0['RURAL'] <- 0
  ruralmargSMOD1 <- sum_GDL_POP
  ruralmargSMOD1['RURAL'] <- 1
  ruralmargSMOD0 <- ruralmargSMOD0 %>% select(GEOLEV1, Sum_urban_pop, RURAL)
  ruralmargSMOD1 <- ruralmargSMOD1 %>% select(GEOLEV1, Sum_rural_pop, RURAL)
  ruralmargSMOD0 <- ruralmargSMOD0 %>% rename('Frequency' = 'Sum_urban_pop')
  ruralmargSMOD1 <- ruralmargSMOD1 %>% rename('Frequency' = 'Sum_rural_pop')
  
  ruralmargSMOD <- rbind(ruralmargSMOD0, ruralmargSMOD1)
  
  ruralmargSMOD <- ruralmargSMOD %>% relocate(Frequency, .after = RURAL)
  
  ruralmargSMOD <- ruralmargSMOD[!is.na(ruralmargSMOD$GEOLEV1),]
  
  
  
} else if (liscodeletters %ni% c('gr', 'be', 'ie', 'cn', 'in', 'jp', 'it')) {
  
  SMODmarg <- left_join(SMODmarg, GDL_pop, by = 'GDLCODE')
  
  SMODmarg <- SMODmarg[SMODmarg$liscountry == liscodeletters,]
  SMODmarg <- SMODmarg[!is.na(SMODmarg$liscountry),]
  
  
  
  ruralmargSMOD <- ruralmarg 
  
  if (liscodeletters == 'us'){
    # for some reason delaware (51) is missing in rural marg LIS
    add_rural51 <- data.frame('GEOLEV1' = c(51, 51), 'RURAL' = c(0,1), 'Frequency' = c(0,0))
    ruralmargSMOD <- rbind(ruralmargSMOD, add_rural51)
  }
  
  
  
  SMODmarg <- SMODmarg %>% rename('GEOLEV1' = 'region_number')
  
  
  ruralmargSMOD <- left_join(ruralmargSMOD, SMODmarg, by = 'GEOLEV1')
  
  ruralmargSMOD0 <- ruralmargSMOD[ruralmargSMOD$RURAL == 0,]
  ruralmargSMOD1 <- ruralmargSMOD[ruralmargSMOD$RURAL == 1,]
  
  sum_indi <- sexmarg %>% group_by(GEOLEV1) %>% summarise(Population = sum(Frequency))
  
  ruralmargSMOD0 <- left_join(ruralmargSMOD0, sum_indi, by = 'GEOLEV1')
  ruralmargSMOD0['New_freq'] <- ruralmargSMOD0$Urban_percent * ruralmargSMOD0$Population
  
  ruralmargSMOD1 <- left_join(ruralmargSMOD1, sum_indi, by = 'GEOLEV1')
  ruralmargSMOD1['New_freq'] <- ruralmargSMOD1$Rural_percent * ruralmargSMOD1$Population
  
  ruralmargSMOD0 <- ruralmargSMOD0 %>% select(GEOLEV1, RURAL, Frequency, New_freq, Population)
  ruralmargSMOD1 <- ruralmargSMOD1 %>% select(GEOLEV1, RURAL, Frequency, New_freq, Population)
  
  ruralmargSMOD <- rbind.data.frame(ruralmargSMOD0, ruralmargSMOD1)
  
  
  
  # in case a region is missing in the smod data
  
  for (row in 1:nrow(ruralmargSMOD)){
    if (is.na(ruralmargSMOD$New_freq[row])){
      print('error in rural marg, NAN value, GDL regions')
      if (ruralmargSMOD$Frequency[row] == 0){
        ruralmargSMOD$New_freq[row] <- 0
      } else if (ruralmargSMOD$Frequency[row] > 0){
        ruralmargSMOD$New_freq[row] <- ruralmargSMOD$Population[row]
      }
    }
  }
  
  ruralmargSMOD <- ruralmargSMOD %>% select(GEOLEV1, RURAL, New_freq)
  ruralmargSMOD <- ruralmargSMOD %>% rename('Frequency' = 'New_freq')
  
} 




rural_tib <- as_tibble(ruralmargSMOD)
#rural_tib <- rural_tib[rural_tib$GEOLEV1 %ni% remove_regions ,]
rural_tib <- rural_tib %>% arrange(GEOLEV1)
rural_tib$GEOLEV1 <- as.integer(rural_tib$GEOLEV1)
rural_tib$RURAL <- as.integer(rural_tib$RURAL)

print('ruraltib')
print(unique(rural_tib$GEOLEV1))


regionnumbers <- unique(rural_tib$GEOLEV1) #not integers, otherwise we can't match them with GDL.  


jointhead100small[, PID := 1:.N] #was jointhead10. 

nr_individuals_per_regio <- c()

error_region <- c()

GDL_country <- GDL_pop[GDL_pop$iso_code == isocode,]
GDL_country <- GDL_country[!is.na(GDL_country$region_number),]


##########################################################


# Merge GEOLEV1 (i.e. liscode regions back to estimated marginals)
educat_marg_est_tib <- left_join(educat_marg_est_tib, GDL_country %>% select(GDLCODE, region_number), by = 'GDLCODE')
age_marg_est_tib <- left_join(age_marg_est_tib, GDL_country %>% select(GDLCODE, region_number), by = 'GDLCODE')
gender_marg_est_tib <- left_join(gender_marg_est_tib, GDL_country %>% select(GDLCODE, region_number), by = 'GDLCODE')
hhsize_marg_est_tib <- left_join(hhsize_marg_est_tib, GDL_country %>% select(GDLCODE, region_number), by = 'GDLCODE')
hhtype_marg_est_tib <- left_join(hhtype_marg_est_tib, GDL_country %>% select(GDLCODE, region_number), by = 'GDLCODE')

print(hhsize_marg_est_tib)

hhsize_marg_est_tib <- hhsize_marg_est_tib %>% select(-GDLCODE)
hhtype_marg_est_tib <- hhtype_marg_est_tib %>% select(-GDLCODE)
age_marg_est_tib <- age_marg_est_tib %>% select(-GDLCODE)
gender_marg_est_tib <- gender_marg_est_tib %>% select(-GDLCODE)
educat_marg_est_tib <- educat_marg_est_tib %>% select(-GDLCODE)

print(hhsize_marg_est_tib)

hhsize_marg_est_tib <- hhsize_marg_est_tib %>% rename(GEOLEV1 = region_number)
hhtype_marg_est_tib <- hhtype_marg_est_tib %>% rename(GEOLEV1 = region_number)
age_marg_est_tib <- age_marg_est_tib %>% rename(GEOLEV1 = region_number)
gender_marg_est_tib <- gender_marg_est_tib %>% rename(GEOLEV1 = region_number)
educat_marg_est_tib <- educat_marg_est_tib %>% rename(GEOLEV1 = region_number)




# Remove mismatch reference sample (survey) and control sample (marginals)
# hhsize, hhtype and educat. 

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




for (regnr in regionnumbers){
  print(regnr)
  #income_tib1reg <- income_tib[income_tib$GEOLEV1 == regnr,]
  #income_tib1reg <- income_tib1reg %>% select(-GEOLEV1)
  
  rural_tib1reg <- rural_tib[rural_tib$GEOLEV1 == regnr,]
  rural_tib1reg <- rural_tib1reg %>% select(-GEOLEV1)
  
  #farm_tib1reg <- farm_tib[farm_tib$GEOLEV1 == regnr,]
  #farm_tib1reg <- farm_tib1reg %>% select(-GEOLEV1)
  
  hhtype_tib1reg <- hhtype_marg_est_tib[hhtype_marg_est_tib$GEOLEV1 == regnr,]
  hhtype_tib1reg <- hhtype_tib1reg %>% select(-GEOLEV1)
  
  hhsize_tib1reg <- hhsize_marg_est_tib[hhsize_marg_est_tib$GEOLEV1 == regnr,]
  hhsize_tib1reg <- hhsize_tib1reg %>% select(-GEOLEV1)
  
  age_tib1reg <- age_marg_est_tib[age_marg_est_tib$GEOLEV1 == regnr,]
  age_tib1reg <- age_tib1reg %>% select(-GEOLEV1)
  
  edu_tib1reg <- educat_marg_est_tib[educat_marg_est_tib$GEOLEV1 == regnr,]
  edu_tib1reg <- edu_tib1reg %>% select(-GEOLEV1)
  
  sex_tib1reg <- gender_marg_est_tib[gender_marg_est_tib$GEOLEV1 == regnr,]
  sex_tib1reg <- sex_tib1reg %>% select(-GEOLEV1)
  
  
  group_control <- list()
  individual_control <- list(hhtype_tib1reg, hhsize_tib1reg, rural_tib1reg, age_tib1reg, edu_tib1reg, sex_tib1reg)
  

  names(group_control) <- c() 
  names(individual_control) <- c('HHTYPE', 'HHSIZECAT', 'RURAL','AGECAT','EDUCAT', 'GENDER')
  
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
  
  
  fit <- ml_fit(ml_problem = fitting_problem, algorithm = "ipu", maxiter = 50)
  
  
  syn_pop_reg <- ml_replicate(fit, algorithm = "trs")
  
  syn_pop_reg$GEOLEV1 <- regnr
  
  gc()
  
  if (liscodeletters %ni% c('gr', 'be', 'ie', 'cn', 'in', 'it', 'jp')){
    
    GDL_region <- GDL_country[GDL_country$region_number == regnr,]
  } else if (liscodeletters %in% c('gr', 'be', 'ie', 'cn', 'in', 'it', 'jp')){
    
    GDL_country_sum <- GDL_country %>% group_by(region_number) %>% summarise(year2015 = sum(year2015))
    
    GDL_region <- GDL_country_sum[GDL_country_sum$region_number == regnr,]
  }
  
  popsize_syn <- nrow(syn_pop_reg)
  
  popsize_GDL <- as.double(GDL_region$year2015) * 1000
  
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
  
  
  
  nr_individuals_per_regio <- append(nr_individuals_per_regio, nrow(syn_pop_reg))
  
  
  if (liscodeletters %in% c('gr', 'be', 'ie', 'cn', 'in', 'it', 'jp')){
    nr_individuals_per_regio_GDL <- c()
    gdlcodes <- c()
    GDL_pop_country <- GDL_pop[GDL_pop$liscountry == liscodeletters,] 
    GDL_pop_country <- GDL_pop_country[!is.na(GDL_pop_country$region_number),]
    GDL_pop_country_regnr <- GDL_pop_country[GDL_pop_country$region_number == regnr,]
    
    for (gdlcode in GDL_pop_country_regnr$GDLCODE){
      GDL_pop_GDLreg <- GDL_pop_country_regnr[GDL_pop_country_regnr$GDLCODE == gdlcode,]
      
      mp_syn_GDL <- GDL_pop_GDLreg$year2015/(sum(GDL_pop_country_regnr$year2015))
      
      if (mp_syn_GDL < 0.999){
        
        cancelHID <- sample(1:maxHID, round(maxHID*(1-mp_syn_GDL)))
        
        syn_pop_reg_sub <- syn_pop_reg[syn_pop_reg$HID %ni% cancelHID,]
      } else {
        syn_pop_reg_sub <- syn_pop_reg
      }
      
      gdlcodes <- append(gdlcodes, gdlcode)
      nr_individuals_per_regio_GDL <- append(nr_individuals_per_regio_GDL, nrow(syn_pop_reg_sub))
      
      name_regGDL <- paste0(isocode, '_may23_synthpop_gdl', as.character(gdlcode), '.dat')
      
      #con = file(name_regGDL, "wb")
      
      #writeBin(c(syn_pop_reg$INCOME, syn_pop_reg$RURAL, syn_pop_reg$FARMING, syn_pop_reg$AGECAT, syn_pop_reg$GENDER, 
      #           syn_pop_reg$EDUCAT, syn_pop_reg$HHTYPE, syn_pop_reg$HID, syn_pop_reg$RELATE, syn_pop_reg$HHSIZECAT, syn_pop_reg$GEOLEV1), con)
      
      #writeBin(c(syn_pop$INCOME, syn_pop$PID), con)
      
      
      #close(con)
    }
    
    
  }
  
  syn_pop <- rbindlist(list(syn_pop, syn_pop_reg), use.names=TRUE)
  rm(syn_pop_reg)
  gc()
  
  
  
  
  
  
  
}



synpophead <- syn_pop[syn_pop$RELATE == 1,]
tableinc <- table(synpophead$INCOME)
tableinc / sum(tableinc)


# per regio, percentages income groepen uitrekenen. 
incomefreq_regio <- data.frame(xtabs(~INCOME + GEOLEV1, data = synpophead))

mp_w1 <- 1-((tableinc[1]/sum(tableinc))-0.2)/(tableinc[1]/sum(tableinc))
mp_w2 <- 1-((tableinc[2]/sum(tableinc))-0.2)/(tableinc[2]/sum(tableinc))
mp_w3 <- 1-((tableinc[3]/sum(tableinc))-0.2)/(tableinc[3]/sum(tableinc))
mp_w4 <- 1-((tableinc[4]/sum(tableinc))-0.2)/(tableinc[4]/sum(tableinc))
mp_w5 <- 1-((tableinc[5]/sum(tableinc))-0.2)/(tableinc[5]/sum(tableinc))



mp_df <- data.frame('INCOME' = c(1,2,3,4,5), 'mp' = c(mp_w1,mp_w2,mp_w3,mp_w4,mp_w5))

incomefreq_regio <- merge(incomefreq_regio, mp_df, on = 'INCOME', how = 'left')
incomefreq_regio['Freq'] <- incomefreq_regio['Freq']*incomefreq_regio['mp']

incomefreq_regio <- incomefreq_regio %>% rename(Frequency = Freq)

income_tib <- as_tibble(incomefreq_regio %>% select(GEOLEV1, INCOME, Frequency))


# for loop with regions. Now with income marginal.  
nr_individuals_per_regio = c()

syn_pop <- data.table()


hhsizemarg_household <- data.frame(xtabs(~HHSIZECAT + GEOLEV1, data = synpophead))
hhsizemarg_household <- hhsizemarg_household %>% rename(Frequency = Freq)

for (regnr in regionnumbers){
  print(regnr)
  
  income_tib1reg <- income_tib[income_tib$GEOLEV1 == regnr,]
  income_tib1reg <- income_tib1reg %>% select(-GEOLEV1)
  
  #correction income not really needed becaue correction_income is ~1. 
  hhsize_household_tib1reg <- hhsizemarg_household[hhsizemarg_household$GEOLEV1 == regnr,]
  hhsizesumfreq_hh <- sum(hhsize_household_tib1reg$Frequency)
  incomesumfreq <- sum(income_tib1reg$Frequency)
  print('hhsizesumfreq_hh')
  print(hhsizesumfreq_hh)
  print('incomesumfreq')
  print(incomesumfreq)
  
  correction_income <- hhsizesumfreq_hh/incomesumfreq
  
  income_tib1reg$Frequency <- income_tib1reg$Frequency*correction_income
  
  rural_tib1reg <- rural_tib[rural_tib$GEOLEV1 == regnr,]
  rural_tib1reg <- rural_tib1reg %>% select(-GEOLEV1)
  
  #farm_tib1reg <- farm_tib[farm_tib$GEOLEV1 == regnr,]
  #farm_tib1reg <- farm_tib1reg %>% select(-GEOLEV1)
  
  hhtype_tib1reg <- hhtype_marg_est_tib[hhtype_marg_est_tib$GEOLEV1 == regnr,]
  hhtype_tib1reg <- hhtype_tib1reg %>% select(-GEOLEV1)
  
  hhsize_tib1reg <- hhsize_marg_est_tib[hhsize_marg_est_tib$GEOLEV1 == regnr,]
  hhsize_tib1reg <- hhsize_tib1reg %>% select(-GEOLEV1)
  
  age_tib1reg <- age_marg_est_tib[age_marg_est_tib$GEOLEV1 == regnr,]
  age_tib1reg <- age_tib1reg %>% select(-GEOLEV1)
  
  edu_tib1reg <- educat_marg_est_tib[educat_marg_est_tib$GEOLEV1 == regnr,]
  edu_tib1reg <- edu_tib1reg %>% select(-GEOLEV1)
  
  sex_tib1reg <- gender_marg_est_tib[gender_marg_est_tib$GEOLEV1 == regnr,]
  sex_tib1reg <- sex_tib1reg %>% select(-GEOLEV1)
  
  
  group_control <- list(income_tib1reg)
  individual_control <- list(hhtype_tib1reg, hhsize_tib1reg, rural_tib1reg, age_tib1reg, edu_tib1reg, sex_tib1reg)
  
  
  names(group_control) <- c('INCOME') 
  names(individual_control) <- c('HHTYPE', 'HHSIZECAT', 'RURAL','AGECAT','EDUCAT', 'GENDER')
  
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
  
  
  fit <- ml_fit(ml_problem = fitting_problem, algorithm = "ipu", maxiter = 50)
  
  
  syn_pop_reg <- ml_replicate(fit, algorithm = "trs")
  
  syn_pop_reg$GEOLEV1 <- regnr
  
  gc()
  
  if (liscodeletters %ni% c('gr', 'be', 'ie', 'cn', 'in', 'it', 'jp')){
    
    GDL_region <- GDL_country[GDL_country$region_number == regnr,]
  } else if (liscodeletters %in% c('gr', 'be', 'ie', 'cn', 'in', 'it', 'jp')){
    
    GDL_country_sum <- GDL_country %>% group_by(region_number) %>% summarise(year2015 = sum(year2015))
    
    GDL_region <- GDL_country_sum[GDL_country_sum$region_number == regnr,]
  }
  
  popsize_syn <- nrow(syn_pop_reg)
  
  popsize_GDL <- as.double(GDL_region$year2015) * 1000
  
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
  
  
  
  nr_individuals_per_regio <- append(nr_individuals_per_regio, nrow(syn_pop_reg))
  
  
  if (liscodeletters %in% c('gr', 'be', 'ie', 'cn', 'in', 'it', 'jp')){
    nr_individuals_per_regio_GDL <- c()
    gdlcodes <- c()
    GDL_pop_country <- GDL_pop[GDL_pop$liscountry == liscodeletters,] 
    GDL_pop_country <- GDL_pop_country[!is.na(GDL_pop_country$region_number),]
    GDL_pop_country_regnr <- GDL_pop_country[GDL_pop_country$region_number == regnr,]
    
    for (gdlcode in GDL_pop_country_regnr$GDLCODE){
      GDL_pop_GDLreg <- GDL_pop_country_regnr[GDL_pop_country_regnr$GDLCODE == gdlcode,]
      
      mp_syn_GDL <- GDL_pop_GDLreg$year2015/(sum(GDL_pop_country_regnr$year2015))
      
      if (mp_syn_GDL < 0.999){
        
        cancelHID <- sample(1:maxHID, round(maxHID*(1-mp_syn_GDL)))
        
        syn_pop_reg_sub <- syn_pop_reg[syn_pop_reg$HID %ni% cancelHID,]
      } else {
        syn_pop_reg_sub <- syn_pop_reg
      }
      
      gdlcodes <- append(gdlcodes, gdlcode)
      nr_individuals_per_regio_GDL <- append(nr_individuals_per_regio_GDL, nrow(syn_pop_reg_sub))
      
      syn_pop_reg$HHSIZECAT <- as.integer(syn_pop_reg$HHSIZECAT)
      syn_pop_reg$RURAL <- as.integer(syn_pop_reg$RURAL)
      syn_pop_reg$GENDER <- as.integer(syn_pop_reg$GENDER)
      syn_pop_reg$AGECAT <- as.integer(syn_pop_reg$AGECAT)
      syn_pop_reg$EDUCAT <- as.integer(syn_pop_reg$EDUCAT)
      syn_pop_reg$FARMING <- as.integer(syn_pop_reg$FARMING)
      syn_pop_reg$INCOME <- as.integer(syn_pop_reg$INCOME)
      syn_pop_reg$HID <- as.integer(syn_pop_reg$HID)
      syn_pop_reg$RELATE <- as.integer(syn_pop_reg$RELATE)
      syn_pop_reg$HHTYPE <- as.integer(syn_pop_reg$HHTYPE)
      syn_pop_reg$GEOLEV1 <- as.integer(syn_pop_reg$GEOLEV1)
      
      
      
      name_regGDL <- paste0(isocode, '_oct23_unknown2_synthpop_', as.character(gdlcode), '.dat')
      
      #con = file(name_regGDL, "wb")
      
      #writeBin(c(syn_pop_reg$INCOME, syn_pop_reg$RURAL, syn_pop_reg$FARMING, syn_pop_reg$AGECAT, syn_pop_reg$GENDER, 
      #           syn_pop_reg$EDUCAT, syn_pop_reg$HHTYPE, syn_pop_reg$HID, syn_pop_reg$RELATE, syn_pop_reg$HHSIZECAT, syn_pop_reg$GEOLEV1), con)
      
      #writeBin(c(syn_pop$INCOME, syn_pop$PID), con)
      
      
      #close(con)
    }
    
    
  }
  
  syn_pop <- rbindlist(list(syn_pop, syn_pop_reg), use.names=TRUE)
  rm(syn_pop_reg)
  gc()
  
  
  
  
  
  
  
}















#regionnumbers_noerror <- regionnumbers[regionnumbers %ni% error_region]


DF_nr_individuals_per_region <- data.frame('Country' = rep(liscode, length(regionnumbers)), 'GEOLEV1' = regionnumbers, 'Nr_individuals' = nr_individuals_per_regio)
print(DF_nr_individuals_per_region)

#DF_variation_popsizes <- data.frame('Country' = rep(liscode, length(regionnumbers)), 'GEOLEV1' = regionnumbers, 'Variation_popsize' = variation_in_pop_array)




##########################################
### THE ERROR PART 2. Other part moved up, because we needed df1tot. 


read_freqs_LIS_regions <- function(txtpathfreq, beginline, endline, regionnumbers){
  
  missingcat <- c()
  missingcat2 <- c()
  
  if (grepl('HHSIZECAT', beginline)){
    
    x <- readLines(txtpathfreq)
    hhsizetable <- x[grep('> print(table(dfmerg$HHSIZECAT))', x, fixed = TRUE):grep('> print(table(dfmerg$INCOMEQQ))', x, fixed = TRUE)]
    
    hhsizetable <- hhsizetable[- 1] #eerste rij eraf. print statement
    hhsizetable <- hhsizetable[- length(hhsizetable)]  #laatste rij eraf. print statement.
    
    hhsizetable = gsub("\\[.*?\\]", "", hhsizetable) #remove square brackets and everything within the brackets. 
    hhsizetable = unlist(strsplit(hhsizetable, split = " "))
    hhsizetable = unlist(lapply(hhsizetable, function(z){ z[!is.na(z) & z != ""]})) #remove empty strings.
    categories = hhsizetable[1:(length(hhsizetable)/2)]
    freqs = hhsizetable[((length(hhsizetable)/2)+1):(length(hhsizetable))]
    freqs = as.integer(freqs)
    missingcat <- setdiff(c('1', '2', '3', '4', '5', '6'), categories)
    missingcat <- as.integer(missingcat)
    
  }
  
  if (grepl('education', beginline)){
    
    x <- readLines(txtpathfreq)
    hhsizetable <- x[grep('> print(table(dfmerg$education))', x, fixed = TRUE):grep('> print(table(dfmerg$sex_number))', x, fixed = TRUE)]
    
    hhsizetable <- hhsizetable[- 1] #eerste rij eraf. print statement
    hhsizetable <- hhsizetable[- length(hhsizetable)]  #laatste rij eraf. print statement.
    
    hhsizetable = gsub("\\[.*?\\]", "", hhsizetable) #remove square brackets and everything within the brackets. 
    hhsizetable = unlist(strsplit(hhsizetable, split = " "))
    hhsizetable = unlist(lapply(hhsizetable, function(z){ z[!is.na(z) & z != ""]})) #remove empty strings.
    categories = hhsizetable[1:(length(hhsizetable)/2)]
    freqs = hhsizetable[((length(hhsizetable)/2)+1):(length(hhsizetable))]
    freqs = as.integer(freqs)
    missingcat2 <- setdiff(c('1', '2', '3', '4', '5'), categories)
    missingcat2 <- as.integer(missingcat2)
    
  }
  
  
  x <- readLines(txtpathfreq)
  begin_pos <- grep(beginline, x, fixed = TRUE)
  end_pos <- grep(endline, x, fixed = TRUE)
  
  text <- x[begin_pos:end_pos]
  
  
  text <- text[- 1] #eerste rij eraf. print statement
  text <- text[- length(text)]  #laatste rij eraf. print statement.
  
  text2 = gsub("\\[.*?\\]", "", text) #remove square brackets and everything within the brackets. 
  text2 = unlist(strsplit(text2, split = " "))
  text2 = unlist(lapply(text2, function(z){ z[!is.na(z) & z != ""]})) #remove empty strings.
  vector_freq1 = as.numeric(text2)
  
  if ((grepl('HHSIZECAT', beginline)) & (length(missingcat) > 0)){
    if (grepl('INCOMEQQ', beginline)){
      for (j in 1:length(regionnumbers)){
        vector_freq1 <- append(vector_freq1, c(0,0,0,0,0), ((j-1)*30)+((missingcat*5)-5))
      }
    } else if((grepl('HHTYPE_CAT', beginline))){
      endloop = 8
    } else if((grepl('AGECAT', beginline))){
      endloop = 7
    } else if((grepl('education', beginline))){
      endloop = 5
    } else if((grepl('sex_number', beginline))){
      endloop = 2
    } else if((grepl('farming_number', beginline))){
      endloop = 2
    }
    
    if (!grepl('INCOMEQQ', beginline)){
      for (i in 1:(endloop*length(regionnumbers))){
        vector_freq1 <- append(vector_freq1, 0, (missingcat-1)+(6*(i-1)))
      } 
    }
    
  }
  

  
  if ((grepl('education', beginline)) & (length(missingcat2) > 0)){
    if (grepl('farming', beginline)){
      for (j in 1:length(regionnumbers)){
        vector_freq1 <- append(vector_freq1, c(0,0), ((j-1)*10)+((missingcat2*2)-2))
      }
    } else if((grepl('HHTYPE_CAT', beginline))){
      for (j in 1:length(regionnumbers)){
        vector_freq1 <- append(vector_freq1, c(0,0,0,0,0,0,0,0), ((j-1)*40)+((missingcat2*8)-8))
      }
    } else if((grepl('sex_number', beginline))){
      for (j in 1:length(regionnumbers)){
        vector_freq1 <- append(vector_freq1, c(0,0), ((j-1)*10)+((missingcat2*2)-2))
      }
    } else if((grepl('INCOMEQQ', beginline))){
      for (j in 1:length(regionnumbers)){
        vector_freq1 <- append(vector_freq1, c(0,0,0,0,0), ((j-1)*25)+((missingcat2*5)-5))
      }
    } else if((grepl('HHSIZECAT', beginline))){
      for (j in 1:length(regionnumbers)){
        vector_freq1 <- append(vector_freq1, c(0,0,0,0,0,0), ((j-1)*30)+((missingcat2*6)-6))
      }
    } else if((grepl('AGECAT', beginline))){
      endloop = 7
    }
    
    if (grepl('AGECAT', beginline)){
      for (i in 1:(endloop*length(regionnumbers))){
        vector_freq1 <- append(vector_freq1, 0, (missingcat2-1)+(5*(i-1))) # length(categories)
      } 
    }
    
  }
  
  
  
  
  
  
  
  l_freq = length(vector_freq1)
  repfactor = l_freq/length(regionnumbers)
  vector_regions = rep(regionnumbers, each = repfactor)
  freqdf <- data.frame('Frequency' = vector_freq1, 'Regions' = vector_regions)
  #correction = freqdf %>% dplyr::group_by(Regions) %>% 
  #  dplyr::summarize(Sum_freq = sum(Frequency))
  
  #freqdf['Sum_freq'] = rep(correction$Sum_freq, each = repfactor)
  #freqdf['Frequency'] = freqdf$Frequency/freqdf$Sum_freq
  
  #freqdf <- freqdf %>% select(-Sum_freq)
  
  return(freqdf)
}


old <- function(){
  #if statement om farming eruit te halen. 
  for (i in 1:length(beginlist_freq)){
    if (i == 1){
      df1 <- read_freqs_LIS_regions(txtpathfreq, beginlist_freq[i], endlist_freq[i], regionnumbers) #uit regiondf halen. 
    } else {
      if (farm_presence == 1){
        df2 <- read_freqs_LIS_regions(txtpathfreq, beginlist_freq[i], endlist_freq[i], regionnumbers)
        
        df1 <- rbind.data.frame(df1, df2)
      } else{
        if (i %ni% c(3,8,9,10,11,12,13)){
          df2 <- read_freqs_LIS_regions(txtpathfreq, beginlist_freq[i], endlist_freq[i], regionnumbers)
          
          df1 <- rbind.data.frame(df1, df2)
          
        }
        
      }
      
    }
    
  }
}





if (farm_presence == 1){
  for (i in 1:length(beginlist_freq)){
    if (i == 1){
      df1 <- read_freqs_LIS_regions(txtpathfreq, beginlist_freq[i], endlist_freq[i], regionnumbers)
    } else {
      df2 <- read_freqs_LIS_regions(txtpathfreq, beginlist_freq[i], endlist_freq[i], regionnumbers)
      df1 <- rbind.data.frame(df1, df2)
      
    }
    
  }
  
}

if (farm_presence == 0){
  for (i in 7:length(beginlist_freq)){
    if (i == 7){
      #changed to 7, was 1. 
      df1 <- read_freqs_LIS_regions(txtpathfreq, beginlist_freq[i], endlist_freq[i], regionnumbers)
    } else {
      df2 <- read_freqs_LIS_regions(txtpathfreq, beginlist_freq[i], endlist_freq[i], regionnumbers)
      
      df1 <- rbind.data.frame(df1, df2)
      
    }
    
  }
  
}














#correct df1tot for regions


regionsvec <- rep(regionnumbers, nr_individuals_per_regio)

syn_pop$GEOLEV1 <- as.factor(regionsvec)
syn_pop$RURAL <- as.factor(syn_pop$RURAL)
syn_pop$HHTYPE <- as.factor(syn_pop$HHTYPE)
syn_pop$FARMING <- as.factor(syn_pop$FARMING)
syn_pop$HHSIZECAT <- as.factor(syn_pop$HHSIZECAT)
syn_pop$INCOME <- as.factor(syn_pop$INCOME)
syn_pop$AGECAT <- as.factor(syn_pop$AGECAT)
syn_pop$EDUCAT <- as.factor(syn_pop$EDUCAT)
syn_pop$GENDER <- as.factor(syn_pop$GENDER)

#levels(syn_pop$RURAL) <- c("0", "1") #1 is rural
#levels(syn_pop$HHTYPE) <- c("1", "2", "3", "4", "5", "6", "7", "8")
#levels(syn_pop$FARMING) <- c("0", "1")
#levels(syn_pop$HHSIZECAT) <- c("1", "2", "3", "4", "5", "6")
#levels(syn_pop$INCOME) <- c("1", "2", "3", "4", "5")
#levels(syn_pop$AGECAT) <- c("1", "2", "3", "4", "5", "6", "7")
#levels(syn_pop$EDUCAT) <- c("1", "2", "3", "4", "5")
#levels(syn_pop$GENDER) <- c("0", "1") #1 is male. 

syn_pophead <- syn_pop[syn_pop$RELATE == 1,]
gc()



gc()
combi8 <- data.frame(xtabs(~ FARMING + HHTYPE + GEOLEV1, data = syn_pophead)/nrow(syn_pophead))

gc()
combi9 <- data.frame(xtabs(~ FARMING + HHSIZECAT + GEOLEV1, data = syn_pophead)/nrow(syn_pophead))

gc()
combi10 <- data.frame(xtabs(~ FARMING + INCOME + GEOLEV1, data = syn_pophead)/nrow(syn_pophead))

gc()
combi11 <- data.frame(xtabs(~ FARMING + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi12 <- data.frame(xtabs(~ FARMING + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi13 <- data.frame(xtabs(~ FARMING + GENDER + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi14 <- data.frame(xtabs(~ INCOME + HHTYPE + GEOLEV1, data = syn_pophead)/nrow(syn_pophead))

gc()
combi15 <- data.frame(xtabs(~ INCOME + HHSIZECAT + GEOLEV1, data = syn_pophead)/nrow(syn_pophead))

gc()
combi16 <- data.frame(xtabs(~ INCOME + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi17 <- data.frame(xtabs(~ INCOME + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi18 <- data.frame(xtabs(~ INCOME + GENDER + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi19 <- data.frame(xtabs(~ HHSIZECAT + HHTYPE + GEOLEV1, data = syn_pophead)/nrow(syn_pophead))

gc()
combi20 <- data.frame(xtabs(~ HHSIZECAT + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi21 <- data.frame(xtabs(~ HHSIZECAT + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi22 <- data.frame(xtabs(~ HHSIZECAT + GENDER + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi23 <- data.frame(xtabs(~ HHTYPE + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi24 <- data.frame(xtabs(~ HHTYPE + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi25 <- data.frame(xtabs(~ HHTYPE + GENDER + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi26 <- data.frame(xtabs(~ GENDER + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi27 <- data.frame(xtabs(~ GENDER + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))

gc()
combi28 <- data.frame(xtabs(~ EDUCAT + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))



if (farm_presence == 1){
  
  allcombi_freqs = c(combi8$Freq, combi9$Freq, combi10$Freq,
                     combi11$Freq, combi12$Freq, combi13$Freq, combi14$Freq, combi15$Freq, combi16$Freq, combi17$Freq, combi18$Freq, combi19$Freq, combi20$Freq,
                     combi21$Freq, combi22$Freq, combi23$Freq, combi24$Freq, combi25$Freq, combi26$Freq, combi27$Freq, combi28$Freq)
  
} else if (farm_presence == 0){
  
  allcombi_freqs = c(combi14$Freq, combi15$Freq, combi16$Freq, combi17$Freq, combi18$Freq, combi19$Freq, combi20$Freq,
                     combi21$Freq, combi22$Freq, combi23$Freq, combi24$Freq, combi25$Freq, combi26$Freq, combi27$Freq, combi28$Freq)
  
  
  
}






#df_ALLREG <- data.frame('Survey' = df1$Frequency, 'Synthetic' = allcombi_freqs, 'Marginal' = allcombi_margfreqs, 'National' = vec1tot_allreg)
df_ALLREG <- data.frame('Survey' = df1$Frequency, 'Synthetic' = allcombi_freqs)

check_sums_survey <- sum(df_ALLREG$Survey)
check_sums_synth <- sum(df_ALLREG$Synthetic)
#check_sums_national <- sum(df_ALLREG$National)
#check_sums_marginal <- sum(df_ALLREG$Marginal)




if ((check_sums_survey - check_sums_synth) <= 0.01){
  
  
  standardized_error_factor <- 100/sum(df_ALLREG$Synthetic)
  print('standerror (removed)')
  print(standardized_error_factor)
  standardized_error_factor = 1
  df_ALLREG$Survey <- df_ALLREG$Survey * standardized_error_factor
  df_ALLREG$Synthetic <- df_ALLREG$Synthetic * standardized_error_factor
  #df_ALLREG$National <- df_ALLREG$National * standardized_error_factor
  #df_ALLREG$Marginal <- df_ALLREG$Marginal * standardized_error_factor
  
  
  df_ALLREG['Sq_error_survey_synth'] <- (df_ALLREG$Survey - df_ALLREG$Synthetic)^2
  #df_ALLREG['Sq_error_survey_national'] <- (df_ALLREG$Survey - df_ALLREG$National)^2
  #df_ALLREG['Sq_error_survey_marginal'] <- (df_ALLREG$Survey - df_ALLREG$Marginal)^2
  df_ALLREG[is.na(df_ALLREG)] <- 0
  
  DF_errors[1,2] <- sum(df_ALLREG$Sq_error_survey_synth)
  #DF_errors[1,3] <- sum(df_ALLREG$Sq_error_survey_national)
  #DF_errors[1,4] <- sum(df_ALLREG$Sq_error_survey_marginal)
  
} else if ((check_sums_survey - check_sums_synth) > 0.01){
  DF_errors[1,2] <- -99
}





filename_dferrors <- paste0("synth_errors_unknown2_LIS_nov23_", isocode, ".csv")

write.csv(DF_errors, filename_dferrors, row.names = FALSE)

print('saved output')






