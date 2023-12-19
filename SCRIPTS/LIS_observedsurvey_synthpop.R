#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)



liscode = as.character(args[1])


liscodeletters <- unlist(strsplit(liscode, ""))
liscodeletters <- paste0(liscodeletters[1], liscodeletters[2])



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



GDL_pop <- read.csv('GDL_match_population_all_LIS.csv', sep = ';', header = TRUE)

GDL_pop <- na.omit(GDL_pop)



liscodecountries <- read.csv('LIS_codes_countries2.csv', sep = ';') #dit bestand is zonder spaties. 
countryname = liscodecountries[liscodecountries$LISCODE == liscode,]$Country


GDL_population <- read.csv('GDL_subnational_population_world.csv', head = TRUE, sep = ',')
GDL_pop2015 <- GDL_population[GDL_population$year == 2015,]
GDLcountry <- GDL_pop2015[GDL_pop2015$country == countryname,]






liscode = liscodecountries[liscodecountries$LISCODE == liscode,]$iso_code
print('liscode isocode')
print(liscode)



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
  
  #characters_inline1test = gsub('[[:digit:]]+', "", characters_inline1test)
  #characters_inline1test = gsub(" ", "", characters_inline1test, fixed = TRUE)
  #gsub("[A-Z]{2,}","",characters_inline1test)
  #characters_inline1test = unlist(lapply(characters_inline1test, function(z){ z[!is.na(z) & z != ""]}))
  #characters_inline1test = gsub("([a-z])([A-Z])","\\1 \\2",characters_inline1test)
  
  
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
  
  
  text <- text[- 1] 
  text <- text[- length(text)] 
  
  region_numbers_string = gsub("\\[.*?\\]", "", text) #remove square brackets and everything within the brackets. 
  region_numbers_string = unlist(strsplit(region_numbers_string, split = " "))
  region_numbers_string = unlist(lapply(region_numbers_string, function(z){ z[!is.na(z) & z != ""]})) #remove empty strings.
  region_numbers = as.numeric(region_numbers_string)
  
  regiondf <- data.frame('region_number' = region_numbers, 'region_name' = region_numbers)
  
  return(regiondf)
  
}

yes_no_nonrelatives <- function(txtpath){
  x <- readLines(txtpath)
  
  begin_pos <- grep("+ } #presence nonrelatives", x, fixed = TRUE)
  end_pos <- grep("> uniquenonreldum <- unique(dfmerg$nonrelativedummy)", x, fixed = TRUE)
  
  text <- x[begin_pos:end_pos]
  
  
  text <- text[- 1] 
  text <- text[- length(text)]  
  
  if (grepl("yesnonrelatives", text, fixed = TRUE) == TRUE){
    nonrelative_ind = 1
  } else{
    nonrelative_ind = 0
  }
  
  return(nonrelative_ind)
  
}

farming_available <- function(txtpath){
  x <- readLines(txtpath)
  
  begin_pos <- grep("> print(farmavailable)", x, fixed = TRUE)
  end_pos <- grep("> #FARMSTATEMENT", x, fixed = TRUE)
  
  text <- x[begin_pos:end_pos]
  
  
  text <- text[- 1]
  text <- text[- length(text)]  
  
  if (grepl("NO", text, fixed = TRUE) == TRUE){
    farming_available = 0
  } else{
    farming_available = 1
  }
  
  return(farming_available)
  
}




text_to_df <- function(txtpath, beginline, endline, hhtype) {
  x <- readLines(txtpath)
  
  begin_pos <- grep(beginline, x, fixed = TRUE)
  end_pos <- grep(endline, x, fixed = TRUE)
  
  text <- x[begin_pos:end_pos]
  
  text <- text[- 1]
  text <- text[- length(text)]   
  
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
  
  variablename <- characters_inline1test[1]
  
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
text_to_df_small_table <- function(txtpath, beginline, endline){
  x <- readLines(txtpath)
  
  begin_pos <- grep(beginline, x, fixed = TRUE)
  end_pos <- grep(endline, x, fixed = TRUE)
  
  text <- x[begin_pos:end_pos]
  text <- text[- (1:2)] #eerste 2 rijen eraf. print statement
  text <- text[- length(text)]  #laatste rij eraf. print statement.  
  
  nr_rows <- length(nchar(text))
  
  if (nr_rows == 0){
    return(0)
  } else if (nr_rows > 0){
    
    for (i in 1:nr_rows){
      
      characters_inline1 = unlist(strsplit(text[i], split = " "))
      characters_inline1 <- characters_inline1[characters_inline1 != ""]
      numbers_inline1 <- as.numeric(characters_inline1)  
      
      if (i == 1){
        list_children1 = c(numbers_inline1)
        print('1if', i)
      } else if (i == 2){
        list_children2 = c(numbers_inline1)
        print('2if', i)
      } else if ((i %% 2) != 0){
        list_children1 <- c(list_children1, numbers_inline1)
        print('3if', i)
      } else if ((i %% 2) == 0){
        list_children2 <- c(list_children2, numbers_inline1)
        print('4if', i)
      }
      
    }
    
    return(list(list_children1,list_children2))
  }
  
  
}
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


new_frequencies <- function(df1, df2){
  
  #var = 'HHSIZECAT'
  
  variables_df1 <- colnames(df1)
  variables_df2 <- colnames(df2)
  
  var = setdiff(variables_df2, variables_df1)
  
  if (length(var) != 1){
    #return
    print('Only 1 variable can be added')
  }
  
  
  
  # we laten age_part alleen afhangen van age_head
  if ("age_part" %in% colnames(df2) & "sex_part" %ni% colnames(df2)){
    df2 <- df2 %>% group_by(age_head, age_part, hhtype) %>% summarise(Frequency = sum(Frequency))
  }
  
  
  # sex niet imputen. 
  if ("sex_part" %in% colnames(df2) & "educ_part" %ni% colnames(df2)){
    df2 <- df2 %>% group_by(age_part, sex_head, sex_part, hhtype) %>% summarise(Frequency = sum(Frequency))
  }
  
  
  # educ_part hangt alleen af van educ_head. Niet meer van age and gender. 
  if ("educ_part" %in% colnames(df2)){
    df2 <- df2 %>% group_by(educ_head, educ_part, hhtype) %>% summarise(Frequency = sum(Frequency))
  }
  
  colnames(df2)[which(names(df2) == var)] <- "var"
  
  #colnames(dfbhh)[which(names(dfbhh) == "dep")] <- dep #terug naar oude namen
  
  
  
  df1$ID <- (1:nrow(df1))
  
  n <- length(unique(df2$var))
  
  
  df1 <- do.call("rbind", replicate(n, df1, simplify = FALSE))
  
  var_column <- kronecker(unique(df2$var), integer(nrow(df1)/n)+1, FUN = "*", make.dimnames = FALSE)
  
  df1$var <- var_column
  
  
  df2 <- rename(df2, Frequency_var = Frequency)
  
  join_on_variables <- colnames(df2)
  join_on_variables <- join_on_variables[join_on_variables != "Frequency_var"]
  
  df1_mergetest <- left_join(df1, df2, by = join_on_variables)
  
  
  Sum_Freq_var <- df1_mergetest %>%
    group_by(ID) %>%
    summarise(Sum_Freq_var = sum(Frequency_var))
  
  
  df1_mergetest <- left_join(df1_mergetest, Sum_Freq_var, by = 'ID')
  
  df1_mergetest$Percent_Freq_var <- df1_mergetest$Frequency_var/df1_mergetest$Sum_Freq_var
  
  
  df1_mergetest$New_weighted_frequency <- df1_mergetest$Frequency * df1_mergetest$Percent_Freq_var
  
  df1_mergetest[is.na(df1_mergetest)] <- 0
  
  
  Check_new_Freq <- df1_mergetest %>%
    group_by(ID) %>%
    summarise(Sum_Percent_Freq_var = sum(Percent_Freq_var))
  
  
  df1_mergetest <- df1_mergetest %>% select(-ID, -Frequency, -Frequency_var, -Sum_Freq_var, -Percent_Freq_var)
  
  colnames(df1_mergetest)[which(names(df1_mergetest) == "var")] <- var #terug naar oude namen
  
  df1_mergetest <- rename(df1_mergetest, Frequency = New_weighted_frequency)
  
  return(df1_mergetest)
  
}

new_frequencies_updatemay23 <- function(df1, df2, var){
  
  # var is age_part, sex_part, or educ_part. 
  # df2 comes from new partner joints. 
  colnames(df2)[which(names(df2) == var)] <- "var"
  
  #colnames(dfbhh)[which(names(dfbhh) == "dep")] <- dep #terug naar oude namen
  
  
  
  df1$ID <- (1:nrow(df1))
  
  n <- length(unique(df2$var))
  
  
  df1 <- do.call("rbind", replicate(n, df1, simplify = FALSE))
  
  var_column <- kronecker(unique(df2$var), integer(nrow(df1)/n)+1, FUN = "*", make.dimnames = FALSE)
  
  df1$var <- var_column
  
  
  df2 <- rename(df2, Frequency_var = Frequency)
  
  join_on_variables <- colnames(df2)
  join_on_variables <- join_on_variables[join_on_variables != "Frequency_var"]
  
  df1_mergetest <- left_join(df1, df2, by = join_on_variables)
  
  
  Sum_Freq_var <- df1_mergetest %>%
    group_by(ID) %>%
    summarise(Sum_Freq_var = sum(Frequency_var))
  
  
  df1_mergetest <- left_join(df1_mergetest, Sum_Freq_var, by = 'ID')
  
  df1_mergetest$Percent_Freq_var <- df1_mergetest$Frequency_var/df1_mergetest$Sum_Freq_var
  
  
  df1_mergetest$New_weighted_frequency <- df1_mergetest$Frequency * df1_mergetest$Percent_Freq_var
  
  df1_mergetest[is.na(df1_mergetest)] <- 0
  
  
  Check_new_Freq <- df1_mergetest %>%
    group_by(ID) %>%
    summarise(Sum_Percent_Freq_var = sum(Percent_Freq_var))
  
  
  df1_mergetest <- df1_mergetest %>% select(-ID, -Frequency, -Frequency_var, -Sum_Freq_var, -Percent_Freq_var)
  
  colnames(df1_mergetest)[which(names(df1_mergetest) == "var")] <- var #terug naar oude namen
  
  df1_mergetest <- rename(df1_mergetest, Frequency = New_weighted_frequency)
  
  return(df1_mergetest)
  
}


fill_zero_cells <- function(jointhead1, farm_presence){
  # remove farming if it exists 
  colnames <- names(jointhead1)
  
  
  hhtypepresent = 0
  if ("hhtype" %in% colnames(jointhead1)) {
    hhtypepresent = 1
    householdtype = jointhead1['hhtype'][[1,1]]
    jointhead1 <- subset(jointhead1, select = -hhtype)
  }
  
  add_farm = 0
  if ("farming_number" %in% colnames(jointhead1) & farm_presence == 0) {
    print(add_farm)
    add_farm = 1
    colnames <- names(jointhead1)
    colnames <- colnames[-length(colnames)]
    colnames <- colnames[-grep("farming_number", colnames)]
    jointhead1 <- jointhead1 %>% group_by(across(all_of(colnames))) %>% summarise(Frequency = sum(Frequency))
  } 
  
  
  colnames <- names(jointhead1)
  colnames <- colnames[-length(colnames)]
  
  # if statement inbouwen voor aantal column names: 
  
  homopresence = 0
  if ("sex_head" %in% colnames(jointhead1) & "sex_part" %in% colnames(jointhead1)) {
    hetero = jointhead1[jointhead1$sex_head != jointhead1$sex_part,]
    homo = jointhead1[jointhead1$sex_head == jointhead1$sex_part,]
    
    homopresence = sum(homo$Frequency)
    
    if (homopresence == 0){
      
      jointhead1 = hetero
      
      #hier ook maken. 
    } else if (homopresence > 0){
      
      print('gay couples present')
      
      hetero = subset(hetero, select = -sex_part)
      
      colnames = names(hetero)
      colnames = colnames[-length(colnames)]
      
      comb4 <- combn(colnames, length(colnames)-1)
      
      jointhead1total <- jointhead1
      
      jointhead1 = hetero
      jointhead0 <- hetero[hetero$Frequency == 0,]
      
      
      
      
      impute1 = c()
      impute2 = c()
      impute3 = c()
      
      
      jh4_1 <- jointhead1 %>% group_by(across(all_of(comb4[,1]))) %>% summarise(Frequency = sum(Frequency))
      jh4_2 <- jointhead1 %>% group_by(across(all_of(comb4[,2]))) %>% summarise(Frequency = sum(Frequency))  
      jh4_3 <- jointhead1 %>% group_by(across(all_of(comb4[,3]))) %>% summarise(Frequency = sum(Frequency))
      
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
      
      
      for (row in 1:nrow(jointhead0)){
        rowjh <- jointhead0[row,]
        
        jh4_1s <- jh4_1[jh4_1[comb4[1,1]] == rowjh[comb4[1,1]][[1]] & jh4_1[comb4[2,1]] == rowjh[comb4[2,1]][[1]],]
        jh4_2s <- jh4_2[jh4_2[comb4[1,2]] == rowjh[comb4[1,2]][[1]] & jh4_2[comb4[2,2]] == rowjh[comb4[2,2]][[1]],]
        jh4_3s <- jh4_3[jh4_3[comb4[1,3]] == rowjh[comb4[1,3]][[1]] & jh4_3[comb4[2,3]] == rowjh[comb4[2,3]][[1]],]
        
        impute1 = c(impute1,jh4_1s$Frequency * marg1[marg1[margname1] == rowjh[margname1][[1]],]$Frequency)
        
        impute2 = c(impute2,jh4_2s$Frequency * marg2[marg2[margname2] == rowjh[margname2][[1]],]$Frequency)
        
        impute3 = c(impute3,jh4_3s$Frequency * marg3[marg3[margname3] == rowjh[margname3][[1]],]$Frequency)
        
        
      }
      
      
      imputationshet <- data.frame(impute1 = impute1, impute2 = impute2, impute3 = impute3)
      imputationshet <- imputationshet %>% 
        mutate(mean_impute = (impute1 + impute2 + impute3) / 3)
      
      
      jointhead1hetero <- jointhead1
      jointhead0hetero <- jointhead0
      
      zero_indices <- which(jointhead1hetero$Frequency == 0)
      jointhead1hetero$imputations <- jointhead1hetero$Frequency
      jointhead1hetero$imputations[zero_indices] <- imputationshet$mean_impute
      
      sumfreq = sum(jointhead1hetero$Frequency)
      sumimpu = sum(jointhead1hetero$imputations)
      
      jointhead1hetero$imputations = jointhead1hetero$imputations * (sumfreq/sumimpu)
      
      jointhead1hetero$sex_part = abs((jointhead1hetero$sex_head)-1)
      
      cols = names(jointhead1hetero)
      cols_reordered <- c(cols[1:3], cols[6], cols[4:5])
      
      jointhead1hetero <- jointhead1hetero[, cols_reordered]
      
      
      homo = subset(homo, select = -sex_part)
      
      colnames = names(homo)
      colnames = colnames[-length(colnames)]
      
      comb4 <- combn(colnames, length(colnames)-1)
      
      
      jointhead1 <- homo
      jointhead0 <- homo[homo$Frequency == 0,]
      
      
      
      
      impute1 = c()
      impute2 = c()
      impute3 = c()
      
      
      
      jh4_1 <- jointhead1 %>% group_by(across(all_of(comb4[,1]))) %>% summarise(Frequency = sum(Frequency))
      jh4_2 <- jointhead1 %>% group_by(across(all_of(comb4[,2]))) %>% summarise(Frequency = sum(Frequency))  
      jh4_3 <- jointhead1 %>% group_by(across(all_of(comb4[,3]))) %>% summarise(Frequency = sum(Frequency))
      
      sumfreq = sum(homo$Frequency)
      
      margname1 = colnames[!grepl(paste(comb4[,1], collapse = "|"), colnames)]
      marg1 = jointhead1 %>% group_by(across(all_of(margname1))) %>% summarise(Frequency = sum(Frequency))
      marg1$Frequency = marg1$Frequency/sumfreq
      
      margname2 = colnames[!grepl(paste(comb4[,2], collapse = "|"), colnames)]
      marg2 = jointhead1 %>% group_by(across(all_of(margname2))) %>% summarise(Frequency = sum(Frequency))
      marg2$Frequency = marg2$Frequency/sumfreq
      
      margname3 = colnames[!grepl(paste(comb4[,3], collapse = "|"), colnames)]
      marg3 = jointhead1 %>% group_by(across(all_of(margname3))) %>% summarise(Frequency = sum(Frequency))
      marg3$Frequency = marg3$Frequency/sumfreq
      
      
      for (row in 1:nrow(jointhead0)){
        rowjh <- jointhead0[row,]
        
        jh4_1s <- jh4_1[jh4_1[comb4[1,1]] == rowjh[comb4[1,1]][[1]] & jh4_1[comb4[2,1]] == rowjh[comb4[2,1]][[1]],]
        jh4_2s <- jh4_2[jh4_2[comb4[1,2]] == rowjh[comb4[1,2]][[1]] & jh4_2[comb4[2,2]] == rowjh[comb4[2,2]][[1]],]
        jh4_3s <- jh4_3[jh4_3[comb4[1,3]] == rowjh[comb4[1,3]][[1]] & jh4_3[comb4[2,3]] == rowjh[comb4[2,3]][[1]],]
        
        impute1 = c(impute1,jh4_1s$Frequency * marg1[marg1[margname1] == rowjh[margname1][[1]],]$Frequency)
        
        impute2 = c(impute2,jh4_2s$Frequency * marg2[marg2[margname2] == rowjh[margname2][[1]],]$Frequency)
        
        impute3 = c(impute3,jh4_3s$Frequency * marg3[marg3[margname3] == rowjh[margname3][[1]],]$Frequency)
        
        
      }
      
      
      imputationshomo <- data.frame(impute1 = impute1, impute2 = impute2, impute3 = impute3)
      imputationshomo <- imputationshomo %>% 
        mutate(mean_impute = (impute1 + impute2 + impute3) / 3)
      
      
      zero_indices <- which(jointhead1$Frequency == 0)
      jointhead1$imputations <- jointhead1$Frequency
      jointhead1$imputations[zero_indices] <- imputationshomo$mean_impute
      
      sumfreq = sum(jointhead1$Frequency)
      sumimpu = sum(jointhead1$imputations)
      
      jointhead1$imputations = jointhead1$imputations * (sumfreq/sumimpu)
      
      jointhead1$sex_part = jointhead1$sex_head
      
      cols = names(jointhead1)
      cols_reordered <- c(cols[1:3], cols[6], cols[4:5])
      
      jointhead1 <- jointhead1[, cols_reordered]
      
      jointhead1 <- rbind(jointhead1, jointhead1hetero)
    }
    
    
  } 
  
  else if (length(colnames) == 6){
    
    comb4 <- combn(colnames, length(colnames)-1)
    
    jointhead0 <- jointhead1[jointhead1$Frequency == 0,]
    #jointhead0 <- jointhead1
    
    
    
    impute1 = c()
    impute2 = c()
    impute3 = c()
    impute4 = c()
    impute5 = c()
    impute6 = c()
    
    
    jh4_1 <- jointhead1 %>% group_by(across(all_of(comb4[,1]))) %>% summarise(Frequency = sum(Frequency))
    jh4_2 <- jointhead1 %>% group_by(across(all_of(comb4[,2]))) %>% summarise(Frequency = sum(Frequency))  
    jh4_3 <- jointhead1 %>% group_by(across(all_of(comb4[,3]))) %>% summarise(Frequency = sum(Frequency))
    jh4_4 <- jointhead1 %>% group_by(across(all_of(comb4[,4]))) %>% summarise(Frequency = sum(Frequency))  
    jh4_5 <- jointhead1 %>% group_by(across(all_of(comb4[,5]))) %>% summarise(Frequency = sum(Frequency)) 
    jh4_6 <- jointhead1 %>% group_by(across(all_of(comb4[,6]))) %>% summarise(Frequency = sum(Frequency)) 
    
    
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
    
    for (row in 1:nrow(jointhead0)){
      rowjh <- jointhead0[row,]
      
      jh4_1s <- jh4_1[jh4_1[comb4[1,1]] == rowjh[comb4[1,1]][[1]] & jh4_1[comb4[2,1]] == rowjh[comb4[2,1]][[1]] & jh4_1[comb4[3,1]] == rowjh[comb4[3,1]][[1]] & jh4_1[comb4[4,1]] == rowjh[comb4[4,1]][[1]] & jh4_1[comb4[5,1]] == rowjh[comb4[5,1]][[1]],]
      jh4_2s <- jh4_2[jh4_2[comb4[1,2]] == rowjh[comb4[1,2]][[1]] & jh4_2[comb4[2,2]] == rowjh[comb4[2,2]][[1]] & jh4_2[comb4[3,2]] == rowjh[comb4[3,2]][[1]] & jh4_2[comb4[4,2]] == rowjh[comb4[4,2]][[1]] & jh4_2[comb4[5,2]] == rowjh[comb4[5,2]][[1]],]
      jh4_3s <- jh4_3[jh4_3[comb4[1,3]] == rowjh[comb4[1,3]][[1]] & jh4_3[comb4[2,3]] == rowjh[comb4[2,3]][[1]] & jh4_3[comb4[3,3]] == rowjh[comb4[3,3]][[1]] & jh4_3[comb4[4,3]] == rowjh[comb4[4,3]][[1]] & jh4_3[comb4[5,3]] == rowjh[comb4[5,3]][[1]],]
      jh4_4s <- jh4_4[jh4_4[comb4[1,4]] == rowjh[comb4[1,4]][[1]] & jh4_4[comb4[2,4]] == rowjh[comb4[2,4]][[1]] & jh4_4[comb4[3,4]] == rowjh[comb4[3,4]][[1]] & jh4_4[comb4[4,4]] == rowjh[comb4[4,4]][[1]] & jh4_4[comb4[5,4]] == rowjh[comb4[5,4]][[1]],]
      jh4_5s <- jh4_5[jh4_5[comb4[1,5]] == rowjh[comb4[1,5]][[1]] & jh4_5[comb4[2,5]] == rowjh[comb4[2,5]][[1]] & jh4_5[comb4[3,5]] == rowjh[comb4[3,5]][[1]] & jh4_5[comb4[4,5]] == rowjh[comb4[4,5]][[1]] & jh4_5[comb4[5,5]] == rowjh[comb4[5,5]][[1]],]
      jh4_6s <- jh4_6[jh4_6[comb4[1,6]] == rowjh[comb4[1,6]][[1]] & jh4_6[comb4[2,5]] == rowjh[comb4[2,6]][[1]] & jh4_6[comb4[3,6]] == rowjh[comb4[3,6]][[1]] & jh4_6[comb4[4,6]] == rowjh[comb4[4,6]][[1]] & jh4_6[comb4[5,6]] == rowjh[comb4[5,6]][[1]],]
      
      impute1 = c(impute1,jh4_1s$Frequency * marg1[marg1[margname1] == rowjh[margname1][[1]],]$Frequency)
      
      impute2 = c(impute2,jh4_2s$Frequency * marg2[marg2[margname2] == rowjh[margname2][[1]],]$Frequency)
      
      impute3 = c(impute3,jh4_3s$Frequency * marg3[marg3[margname3] == rowjh[margname3][[1]],]$Frequency)
      
      impute4 = c(impute4,jh4_4s$Frequency * marg4[marg4[margname4] == rowjh[margname4][[1]],]$Frequency)
      
      impute5 = c(impute5, jh4_5s$Frequency * marg5[marg5[margname5] == rowjh[margname5][[1]],]$Frequency)
      
      impute6 = c(impute6, jh4_6s$Frequency * marg6[marg6[margname6] == rowjh[margname6][[1]],]$Frequency)
      
    }
    
    
    imputations <- data.frame(impute1 = impute1, impute2 = impute2, impute3 = impute3, impute4 = impute4, impute5 = impute5, impute6 = impute6)
    imputations <- imputations %>% 
      mutate(mean_impute = (impute1 + impute2 + impute3 + impute4 + impute5 + impute6) / 6)
  }
  
  else if (length(colnames) == 5){
    
    comb4 <- combn(colnames, length(colnames)-1)
    
    jointhead0 <- jointhead1[jointhead1$Frequency == 0,]
    #jointhead0 <- jointhead1
    
    
    
    impute1 = c()
    impute2 = c()
    impute3 = c()
    impute4 = c()
    impute5 = c()
    
    
    jh4_1 <- jointhead1 %>% group_by(across(all_of(comb4[,1]))) %>% summarise(Frequency = sum(Frequency))
    jh4_2 <- jointhead1 %>% group_by(across(all_of(comb4[,2]))) %>% summarise(Frequency = sum(Frequency))  
    jh4_3 <- jointhead1 %>% group_by(across(all_of(comb4[,3]))) %>% summarise(Frequency = sum(Frequency))
    jh4_4 <- jointhead1 %>% group_by(across(all_of(comb4[,4]))) %>% summarise(Frequency = sum(Frequency))  
    jh4_5 <- jointhead1 %>% group_by(across(all_of(comb4[,5]))) %>% summarise(Frequency = sum(Frequency)) 
    
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
    
    for (row in 1:nrow(jointhead0)){
      rowjh <- jointhead0[row,]
      
      jh4_1s <- jh4_1[jh4_1[comb4[1,1]] == rowjh[comb4[1,1]][[1]] & jh4_1[comb4[2,1]] == rowjh[comb4[2,1]][[1]] & jh4_1[comb4[3,1]] == rowjh[comb4[3,1]][[1]] & jh4_1[comb4[4,1]] == rowjh[comb4[4,1]][[1]],]
      jh4_2s <- jh4_2[jh4_2[comb4[1,2]] == rowjh[comb4[1,2]][[1]] & jh4_2[comb4[2,2]] == rowjh[comb4[2,2]][[1]] & jh4_2[comb4[3,2]] == rowjh[comb4[3,2]][[1]] & jh4_2[comb4[4,2]] == rowjh[comb4[4,2]][[1]],]
      jh4_3s <- jh4_3[jh4_3[comb4[1,3]] == rowjh[comb4[1,3]][[1]] & jh4_3[comb4[2,3]] == rowjh[comb4[2,3]][[1]] & jh4_3[comb4[3,3]] == rowjh[comb4[3,3]][[1]] & jh4_3[comb4[4,3]] == rowjh[comb4[4,3]][[1]],]
      jh4_4s <- jh4_4[jh4_4[comb4[1,4]] == rowjh[comb4[1,4]][[1]] & jh4_4[comb4[2,4]] == rowjh[comb4[2,4]][[1]] & jh4_4[comb4[3,4]] == rowjh[comb4[3,4]][[1]] & jh4_4[comb4[4,4]] == rowjh[comb4[4,4]][[1]],]
      jh4_5s <- jh4_5[jh4_5[comb4[1,5]] == rowjh[comb4[1,5]][[1]] & jh4_5[comb4[2,5]] == rowjh[comb4[2,5]][[1]] & jh4_5[comb4[3,5]] == rowjh[comb4[3,5]][[1]] & jh4_5[comb4[4,5]] == rowjh[comb4[4,5]][[1]],]
      
      impute1 = c(impute1,jh4_1s$Frequency * marg1[marg1[margname1] == rowjh[margname1][[1]],]$Frequency)
      
      impute2 = c(impute2,jh4_2s$Frequency * marg2[marg2[margname2] == rowjh[margname2][[1]],]$Frequency)
      
      impute3 = c(impute3,jh4_3s$Frequency * marg3[marg3[margname3] == rowjh[margname3][[1]],]$Frequency)
      
      impute4 = c(impute4,jh4_4s$Frequency * marg4[marg4[margname4] == rowjh[margname4][[1]],]$Frequency)
      
      impute5 = c(impute5, jh4_5s$Frequency * marg5[marg5[margname5] == rowjh[margname5][[1]],]$Frequency)
      
    }
    
    
    imputations <- data.frame(impute1 = impute1, impute2 = impute2, impute3 = impute3, impute4 = impute4, impute5 = impute5)
    imputations <- imputations %>% 
      mutate(mean_impute = (impute1 + impute2 + impute3 + impute4 + impute5) / 5)
  }
  
  
  
  
  
  
  else if (length(colnames) == 4){
    
    comb4 <- combn(colnames, length(colnames)-1)
    
    jointhead0 <- jointhead1[jointhead1$Frequency == 0,]
    
    
    
    
    impute1 = c()
    impute2 = c()
    impute3 = c()
    impute4 = c()
    
    
    jh4_1 <- jointhead1 %>% group_by(across(all_of(comb4[,1]))) %>% summarise(Frequency = sum(Frequency))
    jh4_2 <- jointhead1 %>% group_by(across(all_of(comb4[,2]))) %>% summarise(Frequency = sum(Frequency))  
    jh4_3 <- jointhead1 %>% group_by(across(all_of(comb4[,3]))) %>% summarise(Frequency = sum(Frequency))
    jh4_4 <- jointhead1 %>% group_by(across(all_of(comb4[,4]))) %>% summarise(Frequency = sum(Frequency))  
    
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
    
    
    for (row in 1:nrow(jointhead0)){
      rowjh <- jointhead0[row,]
      
      jh4_1s <- jh4_1[jh4_1[comb4[1,1]] == rowjh[comb4[1,1]][[1]] & jh4_1[comb4[2,1]] == rowjh[comb4[2,1]][[1]] & jh4_1[comb4[3,1]] == rowjh[comb4[3,1]][[1]],]
      jh4_2s <- jh4_2[jh4_2[comb4[1,2]] == rowjh[comb4[1,2]][[1]] & jh4_2[comb4[2,2]] == rowjh[comb4[2,2]][[1]] & jh4_2[comb4[3,2]] == rowjh[comb4[3,2]][[1]],]
      jh4_3s <- jh4_3[jh4_3[comb4[1,3]] == rowjh[comb4[1,3]][[1]] & jh4_3[comb4[2,3]] == rowjh[comb4[2,3]][[1]] & jh4_3[comb4[3,3]] == rowjh[comb4[3,3]][[1]],]
      jh4_4s <- jh4_4[jh4_4[comb4[1,4]] == rowjh[comb4[1,4]][[1]] & jh4_4[comb4[2,4]] == rowjh[comb4[2,4]][[1]] & jh4_4[comb4[3,4]] == rowjh[comb4[3,4]][[1]],]
      
      impute1 = c(impute1,jh4_1s$Frequency * marg1[marg1[margname1] == rowjh[margname1][[1]],]$Frequency)
      
      impute2 = c(impute2,jh4_2s$Frequency * marg2[marg2[margname2] == rowjh[margname2][[1]],]$Frequency)
      
      impute3 = c(impute3,jh4_3s$Frequency * marg3[marg3[margname3] == rowjh[margname3][[1]],]$Frequency)
      
      impute4 = c(impute4,jh4_4s$Frequency * marg4[marg4[margname4] == rowjh[margname4][[1]],]$Frequency)
      
      
    }
    
    
    imputations <- data.frame(impute1 = impute1, impute2 = impute2, impute3 = impute3, impute4 = impute4)
    imputations <- imputations %>% 
      mutate(mean_impute = (impute1 + impute2 + impute3 + impute4) / 4)
  }
  
  
  if (homopresence == 0){ 
    zero_indices <- which(jointhead1$Frequency == 0)
    jointhead1$imputations <- jointhead1$Frequency
    jointhead1$imputations[zero_indices] <- imputations$mean_impute
    
    sumfreq = sum(jointhead1$Frequency)
    sumimpu = sum(jointhead1$imputations)
    
    jointhead1$imputations = jointhead1$imputations * (sumfreq/sumimpu)
  }
  
  
  # voeg hhtype weer toe: 
  
  if (hhtypepresent == 1) {
    jointhead1['hhtype'] = householdtype
    cols = names(jointhead1)
    cols_reordered <- c(cols[1:(length(cols)-3)], cols[length(cols)], cols[(length(cols)-2):(length(cols)-1)])
    jointhead1 <- jointhead1[, cols_reordered]
    
  }
  
  # voeg farming weer toe: 
  if (add_farm == 1){
    jointhead2 <- jointhead1
    jointhead1['farming_number'] = 0
    jointhead2['farming_number'] = 1
    
    jointhead1 <- rbind(jointhead1, jointhead2)
    jointhead1['Frequency'] = jointhead1['Frequency']/2
    jointhead1['imputations'] = jointhead1['imputations']/2
    
    cols = names(jointhead1)
    cols_reordered <- c(cols[1:2], cols[length(cols)], cols[3:(length(cols)-1)])
    jointhead1 <- jointhead1[, cols_reordered]
    
    
  }
  
  
  jointhead1 <- select(jointhead1, -Frequency)
  jointhead1 <- rename(jointhead1, Frequency = imputations)
  
  return(jointhead1)
  
  
}

fill_zero_cells_hhtype <- function(jointhead1, farm_presence, hhtype){
  indices_head <- c(1,2,6,12,15,21,27,30)
  j100 <- text_to_df(txtpath, beginlines_list[[1]], endlines_list[[1]], T)
  j210 <- text_to_df(txtpath, beginlines_list[[2]], endlines_list[[2]], T)
  j220 <- text_to_df(txtpath, beginlines_list[[6]], endlines_list[[6]], T)
  j230 <- text_to_df(txtpath, beginlines_list[[12]], endlines_list[[12]], T)
  j310 <- text_to_df(txtpath, beginlines_list[[15]], endlines_list[[15]], T)
  j320 <- text_to_df(txtpath, beginlines_list[[21]], endlines_list[[21]], T)
  j330 <- text_to_df(txtpath, beginlines_list[[27]], endlines_list[[27]], T)
  j400 <- text_to_df(txtpath, beginlines_list[[30]], endlines_list[[30]], T)
  
  w100 <- sum(j100$Frequency)
  w210 <- sum(j210$Frequency)
  w220 <- sum(j220$Frequency)
  w230 <- sum(j230$Frequency)
  w310 <- sum(j310$Frequency)
  w320 <- sum(j320$Frequency)
  w330 <- sum(j330$Frequency)
  w400 <- sum(j400$Frequency)
  
  j100 <- j100 %>% 
    rename(Frequency1 = Frequency)
  j210 <- j210 %>% 
    rename(Frequency2 = Frequency)
  j100 <- as.data.frame(na.fill(merge(subset(j100, select = -hhtype), subset(j210, select = -hhtype), by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
  j100$Frequency =  j100$Frequency1 + j100$Frequency2
  j100 <- j100 %>% select(-Frequency1, -Frequency2) 
  
  j100 <- j100 %>% 
    rename(Frequency1 = Frequency)
  j220 <- j220 %>% 
    rename(Frequency2 = Frequency)
  j100 <- as.data.frame(na.fill(merge(j100, subset(j220, select = -hhtype), by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
  j100$Frequency =  j100$Frequency1 + j100$Frequency2
  j100 <- j100 %>% select(-Frequency1, -Frequency2) 
  
  j100 <- j100 %>% 
    rename(Frequency1 = Frequency)
  j230 <- j230 %>% 
    rename(Frequency2 = Frequency)
  j100 <- as.data.frame(na.fill(merge(j100, subset(j230, select = -hhtype), by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
  j100$Frequency =  j100$Frequency1 + j100$Frequency2
  j100 <- j100 %>% select(-Frequency1, -Frequency2)
  
  j100 <- j100 %>% 
    rename(Frequency1 = Frequency)
  j310 <- j310 %>% 
    rename(Frequency2 = Frequency)
  j100 <- as.data.frame(na.fill(merge(j100, subset(j310, select = -hhtype), by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
  j100$Frequency =  j100$Frequency1 + j100$Frequency2
  j100 <- j100 %>% select(-Frequency1, -Frequency2)
  
  j100 <- j100 %>% 
    rename(Frequency1 = Frequency)
  j320 <- j320 %>% 
    rename(Frequency2 = Frequency)
  j100 <- as.data.frame(na.fill(merge(j100, subset(j320, select = -hhtype), by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
  j100$Frequency =  j100$Frequency1 + j100$Frequency2
  j100 <- j100 %>% select(-Frequency1, -Frequency2)
  
  j100 <- j100 %>% 
    rename(Frequency1 = Frequency)
  j330 <- j330 %>% 
    rename(Frequency2 = Frequency)
  j100 <- as.data.frame(na.fill(merge(j100, subset(j330, select = -hhtype), by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
  j100$Frequency =  j100$Frequency1 + j100$Frequency2
  j100 <- j100 %>% select(-Frequency1, -Frequency2)
  
  j100 <- j100 %>% 
    rename(Frequency1 = Frequency)
  j400 <- j400 %>% 
    rename(Frequency2 = Frequency)
  j100 <- as.data.frame(na.fill(merge(j100, subset(j400, select = -hhtype), by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
  j100$Frequency =  j100$Frequency1 + j100$Frequency2
  j100 <- j100 %>% select(-Frequency1, -Frequency2)
  
  w100 <- w100/sum(j100$Frequency)
  w210 <- w210/sum(j100$Frequency)
  w220 <- w220/sum(j100$Frequency)
  w230 <- w230/sum(j100$Frequency)
  w310 <- w310/sum(j100$Frequency)
  w320 <- w320/sum(j100$Frequency)
  w330 <- w330/sum(j100$Frequency)
  w400 <- w400/sum(j100$Frequency)
  
  
  
  if (hhtype == 100){
    j100$Frequency = j100$Frequency * w100
    j100 <- j100 %>% 
      rename(Frequency1 = Frequency)
    jointhead1 <- jointhead1 %>% 
      rename(Frequency2 = Frequency)
    jointhead1 <- as.data.frame(na.fill(merge(jointhead1, j100, by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
    
    jointhead1$Frequency = (jointhead1$Frequency1 + (jointhead1$Frequency2 * 5))/6
    jointhead1 <- jointhead1 %>% select(-Frequency1, -Frequency2)
    jointhead1$hhtype = 100
    
  } else if (hhtype == 210){
    j100$Frequency = j100$Frequency * w210
    j100 <- j100 %>% 
      rename(Frequency1 = Frequency)
    jointhead1 <- jointhead1 %>% 
      rename(Frequency2 = Frequency)
    jointhead1 <- as.data.frame(na.fill(merge(jointhead1, j100, by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
    
    jointhead1$Frequency = (jointhead1$Frequency1 + (jointhead1$Frequency2 * 5))/6
    
    jointhead1sex0 <- jointhead1[jointhead1$sex_head == 0,]
    jointhead1sex0$Frequency <- jointhead1sex0$Frequency *(sum(jointhead1sex0$Frequency2)/sum(jointhead1sex0$Frequency))
    
    jointhead1sex1 <- jointhead1[jointhead1$sex_head == 1,]
    jointhead1sex1$Frequency <- jointhead1sex1$Frequency *(sum(jointhead1sex1$Frequency2)/sum(jointhead1sex1$Frequency))
    
    jointhead1 <- rbind(jointhead1sex0, jointhead1sex1)
    
    jointhead1 <- jointhead1 %>% select(-Frequency1, -Frequency2)
    jointhead1$hhtype = 210
    
  } else if (hhtype == 220){
    j100$Frequency = j100$Frequency * w220
    j100 <- j100 %>% 
      rename(Frequency1 = Frequency)
    jointhead1 <- jointhead1 %>% 
      rename(Frequency2 = Frequency)
    jointhead1 <- as.data.frame(na.fill(merge(jointhead1, j100, by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
    
    jointhead1$Frequency = (jointhead1$Frequency1 + (jointhead1$Frequency2 * 5))/6
    
    jointhead1sex0 <- jointhead1[jointhead1$sex_head == 0,]
    jointhead1sex0$Frequency <- jointhead1sex0$Frequency *(sum(jointhead1sex0$Frequency2)/sum(jointhead1sex0$Frequency))
    
    jointhead1sex1 <- jointhead1[jointhead1$sex_head == 1,]
    jointhead1sex1$Frequency <- jointhead1sex1$Frequency *(sum(jointhead1sex1$Frequency2)/sum(jointhead1sex1$Frequency))
    
    jointhead1 <- rbind(jointhead1sex0, jointhead1sex1)
    
    jointhead1 <- jointhead1 %>% select(-Frequency1, -Frequency2)
    
    jointhead1$hhtype = 220
    
  } else if (hhtype == 230){
    j100$Frequency = j100$Frequency * w230
    j100 <- j100 %>% 
      rename(Frequency1 = Frequency)
    jointhead1 <- jointhead1 %>% 
      rename(Frequency2 = Frequency)
    jointhead1 <- as.data.frame(na.fill(merge(jointhead1, j100, by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
    
    jointhead1$Frequency = (jointhead1$Frequency1 + (jointhead1$Frequency2 * 5))/6
    
    jointhead1sex0 <- jointhead1[jointhead1$sex_head == 0,]
    jointhead1sex0$Frequency <- jointhead1sex0$Frequency *(sum(jointhead1sex0$Frequency2)/sum(jointhead1sex0$Frequency))
    
    jointhead1sex1 <- jointhead1[jointhead1$sex_head == 1,]
    jointhead1sex1$Frequency <- jointhead1sex1$Frequency *(sum(jointhead1sex1$Frequency2)/sum(jointhead1sex1$Frequency))
    
    jointhead1 <- rbind(jointhead1sex0, jointhead1sex1)
    
    jointhead1 <- jointhead1 %>% select(-Frequency1, -Frequency2)
    
    jointhead1$hhtype = 230
    
  } else if (hhtype == 310){
    j100$Frequency = j100$Frequency * w310
    j100 <- j100 %>% 
      rename(Frequency1 = Frequency)
    jointhead1 <- jointhead1 %>% 
      rename(Frequency2 = Frequency)
    jointhead1 <- as.data.frame(na.fill(merge(jointhead1, j100, by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
    
    jointhead1$Frequency = (jointhead1$Frequency1 + (jointhead1$Frequency2 * 5))/6
    
    jointhead1sex0 <- jointhead1[jointhead1$sex_head == 0,]
    jointhead1sex0$Frequency <- jointhead1sex0$Frequency *(sum(jointhead1sex0$Frequency2)/sum(jointhead1sex0$Frequency))
    
    jointhead1sex1 <- jointhead1[jointhead1$sex_head == 1,]
    jointhead1sex1$Frequency <- jointhead1sex1$Frequency *(sum(jointhead1sex1$Frequency2)/sum(jointhead1sex1$Frequency))
    
    jointhead1 <- rbind(jointhead1sex0, jointhead1sex1)
    
    jointhead1 <- jointhead1 %>% select(-Frequency1, -Frequency2)
    
    jointhead1$hhtype = 310
    
  } else if (hhtype == 320){
    j100$Frequency = j100$Frequency * w320
    j100 <- j100 %>% 
      rename(Frequency1 = Frequency)
    jointhead1 <- jointhead1 %>% 
      rename(Frequency2 = Frequency)
    jointhead1 <- as.data.frame(na.fill(merge(jointhead1, j100, by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
    
    jointhead1$Frequency = (jointhead1$Frequency1 + (jointhead1$Frequency2 * 5))/6
    
    jointhead1sex0 <- jointhead1[jointhead1$sex_head == 0,]
    jointhead1sex0$Frequency <- jointhead1sex0$Frequency *(sum(jointhead1sex0$Frequency2)/sum(jointhead1sex0$Frequency))
    
    jointhead1sex1 <- jointhead1[jointhead1$sex_head == 1,]
    jointhead1sex1$Frequency <- jointhead1sex1$Frequency *(sum(jointhead1sex1$Frequency2)/sum(jointhead1sex1$Frequency))
    
    jointhead1 <- rbind(jointhead1sex0, jointhead1sex1)
    
    jointhead1 <- jointhead1 %>% select(-Frequency1, -Frequency2)
    
    jointhead1$hhtype = 320
    
  } else if (hhtype == 330){
    j100$Frequency = j100$Frequency * w330
    j100 <- j100 %>% 
      rename(Frequency1 = Frequency)
    jointhead1 <- jointhead1 %>% 
      rename(Frequency2 = Frequency)
    jointhead1 <- as.data.frame(na.fill(merge(jointhead1, j100, by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
    
    jointhead1$Frequency = (jointhead1$Frequency1 + (jointhead1$Frequency2 * 5))/6
    
    jointhead1sex0 <- jointhead1[jointhead1$sex_head == 0,]
    jointhead1sex0$Frequency <- jointhead1sex0$Frequency *(sum(jointhead1sex0$Frequency2)/sum(jointhead1sex0$Frequency))
    
    jointhead1sex1 <- jointhead1[jointhead1$sex_head == 1,]
    jointhead1sex1$Frequency <- jointhead1sex1$Frequency *(sum(jointhead1sex1$Frequency2)/sum(jointhead1sex1$Frequency))
    
    jointhead1 <- rbind(jointhead1sex0, jointhead1sex1)
    
    jointhead1 <- jointhead1 %>% select(-Frequency1, -Frequency2)
    
    jointhead1$hhtype = 330
    
  } else if (hhtype == 400){
    j100$Frequency = j100$Frequency * w400
    j100 <- j100 %>% 
      rename(Frequency1 = Frequency)
    jointhead1 <- jointhead1 %>% 
      rename(Frequency2 = Frequency)
    jointhead1 <- as.data.frame(na.fill(merge(jointhead1, j100, by = c("INCOMEQQ", "rural_number", "farming_number", "age_head", "sex_head", "educ_head"), all = TRUE), fill = 0))
    
    jointhead1$Frequency = (jointhead1$Frequency1 + (jointhead1$Frequency2 * 5))/6
    
    jointhead1sex0 <- jointhead1[jointhead1$sex_head == 0,]
    jointhead1sex0$Frequency <- jointhead1sex0$Frequency *(sum(jointhead1sex0$Frequency2)/sum(jointhead1sex0$Frequency))
    
    jointhead1sex1 <- jointhead1[jointhead1$sex_head == 1,]
    jointhead1sex1$Frequency <- jointhead1sex1$Frequency *(sum(jointhead1sex1$Frequency2)/sum(jointhead1sex1$Frequency))
    
    jointhead1 <- rbind(jointhead1sex0, jointhead1sex1)
    
    jointhead1 <- jointhead1 %>% select(-Frequency1, -Frequency2)
    
    jointhead1$hhtype = 400
    
  }
  
  return(jointhead1)
  
}

new_partner_joints <- function(partvar, df1tot, educatshead){
  indices_partner <- c(3,4,5,8,9,10,17,18,19,23,24,25)
  if (partvar == 'age'){
    agepart1 <- text_to_df(txtpath, beginlines_list[[3]], endlines_list[[3]], T)
    agepart2 <- text_to_df(txtpath, beginlines_list[[8]], endlines_list[[8]], T) 
    agepart3 <- text_to_df(txtpath, beginlines_list[[17]], endlines_list[[17]], T) 
    agepart4 <- text_to_df(txtpath, beginlines_list[[23]], endlines_list[[23]], T)
    
    a1 <- sum(agepart1$Frequency)
    a2 <- sum(agepart2$Frequency)
    a3 <- sum(agepart3$Frequency)
    a4 <- sum(agepart4$Frequency)
    
    
    agepart1 <- agepart1 %>% 
      rename(Frequency1 = Frequency)
    agepart2 <- agepart2 %>% 
      rename(Frequency2 = Frequency)
    agepart1 <- as.data.frame(na.fill(merge(subset(agepart1, select = -hhtype), subset(agepart2, select = -hhtype), by = c("age_head", "sex_head", "educ_head", "age_part"), all = TRUE), fill = 0))
    agepart1$Frequency =  agepart1$Frequency1 + agepart1$Frequency2
    agepart1 <- subset(agepart1, select = -c(Frequency1, Frequency2))
    
    agepart1 <- agepart1 %>% 
      rename(Frequency1 = Frequency)
    agepart3 <- agepart3 %>% 
      rename(Frequency2 = Frequency)
    agepart1 <- as.data.frame(na.fill(merge(agepart1, subset(agepart3, select = -hhtype), by = c("age_head", "sex_head", "educ_head", "age_part"), all = TRUE), fill = 0))
    agepart1$Frequency =  agepart1$Frequency1 + agepart1$Frequency2
    agepart1 <- subset(agepart1, select = -c(Frequency1, Frequency2))
    
    agepart1 <- agepart1 %>% 
      rename(Frequency1 = Frequency)
    agepart4 <- agepart4 %>% 
      rename(Frequency2 = Frequency)
    agepart1 <- as.data.frame(na.fill(merge(agepart1, subset(agepart4, select = -hhtype), by = c("age_head", "sex_head", "educ_head", "age_part"), all = TRUE), fill = 0))
    agepart1$Frequency =  agepart1$Frequency1 + agepart1$Frequency2
    agepart1 <- subset(agepart1, select = -c(Frequency1, Frequency2))
    
    totalweight = sum(agepart1$Frequency)
    
    ageage <- agepart1 %>% group_by(age_head, age_part) %>% summarise(Frequency = sum(Frequency))
    
    
    ageequal <- sum(ageage[ageage['age_head'] == ageage['age_part'],]$Frequency)/totalweight
    agehead1yearolder <- sum(ageage[ageage['age_head'] == (1+ageage['age_part']),]$Frequency)/totalweight
    agehead2yearolder <- sum(ageage[ageage['age_head'] == (2+ageage['age_part']),]$Frequency)/totalweight
    agehead3yearolder <- sum(ageage[ageage['age_head'] == (3+ageage['age_part']),]$Frequency)/totalweight
    
    
    agehead1yearyounger <- sum(ageage[ageage['age_head'] == (-1+ageage['age_part']),]$Frequency)/totalweight
    agehead2yearyounger <- sum(ageage[ageage['age_head'] == (-2+ageage['age_part']),]$Frequency)/totalweight 
    agehead3yearyounger <- sum(ageage[ageage['age_head'] == (-3+ageage['age_part']),]$Frequency)/totalweight
    
    if (agehead2yearyounger == 0){
      agehead2yearyounger = NA
    } 
    
    if (agehead3yearyounger == 0){
      agehead3yearyounger = NA
    } 
    
    yage <- c(0.0000000001, NA, NA, agehead3yearyounger, agehead2yearyounger, agehead1yearyounger, ageequal) 
    xage <- c(1,2,3,4,5,6,7)
    
    polymodel <- lm(log(yage) ~ poly(xage, sum(!is.na(yage)), raw = TRUE))
    predicted_values_younger <- predict(polymodel, newdata = data.frame(test = c(1,2,3,4,5,6,7)))
    predicted_values_younger
    
    if (agehead3yearolder == 0){
      agehead3yearolder = NA
    } 
    
    yage <- c(ageequal, agehead1yearolder, agehead2yearolder, agehead3yearolder, NA, NA, 0.0000000001) 
    xage <- c(1,2,3,4,5,6,7)
    
    polymodel <- lm(log(yage) ~ poly(xage, sum(!is.na(yage)), raw = TRUE))
    predicted_values_older <- predict(polymodel, newdata = data.frame(test = c(1,2,3,4,5,6,7)))
    predicted_values_older
    
    #2 year younger added bc of palestina. 
    if (is.na(agehead2yearyounger) == TRUE){
      agehead2yearyounger = exp(predicted_values_younger[5])
    } 
    
    if (is.na(agehead3yearyounger) == TRUE){
      agehead3yearyounger = exp(predicted_values_younger[4])
    } 
    
    if (is.na(agehead3yearolder) == TRUE){
      agehead3yearolder = exp(predicted_values_older[4])
    } 
    
    
    age_probs <- data.frame(agediff = c(-5,-4,-3,-2,-1,0,1,2,3,4,5), probs = c(exp(predicted_values_younger[2:3]), 
                                                                               agehead3yearyounger, agehead2yearyounger, agehead1yearyounger, ageequal, agehead1yearolder, 
                                                                               agehead2yearolder, agehead3yearolder, exp(predicted_values_older[5:6])))
    
    
    ageage['agediff'] = ageage['age_head'] - ageage['age_part']
    
    ageheadfreq = ageage %>% group_by(age_head) %>% summarise(Frequency = sum(Frequency))
    ageheadfreq <- ageheadfreq %>% 
      rename(Frequency_agehead = Frequency)
    
    ageage = merge(ageage, age_probs, by = c('agediff'))
    ageage = merge(ageage, ageheadfreq, by = c('age_head'))
    
    
    zero_indices <- which(ageage$Frequency == 0)
    ageage$imputations <- ageage$Frequency
    ageage$imputations[zero_indices] <- ageage$Frequency_agehead[zero_indices] * ageage$probs[zero_indices]
    
    sumfreq = sum(ageage$Frequency)
    sumimpu = sum(ageage$imputations)
    
    ageage$imputations = ageage$imputations * (sumfreq/sumimpu)
    
    ageage <- subset(ageage, select = -c(probs, Frequency_agehead, agediff))
    
    return(ageage)
  }
  
  if (partvar == 'sex'){
    sexpart1 <- text_to_df(txtpath, beginlines_list[[4]], endlines_list[[4]], T)
    sexpart2 <- text_to_df(txtpath, beginlines_list[[9]], endlines_list[[9]], T) 
    sexpart3 <- text_to_df(txtpath, beginlines_list[[18]], endlines_list[[18]], T) 
    sexpart4 <- text_to_df(txtpath, beginlines_list[[24]], endlines_list[[24]], T)
    
    a1 <- sum(sexpart1$Frequency)
    a2 <- sum(sexpart2$Frequency)
    a3 <- sum(sexpart3$Frequency)
    a4 <- sum(sexpart4$Frequency)
    
    sexpart1 <- sexpart1 %>% 
      rename(Frequency1 = Frequency)
    sexpart2 <- sexpart2 %>% 
      rename(Frequency2 = Frequency)
    sexpart1 <- as.data.frame(na.fill(merge(subset(sexpart1, select = -hhtype), subset(sexpart2, select = -hhtype), by = c("educ_head", "sex_head", "age_part", "sex_part"), all = TRUE), fill = 0))
    sexpart1$Frequency =  sexpart1$Frequency1 + sexpart1$Frequency2
    sexpart1 <- subset(sexpart1, select = -c(Frequency1, Frequency2))
    
    sexpart1 <- sexpart1 %>% 
      rename(Frequency1 = Frequency)
    sexpart3 <- sexpart3 %>% 
      rename(Frequency2 = Frequency)
    sexpart1 <- as.data.frame(na.fill(merge(sexpart1, subset(sexpart3, select = -hhtype), by = c("educ_head", "sex_head", "age_part", "sex_part"), all = TRUE), fill = 0))
    sexpart1$Frequency =  sexpart1$Frequency1 + sexpart1$Frequency2
    sexpart1 <- subset(sexpart1, select = -c(Frequency1, Frequency2))
    
    sexpart1 <- sexpart1 %>% 
      rename(Frequency1 = Frequency)
    sexpart4 <- sexpart4 %>% 
      rename(Frequency2 = Frequency)
    sexpart1 <- as.data.frame(na.fill(merge(sexpart1, subset(sexpart4, select = -hhtype), by = c("educ_head", "sex_head", "age_part", "sex_part"), all = TRUE), fill = 0))
    sexpart1$Frequency =  sexpart1$Frequency1 + sexpart1$Frequency2
    sexpart1 <- subset(sexpart1, select = -c(Frequency1, Frequency2))
    
    totalweight = sum(sexpart1$Frequency)
    
    sexsex <- sexpart1 %>% group_by(sex_head, sex_part) %>% summarise(Frequency = sum(Frequency))
    
    differentsex <- sexsex[sexsex$sex_head != sexsex$sex_part,]
    
    samesex <- sexsex[sexsex$sex_head == sexsex$sex_part,]
    
    if (sum(samesex$Frequency) > 0){
      gaycouples = TRUE
      
      if (nrow(samesex) == 1){
        # maybe there are only lesbian or homosexual couples, fix this. 
        if (sum(samesex$sex_head) == 0){
          samesex1 = samesex
          samesex1$sex_head = 1
          samesex1$sex_part = 1
          
          samesex = rbind(samesex, samesex1)
          samesex$Frequency = samesex$Frequency/2
          
        } else if (sum(samesex$sex_head) == 6){
          samesex1 = samesex
          samesex1$sex_head = 0
          samesex1$sex_part = 0
          
          samesex = rbind(samesex, samesex1)
          samesex$Frequency = samesex$Frequency/2
        } else {
          print('error in partner joints, gay couples')
        }
        
      }
      
    }
    # Fill in the zeroes, we assume a uniform distribution: 
    
    sexsex = rbind(differentsex, samesex)
    
    return(sexsex)
    
  }
  
  
  if (partvar == 'educ'){
    edupart1 <- text_to_df(txtpath, beginlines_list[[5]], endlines_list[[5]], T)
    edupart2 <- text_to_df(txtpath, beginlines_list[[10]], endlines_list[[10]], T) 
    edupart3 <- text_to_df(txtpath, beginlines_list[[19]], endlines_list[[19]], T) 
    edupart4 <- text_to_df(txtpath, beginlines_list[[25]], endlines_list[[25]], T)
    
    a1 <- sum(edupart1$Frequency)
    a2 <- sum(edupart2$Frequency)
    a3 <- sum(edupart3$Frequency)
    a4 <- sum(edupart4$Frequency)
    
    
    edupart1 <- edupart1 %>% 
      rename(Frequency1 = Frequency)
    edupart2 <- edupart2 %>% 
      rename(Frequency2 = Frequency)
    edupart1 <- as.data.frame(na.fill(merge(subset(edupart1, select = -hhtype), subset(edupart2, select = -hhtype), by = c("educ_head", "sex_part", "age_part", "educ_part"), all = TRUE), fill = 0))
    edupart1$Frequency =  edupart1$Frequency1 + edupart1$Frequency2
    edupart1 <- subset(edupart1, select = -c(Frequency1, Frequency2))
    
    edupart1 <- edupart1 %>% 
      rename(Frequency1 = Frequency)
    edupart3 <- edupart3 %>% 
      rename(Frequency2 = Frequency)
    edupart1 <- as.data.frame(na.fill(merge(edupart1, subset(edupart3, select = -hhtype), by = c("educ_head", "sex_part", "age_part", "educ_part"), all = TRUE), fill = 0))
    edupart1$Frequency =  edupart1$Frequency1 + edupart1$Frequency2
    edupart1 <- subset(edupart1, select = -c(Frequency1, Frequency2))
    
    edupart1 <- edupart1 %>% 
      rename(Frequency1 = Frequency)
    edupart4 <- edupart4 %>% 
      rename(Frequency2 = Frequency)
    edupart1 <- as.data.frame(na.fill(merge(edupart1, subset(edupart4, select = -hhtype), by = c("educ_head", "sex_part", "age_part", "educ_part"), all = TRUE), fill = 0))
    edupart1$Frequency =  edupart1$Frequency1 + edupart1$Frequency2
    edupart1 <- subset(edupart1, select = -c(Frequency1, Frequency2))
    
    totalweight = sum(edupart1$Frequency)
    
    #educatsunique <- unique(edu_tib$EDUCAT)
    educatsunique <- unique(educatshead, unique(edupart1$educ_part)) # aangepast 19 mei. categorie 1 komt soms alleen bij kinderen voor. 
    
    educatsuniqueOG <- educatsunique
    
    
    if (length(educatsunique) == 4){
      educatsunique <- c(1,2,3,4) 
      
      counter = 1
      for (i in educatsuniqueOG){
        edupart1$educ_head[edupart1$educ_head == i] <- counter
        edupart1$educ_part[edupart1$educ_part == i] <- counter
        counter = counter+1
      }
    } 
    
    
    
    
    
    eduedu <- edupart1 %>% group_by(educ_head, educ_part) %>% summarise(Frequency = sum(Frequency))
    
    #edu_tib_tot <- edu_tib %>% group_by(EDUCAT) %>% summarise(Frequency = sum(Frequency))
    
    eduedu_new <- data.frame(educ_head = kronecker(educatsunique, array(1, dim = c(length(educatsunique))), FUN = "*", make.dimnames = FALSE)  , 
                             educ_part = rep(educatsunique, times = length(educatsunique)),
                             Frequency2 = array(0, dim = length(educatsunique)**2))
    
    if (nrow(eduedu) == nrow(eduedu_new) & nrow(eduedu[eduedu$Frequency == 0,]) == 0){
      eduedu$imputations <- eduedu$Frequency
      
      if (length(educatsuniqueOG) == 4){
        # Replacing the values in the column
        eduedu$educ_head[eduedu$educ_head %in% educatsunique] <- educatsuniqueOG[match(eduedu$educ_head[eduedu$educ_head %in% educatsunique], educatsunique)]
        eduedu$educ_part[eduedu$educ_part %in% educatsunique] <- educatsuniqueOG[match(eduedu$educ_part[eduedu$educ_part %in% educatsunique], educatsunique)]
      }
      
      return(eduedu)
    }
    
    
    else if (nrow(eduedu) < nrow(eduedu_new)){
      educats_head <- unique(eduedu$educ_head)
      eduedu_new <- as.data.frame(na.fill(merge(eduedu_new, eduedu, by = c('educ_head', 'educ_part'), all = TRUE), fill = 0))
      
      eduagemarg = data.frame(educ_head = rep(educatsunique, times = 7),
                              age_head = rep(c(1,2,3,4,5,6,7), times = length(educatsunique)),
                              probs = df1tot[c((nrow(df1tot)-((7*length(educatsunique))-1)):nrow(df1tot)),])
      
      eduagemarg = eduagemarg[eduagemarg['age_head'] != 1,]
      edumarg = eduagemarg %>% group_by(educ_head) %>% summarise(probs = sum(probs))
      
      
      edumarg$probs = edumarg$probs * (1/sum(edumarg$probs))
      
      
      eduedu_new <- merge(eduedu_new, edumarg, by = c('educ_head'), all = TRUE)
      names(eduedu_new)[names(eduedu_new) == "probs"] <- "probs_head" 
      
      names(edumarg)[names(edumarg) == "educ_head"] <- "educ_part" 
      eduedu_new <- merge(eduedu_new, edumarg, by = c('educ_part'), all = TRUE)
      names(eduedu_new)[names(eduedu_new) == "probs"] <- "probs_part" 
      
      #if (length(educatsunique) == 4){
      #  edu4 == TRUE
      #  eduedu_new$educ_head <- as.integer(as.factor(eduedu_new$educ_head))
      #  eduedu_new$educ_part <- as.integer(as.factor(eduedu_new$educ_part))
      #}
      
      totalweight = sum(eduedu_new$Frequency)
      
      eduequal <- sum(eduedu[eduedu['educ_head'] == eduedu['educ_part'],]$Frequency)/totalweight
      eduhead1higher <- sum(eduedu[eduedu['educ_head'] == (1+eduedu['educ_part']),]$Frequency)/totalweight
      eduhead2higher <- sum(eduedu[eduedu['educ_head'] == (2+eduedu['educ_part']),]$Frequency)/totalweight
      
      eduhead1lower <- sum(eduedu[eduedu['educ_head'] == (-1+eduedu['educ_part']),]$Frequency)/totalweight
      eduhead2lower <- sum(eduedu[eduedu['educ_head'] == (-2+eduedu['educ_part']),]$Frequency)/totalweight 
      
      
      if (eduhead2lower == 0){
        eduhead2lower = NA
      } 
      
      yedu <- c(0.0000000001, NA, NA, eduhead2lower, eduhead1lower, eduequal) 
      xedu <- c(1,2,3,4,5,6)
      if (length(educatsunique) == 4){
        yedu <- c(0.0000000001, NA, eduhead2lower, eduhead1lower, eduequal)
        xedu <- c(1,2,3,4,5)
      }
      
      polymodel <- lm(log(yedu) ~ poly(xedu, sum(!is.na(yedu)), raw = TRUE))
      predicted_values_lower <- predict(polymodel, newdata = data.frame(test = xedu))
      
      
      if (eduhead2higher == 0){
        eduhead2higher = NA
      } 
      
      yedu <- c(eduequal, eduhead1higher, eduhead2higher, NA, NA, 0.0000000001) 
      xedu <- c(1,2,3,4,5,6)
      if (length(educatsunique) == 4){
        xedu <- c(1,2,3,4,5)
        yedu <- c(eduequal, eduhead1higher, eduhead2higher, NA, 0.0000000001)
      }
      
      polymodel <- lm(log(yedu) ~ poly(xedu, sum(!is.na(yedu)), raw = TRUE))
      predicted_values_higher <- predict(polymodel, newdata = data.frame(test = xedu))
      
      
      if (is.na(eduhead2lower) == TRUE){
        eduhead2lower = exp(predicted_values_lower[4]) #exp bij. 26 mei.
      } 
      
      if (is.na(eduhead2higher) == TRUE){
        eduhead2higher = exp(predicted_values_higher[3])
      } 
      
      
      edu_probs <- data.frame(edudiff = c(-4,-3,-2,-1,0,1,2,3,4), probs_edudiff = c(exp(predicted_values_lower[2:3]), 
                                                                                    eduhead2lower, eduhead1lower, eduequal, eduhead1higher, 
                                                                                    eduhead2higher, exp(predicted_values_higher[4:5])))
      
      
      eduedu_new['edudiff'] = eduedu_new['educ_head'] - eduedu_new['educ_part']
      
      eduedu_new$Frequency_head = eduedu_new$probs_head * totalweight
      
      eduedu_new$Frequency_part = eduedu_new$probs_part * totalweight
      
      eduedu_new <- merge(eduedu_new, edu_probs, by = c('edudiff'), all = TRUE)
      
      #eduedu_new$testimpu <- eduedu_new$Frequency2 * eduedu_new$probs_edudiff
      
      eduedu_new <- eduedu_new %>% mutate(Frequencymin = pmin(Frequency_head, Frequency_part)) 
      
      indices_sameedu <- which(eduedu_new$educ_head == eduedu_new$educ_part)
      eduedu_new$Frequencymin[indices_sameedu] <- eduedu_new$Frequencymin[indices_sameedu] /2
      
      
      zero_indices <- which(eduedu_new$Frequency == 0)
      eduedu_new$imputations <- eduedu_new$Frequency
      eduedu_new$imputations[zero_indices] <- eduedu_new$Frequencymin[zero_indices] * eduedu_new$probs_edudiff[zero_indices]
      
      # then rescale so that totals add up.
      eduedu_new <- eduedu_new[order(eduedu_new$educ_head, eduedu_new$educ_part),]
      
      edumarg$Freq <- edumarg$probs * totalweight
      
      eduedu_new1 <- eduedu_new[(eduedu_new['educ_head'] == 1 | eduedu_new['educ_part'] == 1),]
      
      
      sumfreq = sum(eduedu_new$Frequency)
      sumimpu = sum(eduedu_new$imputations)
      
      eduedu_new$imputations = eduedu_new$imputations * (sumfreq/sumimpu)
      
      eduedu_new <- subset(eduedu_new, select = -c(Frequency2, probs_head, probs_part, edudiff,
                                                   Frequency_head, Frequency_part, Frequencymin, probs_edudiff))
      
      eduedu_new <- eduedu_new[order(eduedu_new$educ_head, eduedu_new$educ_part),]
      
      if (length(educatsuniqueOG) == 4){
        # Replacing the values in the column
        eduedu_new$educ_head[eduedu_new$educ_head %in% educatsunique] <- educatsuniqueOG[match(eduedu_new$educ_head[eduedu_new$educ_head %in% educatsunique], educatsunique)]
        eduedu_new$educ_part[eduedu_new$educ_part %in% educatsunique] <- educatsuniqueOG[match(eduedu_new$educ_part[eduedu_new$educ_part %in% educatsunique], educatsunique)]
      }
      
      
      
      return(eduedu_new)
      
    } else{
      
      eduequal <- sum(eduedu[eduedu['educ_head'] == eduedu['educ_part'],]$Frequency)/totalweight
      eduhead1higher <- sum(eduedu[eduedu['educ_head'] == (1+eduedu['educ_part']),]$Frequency)/totalweight
      eduhead2higher <- sum(eduedu[eduedu['educ_head'] == (2+eduedu['educ_part']),]$Frequency)/totalweight
      
      eduhead1lower <- sum(eduedu[eduedu['educ_head'] == (-1+eduedu['educ_part']),]$Frequency)/totalweight
      eduhead2lower <- sum(eduedu[eduedu['educ_head'] == (-2+eduedu['educ_part']),]$Frequency)/totalweight 
      
      
      if (eduhead2lower == 0){
        eduhead2lower = NA
      } 
      
      yedu <- c(0.0000000001, NA, NA, eduhead2lower, eduhead1lower, eduequal) 
      xedu <- c(1,2,3,4,5,6)
      if (length(educatsunique) == 4){
        yedu <- c(0.0000000001, NA, eduhead2lower, eduhead1lower, eduequal)
        xedu <- c(1,2,3,4,5)
      }
      
      polymodel <- lm(log(yedu) ~ poly(xedu, sum(!is.na(yedu)), raw = TRUE))
      predicted_values_lower <- predict(polymodel, newdata = data.frame(test = xedu))
      
      
      if (eduhead2higher == 0){
        eduhead2higher = NA
      } 
      
      yedu <- c(eduequal, eduhead1higher, eduhead2higher, NA, NA, 0.0000000001) 
      xedu <- c(1,2,3,4,5,6)
      if (length(educatsunique) == 4){
        yedu <- c(eduequal, eduhead1higher, eduhead2higher, NA, 0.0000000001) 
        xedu <- c(1,2,3,4,5)
      }
      
      polymodel <- lm(log(yedu) ~ poly(xedu, sum(!is.na(yedu)), raw = TRUE))
      predicted_values_higher <- predict(polymodel, newdata = data.frame(test = xedu))
      
      
      if (is.na(eduhead2lower) == TRUE){
        eduhead2lower = predicted_values_lower[4]
      } 
      
      if (is.na(eduhead2higher) == TRUE){
        eduhead2higher = predicted_values_higher[3]
      } 
      
      
      edu_probs <- data.frame(edudiff = c(-4,-3,-2,-1,0,1,2,3,4), probs_edudiff = c(exp(predicted_values_lower[2:3]), 
                                                                                    eduhead2lower, eduhead1lower, eduequal, eduhead1higher, 
                                                                                    eduhead2higher, exp(predicted_values_higher[4:5])))
      
      
      eduedu['edudiff'] = eduedu['educ_head'] - eduedu['educ_part']
      
      eduheadfreq = eduedu %>% group_by(educ_head) %>% summarise(Frequency = sum(Frequency))
      eduheadfreq <- eduheadfreq %>% 
        rename(Frequency_eduhead = Frequency)
      
      eduedu = merge(eduedu, edu_probs, by = c('edudiff'))
      eduedu = merge(eduedu, eduheadfreq, by = c('educ_head'))
      
      
      zero_indices <- which(eduedu$Frequency == 0)
      eduedu$imputations <- eduedu$Frequency
      eduedu$imputations[zero_indices] <- eduedu$Frequency_eduhead[zero_indices] * eduedu$probs[zero_indices]
      
      sumfreq = sum(eduedu$Frequency)
      sumimpu = sum(eduedu$imputations)
      
      eduedu$imputations = eduedu$imputations * (sumfreq/sumimpu)
      
      eduedu <- subset(eduedu, select = -c(probs_edudiff, Frequency_eduhead, edudiff))
      
      if (length(educatsuniqueOG) == 4){
        # Replacing the values in the column
        eduedu$educ_head[eduedu$educ_head %in% educatsunique] <- educatsuniqueOG[match(eduedu$educ_head[eduedu$educ_head %in% educatsunique], educatsunique)]
        eduedu$educ_part[eduedu$educ_part %in% educatsunique] <- educatsuniqueOG[match(eduedu$educ_part[eduedu$educ_part %in% educatsunique], educatsunique)]
      }
      
      return(eduedu)
    }
    
    
    
    
  }
  
  
}

add_partner <- function(jointhead, txtpath, HIDbegin){
  check210a = sum(jointhead$Frequency)
  #jointpartage <- text_to_df(txtpath, beginlines_list[[indices_list[1]]], endlines_list[[indices_list[1]]], TRUE)
  jointpartage <- new_partner_joints('age', df1tot, educatshead)
  
  jointpartage <- jointpartage %>% select(-Frequency)
  jointpartage$Frequency <- jointpartage$imputations
  jointpartage <- jointpartage %>% select(-imputations)
  
  jointhead <- new_frequencies_updatemay23(jointhead, jointpartage, 'age_part')
  #jointpartsex <- text_to_df(txtpath, beginlines_list[[indices_list[2]]], endlines_list[[indices_list[2]]], TRUE)
  jointpartsex <- new_partner_joints('sex', df1tot, educatshead)
  jointhead <- new_frequencies_updatemay23(jointhead, jointpartsex, 'sex_part') 
  #jointparteduc <- text_to_df(txtpath, beginlines_list[[indices_list[3]]], endlines_list[[indices_list[3]]], TRUE)
  jointparteduc <- new_partner_joints('educ', df1tot, educatshead)
  
  jointparteduc <- jointparteduc %>% select(-Frequency)
  jointparteduc$Frequency <- jointparteduc$imputations
  jointparteduc <- jointparteduc %>% select(-imputations)
  
  jointhead <- new_frequencies_updatemay23(jointhead, jointparteduc, 'educ_part') 
  check210b = sum(jointhead$Frequency)
  if (0.9 * check210a > check210b){
    print('error add partner, more than 10% households disappeared')
  }
  jointhead <- jointhead[! jointhead$Frequency == 0,] 
  jointhead <- trs_frequency(jointhead)
  #jointhead <- as.data.frame(lapply(jointhead, rep, jointhead$Truncated))
  
  #let op: as.data.table creates a copy of the dataframe, while setDT() converts it
  # to a table inplace
  jointhead <- as.data.table(lapply(jointhead, rep, jointhead$Truncated))
  
  #misschien hier de table maken. as.data.table. 
  #jointhead['HID'] <- 1:nrow(jointhead) #each household gets an HH ID. 
  jointhead[, HID := (1+HIDbegin):(.N+HIDbegin)]
  
  #jointhead <- jointhead %>% 
  #  tidyr::pivot_longer(
  #    cols = c("age_head", "age_part"), 
  #    names_to = "head_or_part_age", 
  #    values_to = "age")
  
  #jointhead <- jointhead %>% 
  #  tidyr::pivot_longer(
  #    cols = c("sex_head", "sex_part"), 
  #    names_to = "head_or_part_sex", 
  #    values_to = "sex")
  gc()
  
  jointhead = melt(jointhead, measure.vars = c("age_head", "age_part"), variable.name = 'head_or_part_age', value.name = 'age')
  jointhead = melt(jointhead, measure.vars = c("sex_head", "sex_part"), variable.name = 'head_or_part_sex', value.name = 'sex')
  
  jointhead = jointhead[! (head_or_part_age == "age_head" & head_or_part_sex == "sex_part")]
  jointhead = jointhead[! (head_or_part_age == "age_part" & head_or_part_sex == "sex_head")]
  
  jointhead = melt(jointhead, measure.vars = c("educ_head", "educ_part"), variable.name = 'head_or_part_educ', value.name = 'educ')
  
  jointhead = jointhead[! (head_or_part_age == "age_head" & head_or_part_educ == "educ_part")]
  jointhead = jointhead[! (head_or_part_age == "age_part" & head_or_part_educ == "educ_head")]
  
  #jointhead["relate"] <- 1
  #jointhead$relate[jointhead["head_or_part_age"] == "age_part"] <- 2
  
  # dt[, col2 := as.character(col2)][col2 == "1", col2 := "bigDog"]
  jointhead[, head_or_part_age := as.integer(head_or_part_age)][head_or_part_age == 'age_part', head_or_part_age := 2]
  setnames(jointhead, 'head_or_part_age', 'relate') #relate is new name. 
  
  jointhead[,c('head_or_part_sex', 'head_or_part_educ'):=NULL]
  
  #jointhead <- jointhead %>% select(-head_or_part_age, -head_or_part_sex, -head_or_part_educ)
  
  return(jointhead)
  
}

add_cat_hhsize2 <- function(df, joint_hhsize2){  
  variablenames_hhsize2 <- colnames(joint_hhsize2)
  hhsizecats <- unique(joint_hhsize2$HHSIZECAT)
  
  setDT(joint_hhsize2)
  
  #we can do this faster with left_join. Done. 
  #dan moet dit ook allemaal table worden. 
  
  if ("CHILD_CAT" %in% variablenames_hhsize2 == TRUE){
    #joint_hhsize_child <- joint_hhsize2 %>% 
    #  dplyr::group_by(HHSIZECAT, CHILD_CAT) %>% 
    #  dplyr::summarize(Frequency = sum(Frequency))
    
    #sum_freq_child <- joint_hhsize_child %>%
    #  dplyr::group_by(HHSIZECAT) %>%
    #  dplyr::summarise(Sum_Freq = sum(Frequency))
    
    joint_hhsize_child <- joint_hhsize2[, .(Frequency = sum(Frequency)), by = .(HHSIZECAT, CHILD_CAT)]
    sum_freq_child <- joint_hhsize_child[, .(Sum_Freq = sum(Frequency)), by = .(HHSIZECAT)]
    
    
    
    joint_hhsize_child <- joint_hhsize_child[sum_freq_child, on = .(HHSIZECAT), nomatch = NULL]
    joint_hhsize_child[, Probabilities := Frequency / Sum_Freq]
    
    #joint_hhsize_child <- left_join(joint_hhsize_child, sum_freq_child, by = "HHSIZECAT")
    #joint_hhsize_child["Probabilities"] <- joint_hhsize_child$Frequency / joint_hhsize_child$Sum_Freq 
    
    joint_hhsize_child[is.na(joint_hhsize_child)] <- 0
    
    childcats <- unique(joint_hhsize2$CHILD_CAT)
    
    joint_hhsize_child[,c('Frequency', 'Sum_Freq'):=NULL]
    
    joint_hhsize_child = dcast(joint_hhsize_child, HHSIZECAT ~ CHILD_CAT, value.var = "Probabilities")
    
    df = df[joint_hhsize_child, on = .(HHSIZECAT), nomatch = NULL]
    
    
    prob_matrix <- as.matrix(df %>% select(as.character(childcats)))
    
    
    compare_vector <- runif(nrow(prob_matrix))
    cumul.prob_matrix <- prob_matrix %*% upper.tri(diag(ncol(prob_matrix)), diag = TRUE) / rowSums(prob_matrix)
    #df["CHILD_CAT"] <- rowSums(compare_vector > cumul.prob_matrix) + childcats[1]
    
    #df <- as.data.table(df)
    df[,CHILD_CAT := rowSums(compare_vector > cumul.prob_matrix) + childcats[1]]
    #df <- as.data.frame(df)
    #df <- df %>% select(-(as.character(childcats)))
    df[,c(as.character(childcats)):=NULL]
    
  }
  
  if ("RELATIVE_CAT" %in% variablenames_hhsize2 == TRUE & "CHILD_CAT" %in% variablenames_hhsize2 == TRUE){
    
    
    joint_hhsize_relative <- joint_hhsize2[, .(Frequency = sum(Frequency)), by = .(HHSIZECAT, CHILD_CAT, RELATIVE_CAT)]
    sum_freq_relative <- joint_hhsize_relative[, .(Sum_Freq = sum(Frequency)), by = .(HHSIZECAT, CHILD_CAT)]
    
    joint_hhsize_relative <- joint_hhsize_relative[sum_freq_relative, on = .(HHSIZECAT, CHILD_CAT), nomatch = NULL]
    joint_hhsize_relative[, Probabilities := Frequency / Sum_Freq]
    
    joint_hhsize_relative[is.na(joint_hhsize_relative)] <- 0
    
    relativecats <- unique(joint_hhsize2$RELATIVE_CAT)
    
    joint_hhsize_relative[,c('Frequency', 'Sum_Freq'):=NULL]
    
    joint_hhsize_relative = dcast(joint_hhsize_relative, HHSIZECAT + CHILD_CAT ~ RELATIVE_CAT, value.var = "Probabilities")
    
    df = df[joint_hhsize_relative, on = .(HHSIZECAT, CHILD_CAT), nomatch = NULL]
    
    prob_matrix <- as.matrix(df %>% select(as.character(relativecats)))
    compare_vector <- runif(nrow(prob_matrix))
    cumul.prob_matrix <- prob_matrix %*% upper.tri(diag(ncol(prob_matrix)), diag = TRUE) / rowSums(prob_matrix)
    df[,RELATIVE_CAT := rowSums(compare_vector > cumul.prob_matrix) + relativecats[1]]
    
    df[,c(as.character(relativecats)):=NULL]
    
    
    
  } 
  
  if ("RELATIVE_CAT" %in% variablenames_hhsize2 == TRUE & "CHILD_CAT" %in% variablenames_hhsize2 == FALSE){
    joint_hhsize_relative <- joint_hhsize2[, .(Frequency = sum(Frequency)), by = .(HHSIZECAT, RELATIVE_CAT)]
    sum_freq_relative <- joint_hhsize_relative[, .(Sum_Freq = sum(Frequency)), by = .(HHSIZECAT)]
    
    joint_hhsize_relative <- joint_hhsize_relative[sum_freq_relative, on = .(HHSIZECAT), nomatch = NULL]
    joint_hhsize_relative[, Probabilities := Frequency / Sum_Freq]
    
    joint_hhsize_relative[is.na(joint_hhsize_relative)] <- 0
    
    relativecats <- unique(joint_hhsize2$RELATIVE_CAT)
    
    joint_hhsize_relative[,c('Frequency', 'Sum_Freq'):=NULL]
    
    joint_hhsize_relative = dcast(joint_hhsize_relative, HHSIZECAT ~ RELATIVE_CAT, value.var = "Probabilities")
    
    df = df[joint_hhsize_relative, on = .(HHSIZECAT), nomatch = NULL]
    
    prob_matrix <- as.matrix(df %>% select(as.character(relativecats)))
    compare_vector <- runif(nrow(prob_matrix))
    cumul.prob_matrix <- prob_matrix %*% upper.tri(diag(ncol(prob_matrix)), diag = TRUE) / rowSums(prob_matrix)
    df[,RELATIVE_CAT := rowSums(compare_vector > cumul.prob_matrix) + relativecats[1]]
    
    df[,c(as.character(relativecats)):=NULL]
    
  }
  
  
  if ("NONRELATIVE_CAT" %in% variablenames_hhsize2 == TRUE & "CHILD_CAT" %in% variablenames_hhsize2 == TRUE){
    joint_hhsize_nonrelative <- joint_hhsize2[, .(Frequency = sum(Frequency)), by = .(HHSIZECAT, CHILD_CAT, RELATIVE_CAT, NONRELATIVE_CAT)]
    sum_freq_nonrelative <- joint_hhsize_nonrelative[, .(Sum_Freq = sum(Frequency)), by = .(HHSIZECAT, CHILD_CAT, RELATIVE_CAT)]
    
    joint_hhsize_nonrelative <- joint_hhsize_nonrelative[sum_freq_nonrelative, on = .(HHSIZECAT, CHILD_CAT, RELATIVE_CAT), nomatch = NULL]
    joint_hhsize_nonrelative[, Probabilities := Frequency / Sum_Freq]
    
    joint_hhsize_nonrelative[is.na(joint_hhsize_nonrelative)] <- 0
    
    nonrelativecats <- unique(joint_hhsize2$NONRELATIVE_CAT)
    
    joint_hhsize_nonrelative[,c('Frequency', 'Sum_Freq'):=NULL]
    
    joint_hhsize_nonrelative = dcast(joint_hhsize_nonrelative, HHSIZECAT + CHILD_CAT + RELATIVE_CAT ~ NONRELATIVE_CAT, value.var = "Probabilities")
    
    df = df[joint_hhsize_nonrelative, on = .(HHSIZECAT, CHILD_CAT, RELATIVE_CAT), nomatch = NULL]
    
    prob_matrix <- as.matrix(df %>% select(as.character(nonrelativecats)))
    compare_vector <- runif(nrow(prob_matrix))
    cumul.prob_matrix <- prob_matrix %*% upper.tri(diag(ncol(prob_matrix)), diag = TRUE) / rowSums(prob_matrix)
    df[,NONRELATIVE_CAT := rowSums(compare_vector > cumul.prob_matrix) + nonrelativecats[1]]
    
    df[,c(as.character(nonrelativecats)):=NULL]
    
  }
  
  if ("NONRELATIVE_CAT" %in% variablenames_hhsize2 == TRUE & "CHILD_CAT" %in% variablenames_hhsize2 == FALSE){
    joint_hhsize_nonrelative <- joint_hhsize2[, .(Frequency = sum(Frequency)), by = .(HHSIZECAT, RELATIVE_CAT, NONRELATIVE_CAT)]
    sum_freq_nonrelative <- joint_hhsize_nonrelative[, .(Sum_Freq = sum(Frequency)), by = .(HHSIZECAT, RELATIVE_CAT)]
    
    joint_hhsize_nonrelative <- joint_hhsize_nonrelative[sum_freq_nonrelative, on = .(HHSIZECAT, RELATIVE_CAT), nomatch = NULL]
    joint_hhsize_nonrelative[, Probabilities := Frequency / Sum_Freq]
    
    joint_hhsize_nonrelative[is.na(joint_hhsize_nonrelative)] <- 0
    
    nonrelativecats <- unique(joint_hhsize2$NONRELATIVE_CAT)
    
    joint_hhsize_nonrelative[,c('Frequency', 'Sum_Freq'):=NULL]
    
    joint_hhsize_nonrelative = dcast(joint_hhsize_nonrelative, HHSIZECAT + RELATIVE_CAT ~ NONRELATIVE_CAT, value.var = "Probabilities")
    
    df = df[joint_hhsize_nonrelative, on = .(HHSIZECAT, RELATIVE_CAT), nomatch = NULL]
    
    prob_matrix <- as.matrix(df %>% select(as.character(nonrelativecats)))
    compare_vector <- runif(nrow(prob_matrix))
    cumul.prob_matrix <- prob_matrix %*% upper.tri(diag(ncol(prob_matrix)), diag = TRUE) / rowSums(prob_matrix)
    df[,NONRELATIVE_CAT := rowSums(compare_vector > cumul.prob_matrix) + nonrelativecats[1]]
    
    df[,c(as.character(nonrelativecats)):=NULL]
    
  }
  
  
  return(df)
  
}

add_age_child_rel_nonrel <- function(df, jointagesex345, child_rel_or_nonrel, edu_tib, educatshead){ 
  setDT(jointagesex345)
  agecatsall <- unique(jointagesex345$AGECAT)
  
  #all_educats <- unique(edu_tib$EDUCAT)
  all_educats <- educatshead
  
  if (child_rel_or_nonrel == "CHILD"){
    jointagesex345_nozero <- jointagesex345[jointagesex345$Frequency > 0,]
    
    jointagesex345_sum <- jointagesex345[, .(Sum_Freq = sum(Frequency)), by = .(educ_head, age_head)] #sex_head eruit gooien. 18 mei.  
    jointagesex345 <- jointagesex345[, .(Frequency = sum(Frequency)), by = .(educ_head, age_head, AGECAT)]
    
    jointagesex345 <- jointagesex345[jointagesex345_sum, on = .(educ_head, age_head), nomatch = NULL]
    
    jointagesex345[, Probabilities := Frequency / Sum_Freq] 
    
    jointagesex345[is.na(jointagesex345)] <- 0
    
    # meer educats toevoegen. 
    educ_head_educats <- unique(jointagesex345$educ_head) 
    
    if (length(educ_head_educats) < length(all_educats)){
      missing_edu <- setdiff(all_educats, educ_head_educats)
      for (miscat in c(3,4,2,5,1)){
        if (miscat %in% missing_edu){
          print('miscat educ')
          print(miscat)
          pos_mis_edu <- match(miscat,all_educats)
          jointagesex345_lower <- jointagesex345[jointagesex345$educ_head == all_educats[pos_mis_edu-1],]
          jointagesex345_higher <- jointagesex345[jointagesex345$educ_head == all_educats[pos_mis_edu+1],]
          
          if (nrow(jointagesex345_lower) > 0 & nrow(jointagesex345_higher) > 0){
            jointagesex345_missing <- (jointagesex345_lower + jointagesex345_higher)/2
            jointagesex345_missing$educ_head <- miscat
            
            jointagesex345 <- bind_rows(jointagesex345, jointagesex345_missing)
            
          } else if (nrow(jointagesex345_lower) == 0){
            jointagesex345_missing <- jointagesex345_higher
            jointagesex345_missing$educ_head <- miscat
            
            jointagesex345 <- bind_rows(jointagesex345, jointagesex345_missing)
          } else if (nrow(jointagesex345_higher) == 0){
            jointagesex345_missing <- jointagesex345_lower
            jointagesex345_missing$educ_head <- miscat
            
            jointagesex345 <- bind_rows(jointagesex345, jointagesex345_missing)
          }
        }
      }
    }
    
    
    
    agecats <- sort(unique(jointagesex345_nozero$AGECAT))
    
    jointagesex345[,c('Frequency', 'Sum_Freq'):=NULL]
    
    jointagesex345 = dcast(jointagesex345, educ_head + age_head ~ AGECAT, value.var = "Probabilities")
    
    # 19 mei: if row contains all zeroes, fix this:
    jointagesex345 <- as.data.frame(jointagesex345)
    for (row in (1:nrow(jointagesex345))){
      if (sum(jointagesex345[row,3:ncol(jointagesex345)]) == 0){
        print(row)
        jointagesex345_removerow <- jointagesex345[-row, ]
        jointagesex345_noedu <- jointagesex345_removerow[jointagesex345_removerow$age_head == jointagesex345[row,2],] 
        jointagesex345_noedu_mean <- colMeans(jointagesex345_noedu[ , 3:ncol(jointagesex345)])
        jointagesex345[row, 3:ncol(jointagesex345)] <- jointagesex345_noedu_mean
      }
      
    }
    
    jointagesex345 <- as.data.table(jointagesex345)
    
    
    
    setnames(jointagesex345, c('educ_head', 'age_head'), c('educ', 'age')) #relate is new name. 
    
    df = df[jointagesex345, on = .(educ, age), nomatch = NULL]
    
    #remove the rows where a nan is present. age1 bestaat niet namelijk. 
    
    prob_matrix <- as.matrix(df %>% select(as.character(agecats)))
    compare_vector <- runif(nrow(prob_matrix))
    cumul.prob_matrix <- prob_matrix %*% upper.tri(diag(ncol(prob_matrix)), diag = TRUE) / rowSums(prob_matrix)
    df[,AGE_CHILD := rowSums(compare_vector > cumul.prob_matrix)]
    while (length(unique(df$AGE_CHILD)) < length(agecats)){
      tableage <- as.data.frame(table(jointagesex345_nozero$AGECAT))
      welkeweg <- which.min(tableage$Freq)
      welke_age_weg <- agecats[welkeweg]
      agecats <- agecats[-welkeweg]
      jointagesex345_nozero <- jointagesex345_nozero[jointagesex345_nozero$AGECAT != welke_age_weg,] #19 mei, volgens mij gaat er iets fout met de index. aangepast, welke_age_weg.  
    }
    df[, AGE_CHILD := factor(AGE_CHILD, levels = sort(unique(df$AGE_CHILD)), labels = agecats)]
    df[, AGE_CHILD:=as.numeric(as.character(AGE_CHILD))]
    
    agecats <- unique(jointagesex345_nozero$AGECAT)
    df[,c(as.character(agecats)):=NULL]
    
    remove_othercolumns <- setdiff(agecatsall, agecats)
    if (length(remove_othercolumns) > 0){
      for (i in remove_othercolumns){
        df[,c(as.character(i)):=NULL]
      }
      
    }
    
    
  } else {
    jointagesex345_nozero <- jointagesex345[jointagesex345$Frequency >0 ,]
    
    
    jointagesex345_formarginals  <- jointagesex345[, .(Sum_Freq = sum(Frequency)), by = .(AGECAT)]
    jointagesex345_formarginals[, Probabilities := jointagesex345_formarginals$Sum_Freq/sum(jointagesex345_formarginals$Sum_Freq)]
    jointagesex345_formarginals[, c('Sum_Freq') := NULL]
    
    jointagesex345_sum <- jointagesex345[, .(Sum_Freq = sum(Frequency)), by = .(age_head)] #educ_head eruit gegooid. 18 mei. 
    jointagesex345 <- jointagesex345[, .(Frequency = sum(Frequency)), by = .(age_head, AGECAT)]
    
    jointagesex345 <- jointagesex345[jointagesex345_sum, on = .(age_head), nomatch = NULL]
    
    jointagesex345[, Probabilities := Frequency / Sum_Freq] 
    
    jointagesex345[is.na(jointagesex345)] <- 0
    
    #agecats <- unique(jointagesex345$AGECAT)
    agecats <- unique(jointagesex345_nozero$AGECAT)
    
    jointagesex345[,c('Frequency', 'Sum_Freq'):=NULL]
    
    jointagesex345 = dcast(jointagesex345, age_head ~ AGECAT, value.var = "Probabilities")
    setnames(jointagesex345, c('age_head'), c('age'))
    
    df = df[jointagesex345, on = .(age), nomatch = NULL]
    
    prob_matrix <- as.matrix(df %>% select(as.character(agecats)))
    compare_vector <- runif(nrow(prob_matrix))
    cumul.prob_matrix <- prob_matrix %*% upper.tri(diag(ncol(prob_matrix)), diag = TRUE) / rowSums(prob_matrix)
    if (child_rel_or_nonrel == 'RELATIVE'){
      df[,AGE_RELATIVE := rowSums(compare_vector > cumul.prob_matrix)]
      while (length(unique(df$AGE_RELATIVE)) < length(agecats)){
        tableage <- as.data.frame(table(jointagesex345_nozero$AGECAT))
        welkeweg <- which.min(tableage$Freq)
        welke_age_weg <- agecats[welkeweg]
        agecats <- agecats[-welkeweg]
        jointagesex345_nozero <- jointagesex345_nozero[jointagesex345_nozero$AGECAT != welke_age_weg,]
      }
      df[, AGE_RELATIVE := factor(AGE_RELATIVE, levels = sort(unique(df$AGE_RELATIVE)), labels = agecats)]
      df[, AGE_RELATIVE:=as.numeric(as.character(AGE_RELATIVE))]
    } else if (child_rel_or_nonrel == 'NONRELATIVE'){
      
      df[,AGE_NONRELATIVE := rowSums(compare_vector > cumul.prob_matrix)]
      
      if (sum(is.na(df$AGE_NONRELATIVE)) > 0){
        df[, replace_these_nans := is.na(AGE_NONRELATIVE)]
        for (i in agecats){
          df[,c(as.character(i)):=NULL]
        }
        
        for (i in 1:nrow(jointagesex345_formarginals)){
          df[, as.character(jointagesex345_formarginals[i,1]) := jointagesex345_formarginals[i,2]]
          
        }
        
        prob_matrix <- as.matrix(df %>% select(as.character(agecats)))
        compare_vector <- runif(nrow(prob_matrix))
        cumul.prob_matrix <- prob_matrix %*% upper.tri(diag(ncol(prob_matrix)), diag = TRUE) / rowSums(prob_matrix)
        
        df[,AGE_NONRELATIVE_NAN := rowSums(compare_vector > cumul.prob_matrix)]
        
        df[is.na(df)] <- 0
        
        df[,AGE_NONRELATIVE := AGE_NONRELATIVE + (replace_these_nans * AGE_NONRELATIVE_NAN)]
        
        df[, replace_these_nans := NULL]
        df[, AGE_NONRELATIVE_NAN := NULL]
        
      }
      
      
      while (length(unique(df$AGE_NONRELATIVE)) < length(agecats)){
        tableage <- as.data.frame(table(jointagesex345_nozero$AGECAT))
        welkeweg <- which.min(tableage$Freq)
        welke_age_weg <- agecats[welkeweg]
        agecats <- agecats[-welkeweg]
        jointagesex345_nozero <- jointagesex345_nozero[jointagesex345_nozero$AGECAT != welke_age_weg,]
      }
      df[, AGE_NONRELATIVE := factor(AGE_NONRELATIVE, levels = sort(unique(df$AGE_NONRELATIVE)), labels = agecats)]
      df[, AGE_NONRELATIVE:=as.numeric(as.character(AGE_NONRELATIVE))]
    }
    
    df[,c(as.character(agecats)):=NULL]
    
    remove_othercolumns <- setdiff(agecatsall, agecats)
    if (length(remove_othercolumns) > 0){
      for (i in remove_othercolumns){
        df[,c(as.character(i)):=NULL]
      }
      
    }
  }
  
  
  return(df)
  
}

add_sex_child_rel_nonrel <- function(df, jointagesex345, child_rel_or_nonrel){ 
  setDT(jointagesex345)
  if (child_rel_or_nonrel == 'CHILD'){
    
    jointagesex345_sum <- jointagesex345[, .(Sum_Freq = sum(Frequency)), by = .(AGECAT)] #sex child on age child. 
    jointagesex345 <- jointagesex345[, .(Frequency = sum(Frequency)), by = .(AGECAT, sex_number)]
    jointagesex345 <- jointagesex345[jointagesex345_sum, on = .(AGECAT), nomatch = NULL]
    
    jointagesex345[, Probabilities := Frequency / Sum_Freq] 
    
    jointagesex345[is.na(jointagesex345)] <- 0
    
    sexcats <- unique(jointagesex345$sex_number)
    
    jointagesex345[,c('Frequency', 'Sum_Freq'):=NULL]
    
    jointagesex345 = dcast(jointagesex345, AGECAT ~ sex_number, value.var = "Probabilities")
    
    jointagesex345[1,2] <- 0.5
    jointagesex345[1,3] <- 0.5
    
    setnames(jointagesex345, c('AGECAT'), c('AGE_CHILD')) #relate is new name. 
    
    df = df[jointagesex345, on = .(AGE_CHILD), nomatch = NULL]
    
    prob_matrix <- as.matrix(df %>% select(as.character(sexcats)))
    compare_vector <- runif(nrow(prob_matrix))
    cumul.prob_matrix <- prob_matrix %*% upper.tri(diag(ncol(prob_matrix)), diag = TRUE) / rowSums(prob_matrix)
    df[,SEX_CHILD := rowSums(compare_vector > cumul.prob_matrix) + sexcats[1]]
    
    df[,c(as.character(sexcats)):=NULL]
    
    
    
  } else { 
    jointagesex345_sum <- jointagesex345[, .(Sum_Freq = sum(Frequency)), by = .(AGECAT)] #18 mei. relative sex op age relative. 
    jointagesex345 <- jointagesex345[, .(Frequency = sum(Frequency)), by = .(AGECAT, sex_number)]
    
    jointagesex345 <- jointagesex345[jointagesex345_sum, on = .(AGECAT), nomatch = NULL]
    
    jointagesex345[, Probabilities := Frequency / Sum_Freq] 
    
    jointagesex345[is.na(jointagesex345)] <- 0
    
    sexcats <- unique(jointagesex345$sex_number)
    
    jointagesex345[,c('Frequency', 'Sum_Freq'):=NULL]
    
    jointagesex345 = dcast(jointagesex345, AGECAT ~ sex_number, value.var = "Probabilities")
    
    setnames(jointagesex345, c('AGECAT'), c(paste0('AGE_', child_rel_or_nonrel)))
    
    if (child_rel_or_nonrel == 'RELATIVE'){
      df = df[jointagesex345, on = .(AGE_RELATIVE), nomatch = NULL] #age weggehaald. 19 mei. 
    } else if (child_rel_or_nonrel == 'NONRELATIVE'){
      df = df[jointagesex345, on = .(AGE_NONRELATIVE), nomatch = NULL] #age weggehaald. 19 mei.
    }
    
    prob_matrix <- as.matrix(df %>% select(as.character(sexcats)))
    compare_vector <- runif(nrow(prob_matrix))
    cumul.prob_matrix <- prob_matrix %*% upper.tri(diag(ncol(prob_matrix)), diag = TRUE) / rowSums(prob_matrix)
    if (child_rel_or_nonrel == 'RELATIVE'){
      df[,SEX_RELATIVE := rowSums(compare_vector > cumul.prob_matrix) + sexcats[1]]
    } else if (child_rel_or_nonrel == 'NONRELATIVE'){
      df[,SEX_NONRELATIVE := rowSums(compare_vector > cumul.prob_matrix) + sexcats[1]]
    }
    
    df[,c(as.character(sexcats)):=NULL]
    
  }
  
  return(df)
} 

add_educ_child_rel_nonrel <- function(df, jointeduc345, child_rel_or_nonrel, edu_tib, educatshead){ 
  setDT(jointeduc345)
  educcatsall <- unique(jointeduc345$education)
  
  #all_educats <- unique(edu_tib$EDUCAT) # dit misschien aanpassen. 
  all_educats <- educatshead # aangepast 22 mei. 
  
  jointeduc345_formarginals  <- jointeduc345[, .(Sum_Freq = sum(Frequency)), by = .(education)]
  jointeduc345_formarginals[, Probabilities := jointeduc345_formarginals$Sum_Freq/sum(jointeduc345_formarginals$Sum_Freq)]
  jointeduc345_formarginals[, c('Sum_Freq') := NULL]
  
  
  jointeduc345_nozero <- jointeduc345[jointeduc345$Frequency > 0 ,]
  jointeduc345_sum <- jointeduc345[, .(Sum_Freq = sum(Frequency)), by = .(educ_head, AGECAT, sex_number)]
  jointeduc345 <- jointeduc345[jointeduc345_sum, on = .(educ_head, AGECAT, sex_number), nomatch = NULL]
  jointeduc345[, Probabilities := Frequency / Sum_Freq] 
  jointeduc345[is.na(jointeduc345)] <- 0
  
  # Probabilities: 
  jointeduc345$New_Probabilities <- jointeduc345$Probabilities + 0.001
  jointeduc345_AGECAT1 <- jointeduc345[jointeduc345$AGECAT == 1,]
  jointeduc345_AGECAT1$New_Probabilities <- jointeduc345_AGECAT1$Probabilities
  
  jointeduc345 <- jointeduc345[jointeduc345$AGECAT != 1,]
  
  jointeduc345 <- bind_rows(jointeduc345_AGECAT1, jointeduc345)
  
  jointeduc345_sumprob <- jointeduc345[, .(Sum_Probabilities = sum(New_Probabilities)), by = .(educ_head, AGECAT, sex_number)]
  jointeduc345_sumprob$Sum_Probabilities <- 1/jointeduc345_sumprob$Sum_Probabilities
  jointeduc345 <- jointeduc345[jointeduc345_sumprob, on = .(educ_head, AGECAT, sex_number), nomatch = NULL]
  
  jointeduc345$Probabilities <- jointeduc345$New_Probabilities * jointeduc345$Sum_Probabilities
  
  jointeduc345[, c('Sum_Probabilities', 'New_Probabilities') := NULL]
  
  jointeduc345[is.na(jointeduc345)] <- 0
  
  educ_head_educats <- unique(jointeduc345$educ_head) 
  
  if (length(educ_head_educats) < length(all_educats)){
    missing_edu <- setdiff(all_educats, educ_head_educats)
    for (miscat in c(3,4,2,5,1)){
      if (miscat %in% missing_edu){
        print('miscat educ')
        print(miscat)
        pos_mis_edu <- match(miscat,all_educats)
        jointeduc345_lower <- jointeduc345[jointeduc345$educ_head == all_educats[pos_mis_edu-1],]
        jointeduc345_higher <- jointeduc345[jointeduc345$educ_head == all_educats[pos_mis_edu+1],]
        
        if (nrow(jointeduc345_lower) > 0 & nrow(jointeduc345_higher) > 0){
          jointeduc345_missing <- (jointeduc345_lower + jointeduc345_higher)/2
          jointeduc345_missing$educ_head <- miscat
          
          jointeduc345 <- bind_rows(jointeduc345, jointeduc345_missing)
          
        } else if (nrow(jointeduc345_lower) == 0){
          jointeduc345_missing <- jointeduc345_higher
          jointeduc345_missing$educ_head <- miscat
          
          jointeduc345 <- bind_rows(jointeduc345, jointeduc345_missing)
        } else if (nrow(jointeduc345_higher) == 0){
          jointeduc345_missing <- jointeduc345_lower
          jointeduc345_missing$educ_head <- miscat
          
          jointeduc345 <- bind_rows(jointeduc345, jointeduc345_missing)
        }
      }
    }
  }
  
  # above added 18 may. 
  
  
  
  
  #educcats <- unique(jointeduc345$education)
  educcats <- unique(jointeduc345_nozero$education)
  
  jointeduc345[,c('Frequency', 'Sum_Freq'):=NULL]
  
  jointeduc345 = dcast(jointeduc345, educ_head + AGECAT + sex_number ~ education, value.var = "Probabilities")
  
  setnames(jointeduc345, c('educ_head', 'AGECAT', 'sex_number'), c('educ', paste0('AGE_', child_rel_or_nonrel), paste0('SEX_', child_rel_or_nonrel)))
  
  # 4 regels hieronder toegevoegd om de gelijke probabilities op te vullen met de marginal. 22 mei. 
  jointeduc345 = as.data.frame(jointeduc345)
  for (row in 1:nrow(jointeduc345)){
    if(all(jointeduc345[row, (ncol(jointeduc345) - (length(educcats)-1)):ncol(jointeduc345)] == jointeduc345[row, ncol(jointeduc345)])){
      jointeduc345[row, (ncol(jointeduc345) - (length(educcats)-1)):ncol(jointeduc345)] <- jointeduc345_formarginals$Probabilities
    }
  }
  
  setDT(jointeduc345)
  
  
  if (child_rel_or_nonrel == "CHILD"){
    df = df[jointeduc345, on = .(educ, AGE_CHILD, SEX_CHILD), nomatch = NULL] 
  } else if (child_rel_or_nonrel == "RELATIVE"){
    df = df[jointeduc345, on = .(educ, AGE_RELATIVE, SEX_RELATIVE), nomatch = NULL] 
  } else if (child_rel_or_nonrel == "NONRELATIVE"){
    df = df[jointeduc345, on = .(educ, AGE_NONRELATIVE, SEX_NONRELATIVE), nomatch = NULL] 
  }
  
  
  prob_matrix <- as.matrix(df %>% select(as.character(educcats))) #geen nans
  
  compare_vector <- runif(nrow(prob_matrix)) #geen nans
  cumul.prob_matrix <- prob_matrix %*% upper.tri(diag(ncol(prob_matrix)), diag = TRUE) / rowSums(prob_matrix) #hier zitten nans in. 
  
  if (child_rel_or_nonrel == "CHILD"){
    #df["EDUC_CHILD"] <- rowSums(compare_vector > cumul.prob_matrix) + educcats[1]
    df[, EDUC_CHILD := rowSums(compare_vector > cumul.prob_matrix)]
    while (length(unique(df$EDUC_CHILD)) < length(educcats)){
      tableedu <- as.data.frame(table(jointeduc345_nozero$education))
      welkeweg <- which.min(tableedu$Freq)
      welke_edu_weg <- educcats[welkeweg]
      educcats <- educcats[-welkeweg]
      jointeduc345_nozero <- jointeduc345_nozero[jointeduc345_nozero$education != welke_edu_weg,]
    }
    df[, EDUC_CHILD := factor(EDUC_CHILD, levels = sort(unique(df$EDUC_CHILD)), labels = educcats)]
    df[, EDUC_CHILD:=as.numeric(as.character(EDUC_CHILD))]
  } else if (child_rel_or_nonrel == "RELATIVE"){
    #df["EDUC_RELATIVE"] <- rowSums(compare_vector > cumul.prob_matrix) + educcats[1]
    df[, EDUC_RELATIVE := rowSums(compare_vector > cumul.prob_matrix)] #hier komen nans in, maar hoe dan. 
    while (length(unique(df$EDUC_RELATIVE)) < length(educcats)){
      tableedu <- as.data.frame(table(jointeduc345_nozero$education))
      welkeweg <- which.min(tableedu$Freq)
      welke_edu_weg <- educcats[welkeweg]
      educcats <- educcats[-welkeweg]
      jointeduc345_nozero <- jointeduc345_nozero[jointeduc345_nozero$education != welke_edu_weg,] #hier ook aangepast 19 mei. 
    }
    df[, EDUC_RELATIVE := factor(EDUC_RELATIVE, levels = sort(unique(df$EDUC_RELATIVE)), labels = educcats)]
    df[, EDUC_RELATIVE:=as.numeric(as.character(EDUC_RELATIVE))]
  } else if (child_rel_or_nonrel == "NONRELATIVE"){
    #df["EDUC_NONRELATIVE"] <- rowSums(compare_vector > cumul.prob_matrix) + educcats[1]
    df[, EDUC_NONRELATIVE := rowSums(compare_vector > cumul.prob_matrix)]
    
    if (sum(is.na(df$EDUC_NONRELATIVE)) > 0){
      df[, replace_these_nans := is.na(EDUC_NONRELATIVE)]
      for (i in educcats){
        df[,c(as.character(i)):=NULL]
      }
      
      for (i in 1:nrow(jointeduc345_formarginals)){
        df[, as.character(jointeduc345_formarginals[i,1]) := jointeduc345_formarginals[i,2]]
        
      }
      
      prob_matrix <- as.matrix(df %>% select(as.character(educcats)))
      compare_vector <- runif(nrow(prob_matrix))
      cumul.prob_matrix <- prob_matrix %*% upper.tri(diag(ncol(prob_matrix)), diag = TRUE) / rowSums(prob_matrix)
      
      df[,EDUC_NONRELATIVE_NAN := rowSums(compare_vector > cumul.prob_matrix)]
      
      df[is.na(df)] <- 0
      
      df[,EDUC_NONRELATIVE := EDUC_NONRELATIVE + (replace_these_nans * EDUC_NONRELATIVE_NAN)]
      
      df[, EDUC_NONRELATIVE_NAN := NULL]
      df[, replace_these_nans := NULL]
      
    }
    
    while (length(unique(df$EDUC_NONRELATIVE)) < length(educcats)){
      tableedu <- as.data.frame(table(jointeduc345_nozero$education))
      welkeweg <- which.min(tableedu$Freq)
      welke_edu_weg <- educcats[welkeweg]
      educcats <- educcats[-welkeweg]
      jointeduc345_nozero <- jointeduc345_nozero[jointeduc345_nozero$education != welke_edu_weg,]
    }
    df[, EDUC_NONRELATIVE := factor(EDUC_NONRELATIVE, levels = sort(unique(df$EDUC_NONRELATIVE)), labels = educcats)]
    df[, EDUC_NONRELATIVE:=as.numeric(as.character(EDUC_NONRELATIVE))]
  }
  
  
  
  educcats <- unique(jointeduc345_nozero$education)
  df[,c(as.character(educcats)):=NULL]
  remove_othercolumns <- setdiff(educcatsall, educcats)
  if (length(remove_othercolumns) > 0){
    for (i in remove_othercolumns){
      df[,c(as.character(i)):=NULL]
    }
    
  }
  
  return(df)
  
}  

add_children_characteristics <- function(jointhead_head, children_table, txtpath, beginlines_list, endlines_list, add_age_child_rel_nonrel, add_sex_child_rel_nonrel, add_educ_child_rel_nonrel){
  
  jointhead_head[, Nr_children := CHILD_CAT + 
                   as.integer(CHILD_CAT == unlist(children_table[1])[1]) * 
                   (sample(unlist(children_table[1]), size = nrow(jointhead_head), 
                           prob = c(unlist(children_table[2])), replace = TRUE)-(unlist(children_table[1])[1]))]
  
  jointhead_head[, Head := 1]
  
  
  #jointhead_head["Head"] <- 1
  
  #jointhead_head <- jointhead_head %>% 
  #  pivot_longer(c('Head', 'Nr_children'), names_to = "relate_test", values_to = "to_rep")
  
  jointhead_head = melt(jointhead_head, measure.vars = c('Head', 'Nr_children'), variable.name = 'relate_update', value.name = 'to_rep')
  
  jointhead_head <- as.data.table(lapply(jointhead_head, rep, jointhead_head$to_rep))
  gc()
  #jointhead_head['relate_update'] <- 0
  
  jointhead_head[, relate_update := as.integer(relate_update)]
  jointhead_head[relate_update == 2, relate_update := 3]
  gc()
  
  jointchild_agesex <- text_to_df(txtpath, beginlines_list[[33]], endlines_list[[33]], FALSE)
  
  jointchild_agesex$Frequency[jointchild_agesex$age_head == 2 & jointchild_agesex$AGECAT == 2] <- 0
  jointchild_agesex$Frequency[jointchild_agesex$age_head == 3 & jointchild_agesex$AGECAT == 3] <- 0
  jointchild_agesex$Frequency[jointchild_agesex$age_head == 4 & jointchild_agesex$AGECAT == 4] <- 0
  jointchild_agesex$Frequency[jointchild_agesex$age_head == 5 & jointchild_agesex$AGECAT == 5] <- 0
  jointchild_agesex$Frequency[jointchild_agesex$age_head == 6 & jointchild_agesex$AGECAT == 6] <- 0
  
  jointchild_educ <- text_to_df(txtpath, beginlines_list[[34]], endlines_list[[34]], FALSE)
  
  jointhead_head[,c('to_rep', 'Truncated', 'Frequency'):=NULL]
  
  jointhead_head <- add_age_child_rel_nonrel(jointhead_head, jointchild_agesex, "CHILD", edu_tib, educatshead)
  gc()
  
  jointhead_head <- add_sex_child_rel_nonrel(jointhead_head, jointchild_agesex, "CHILD")
  gc()
  
  jointhead_head <- add_educ_child_rel_nonrel(jointhead_head, jointchild_educ, "CHILD", edu_tib, educatshead)
  gc()
  
  return(jointhead_head)
  
}

add_relatives_nonrelatives_characteristics <- function(jointhead_head, relative_table, non_relative_table, nonrelative_ind, txtpath, beginlines_list, endlines_list, add_age_child_rel_nonrel, add_sex_child_rel_nonrel, add_educ_child_rel_nonrel){
  
  print('warning: if hhtype has children, do not use this function')
  
  jointhead_head[, Nr_relatives := RELATIVE_CAT + 
                   as.integer(RELATIVE_CAT == unlist(relative_table[1])[1]) * 
                   (sample(unlist(relative_table[1]), size = nrow(jointhead_head), 
                           prob = c(unlist(relative_table[2])), replace = TRUE)-(unlist(relative_table[1])[1]))]
  
  if (nonrelative_ind != 0){
    
    if(length(unlist(nonrelative_table[2])) == 1){
      jointhead_head[, Nr_nonrelatives := NONRELATIVE_CAT + 
                       (as.integer(NONRELATIVE_CAT == unlist(nonrelative_table[1]) * (unlist(nonrelative_table[1]) - max(jointhead_head$NONRELATIVE_CAT))))]
    } else if (length(unlist(nonrelative_table[2])) > 1){             
      
      jointhead_head[, Nr_nonrelatives := NONRELATIVE_CAT + 
                       as.integer(NONRELATIVE_CAT == unlist(nonrelative_table[1])[1]) * 
                       (sample(unlist(nonrelative_table[1]), size = nrow(jointhead_head), 
                               prob = c(unlist(nonrelative_table[2])), replace = TRUE)-(unlist(nonrelative_table[1])[1]))]
    }
    
    jointhead_head[, Head := 1]
    
    jointhead_head = melt(jointhead_head, measure.vars = c('Head', 'Nr_relatives', 'Nr_nonrelatives'), variable.name = 'relate_update', value.name = 'to_rep')
    
    jointhead_head <- as.data.table(lapply(jointhead_head, rep, jointhead_head$to_rep))
    gc()
    
    jointhead_head[relate_update == "Head", relate_update := "1"]
    jointhead_head[relate_update == "Nr_relatives", relate_update := "2"]
    jointhead_head[relate_update == "Nr_nonrelatives", relate_update := "3"]
    
    
    jointhead_head[, relate_update := as.integer(relate_update)]
    
    jointhead_head[relate_update == 4, relate_update := 1]
    jointhead_head[relate_update == 5, relate_update := 4]
    jointhead_head[relate_update == 6, relate_update := 5]
    gc()
    
    
    jointrelative_agesex <- text_to_df(txtpath, beginlines_list[[35]], endlines_list[[35]], FALSE)
    jointrelative_educ <- text_to_df(txtpath, beginlines_list[[36]], endlines_list[[36]], FALSE)
    jointnonrelative_agesex <- text_to_df(txtpath, beginlines_list[[37]], endlines_list[[37]], FALSE)
    jointnonrelative_educ <- text_to_df(txtpath, beginlines_list[[38]], endlines_list[[38]], FALSE)
    
    jointhead_head[,c('to_rep', 'Truncated', 'Frequency'):=NULL]
    
    jointhead_head <- add_age_child_rel_nonrel(jointhead_head, jointrelative_agesex, "RELATIVE", edu_tib, educatshead)
    gc()
    
    jointhead_head <- add_sex_child_rel_nonrel(jointhead_head, jointrelative_agesex, "RELATIVE")
    gc()
    
    jointhead_head <- add_educ_child_rel_nonrel(jointhead_head, jointrelative_educ, "RELATIVE", edu_tib, educatshead)
    gc()
    
    jointhead_head <- add_age_child_rel_nonrel(jointhead_head, jointnonrelative_agesex, "NONRELATIVE", edu_tib, educatshead)
    gc()
    
    jointhead_head <- add_sex_child_rel_nonrel(jointhead_head, jointnonrelative_agesex, "NONRELATIVE")
    gc()
    
    jointhead_head <- add_educ_child_rel_nonrel(jointhead_head, jointnonrelative_educ, "NONRELATIVE", edu_tib, educatshead)
    gc()
    
  } else if (nonrelative_ind == 0){
    
    jointhead_head[, Head := 1]
    
    jointhead_head = melt(jointhead_head, measure.vars = c('Head', 'Nr_relatives'), variable.name = 'relate_update', value.name = 'to_rep')
    
    jointhead_head <- as.data.table(lapply(jointhead_head, rep, jointhead_head$to_rep))
    gc()
    
    jointhead_head[relate_update == "Head", relate_update := "1"]
    jointhead_head[relate_update == "Nr_relatives", relate_update := "2"]
    
    
    jointhead_head[, relate_update := as.integer(relate_update)]
    
    jointhead_head[relate_update == 3, relate_update := 1]
    jointhead_head[relate_update == 4, relate_update := 4]
    
    gc()
    
    
    jointrelative_agesex <- text_to_df(txtpath, beginlines_list[[35]], endlines_list[[35]], FALSE)
    jointrelative_educ <- text_to_df(txtpath, beginlines_list[[36]], endlines_list[[36]], FALSE)
    
    jointhead_head[,c('to_rep', 'Truncated', 'Frequency'):=NULL]
    
    jointhead_head <- add_age_child_rel_nonrel(jointhead_head, jointrelative_agesex, "RELATIVE", edu_tib, educatshead)
    gc()
    
    jointhead_head <- add_sex_child_rel_nonrel(jointhead_head, jointrelative_agesex, "RELATIVE")
    gc()
    
    jointhead_head <- add_educ_child_rel_nonrel(jointhead_head, jointrelative_educ, "RELATIVE", edu_tib, educatshead)
    gc()
  }
  
  return(jointhead_head)
  
}

add_345_characteristics <- function(jointhead_head, children_table, relative_table, non_relative_table, nonrelative_ind, txtpath, beginlines_list, endlines_list, add_age_child_rel_nonrel, add_sex_child_rel_nonrel, add_educ_child_rel_nonrel){
  
  jointhead_head[, Nr_children := CHILD_CAT + 
                   as.integer(CHILD_CAT == unlist(children_table[1])[1]) * 
                   (sample(unlist(children_table[1]), size = nrow(jointhead_head), 
                           prob = c(unlist(children_table[2])), replace = TRUE)-(unlist(children_table[1])[1]))]
  
  
  jointhead_head[, Nr_relatives := RELATIVE_CAT + 
                   as.integer(RELATIVE_CAT == unlist(relative_table[1])[1]) * 
                   (sample(unlist(relative_table[1]), size = nrow(jointhead_head), 
                           prob = c(unlist(relative_table[2])), replace = TRUE)-(unlist(relative_table[1])[1]))]
  
  if (nonrelative_ind != 0){
    
    if(length(unlist(nonrelative_table[2])) == 1){
      jointhead_head[, Nr_nonrelatives := NONRELATIVE_CAT + 
                       (as.integer(NONRELATIVE_CAT == unlist(nonrelative_table[1]) * (unlist(nonrelative_table[1]) - max(jointhead_head$NONRELATIVE_CAT))))]
    } else if (length(unlist(nonrelative_table[2])) > 1){             
      
      jointhead_head[, Nr_nonrelatives := NONRELATIVE_CAT + 
                       as.integer(NONRELATIVE_CAT == unlist(nonrelative_table[1])[1]) * 
                       (sample(unlist(nonrelative_table[1]), size = nrow(jointhead_head), 
                               prob = c(unlist(nonrelative_table[2])), replace = TRUE)-(unlist(nonrelative_table[1])[1]))]
    }
    
    jointhead_head[, Head := 1]
    
    jointhead_head = melt(jointhead_head, measure.vars = c('Head', 'Nr_children', 'Nr_relatives', 'Nr_nonrelatives'), variable.name = 'relate_update', value.name = 'to_rep')
    
    jointhead_head <- as.data.table(lapply(jointhead_head, rep, jointhead_head$to_rep))
    gc()
    
    jointhead_head[relate_update == "Head", relate_update := "1"]
    jointhead_head[relate_update == "Nr_children", relate_update := "2"]
    jointhead_head[relate_update == "Nr_relatives", relate_update := "3"]
    jointhead_head[relate_update == "Nr_nonrelatives", relate_update := "4"]
    
    
    jointhead_head[, relate_update := as.integer(relate_update)]
    
    jointhead_head[relate_update == 5, relate_update := 1]
    jointhead_head[relate_update == 6, relate_update := 3]
    jointhead_head[relate_update == 7, relate_update := 4]
    jointhead_head[relate_update == 8, relate_update := 5]
    gc()
    
    
    jointchild_agesex <- text_to_df(txtpath, beginlines_list[[33]], endlines_list[[33]], FALSE)
    jointchild_educ <- text_to_df(txtpath, beginlines_list[[34]], endlines_list[[34]], F)
    jointrelative_agesex <- text_to_df(txtpath, beginlines_list[[35]], endlines_list[[35]], F)
    jointrelative_educ <- text_to_df(txtpath, beginlines_list[[36]], endlines_list[[36]], F)
    jointnonrelative_agesex <- text_to_df(txtpath, beginlines_list[[37]], endlines_list[[37]], F)
    jointnonrelative_educ <- text_to_df(txtpath, beginlines_list[[38]], endlines_list[[38]], F)
    
    jointhead_head[,c('to_rep', 'Truncated', 'Frequency'):=NULL]
    
    jointhead_head <- add_age_child_rel_nonrel(jointhead_head, jointchild_agesex, "CHILD", edu_tib, educatshead)
    gc()
    
    jointhead_head <- add_sex_child_rel_nonrel(jointhead_head, jointchild_agesex, "CHILD")
    gc()
    
    jointhead_head <- add_educ_child_rel_nonrel(jointhead_head, jointchild_educ, "CHILD", edu_tib, educatshead)
    gc()
    
    jointhead_head <- add_age_child_rel_nonrel(jointhead_head, jointrelative_agesex, "RELATIVE", edu_tib, educatshead)
    gc()
    
    jointhead_head <- add_sex_child_rel_nonrel(jointhead_head, jointrelative_agesex, "RELATIVE")
    gc()
    
    jointhead_head <- add_educ_child_rel_nonrel(jointhead_head, jointrelative_educ, "RELATIVE", edu_tib, educatshead)
    gc()
    
    jointhead_head <- add_age_child_rel_nonrel(jointhead_head, jointnonrelative_agesex, "NONRELATIVE", edu_tib, educatshead)
    gc()
    
    jointhead_head <- add_sex_child_rel_nonrel(jointhead_head, jointnonrelative_agesex, "NONRELATIVE")
    gc()
    
    jointhead_head <- add_educ_child_rel_nonrel(jointhead_head, jointnonrelative_educ, "NONRELATIVE", edu_tib, educatshead)
    gc()
    
  } else if (nonrelative_ind == 0){
    jointhead_head[, Head := 1]
    
    jointhead_head = melt(jointhead_head, measure.vars = c('Head', 'Nr_children', 'Nr_relatives'), variable.name = 'relate_update', value.name = 'to_rep')
    
    jointhead_head <- as.data.table(lapply(jointhead_head, rep, jointhead_head$to_rep))
    gc()
    
    jointhead_head[relate_update == "Head", relate_update := "1"]
    jointhead_head[relate_update == "Nr_children", relate_update := "2"]
    jointhead_head[relate_update == "Nr_relatives", relate_update := "3"]
    
    
    jointhead_head[, relate_update := as.integer(relate_update)]
    
    jointhead_head[relate_update == 4, relate_update := 1]
    jointhead_head[relate_update == 5, relate_update := 3]
    jointhead_head[relate_update == 6, relate_update := 4]
    gc()
    
    
    jointchild_agesex <- text_to_df(txtpath, beginlines_list[[33]], endlines_list[[33]], FALSE)
    jointchild_educ <- text_to_df(txtpath, beginlines_list[[34]], endlines_list[[34]], F)
    jointrelative_agesex <- text_to_df(txtpath, beginlines_list[[35]], endlines_list[[35]], F)
    jointrelative_educ <- text_to_df(txtpath, beginlines_list[[36]], endlines_list[[36]], F)
    
    jointhead_head[,c('to_rep', 'Truncated', 'Frequency'):=NULL]
    
    jointhead_head <- add_age_child_rel_nonrel(jointhead_head, jointchild_agesex, "CHILD", edu_tib, educatshead)
    gc()
    
    jointhead_head <- add_sex_child_rel_nonrel(jointhead_head, jointchild_agesex, "CHILD")
    gc()
    
    jointhead_head <- add_educ_child_rel_nonrel(jointhead_head, jointchild_educ, "CHILD", edu_tib, educatshead)
    gc()
    
    jointhead_head <- add_age_child_rel_nonrel(jointhead_head, jointrelative_agesex, "RELATIVE", edu_tib, educatshead)
    gc()
    
    jointhead_head <- add_sex_child_rel_nonrel(jointhead_head, jointrelative_agesex, "RELATIVE")
    gc()
    
    jointhead_head <- add_educ_child_rel_nonrel(jointhead_head, jointrelative_educ, "RELATIVE", edu_tib, educatshead)
    gc()
    
    
  }
  
  return(jointhead_head)
  
}
finalize_hhtype220_230 <- function(jointhead220_head, jointhead220_partner){
  
  #jointhead220_head['age_test'] <- 0
  #jointhead220_head$age_test <- (as.integer(jointhead220_head$relate_update == 1) * jointhead220_head$age) +
  #  (as.integer(jointhead220_head$relate_update == 3) * jointhead220_head$AGE_CHILD)
  jointhead220_head[, age_test := (as.integer(relate_update == 1) * age) +
                      (as.integer(relate_update == 3) * AGE_CHILD)]
  
  #jointhead220_head <- jointhead220_head %>% select(-AGE_CHILD, -age)
  jointhead220_head[,c('age', 'AGE_CHILD') := NULL]
  
  
  jointhead220_head[, sex_test := (as.integer(relate_update == 1) * sex) +
                      (as.integer(relate_update == 3) * SEX_CHILD)]
  
  jointhead220_head[,c('sex', 'SEX_CHILD') := NULL]
  
  jointhead220_head[, educ_test := (as.integer(relate_update == 1) * educ) +
                      (as.integer(relate_update == 3) * EDUC_CHILD)]
  
  jointhead220_head[,c('educ', 'EDUC_CHILD') := NULL]
  
  
  setnames(jointhead220_head, c('age_test', 'sex_test', 'educ_test', 'relate_update', 'farming_number', 
                                'rural_number', 'INCOMEQQ', 'hhtype'), c('AGECAT','GENDER', 'EDUCAT', 'RELATE', 
                                                                         'FARMING', 'RURAL', 'INCOME', 'HHTYPE'))
  
  #here remove columns. 
  
  to_remove = setdiff(colnames(jointhead220_head), c('AGECAT','GENDER', 'EDUCAT', 'RELATE', 
                                                     'FARMING', 'RURAL', 'INCOME', 'HHTYPE', 'HID', 'HHSIZECAT'))
  
  jointhead220_head[, c(to_remove):= NULL]
  
  gc()
  
  if (is.null(jointhead220_partner) == FALSE){
    
    
    
    setnames(jointhead220_partner, c('age', 'sex', 'educ', 'relate', 'farming_number', 
                                     'rural_number', 'INCOMEQQ', 'hhtype'), 
             c('AGECAT', 'GENDER', 'EDUCAT', 'RELATE', 'FARMING', 'RURAL', 'INCOME', 'HHTYPE'))
    
    
    jointhead220_partner[, c('Frequency', 'Truncated') := NULL]
    
    
    jointhead220_head <- rbindlist(list(jointhead220_head, jointhead220_partner), use.names=TRUE)
    
    gc()
  }
  
  setkeyv(jointhead220_head, c('HID', 'RELATE'))
  
  return(jointhead220_head)
  
  
}
finalize_hhtype310_400 <- function(jointhead310_head, jointhead310_partner, nonrelative_ind){
  
  #kijk naar rbindlist. ipv. do.call(rbind). 
  
  if (nonrelative_ind != 0){
    
    jointhead310_head[, age_test := (as.integer(relate_update == 1) * age) +
                        (as.integer(relate_update == 4) * AGE_RELATIVE) +
                        (as.integer(relate_update == 5) * AGE_NONRELATIVE)]
    
    jointhead310_head[, c('AGE_RELATIVE', 'AGE_NONRELATIVE', 'age'):= NULL]
    
    
    jointhead310_head[, sex_test := (as.integer(relate_update == 1) * sex) +
                        (as.integer(relate_update == 4) * SEX_RELATIVE) +
                        (as.integer(relate_update == 5) * SEX_NONRELATIVE)]
    
    jointhead310_head[, c('SEX_RELATIVE', 'SEX_NONRELATIVE', 'sex'):= NULL]
    
    jointhead310_head[, educ_test := (as.integer(relate_update == 1) * educ) +
                        (as.integer(relate_update == 4) * EDUC_RELATIVE) +
                        (as.integer(relate_update == 5) * EDUC_NONRELATIVE)]
    
    jointhead310_head[, c('EDUC_RELATIVE', 'EDUC_NONRELATIVE', 'educ'):= NULL]
    
  } else{
    
    jointhead310_head[, age_test := (as.integer(relate_update == 1) * age) +
                        (as.integer(relate_update == 4) * AGE_RELATIVE)]
    
    jointhead310_head[, c('AGE_RELATIVE', 'age'):= NULL]
    
    
    jointhead310_head[, sex_test := (as.integer(relate_update == 1) * sex) +
                        (as.integer(relate_update == 4) * SEX_RELATIVE)]
    
    jointhead310_head[, c('SEX_RELATIVE', 'sex'):= NULL]
    
    jointhead310_head[, educ_test := (as.integer(relate_update == 1) * educ) +
                        (as.integer(relate_update == 4) * EDUC_RELATIVE)]
    
    jointhead310_head[, c('EDUC_RELATIVE', 'educ'):= NULL]
    
    
  }
  
  setnames(jointhead310_head, c('age_test', 'sex_test', 'educ_test', 'relate_update', 'farming_number', 
                                'rural_number', 'INCOMEQQ', 'hhtype'), 
           c('AGECAT','GENDER', 'EDUCAT', 'RELATE', 'FARMING', 'RURAL', 'INCOME', 'HHTYPE'))
  
  
  to_remove = setdiff(colnames(jointhead310_head), c('AGECAT','GENDER', 'EDUCAT', 'RELATE', 
                                                     'FARMING', 'RURAL', 'INCOME', 'HHTYPE', 'HID', 'HHSIZECAT'))
  
  jointhead310_head[, c(to_remove):= NULL]
  
  gc()
  
  if (is.null(jointhead310_partner) == FALSE){
    
    setnames(jointhead310_partner, c('age', 'sex', 'educ', 'relate', 'farming_number', 
                                     'rural_number', 'INCOMEQQ', 'hhtype'), 
             c('AGECAT', 'GENDER', 'EDUCAT', 'RELATE', 'FARMING', 'RURAL', 'INCOME', 'HHTYPE'))
    
    
    jointhead310_partner[, c('Frequency', 'Truncated') := NULL]
    
    
    
    jointhead310_head <- rbindlist(list(jointhead310_head, jointhead310_partner), use.names=TRUE)
    
    gc()
  }
  
  setkeyv(jointhead310_head, c('HID', 'RELATE'))
  
  return(jointhead310_head)
  
  
} #hier ook nonrel fixen
finalize_hhtype320_330 <- function(jointhead320_head, jointhead320_partner, nonrelative_ind){
  
  if (nonrelative_ind != 0){
    
    jointhead320_head[, age_test := (as.integer(relate_update == 1) * age) +
                        (as.integer(relate_update == 3) * AGE_CHILD) + #let even op as integer. wordt dat niet 1, 2. werkt! 
                        (as.integer(relate_update == 4) * AGE_RELATIVE) +
                        (as.integer(relate_update == 5) * AGE_NONRELATIVE)]
    
    jointhead320_head[, c('AGE_CHILD', 'AGE_RELATIVE', 'AGE_NONRELATIVE', 'age'):= NULL]
    
    
    jointhead320_head[, sex_test := (as.integer(relate_update == 1) * sex) +
                        (as.integer(relate_update == 3) * SEX_CHILD) +  
                        (as.integer(relate_update == 4) * SEX_RELATIVE) +
                        (as.integer(relate_update == 5) * SEX_NONRELATIVE)]
    
    jointhead320_head[, c('SEX_CHILD', 'SEX_RELATIVE', 'SEX_NONRELATIVE', 'sex'):= NULL]
    
    jointhead320_head[, educ_test := (as.integer(relate_update == 1) * educ) +
                        (as.integer(relate_update == 3) * EDUC_CHILD) +  
                        (as.integer(relate_update == 4) * EDUC_RELATIVE) +
                        (as.integer(relate_update == 5) * EDUC_NONRELATIVE)]
    
    jointhead320_head[, c('EDUC_CHILD', 'EDUC_RELATIVE', 'EDUC_NONRELATIVE', 'educ'):= NULL]
    
  } else{
    jointhead320_head[, age_test := (as.integer(relate_update == 1) * age) +
                        (as.integer(relate_update == 3) * AGE_CHILD) + #let even op as integer. wordt dat niet 1, 2. werkt! 
                        (as.integer(relate_update == 4) * AGE_RELATIVE)]
    
    jointhead320_head[, c('AGE_CHILD', 'AGE_RELATIVE', 'age'):= NULL]
    
    
    jointhead320_head[, sex_test := (as.integer(relate_update == 1) * sex) +
                        (as.integer(relate_update == 3) * SEX_CHILD) +  
                        (as.integer(relate_update == 4) * SEX_RELATIVE)]
    
    jointhead320_head[, c('SEX_CHILD', 'SEX_RELATIVE', 'sex'):= NULL]
    
    jointhead320_head[, educ_test := (as.integer(relate_update == 1) * educ) +
                        (as.integer(relate_update == 3) * EDUC_CHILD) +  
                        (as.integer(relate_update == 4) * EDUC_RELATIVE)]
    
    jointhead320_head[, c('EDUC_CHILD', 'EDUC_RELATIVE', 'educ'):= NULL]
    
    
  }  
  
  
  setnames(jointhead320_head, c('age_test', 'sex_test', 'educ_test', 'relate_update', 'farming_number', 
                                'rural_number', 'INCOMEQQ', 'hhtype'), 
           c('AGECAT','GENDER', 'EDUCAT', 'RELATE', 'FARMING', 'RURAL', 'INCOME', 'HHTYPE'))
  
  #dit 
  to_remove = setdiff(colnames(jointhead320_head), c('AGECAT','GENDER', 'EDUCAT', 'RELATE', 
                                                     'FARMING', 'RURAL', 'INCOME', 'HHTYPE', 'HID', 'HHSIZECAT'))
  
  jointhead320_head[, c(to_remove):= NULL]
  
  gc()
  
  if (is.null(jointhead320_partner) == FALSE){
    
    setnames(jointhead320_partner, c('age', 'sex', 'educ', 'relate', 'farming_number', 
                                     'rural_number', 'INCOMEQQ', 'hhtype'), 
             c('AGECAT', 'GENDER', 'EDUCAT', 'RELATE', 'FARMING', 'RURAL', 'INCOME', 'HHTYPE'))
    
    
    jointhead320_partner[, c('Frequency', 'Truncated') := NULL]
    
    
    jointhead320_head <- rbindlist(list(jointhead320_head, jointhead320_partner), use.names=TRUE)
    
    #order op HID is wel mooi. 
    gc()
  }
  
  setkeyv(jointhead320_head, c('HID', 'RELATE'))
  
  return(jointhead320_head)
  
  
} #hier ook nonrel fixen






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





smalltable_beginlines_list = list("> print(xtabs(hpopwgt~nr_children, data = dfmerg_hh_child4)/sum(dfmerg_hh_child4$hpopwgt, na.rm = TRUE))", 
                                  "> print(xtabs(hpopwgt~nr_relative, data = dfmerg_hh_relative3)/sum(dfmerg_hh_relative3$hpopwgt, na.rm = TRUE))",
                                  "+ } #FORNONRELATIVESSMALLTABLE")

smalltable_endlines_list = list("> dfmerg_hh_relative3 <- dfmerg_hh[dfmerg_hh$nr_relative > 2,]", 
                                "> dfmerg_hh_nonrelative2 <- dfmerg_hh[dfmerg_hh$nr_nonrelative > 1,]",
                                "> dfmerg$CHILD_CAT <- 0")




##### availability farming 
txtpath <- paste0(liscode, "_compl_1reg.txt")
farm_presence <- farming_available(txtpath)
print(farm_presence)


#############################
######### THE ERROR #########





# And we need edu_marg for unique education categories: 

# Deze marginals eruit halen anders: 

marginals_beginlist = list('> print(ftable(incomemarg.table))', '> print(ftable(ruralmarg.table))', 
                           '> print(ftable(farmingmarg.table))', '> print(ftable(hhtypemarg.table))',
                           '> print(ftable(hhsizecatmarg.table))', '> print(ftable(ageindimarg.table))',
                           '> print(ftable(educindimarg.table))', '> print(ftable(sexindimarg.table))')
marginals_endlist = list('> print(ftable(ruralmarg.table))', '> print(ftable(farmingmarg.table))', 
                         '> print(ftable(hhtypemarg.table))', '> print(ftable(hhsizecatmarg.table))',
                         '> print(ftable(ageheadmarg.table))', '> print(ftable(educindimarg.table))',
                         '> print(ftable(sexindimarg.table))', '> # BIJNA EINDE')


edumarg <- text_to_df(txtpath, '> print(ftable(educindimarg.table))', '> print(ftable(sexindimarg.table))', F)
edumarg <- edumarg %>% rename(EDUCAT = education, GEOLEV1 = region_number)
edu_tib <- as_tibble(edumarg)
#edu_tib <- edu_tib[edu_tib$GEOLEV1 %ni% remove_regions ,]
edu_tib <- edu_tib %>% arrange(GEOLEV1)
edu_tib$GEOLEV1 <- as.integer(edu_tib$GEOLEV1)
edu_tib$EDUCAT <- as.integer(edu_tib$EDUCAT)








##########################################



DF_errors <- data.frame('Country' = liscode, 'Synth_sq_error' = 0, 'National_sq_error' = 0, 'Marginal_sq_error' = 0)


#txtpath <- paste0(liscode, "_complete.txt")
txtpath <- paste0(liscode, "_compl_1reg.txt")

regiondf <- region_numbers(txtpath)

if (liscodeletters == 'us'){
  regiondf <- regiondf[regiondf$region_number != 95,] #hawaii US out, no data. 
}


counter_liscode = 1



print(liscode)

txtpathbegin <- "C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\"



syn_pop <- data.table()
nosynth_regions <- c()



# read names region. 


children_table <- text_to_df_small_table(txtpath, smalltable_beginlines_list[1], smalltable_endlines_list[1])
relative_table <- text_to_df_small_table(txtpath, smalltable_beginlines_list[2], smalltable_endlines_list[2])
nonrelative_table <- text_to_df_small_table(txtpath, smalltable_beginlines_list[3], smalltable_endlines_list[3])

nonrelative_ind <- yes_no_nonrelatives(txtpath)










#single households 
jointhead100 <- text_to_df(txtpath, beginlines_list[[1]], endlines_list[[1]], T)
jointhead100 <- fill_zero_cells(jointhead100, farm_presence)
jointhead100 <- fill_zero_cells_hhtype(jointhead100, farm_presence, 100)

jointhead100 <- jointhead100[order(jointhead100$INCOMEQQ, jointhead100$rural_number, 
                                   jointhead100$farming_number, jointhead100$age_head,  jointhead100$sex_head, 
                                   jointhead100$educ_head),]





jointhead100 <- trs_frequency(jointhead100)

jointhead100 <- as.data.table(lapply(jointhead100, rep, jointhead100$Truncated))

setnames(jointhead100, c('age_head', 'sex_head', 'educ_head', 'farming_number', 
                         'rural_number', 'INCOMEQQ', 'hhtype'), c('AGECAT','GENDER', 'EDUCAT', 
                                                                  'FARMING', 'RURAL', 'INCOME', 'HHTYPE'))
jointhead100[, RELATE := 1]
jointhead100[, HHSIZECAT := 1] 

jointhead100[, c('Frequency', 'Truncated') := NULL]

gc()

samplesize = 1
if (samplesize != 1){
  sampleHH <- sample(1:nrow(jointhead100), round(nrow(jointhead100)*samplesize))
  jointhead100 <- jointhead100[sampleHH]
}

jointhead100[, HID := 1:.N]
#jointhead100[, WEIGHTS := 1]

gc()


l100 <- nrow(jointhead100)


groupbynames <- c("INCOME", "RURAL", "FARMING", "AGECAT", "GENDER", "EDUCAT")
jointhead100[,by=groupbynames,WEIGHTS:=.N][]
jointhead100small <- unique(jointhead100, by = groupbynames)

rm(jointhead100)
gc()

print('single households created')

educatshead <- unique(jointhead100small$EDUCAT)

###########################################
#couple households   
# in cz in the couples there are no educ 1 and 2 in educ_head. 

jointhead210 <- text_to_df(txtpath, beginlines_list[[2]], endlines_list[[2]], T)
jointhead210 <- fill_zero_cells(jointhead210, farm_presence)
jointhead210 <- fill_zero_cells_hhtype(jointhead210, farm_presence, 210)

jointhead210 <- jointhead210[order(jointhead210$INCOMEQQ, jointhead210$rural_number, 
                                   jointhead210$farming_number, jointhead210$age_head,  jointhead210$sex_head, 
                                   jointhead210$educ_head),]



indices_list = c(3,4,5)
samplesize = 1

jointhead210 <- add_partner(jointhead210, txtpath, l100)
gc()


setnames(jointhead210, c('age', 'sex', 'educ', 'farming_number', 
                         'rural_number', 'INCOMEQQ', 'hhtype', 'relate'), c('AGECAT','GENDER', 'EDUCAT', 
                                                                            'FARMING', 'RURAL', 'INCOME', 'HHTYPE', 'RELATE'))

jointhead210[, HHSIZECAT := 2]

jointhead210[, c('Frequency', 'Truncated') := NULL]

setkeyv(jointhead210, c('HID', 'RELATE'))
gc()

l210 <- nrow(jointhead210) + l100


#WEIGHTS
setkeyv(jointhead210, c("HID", "RELATE", "AGECAT", "EDUCAT", "GENDER"))

jointhead210[, pid:= 1:.N, by = HID]

jointhead210_headwide <- dcast(jointhead210, HID ~ pid, value.var = c("INCOME", "RURAL", "AGECAT", "EDUCAT", "GENDER", "RELATE", "FARMING"))

groupbynames <- names(jointhead210_headwide)
groupbynames <- groupbynames[2:length(groupbynames)]

jointhead210_headwide[,by=groupbynames,WEIGHTS:=.N][]
jointhead210_headwide <- unique(jointhead210_headwide, by = groupbynames)

jointhead210_headsmall <- jointhead210[jointhead210_headwide[,c("HID", "WEIGHTS")], on = .(HID), nomatch = NULL]

jointhead210_headsmall[, pid := NULL]

rm(jointhead210)
rm(jointhead210_headwide)
gc()

print('couple households created')
###########################################
#couple households with children


jointhead220 <- text_to_df(txtpath, beginlines_list[[6]], endlines_list[[6]], T)
jointhead220 <- fill_zero_cells(jointhead220, farm_presence)
jointhead220 <- fill_zero_cells_hhtype(jointhead220, farm_presence, 220)
joint_hhsize1 <- text_to_df(txtpath, beginlines_list[[7]], endlines_list[[7]], T)
joint_hhsize1 <- fill_zero_cells(joint_hhsize1, farm_presence)

#df1 <- jointhead220
#df2 <- joint_hhsize1


jointhead220 <- new_frequencies(jointhead220, joint_hhsize1)
gc()

indices_list = c(8,9,10)
jointhead220 <- add_partner(jointhead220, txtpath, l210)
gc()

jointhead220 <- jointhead220[order(jointhead220$HID),]



jointhead220_head <- jointhead220[relate == 1]
jointhead220_part <- jointhead220[relate == 2]

l220 <- nrow(jointhead220_head) + l210


joint_hhsize2 <- text_to_df(txtpath, beginlines_list[[11]], endlines_list[[11]], T)

#df <- jointhead220_head

jointhead220_head <- add_cat_hhsize2(jointhead220_head, joint_hhsize2)
gc()

#jointhead_head <- jointhead220_head

#add children characteristics
# kijk hoe ik een functie in een functie kan aanroepen. 




jointhead220_head <- add_children_characteristics(jointhead220_head, children_table, txtpath, beginlines_list, endlines_list, add_age_child_rel_nonrel, add_sex_child_rel_nonrel, add_educ_child_rel_nonrel)


#finalize. rename everything.
jointhead220_head <- finalize_hhtype220_230(jointhead220_head, jointhead220_part)



rm(jointhead220)
rm(jointhead220_part)

#WEIGHTS
setkeyv(jointhead220_head, c("HID", "RELATE", "AGECAT", "EDUCAT", "GENDER"))

jointhead220_head[, pid:= 1:.N, by = HID]

jointhead220_headwide <- dcast(jointhead220_head, HID ~ pid, value.var = c("INCOME", "RURAL", "AGECAT", "EDUCAT", "GENDER", "RELATE", "FARMING"))

groupbynames <- names(jointhead220_headwide)
groupbynames <- groupbynames[2:length(groupbynames)]

jointhead220_headwide[,by=groupbynames,WEIGHTS:=.N][]
jointhead220_headwide <- unique(jointhead220_headwide, by = groupbynames)

jointhead220_headsmall <- jointhead220_head[jointhead220_headwide[,c("HID", "WEIGHTS")], on = .(HID), nomatch = NULL]

jointhead220_headsmall[, pid := NULL]

rm(jointhead220_head)
rm(jointhead220_headwide)
gc()

print('couple with children created')
###########################################
# one parent with children
jointhead230 <- text_to_df(txtpath, beginlines_list[[12]], endlines_list[[12]], T)
jointhead230 <- fill_zero_cells(jointhead230, farm_presence)


joint_hhsize1 <- text_to_df(txtpath, beginlines_list[[13]], endlines_list[[13]], T)
joint_hhsize1 <- fill_zero_cells(joint_hhsize1, farm_presence)


jointhead230 <- new_frequencies(jointhead230, joint_hhsize1)
gc()

jointhead230 <- jointhead230[! jointhead230$Frequency == 0,] 
jointhead230 <- trs_frequency(jointhead230)
jointhead230 <- as.data.table(lapply(jointhead230, rep, jointhead230$Truncated))
samplesize = 1
if (samplesize != 1){
  sampleHH <- sample(1:nrow(jointhead230), round(nrow(jointhead230)*samplesize))
  jointhead230 <- jointhead230[sampleHH]
}



jointhead230[,HID:=(1+l220):(.N+l220)] #each household gets an HH ID. 

l230 <- nrow(jointhead230) + l220

setnames(jointhead230, c('age_head', 'educ_head', 'sex_head'), c('age', 'educ', 'sex'))
joint_hhsize2 <- text_to_df(txtpath, beginlines_list[[14]], endlines_list[[14]], T)

jointhead230_head <- add_cat_hhsize2(jointhead230, joint_hhsize2)
gc()

#add children characteristics
jointhead230_head <- add_children_characteristics(jointhead230_head, children_table, txtpath, beginlines_list, endlines_list, add_age_child_rel_nonrel, add_sex_child_rel_nonrel, add_educ_child_rel_nonrel)


#finalize. rename everything.
jointhead230_partner = NULL
jointhead230_head <- finalize_hhtype220_230(jointhead230_head, jointhead230_partner)

rm(jointhead230)

#WEIGHTS
setkeyv(jointhead230_head, c("HID", "RELATE", "AGECAT", "EDUCAT", "GENDER"))

jointhead230_head[, pid:= 1:.N, by = HID]

jointhead230_headwide <- dcast(jointhead230_head, HID ~ pid, value.var = c("INCOME", "RURAL", "AGECAT", "EDUCAT", "GENDER", "RELATE", "FARMING"))

groupbynames <- names(jointhead230_headwide)
groupbynames <- groupbynames[2:length(groupbynames)]

jointhead230_headwide[,by=groupbynames,WEIGHTS:=.N][]
jointhead230_headwide <- unique(jointhead230_headwide, by = groupbynames)

jointhead230_headsmall <- jointhead230_head[jointhead230_headwide[,c("HID", "WEIGHTS")], on = .(HID), nomatch = NULL]

jointhead230_headsmall[, pid := NULL]

rm(jointhead230_head)
rm(jointhead230_headwide)
gc()

print('one parent with children created')

###########################################
# couple with relatives and nonrelatives
jointhead310 <- text_to_df(txtpath, beginlines_list[[15]], endlines_list[[15]], T)
jointhead310 <- fill_zero_cells(jointhead310, farm_presence)
joint_hhsize1 <- text_to_df(txtpath, beginlines_list[[16]], endlines_list[[16]], T)
joint_hhsize1 <- fill_zero_cells(joint_hhsize1, farm_presence)
jointhead310 <- new_frequencies(jointhead310, joint_hhsize1)
gc()


jointhead310 <- add_partner(jointhead310, txtpath, l230)
gc()

jointhead310_head <- jointhead310[relate == 1]
jointhead310_part <- jointhead310[relate == 2]

l310 <- nrow(jointhead310_head) + l230

joint_hhsize2 <- text_to_df(txtpath, beginlines_list[[20]], endlines_list[[20]], T)

jointhead310_head <- add_cat_hhsize2(jointhead310_head, joint_hhsize2)
gc()


#add relatives characteristics
jointhead310_head <- add_relatives_nonrelatives_characteristics(jointhead310_head, relative_table, nonrelative_table, nonrelative_ind, txtpath, beginlines_list, endlines_list, add_age_child_rel_nonrel, add_sex_child_rel_nonrel, add_educ_child_rel_nonrel)


#finalize. rename everything.
jointhead310_head <- finalize_hhtype310_400(jointhead310_head, jointhead310_part, nonrelative_ind)

rm(jointhead310)
rm(jointhead310_part)

#WEIGHTS
setkeyv(jointhead310_head, c("HID", "RELATE", "AGECAT", "EDUCAT", "GENDER"))

jointhead310_head[, pid:= 1:.N, by = HID]


jointhead310_headwide <- dcast(jointhead310_head, HID ~ pid, value.var = c("INCOME", "RURAL", "AGECAT", "EDUCAT", "GENDER", "RELATE", "FARMING"))

groupbynames <- names(jointhead310_headwide)
groupbynames <- groupbynames[2:length(groupbynames)]

jointhead310_headwide[,by=groupbynames,WEIGHTS:=.N][]
jointhead310_headwide <- unique(jointhead310_headwide, by = groupbynames)

jointhead310_headsmall <- jointhead310_head[jointhead310_headwide[,c("HID", "WEIGHTS")], on = .(HID), nomatch = NULL]

jointhead310_headsmall[, pid := NULL]

rm(jointhead310_head)
rm(jointhead310_headwide)
gc()

print('couple with relatives and nonrelatives created')
###########################################
# couple with children, relatives and nonrelatives
samplesize = 1
jointhead320 <- text_to_df(txtpath, beginlines_list[[21]], endlines_list[[21]], T)
jointhead320 <- fill_zero_cells(jointhead320, farm_presence)
joint_hhsize1 <- text_to_df(txtpath, beginlines_list[[22]], endlines_list[[22]], T)
joint_hhsize1 <- fill_zero_cells(joint_hhsize1, farm_presence)
jointhead320 <- new_frequencies(jointhead320, joint_hhsize1)
gc()

indices_list = c(23,24,25)
jointhead320 <- add_partner(jointhead320, txtpath, l310)
gc()

jointhead320_head <- jointhead320[relate == 1]
jointhead320_part <- jointhead320[relate == 2]

l320 <- nrow(jointhead320_head) + l310

joint_hhsize2 <- text_to_df(txtpath, beginlines_list[[26]], endlines_list[[26]], T)

jointhead320_head <- add_cat_hhsize2(jointhead320_head, joint_hhsize2)
gc()

#add children relatives characteristics

jointhead320_head <- add_345_characteristics(jointhead320_head, children_table, relative_table, nonrelative_table, nonrelative_ind, txtpath, beginlines_list, endlines_list, add_age_child_rel_nonrel, add_sex_child_rel_nonrel, add_educ_child_rel_nonrel)


#finalize. rename everything.
jointhead320_head <- finalize_hhtype320_330(jointhead320_head, jointhead320_part, nonrelative_ind)

rm(jointhead320)
rm(jointhead320_part)
gc()

#WEIGHTS
setkeyv(jointhead320_head, c("HID", "RELATE", "AGECAT", "EDUCAT", "GENDER"))

jointhead320_head[, pid:= 1:.N, by = HID]

jointhead320_headwide <- dcast(jointhead320_head, HID ~ pid, value.var = c("INCOME", "RURAL", "AGECAT", "EDUCAT", "GENDER", "RELATE", "FARMING"))

groupbynames <- names(jointhead320_headwide)
groupbynames <- groupbynames[2:length(groupbynames)]

jointhead320_headwide[,by=groupbynames,WEIGHTS:=.N][]
jointhead320_headwide <- unique(jointhead320_headwide, by = groupbynames)

jointhead320_headsmall <- jointhead320_head[jointhead320_headwide[,c("HID", "WEIGHTS")], on = .(HID), nomatch = NULL]

jointhead320_headsmall[, pid := NULL]

rm(jointhead320_head)
rm(jointhead320_headwide)
gc()

print('couple with children, relatives and nonrelatives created')
###########################################
# one parent with children, relatives and nonrelatives

jointhead330 <- text_to_df(txtpath, beginlines_list[[27]], endlines_list[[27]], T)
jointhead330 <- fill_zero_cells(jointhead330, farm_presence)
joint_hhsize1 <- text_to_df(txtpath, beginlines_list[[28]], endlines_list[[28]], T)
joint_hhsize1 <- fill_zero_cells(joint_hhsize1, farm_presence)
jointhead330 <- new_frequencies(jointhead330, joint_hhsize1)
gc()

jointhead330 <- jointhead330[! jointhead330$Frequency == 0,] 
jointhead330 <- trs_frequency(jointhead330)
jointhead330 <- as.data.table(lapply(jointhead330, rep, jointhead330$Truncated))
samplesize = 1
if (samplesize != 1){
  sampleHH <- sample(1:nrow(jointhead330), round(nrow(jointhead330)*samplesize))
  jointhead330 <- jointhead330[sampleHH]
}

jointhead330[, HID := (l320+1):(.N + l320)] #each household gets an HH ID. 

l330 <- nrow(jointhead330) + l320

setnames(jointhead330, c('age_head', 'educ_head', 'sex_head'), c('age', 'educ', 'sex'))

joint_hhsize2 <- text_to_df(txtpath, beginlines_list[[29]], endlines_list[[29]], T)

jointhead330_head <- add_cat_hhsize2(jointhead330, joint_hhsize2)
gc()


#add children, relatives and nonrelatives characteristics
jointhead330_head <- add_345_characteristics(jointhead330_head, children_table, relative_table, nonrelative_table, nonrelative_ind, txtpath, beginlines_list, endlines_list, add_age_child_rel_nonrel, add_sex_child_rel_nonrel, add_educ_child_rel_nonrel)


#finalize. rename everything.
jointhead330_partner = NULL
jointhead330_head <- finalize_hhtype320_330(jointhead330_head, jointhead330_partner, nonrelative_ind)

rm(jointhead330)

#WEIGHTS
setkeyv(jointhead330_head, c("HID", "RELATE", "AGECAT", "EDUCAT", "GENDER"))

jointhead330_head[, pid:= 1:.N, by = HID]

jointhead330_headwide <- dcast(jointhead330_head, HID ~ pid, value.var = c("INCOME", "RURAL", "AGECAT", "EDUCAT", "GENDER", "RELATE", "FARMING"))

groupbynames <- names(jointhead330_headwide)
groupbynames <- groupbynames[2:length(groupbynames)]

jointhead330_headwide[,by=groupbynames,WEIGHTS:=.N][]
jointhead330_headwide <- unique(jointhead330_headwide, by = groupbynames)

jointhead330_headsmall <- jointhead330_head[jointhead330_headwide[,c("HID", "WEIGHTS")], on = .(HID), nomatch = NULL]

jointhead330_headsmall[, pid := NULL]

rm(jointhead330_head)
rm(jointhead330_headwide)
gc()

print('one parent with children, relatives and nonrelatives created')
###########################################
# 400: head with relatives and nonrelatives

jointhead400 <- text_to_df(txtpath, beginlines_list[[30]], endlines_list[[30]], T)
jointhead400 <- fill_zero_cells(jointhead400, farm_presence)
joint_hhsize1 <- text_to_df(txtpath, beginlines_list[[31]], endlines_list[[31]], T)
joint_hhsize1 <- fill_zero_cells(joint_hhsize1, farm_presence)
jointhead400 <- new_frequencies(jointhead400, joint_hhsize1)
gc()

jointhead400 <- jointhead400[! jointhead400$Frequency == 0,] 
jointhead400 <- trs_frequency(jointhead400)
jointhead400 <- as.data.table(lapply(jointhead400, rep, jointhead400$Truncated))
samplesize = 1
if (samplesize != 1){
  sampleHH <- sample(1:nrow(jointhead400), round(nrow(jointhead400)*samplesize))
  jointhead400 <- jointhead400[sampleHH]
}
jointhead400[, HID := (l330+1):(.N + l330)] #each household gets an HH ID. 

l400 <- nrow(jointhead400) + l330

setnames(jointhead400, c('age_head', 'educ_head', 'sex_head'), c('age', 'educ', 'sex'))

joint_hhsize2 <- text_to_df(txtpath, beginlines_list[[32]], endlines_list[[32]], T)

jointhead400_head <- add_cat_hhsize2(jointhead400, joint_hhsize2)
gc()

#add relatives, nonrelatives characteristics
jointhead400_head <- add_relatives_nonrelatives_characteristics(jointhead400_head, relative_table, nonrelative_table, nonrelative_ind, txtpath, beginlines_list, endlines_list, add_age_child_rel_nonrel, add_sex_child_rel_nonrel, add_educ_child_rel_nonrel)


#finalize. rename everything.
jointhead400_partner = NULL
jointhead400_head <- finalize_hhtype310_400(jointhead400_head, jointhead400_partner, nonrelative_ind)

rm(jointhead400)

#WEIGHTS
setkeyv(jointhead400_head, c("HID", "RELATE", "AGECAT", "EDUCAT", "GENDER"))

jointhead400_head[, pid:= 1:.N, by = HID]

jointhead400_headwide <- dcast(jointhead400_head, HID ~ pid, value.var = c("INCOME", "RURAL", "AGECAT", "EDUCAT", "GENDER", "RELATE", "FARMING"))

groupbynames <- names(jointhead400_headwide)
groupbynames <- groupbynames[2:length(groupbynames)]

jointhead400_headwide[,by=groupbynames,WEIGHTS:=.N][]
jointhead400_headwide <- unique(jointhead400_headwide, by = groupbynames)

jointhead400_headsmall <- jointhead400_head[jointhead400_headwide[,c("HID", "WEIGHTS")], on = .(HID), nomatch = NULL]

jointhead400_headsmall[, pid := NULL]

rm(jointhead400_head)
rm(jointhead400_headwide)
gc()

print('all household created, not binded')


## BIND EVERYTHING

jointhead100small <- rbindlist(list(jointhead100small, jointhead210_headsmall), use.names=TRUE)
gc()
jointhead100small <- rbindlist(list(jointhead100small, jointhead220_headsmall), use.names=TRUE)
gc()
jointhead100small <- rbindlist(list(jointhead100small, jointhead230_headsmall), use.names=TRUE)
gc()
jointhead100small <- rbindlist(list(jointhead100small, jointhead310_headsmall), use.names=TRUE)
gc()
jointhead100small <- rbindlist(list(jointhead100small, jointhead320_headsmall), use.names=TRUE)
gc()
jointhead100small <- rbindlist(list(jointhead100small, jointhead330_headsmall), use.names=TRUE)
gc()
jointhead100small <- rbindlist(list(jointhead100small, jointhead400_headsmall), use.names=TRUE)
gc()

print('all household created and binded')

# LIS data opslaan als binary file. 
jointhead100small[, HID:=as.integer(HID)]
jointhead100small[, HHSIZECAT := as.integer(HHSIZECAT)]
jointhead100small[, RURAL := as.integer(RURAL)]
jointhead100small[, HHTYPE := as.integer(as.factor(HHTYPE))]


jointhead100small[, GENDER := as.integer(GENDER)]
jointhead100small[, AGECAT := as.integer(AGECAT)]
jointhead100small[, EDUCAT := as.integer(EDUCAT)]
jointhead100small[, INCOME := as.integer(INCOME)]
jointhead100small[, FARMING := as.integer(FARMING)]
jointhead100small[, RELATE := as.integer(RELATE)]

columnnames <- colnames(jointhead100small)

#jointhead100[, PID := NULL]

#sampleHH <- sample(1:max(jointhead100$HID), round(max(jointhead100$HID)/2))

#jointhead50 <- jointhead100[HID %in% sampleHH]


filenamelisdata <- paste0(liscode, "_LISdata_may23.dat")

con = file(filenamelisdata, "wb")

writeBin(c(jointhead100small$INCOME, jointhead100small$RURAL, jointhead100small$FARMING, jointhead100small$AGECAT, jointhead100small$GENDER, 
           jointhead100small$EDUCAT, jointhead100small$HHTYPE, jointhead100small$HID, jointhead100small$RELATE, jointhead100small$HHSIZECAT, 
           jointhead100small$WEIGHTS), con)

#writeBin(c(syn_pop$INCOME, syn_pop$PID), con)


close(con)

print('saved LIS data')

filename_length_LIS = paste0('length_LIS_survey_may23', liscode, '.csv')

write.csv(nrow(jointhead100small), filename_length_LIS, row.names = FALSE)

########################################################################################################
################ SYNTHETIC POPULATION #############################

#smaller_sampleHH <- sample(1:max(jointhead100$HID), round(max(jointhead100$HID)/10))

#jointhead10 <- jointhead100[HID %in% smaller_sampleHH]



countryname = liscodecountries[liscodecountries$iso_code == liscode,]$Country




GDL_pop <- read.csv('GDL_subnational_population_world.csv', head = TRUE, sep = ',')

GDL_pop_country <- GDL_pop[GDL_pop$country == countryname,]




#hhsize
hhsize_marg_est <- read.csv('Est_marginals_HHSIZECAT_sept23.csv', sep = ',', header = TRUE)
hhsize_marg_est <- left_join(hhsize_marg_est, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
hhsize_marg_est <- hhsize_marg_est[hhsize_marg_est$country == countryname,]
hhsize_marg_est <- hhsize_marg_est %>% select(-country)

hhsize_marg_est <- pivot_longer(hhsize_marg_est, cols = c(X1est, X2est, X3est, X4est, X5est, X6est), names_to = "HHSIZECAT", values_to = "Frequency")
hhsize_marg_est$HHSIZECAT <- as.integer(as.factor(hhsize_marg_est$HHSIZECAT))

hhsize_marg_est_tib <- as_tibble(hhsize_marg_est)


#hhtype
hhtype_marg_est <- read.csv('Est_marginals_HHTYPE_sept23.csv', sep = ',', header = TRUE)
hhtype_marg_est <- left_join(hhtype_marg_est, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
hhtype_marg_est <- hhtype_marg_est[hhtype_marg_est$country == countryname,]
hhtype_marg_est <- hhtype_marg_est %>% select(-country)

hhtype_marg_est <- pivot_longer(hhtype_marg_est, cols = c(X1est, X2est, X3est, X4est, X5est, X6est, X7est, X8est), names_to = "HHTYPE", values_to = "Frequency")
hhtype_marg_est$HHTYPE <- as.integer(as.factor(hhtype_marg_est$HHTYPE))

hhtype_marg_est_tib <- as_tibble(hhtype_marg_est)


# age
age_marg_est <- read.csv('Est_marginals_AGECAT_sept23.csv', sep = ',', header = TRUE)
age_marg_est <- left_join(age_marg_est, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
age_marg_est <- age_marg_est[age_marg_est$country == countryname,]
age_marg_est <- age_marg_est %>% select(-country)

age_marg_est <- pivot_longer(age_marg_est, cols = c(X1est, X2est, X3est, X4est, X5est, X6est, X7est), names_to = "AGECAT", values_to = "Frequency")
age_marg_est$AGECAT <- as.integer(as.factor(age_marg_est$AGECAT))


age_marg_est_tib <- as_tibble(age_marg_est)


# gender
gender_marg_est <- read.csv('Est_marginals_GENDER_sept23.csv', sep = ',', header = TRUE)
gender_marg_est <- left_join(gender_marg_est, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
gender_marg_est <- gender_marg_est[gender_marg_est$country == countryname,]
gender_marg_est <- gender_marg_est %>% select(-country)

gender_marg_est <- pivot_longer(gender_marg_est, cols = c(X1est, X2est), names_to = "GENDER", values_to = "Frequency")
gender_marg_est$GENDER <- as.integer(as.factor(gender_marg_est$GENDER))
gender_marg_est$GENDER <- gender_marg_est$GENDER -1

gender_marg_est_tib <- as_tibble(gender_marg_est)


# educat
educat_marg_est <- read.csv('Est_marginals_EDUCAT_sept23.csv', sep = ',', header = TRUE)
educat_marg_est <- left_join(educat_marg_est, GDL_pop2015 %>% select(country, GDLCODE), by = 'GDLCODE')
educat_marg_est <- educat_marg_est[educat_marg_est$country == countryname,]
educat_marg_est <- educat_marg_est %>% select(-country)

educat_marg_est <- pivot_longer(educat_marg_est, cols = c(X1est, X2est, X3est, X4est, X5est), names_to = "EDUCAT", values_to = "Frequency")
educat_marg_est$EDUCAT <- as.integer(as.factor(educat_marg_est$EDUCAT))

educat_marg_est_tib <- as_tibble(educat_marg_est)







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
####### 22 aUG '23: liscode GEOLEV1 fixen als we geen lis landen hebben. 
####### Dan hoeven we lis niet te matchen. Dus er zijn geen regionumbers, maar GDL codes. 




rural_tib <- as_tibble(ruralmargSMOD)
#rural_tib <- rural_tib[rural_tib$GEOLEV1 %ni% remove_regions ,]


print('ruraltib')
print(unique(rural_tib$GDLCODE))


regionnumbers <- unique(rural_tib$GDLCODE) #not integers, otherwise we can't match them with GDL.  


jointhead100small[, PID := 1:.N] #was jointhead10. 

nr_individuals_per_regio <- c(-99)

error_region <- c()



##########################################################
########## 22-8-23: MAAK CONROL AND REF SAMPLE GELIJK. 


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


# INCOME: zelfde manier als DHS doen. 


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


ml_fitWEIGHTS <- jointhead100small$WEIGHTS    #numeric(nrow(jointhead100small)) + 1

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


nr_individuals_per_regio = -99

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
  
  #variation_regions <- append(variation_regions, nrow(fit))
  
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

print('incfreq1')
print(INCOMEfreq_regio)

INCOMEfreq_regio['Frequency'] <- INCOMEfreq_regio$Freq*INCOMEfreq_regio$mp
print('incfreq2')
print(INCOMEfreq_regio)
INCOMEfreq_regio <- INCOMEfreq_regio %>% select(-Freq)
print('incfreq3')
print(INCOMEfreq_regio)
INCOMEfreq_regio <- INCOMEfreq_regio %>% select(GDLCODE, INCOME, Frequency)
print('incfreq4')
print(INCOMEfreq_regio)
INCOME_tib <- as_tibble(INCOMEfreq_regio)
print('income_tib')
print(INCOME_tib)



INCOME_tib <- as_tibble(INCOMEfreq_regio %>% select(GDLCODE, INCOME, Frequency))
print('income_tib')
print(INCOME_tib)

# for loop with regions. Now with INCOME marginal.  
nr_individuals_per_regio = c()
variation_regions = c()

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
  
  variation_regions <- append(variation_regions, length(fitweights))
  
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
  
  name = paste0(countryname, '_knownsurvey_LIS_oct23_synthpop_', as.character(regnr), '.dat')
  
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





name_individualfile <- paste0('length_knownsurvey_LIS_', countryname, '.csv') 
DF_nr_individuals_per_region <- data.frame('Country' = rep(countryname, length(GEOLEV1regions)), 'GDLCODE' = GEOLEV1regions, 'Nr_individuals' = nr_individuals_per_regio)

DF_variation_popsizes <- data.frame('Country' = rep(countryname, length(GEOLEV1regions)), 'GDLCODE' = GEOLEV1regions, 'Variation_popsize' = variation_regions)

write.csv(DF_nr_individuals_per_region, name_individualfile, row.names = FALSE)




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


