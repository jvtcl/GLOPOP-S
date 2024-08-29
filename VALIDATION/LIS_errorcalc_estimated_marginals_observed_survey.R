#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)


isocode = as.character(args[1])

uselisdata = 'yes'


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


#isocode = 'cz13'


#setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\LISapril2024')


liscodecountries <- read.csv('LIS_codes_countries2.csv', sep = ';')
liscode <- liscodecountries[liscodecountries$iso_code == isocode,]$LISCODE


liscodeletters <- unlist(strsplit(liscode, ""))
liscodeletters <- paste0(liscodeletters[1], liscodeletters[2])

print(liscodeletters)



GDL_pop <- read.csv('GDL_match_population_all_LIS.csv', sep = ';', header = TRUE)

GDL_pop <- na.omit(GDL_pop)



# FUNCTIONS
region_names <- function(txtpath){
  x <- readLines(txtpath)
  
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
  
  
  text <- text[- 1] #eerste rij eraf. print statement
  text <- text[- length(text)]  #laatste rij eraf. print statement.
  
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
  # Indicates if non-relatives are present in the LIS survey 
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
  # this function indicates if the variable farming was available
  # if farming is not available, farming was generated as a random 0/1 variable
  # in the LIS server. 
  
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
  # This function reads a joint distribution table from a LIS txt file
  # and gives a joint distribution table as output
  
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
  # transforms non integer weights to integer weights
  # using 'truncate, replicate, sample' algorithm of Lovelace(2013). 
  
  
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
  
  df$Truncated <- truncated
  
  return(df)  
  
} 



new_frequencies <- function(df1, df2){
  
  # We use this function to merge household size to the joint distribution of  
  # income, rural/urban, household type, education of head, age of head, and gender of head. 
  # We use new_frequencies_may23 to add the partner's variables (age, gender, education) 
  # to the joint distribution. 
  
  variables_df1 <- colnames(df1)
  variables_df2 <- colnames(df2)
  
  var = setdiff(variables_df2, variables_df1)
  
  if (length(var) != 1){
    print('Only 1 variable can be added')
  }
  
  
  
  # we groupby age_head and age_part so that the partner's age only depends on the head's age
  if ("age_part" %in% colnames(df2) & "sex_part" %ni% colnames(df2)){
    df2 <- df2 %>% group_by(age_head, age_part, hhtype) %>% summarise(Frequency = sum(Frequency))
  }
  
  
  # gender of partner (sex_part) only depends on gender of head.  
  if ("sex_part" %in% colnames(df2) & "educ_part" %ni% colnames(df2)){
    df2 <- df2 %>% group_by(age_part, sex_head, sex_part, hhtype) %>% summarise(Frequency = sum(Frequency))
  }
  
  
  # Education of partner only depends on educ_head.  
  if ("educ_part" %in% colnames(df2)){
    df2 <- df2 %>% group_by(educ_head, educ_part, hhtype) %>% summarise(Frequency = sum(Frequency))
  }
  
  colnames(df2)[which(names(df2) == var)] <- "var"
  
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
  
  colnames(df1_mergetest)[which(names(df1_mergetest) == "var")] <- var #back to old names
  
  df1_mergetest <- rename(df1_mergetest, Frequency = New_weighted_frequency)
  
  return(df1_mergetest)
  
}


new_frequencies_updatemay23 <- function(df1, df2, var){
  
  # with this function we merge the partner's variables to the joint distribution
  # var is age_part, sex_part, or educ_part. 
  # df2 comes from new partner joints. 
  # Zeros in df2 are already imputed. 
  
  
  colnames(df2)[which(names(df2) == var)] <- "var"
  
  
  
  
  
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
  # With this function we fill zeros in the joint distributions
  
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
  
  
  # Here we impute the joint distribution of the head's gender and the partner's gender
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
      jh4_6s <- jh4_6[jh4_6[comb4[1,6]] == rowjh[comb4[1,6]][[1]] & jh4_6[comb4[2,6]] == rowjh[comb4[2,6]][[1]] & jh4_6[comb4[3,6]] == rowjh[comb4[3,6]][[1]] & jh4_6[comb4[4,6]] == rowjh[comb4[4,6]][[1]] & jh4_6[comb4[5,6]] == rowjh[comb4[5,6]][[1]],]
      
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
  # We again fill zeros in the joint distribution. 
  # All joint distribition tables are created per household type
  # Here we factorize the household type from the joint distribution so to say
  # and fill gaps. 
  
  
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
txtpath <- paste0(liscode, "_compl_update.txt")
farm_presence <- farming_available(txtpath)

# set to 0 for error calculation
farm_presence <- 0


#############################
######### THE ERROR #########

# txtpathfreq file contains the frequencies of each pair of characteristics per region and at a national level

txtpathfreq <- paste0(liscode, "_freq_update.txt")


beginlist_freq = list('> print((data.frame(xtabs(hpopwgt ~ farming_number + HHTYPE_CAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                      '> print((data.frame(xtabs(hpopwgt ~ farming_number + HHSIZECAT + region_number, data = dfhead))$Freq)/sum(dfhead$hpopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ farming_number + INCOMEQQ + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ farming_number + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ farming_number + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ farming_number + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + HHTYPE_CAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                      '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + HHSIZECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
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
                    '> print((data.frame(xtabs(ppopwgt ~ farming_number + INCOMEQQ + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ farming_number + AGECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ farming_number + education + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ farming_number + sex_number + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + HHTYPE_CAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                    '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + HHSIZECAT + region_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
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
                           '> print((data.frame(xtabs(ppopwgt ~ farming_number + INCOMEQQ, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ farming_number + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ farming_number + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ farming_number + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + HHTYPE_CAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                           '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + HHSIZECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
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
                         '> print((data.frame(xtabs(ppopwgt ~ farming_number + INCOMEQQ, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ farming_number + AGECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ farming_number + education, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ farming_number + sex_number, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + HHTYPE_CAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
                         '> print((data.frame(xtabs(ppopwgt ~ INCOMEQQ + HHSIZECAT, data = dfmerg))$Freq)/sum(dfmerg$ppopwgt))',
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
  
  x <- readLines(txtpathfreq)
  begin_pos <- grep(beginline, x, fixed = TRUE)
  end_pos <- grep(endline, x, fixed = TRUE)
  
  text <- x[begin_pos:end_pos]
  
  
  text <- text[- 1]
  text <- text[- length(text)]  
  
  text2 = gsub("\\[.*?\\]", "", text) 
  text2 = unlist(strsplit(text2, split = " "))
  text2 = unlist(lapply(text2, function(z){ z[!is.na(z) & z != ""]})) 
  vector_freqtotal = as.numeric(text2)
  
  dffreqtotal <- data.frame('National' = vector_freqtotal)
  
  return(dffreqtotal)
}


if (farm_presence == 1){
  for (i in 1:length(beginlist_freqtotal)){
    if (i == 1){
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
      df1tot <- read_freqs_LIS(txtpathfreq, beginlist_freqtotal[i], endlist_freqtotal[i])
    } else {
      df2tot <- read_freqs_LIS(txtpathfreq, beginlist_freqtotal[i], endlist_freqtotal[i])
      df1tot <- rbind.data.frame(df1tot, df2tot)
      
    }
    
  }
  
}





# And we need edu_marg for unique education categories: 

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



DF_errors <- data.frame('iso_code' = isocode, 'Synth_sq_error' = 0, 'National_sq_error' = 0, 'Marginal_sq_error' = 0)


txtpath <- paste0(liscode, "_compl_update.txt")

regiondf <- region_numbers(txtpath)

if (liscodeletters == 'us'){
  regiondf <- regiondf[regiondf$region_number != 95,] #hawaii US out, there is no data. 
}


syn_pop <- data.table()
nosynth_regions <- c()



# probabilities of number of children, relatives and nonrelatives. 
children_table <- text_to_df_small_table(txtpath, smalltable_beginlines_list[1], smalltable_endlines_list[1])
relative_table <- text_to_df_small_table(txtpath, smalltable_beginlines_list[2], smalltable_endlines_list[2])
nonrelative_table <- text_to_df_small_table(txtpath, smalltable_beginlines_list[3], smalltable_endlines_list[3])

nonrelative_ind <- yes_no_nonrelatives(txtpath)



# If uselisdata == 'no', i.e. second argument as.character(args[2]) is 'no', then we build the households from the LIS textfiles. 
# If uselisdata == 'yes', then we skip this step and we use the isocode_LISdata_may23.dat that we created by the code under 
# uselisdata == 'no'. 

if (uselisdata == 'no'){
  
  #single households 
  jointhead100 <- text_to_df(txtpath, beginlines_list[[1]], endlines_list[[1]], T)
  jointhead100$Frequency <- jointhead100$Frequency
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
  
  
  jointhead100[, HID := 1:.N]
  
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
  
  jointhead210 <- text_to_df(txtpath, beginlines_list[[2]], endlines_list[[2]], T)
  jointhead210 <- fill_zero_cells(jointhead210, farm_presence)
  jointhead210 <- fill_zero_cells_hhtype(jointhead210, farm_presence, 210)
  
  jointhead210 <- jointhead210[order(jointhead210$INCOMEQQ, jointhead210$rural_number, 
                                     jointhead210$farming_number, jointhead210$age_head,  jointhead210$sex_head, 
                                     jointhead210$educ_head),]
  
  
  
  indices_list = c(3,4,5)
  
  
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
  
  jointhead220_head <- add_cat_hhsize2(jointhead220_head, joint_hhsize2)
  gc()
  
  
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
  
  
  jointhead230[,HID:=(1+l220):(.N+l220)] 
  
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
  
  if (farm_presence == 0){
    jointhead100small[, FARMING := -1]
    
  }
  
  columnnames <- colnames(jointhead100small)
  
  #jointhead100[, PID := NULL]
  
  #sampleHH <- sample(1:max(jointhead100$HID), round(max(jointhead100$HID)/2))
  
  #jointhead50 <- jointhead100[HID %in% sampleHH]
  
  
  filenamelisdata <- paste0(isocode, "_LISdata_may24.dat")
  
  con = file(filenamelisdata, "wb")
  
  writeBin(c(jointhead100small$INCOME, jointhead100small$RURAL, jointhead100small$FARMING, jointhead100small$AGECAT, jointhead100small$GENDER, 
             jointhead100small$EDUCAT, jointhead100small$HHTYPE, jointhead100small$HID, jointhead100small$RELATE, jointhead100small$HHSIZECAT, 
             jointhead100small$WEIGHTS), con)
  
  
  close(con)
  
  print('saved LIS data')
  
  filename_length_LIS = paste0('length_LIS_survey_', isocode, '.csv')
  
  write.table(nrow(jointhead100small), filename_length_LIS, row.names = FALSE, sep = ',')
  
} else if (uselisdata == 'yes'){
  
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
  
}




########################################################################################################
################ SYNTHETIC POPULATION #############################

# MARGINALS 
marginals_beginlist = list('> print(ftable(incomemarg.table))', '> print(ftable(ruralmarg.table))', 
                           '> print(ftable(farmingmarg.table))', '> print(ftable(hhtypemarg.table))',
                           '> print(ftable(hhsizecatmarg.table))', '> print(ftable(ageindimarg.table))',
                           '> print(ftable(educindimarg.table))', '> print(ftable(sexindimarg.table))')
marginals_endlist = list('> print(ftable(ruralmarg.table))', '> print(ftable(farmingmarg.table))', 
                         '> print(ftable(hhtypemarg.table))', '> print(ftable(hhsizecatmarg.table))',
                         '> print(ftable(ageheadmarg.table))', '> print(ftable(educindimarg.table))',
                         '> print(ftable(sexindimarg.table))', '> # BIJNA EINDE')


incomemarg <- text_to_df(txtpath, marginals_beginlist[[1]], marginals_endlist[[1]], F)
incomemarg <- incomemarg %>% rename(INCOME = INCOMEQQ, GEOLEV1 = region_number)
income_tib <- as_tibble(incomemarg)
income_tib <- income_tib %>% arrange(GEOLEV1)

initial_regionnumbers <- unique(incomemarg$GEOLEV1)
income_tib$GEOLEV1 <- as.integer(income_tib$GEOLEV1)
income_tib$INCOME <- as.integer(income_tib$INCOME)

ruralmarg <- text_to_df(txtpath, marginals_beginlist[[2]], marginals_endlist[[2]], F)
ruralmarg <- ruralmarg %>% rename(RURAL = rural_number, GEOLEV1 = region_number)
rural_tib <- as_tibble(ruralmarg)
rural_tib <- rural_tib %>% arrange(GEOLEV1)
rural_tib$GEOLEV1 <- as.integer(rural_tib$GEOLEV1)
rural_tib$RURAL <- as.integer(rural_tib$RURAL)

farmmarg <- text_to_df(txtpath, marginals_beginlist[[3]], marginals_endlist[[3]], F)
farmmarg <- farmmarg %>% rename(FARMING = farming_number, GEOLEV1 = region_number)
farm_tib <- as_tibble(farmmarg)
farm_tib <- farm_tib %>% arrange(GEOLEV1)
farm_tib$GEOLEV1 <- as.integer(farm_tib$GEOLEV1)
farm_tib$FARMING <- as.integer(farm_tib$FARMING)

hhtypemarg <- text_to_df(txtpath, marginals_beginlist[[4]], marginals_endlist[[4]], F)
hhtypemarg <- hhtypemarg %>% rename(HHTYPE = HHTYPE_CAT, GEOLEV1 = region_number)
hhtype_tib <- as_tibble(hhtypemarg)
hhtype_tib <- hhtype_tib %>% arrange(GEOLEV1)
hhtype_tib$GEOLEV1 <- as.integer(hhtype_tib$GEOLEV1)
hhtype_tib$HHTYPE <- as.integer(as.factor(hhtype_tib$HHTYPE))

hhsizemarg <- text_to_df(txtpath, marginals_beginlist[[5]], marginals_endlist[[5]], F)
hhsizemarg <- hhsizemarg %>% rename(GEOLEV1 = region_number)
hhsize_tib <- as_tibble(hhsizemarg)
hhsize_tib <- hhsize_tib %>% arrange(GEOLEV1)
hhsize_tib$GEOLEV1 <- as.integer(hhsize_tib$GEOLEV1)
hhsize_tib$HHSIZECAT <- as.integer(hhsize_tib$HHSIZECAT)

agemarg <- text_to_df(txtpath, marginals_beginlist[[6]], marginals_endlist[[6]], F)
agemarg <- agemarg %>% rename(GEOLEV1 = region_number)
age_tib <- as_tibble(agemarg)
age_tib <- age_tib %>% arrange(GEOLEV1)
age_tib$GEOLEV1 <- as.integer(age_tib$GEOLEV1)
age_tib$AGECAT <- as.integer(age_tib$AGECAT)

edumarg <- text_to_df(txtpath, marginals_beginlist[[7]], marginals_endlist[[7]], F)
edumarg <- edumarg %>% rename(EDUCAT = education, GEOLEV1 = region_number)
edu_tib <- as_tibble(edumarg)
edu_tib <- edu_tib %>% arrange(GEOLEV1)
edu_tib$GEOLEV1 <- as.integer(edu_tib$GEOLEV1)
edu_tib$EDUCAT <- as.integer(edu_tib$EDUCAT)

sexmarg <- text_to_df(txtpath, marginals_beginlist[[8]], marginals_endlist[[8]], F)
sexmarg <- sexmarg %>% rename(GENDER = sex_number, GEOLEV1 = region_number)
sex_tib <- as_tibble(sexmarg)
sex_tib <- sex_tib %>% arrange(GEOLEV1)
sex_tib$GEOLEV1 <- as.integer(sex_tib$GEOLEV1)
sex_tib$GENDER <- as.integer(sex_tib$GENDER)


# estimated marginals: 

# when there is a 0 in the education category, then we need to alter the estimated marginal accordingly. 

# income
est_marg_income <- read.csv('Est_marginals_INCOME_june24.csv')
est_marg_income <- est_marg_income[est_marg_income$ISOcode == isocode,]
est_marg_income <- left_join(est_marg_income, GDL_pop %>% select(GDLcode, region_number), by = 'GDLcode')

est_marg_income_gr <- est_marg_income %>% group_by(region_number) %>% summarize(X1est = sum(X1est), 
                                                                                X2est = sum(X2est), X3est = sum(X3est), X4est = sum(X4est), X5est = sum(X5est))

est_marg_income_gr <- est_marg_income_gr %>% 
  pivot_longer(
    cols = X1est:X5est,   
    names_to = "INCOME",    
    values_to = "Frequency"       
  )

est_marg_income_gr['INCOME'] <- as.integer(as.factor(est_marg_income_gr$INCOME))

est_marg_income_gr <- est_marg_income_gr %>% rename(GEOLEV1 = region_number)

print('est_marg_income_gr')
print(est_marg_income_gr)

# hhtype
est_marg_hhtype <- read.csv('Est_marginals_HHTYPE_june24.csv')
est_marg_hhtype <- est_marg_hhtype[est_marg_hhtype$ISOcode == isocode,]
est_marg_hhtype <- left_join(est_marg_hhtype, GDL_pop %>% select(GDLcode, region_number), by = 'GDLcode')

est_marg_hhtype_gr <- est_marg_hhtype %>% group_by(region_number) %>% summarize(X1est = sum(X1est), 
                                                                                X2est = sum(X2est), X3est = sum(X3est), X4est = sum(X4est), X5est = sum(X5est),
                                                                                X6est = sum(X6est), X7est = sum(X7est), X8est = sum(X8est))

est_marg_hhtype_gr <- est_marg_hhtype_gr %>% 
  pivot_longer(
    cols = X1est:X8est,   
    names_to = "HHTYPE",    
    values_to = "Frequency"       
  )

est_marg_hhtype_gr['HHTYPE'] <- as.integer(as.factor(est_marg_hhtype_gr$HHTYPE))

est_marg_hhtype_gr <- est_marg_hhtype_gr %>% rename(GEOLEV1 = region_number)

# hhsize
est_marg_hhsize <- read.csv('Est_marginals_HHSIZECAT_june24.csv')
est_marg_hhsize <- est_marg_hhsize[est_marg_hhsize$ISOcode == isocode,]
est_marg_hhsize <- left_join(est_marg_hhsize, GDL_pop %>% select(GDLcode, region_number), by = 'GDLcode')

est_marg_hhsize_gr <- est_marg_hhsize %>% group_by(region_number) %>% summarize(X1est = sum(X1est), 
                                                                                X2est = sum(X2est), X3est = sum(X3est), X4est = sum(X4est), X5est = sum(X5est), X6est = sum(X6est))

est_marg_hhsize_gr <- est_marg_hhsize_gr %>% 
  pivot_longer(
    cols = X1est:X6est,   
    names_to = "HHSIZECAT",    
    values_to = "Frequency"       
  )

est_marg_hhsize_gr['HHSIZECAT'] <- as.integer(as.factor(est_marg_hhsize_gr$HHSIZECAT))

est_marg_hhsize_gr <- est_marg_hhsize_gr %>% rename(GEOLEV1 = region_number)

LIS_hhsizecats <- unique(hhsize_tib$HHSIZECAT)

missing_hhsize <- setdiff(c(1,2,3,4,5,6), LIS_hhsizecats)

if (length(missing_hhsize) == 1){
  
  est_marg_hhsize_gr <- est_marg_hhsize_gr[est_marg_hhsize_gr$HHSIZECAT != missing_hhsize,]
  
  est_marg_hhsize_gr_pop <- est_marg_hhsize %>% group_by(region_number) %>% summarize(Population = sum(Population))
  est_marg_hhsize_gr_pop <- est_marg_hhsize_gr_pop %>% rename(GEOLEV1 = region_number)
  
  est_marg_hhsize_gr <- left_join(est_marg_hhsize_gr, est_marg_hhsize_gr_pop, by = 'GEOLEV1')                                                                
  
  est_marg_hhsize_poplis <- est_marg_hhsize_gr %>% group_by(GEOLEV1) %>% summarize(LIS_population = sum(Frequency))
  
  est_marg_hhsize_gr <- left_join(est_marg_hhsize_gr, est_marg_hhsize_poplis, by = 'GEOLEV1')                                                                
  
  est_marg_hhsize_gr$Frequency <- est_marg_hhsize_gr$Frequency * (est_marg_hhsize_gr$Population/est_marg_hhsize_gr$LIS_population)
  
  est_marg_hhsize_gr <- est_marg_hhsize_gr %>% select(-Population, -LIS_population)
  
}


# agecat
est_marg_age <- read.csv('Est_marginals_AGECAT_june24.csv')
est_marg_age <- est_marg_age[est_marg_age$ISOcode == isocode,]
est_marg_age <- left_join(est_marg_age, GDL_pop %>% select(GDLcode, region_number), by = 'GDLcode')

est_marg_age_gr <- est_marg_age %>% group_by(region_number) %>% summarize(X1est = sum(X1est), 
                                                                          X2est = sum(X2est), X3est = sum(X3est), X4est = sum(X4est), X5est = sum(X5est),
                                                                          X6est = sum(X6est), X7est = sum(X7est), X8est = sum(X8est))

est_marg_age_gr <- est_marg_age_gr %>% 
  pivot_longer(
    cols = X1est:X8est,   
    names_to = "AGECAT",    
    values_to = "Frequency"       
  )

est_marg_age_gr['AGECAT'] <- as.integer(as.factor(est_marg_age_gr$AGECAT))

est_marg_age_gr <- est_marg_age_gr %>% rename(GEOLEV1 = region_number)

# educat
est_marg_edu <- read.csv('Est_marginals_EDUCAT_june24.csv')
est_marg_edu <- est_marg_edu[est_marg_edu$ISOcode == isocode,]
est_marg_edu <- left_join(est_marg_edu, GDL_pop %>% select(GDLcode, region_number), by = 'GDLcode')

est_marg_edu_gr <- est_marg_edu %>% group_by(region_number) %>% summarize(X1est = sum(X1est), 
                                                                          X2est = sum(X2est), X3est = sum(X3est), X4est = sum(X4est), X5est = sum(X5est))

est_marg_edu_gr <- est_marg_edu_gr %>% 
  pivot_longer(
    cols = X1est:X5est,   
    names_to = "EDUCAT",    
    values_to = "Frequency"       
  )

est_marg_edu_gr['EDUCAT'] <- as.integer(as.factor(est_marg_edu_gr$EDUCAT))

est_marg_edu_gr <- est_marg_edu_gr %>% rename(GEOLEV1 = region_number)

LIS_educats <- unique(edu_tib$EDUCAT)

missing_edu <- setdiff(c(1,2,3,4,5), LIS_educats)

if (length(missing_edu) == 1){
  
  est_marg_edu_gr <- est_marg_edu_gr[est_marg_edu_gr$EDUCAT != missing_edu,]
  
  est_marg_edu_gr_pop <- est_marg_edu %>% group_by(region_number) %>% summarize(Population = sum(Population))
  est_marg_edu_gr_pop <- est_marg_edu_gr_pop %>% rename(GEOLEV1 = region_number)
  
  est_marg_edu_gr <- left_join(est_marg_edu_gr, est_marg_edu_gr_pop, by = 'GEOLEV1')                                                                
  
  est_marg_edu_poplis <- est_marg_edu_gr %>% group_by(GEOLEV1) %>% summarize(LIS_population = sum(Frequency))
  
  est_marg_edu_gr <- left_join(est_marg_edu_gr, est_marg_edu_poplis, by = 'GEOLEV1')                                                                
  
  est_marg_edu_gr$Frequency <- est_marg_edu_gr$Frequency * (est_marg_edu_gr$Population/est_marg_edu_gr$LIS_population)
  
  est_marg_edu_gr <- est_marg_edu_gr %>% select(-Population, -LIS_population)
  
}

if (length(missing_edu) == 2){
  
  print('two missing cats in edu!!')
  
}


# gender
est_marg_gender <- read.csv('Est_marginals_GENDER_june24.csv')
est_marg_gender <- est_marg_gender[est_marg_gender$ISOcode == isocode,]
est_marg_gender <- left_join(est_marg_gender, GDL_pop %>% select(GDLcode, region_number), by = 'GDLcode')

est_marg_gender_gr <- est_marg_gender %>% group_by(region_number) %>% summarize(X1est = sum(X1est), X2est = sum(X2est))

est_marg_gender_gr <- est_marg_gender_gr %>% 
  pivot_longer(
    cols = X1est:X2est,   
    names_to = "GENDER",    
    values_to = "Frequency"       
  )

est_marg_gender_gr['GENDER'] <- as.integer(as.factor(est_marg_gender_gr$GENDER))-1

est_marg_gender_gr <- est_marg_gender_gr %>% rename(GEOLEV1 = region_number)



# NEW RURAL MARGINAL
#SMODmarg <- read.csv('individual_marginals_UrbanRural.csv', sep = ';', header = TRUE)
SMODmarg <- read.csv('rural_urban_marginals_march24.csv', sep = ',', header = TRUE)
GDL_pop <- read.csv('GDL_match_population_all_LIS.csv', sep = ';', header = TRUE)
population <- read.csv('GHSpopulationGDLregions.csv', sep = ',', header = TRUE)

SMODmarg <- read.csv('GHSpopulationGDLregions.csv', sep = ',', header = TRUE)

GDL_pop <- left_join(GDL_pop, population %>% select(GDLcode, Population), by = 'GDLcode')


# we kunnen population gebruiken ipv SMODmarg. 

if (liscodeletters %in% c('gr', 'be', 'ie', 'cn', 'in', 'it', 'jp', 'au')){
  GDL_SMOD <- read.csv('fix_regions_griebe.csv', sep = ',', header = TRUE)
  GDL_SMOD['Rural_pop'] <- GDL_SMOD['Totalpop'] * GDL_SMOD['Rural_percent']
  GDL_SMOD['Urban_pop'] <- GDL_SMOD['Totalpop'] * GDL_SMOD['Urban_percent']
  
  sum_GDL_POP <- GDL_SMOD %>% group_by(liscountry, region_number) %>% summarise(Sum_rural_pop = sum(Rural_pop), Sum_urban_pop = sum(Urban_pop), Sum_year2015 = sum(Totalpop))
  
  sum_GDL_POP <- sum_GDL_POP[sum_GDL_POP$liscountry == liscodeletters,]
  sum_GDL_POP <- sum_GDL_POP %>% ungroup() %>% select(-liscountry)
  
  sum_GDL_POP <- sum_GDL_POP %>% rename('GEOLEV1' = 'region_number')
  
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
  
  
  
} else if (liscodeletters %ni% c('gr', 'be', 'ie', 'cn', 'in', 'it', 'jp', 'au')) {
  
  SMODmarg <- left_join(SMODmarg, GDL_pop %>% select(GDLcode, liscountry, region_number), by = 'GDLcode')
  
  SMODmarg <- SMODmarg[SMODmarg$liscountry == liscodeletters,]
  SMODmarg <- SMODmarg[!is.na(SMODmarg$liscountry),]
  
  #ruralmargSMOD <- ruralmarg
  
  #if (liscodeletters == 'us'){
  # for some reason delaware (51) is missing in rural marg LIS
  #  add_rural51 <- data.frame('GEOLEV1' = c(51, 51), 'RURAL' = c(0,1), 'Frequency' = c(0,0))
  #  ruralmargSMOD <- rbind(ruralmargSMOD, add_rural51)
  #}
  
  
  
  SMODmarg <- SMODmarg %>% rename('GEOLEV1' = 'region_number')
  
  
  ruralmargSMOD <- SMODmarg
  
  ruralmargSMOD0 <- ruralmargSMOD
  ruralmargSMOD1 <- ruralmargSMOD
  ruralmargSMOD0['RURAL'] <- 0
  ruralmargSMOD1['RURAL'] <- 1
  ruralmargSMOD0['Frequency'] <- ruralmargSMOD0$Urbanpop
  ruralmargSMOD1['Frequency'] <- ruralmargSMOD1$Ruralpop
  
  
  ruralmargSMOD <- rbind.data.frame(ruralmargSMOD0, ruralmargSMOD1)
  
  ruralmargSMOD <- ruralmargSMOD %>% select(GEOLEV1, RURAL, Frequency)
  
  ruralmargSMOD <- ruralmargSMOD[!is.na(ruralmargSMOD$GEOLEV1),]
  
  print(ruralmargSMOD)
  
  
  
  
  # kan mis gaan als er een regio mist. 
  
  
  
}



rural_tib <- as_tibble(ruralmargSMOD)
rural_tib <- rural_tib %>% arrange(GEOLEV1)
rural_tib$GEOLEV1 <- as.integer(rural_tib$GEOLEV1)
rural_tib$RURAL <- as.integer(rural_tib$RURAL)

print('ruraltib')
print(rural_tib)
print(unique(rural_tib$GEOLEV1))


# remove mismatch marginals (control) and survey data (reference)


survey_hhsize_cats <- unique(jointhead100small$HHSIZECAT)
marg_hhsize_cats <- unique(hhsize_tib$HHSIZECAT)

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
  OG_weights <- hhsize_tib %>% group_by(GEOLEV1) %>% summarise(OG_Frequency = sum(Frequency))
  hhsize_tib <- hhsize_tib[hhsize_tib$HHSIZECAT %ni% missingcat,]
  NEW_weights <- hhsize_tib %>% group_by(GEOLEV1) %>% summarise(NEW_Frequency = sum(Frequency))
  hhsize_tib <- left_join(hhsize_tib, OG_weights, by = 'GEOLEV1')
  hhsize_tib <- left_join(hhsize_tib, NEW_weights, by = 'GEOLEV1')
  hhsize_tib$correction_factor <- hhsize_tib$OG_Frequency / hhsize_tib$NEW_Frequency
  hhsize_tib$Frequency <- hhsize_tib$Frequency * hhsize_tib$correction_factor
  
  hhsize_tib <- hhsize_tib %>% select(-OG_Frequency, -NEW_Frequency, -correction_factor)
}



survey_edu_cats <- unique(jointhead100small$EDUCAT)
marg_edu_cats <- unique(edu_tib$EDUCAT)

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
  OG_weights <- edu_tib %>% group_by(GEOLEV1) %>% summarise(OG_Frequency = sum(Frequency))
  edu_tib <- edu_tib[edu_tib$EDUCAT %ni% missingcat,]
  NEW_weights <- edu_tib %>% group_by(GEOLEV1) %>% summarise(NEW_Frequency = sum(Frequency))
  edu_tib <- left_join(edu_tib, OG_weights, by = 'GEOLEV1')
  edu_tib <- left_join(edu_tib, NEW_weights, by = 'GEOLEV1')
  edu_tib$correction_factor <- edu_tib$OG_Frequency / edu_tib$NEW_Frequency
  edu_tib$Frequency <- edu_tib$Frequency * edu_tib$correction_factor
  
  edu_tib <- edu_tib %>% select(-OG_Frequency, -NEW_Frequency, -correction_factor)
}



survey_hhtype_cats <- unique(jointhead100small$HHTYPE)
marg_hhtype_cats <- unique(hhtype_tib$HHTYPE)

if (length(marg_hhtype_cats) < length(survey_hhtype_cats)){
  print('hhtype survey removed')
  missingcat <- setdiff(survey_hhtype_cats, marg_hhtype_cats)
  missingcat <- c(missingcat)
  jointhead100small_missingcat <- jointhead100small[jointhead100small$HHTYPE %in% missingcat ,]
  missingcat_hid <- unique(jointhead100small_missingcat$HID)
  
  jointhead100small <- jointhead100small[!jointhead100small$HID %in% missingcat_hid ,]
}


if (length(marg_hhtype_cats) > length(survey_hhtype_cats)){
  print('hhtype marginal removed')
  missingcat <- setdiff(marg_hhtype_cats, survey_hhtype_cats)
  missingcat <- c(missingcat)
  OG_weights <- hhtype_tib %>% group_by(GEOLEV1) %>% summarise(OG_Frequency = sum(Frequency))
  hhtype_tib <- hhtype_tib[hhtype_tib$HHTYPE %ni% missingcat,]
  NEW_weights <- hhtype_tib %>% group_by(GEOLEV1) %>% summarise(NEW_Frequency = sum(Frequency))
  hhtype_tib <- left_join(hhtype_tib, OG_weights, by = 'GEOLEV1')
  hhtype_tib <- left_join(hhtype_tib, NEW_weights, by = 'GEOLEV1')
  hhtype_tib$correction_factor <- hhtype_tib$OG_Frequency / hhtype_tib$NEW_Frequency
  hhtype_tib$Frequency <- hhtype_tib$Frequency * hhtype_tib$correction_factor
  
  hhtype_tib <- hhtype_tib %>% select(-OG_Frequency, -NEW_Frequency, -correction_factor)
}




regionnumbers <- unique(rural_tib$GEOLEV1) #not integers, otherwise we can't match them with GDL.  


jointhead100small[, PID := 1:.N] 

nr_individuals_per_regio <- c()
nr_individuals_per_regio_GDL <- c()


GDL_country <- GDL_pop[GDL_pop$liscountry == liscodeletters,]
GDL_country <- GDL_country[!is.na(GDL_country$region_number),] 


est_marg_income_gr <- na.omit(est_marg_income_gr)
est_marg_hhtype_gr <- na.omit(est_marg_hhtype_gr)
est_marg_hhsize_gr <- na.omit(est_marg_hhsize_gr)
est_marg_age_gr <- na.omit(est_marg_age_gr)
est_marg_edu_gr <- na.omit(est_marg_edu_gr)
est_marg_gender_gr <- na.omit(est_marg_gender_gr)


for (regnr in regionnumbers){
  
  print('regnr')
  print(regnr)
  
  income_tib1reg <- est_marg_income_gr[est_marg_income_gr$GEOLEV1 == regnr,]
  income_tib1reg <- income_tib1reg %>% select(-GEOLEV1)
  
  print('income_tib1reg')
  print(income_tib1reg)
  
  rural_tib1reg <- rural_tib[rural_tib$GEOLEV1 == regnr,]
  rural_tib1reg <- rural_tib1reg %>% select(-GEOLEV1)
  
  hhtype_tib1reg <- est_marg_hhtype_gr[est_marg_hhtype_gr$GEOLEV1 == regnr,]
  hhtype_tib1reg <- hhtype_tib1reg %>% select(-GEOLEV1)
  
  hhsize_tib1reg <- est_marg_hhsize_gr[est_marg_hhsize_gr$GEOLEV1 == regnr,]
  hhsize_tib1reg <- hhsize_tib1reg %>% select(-GEOLEV1)
  
  age_tib1reg <- est_marg_age_gr[est_marg_age_gr$GEOLEV1 == regnr,]
  age_tib1reg <- age_tib1reg %>% select(-GEOLEV1)
  
  print('age_tib1reg')
  print(age_tib1reg)
  
  edu_tib1reg <- est_marg_edu_gr[est_marg_edu_gr$GEOLEV1 == regnr,]
  edu_tib1reg <- edu_tib1reg %>% select(-GEOLEV1)
  
  print('edu_tib1reg')
  print(edu_tib1reg)
  
  sex_tib1reg <- est_marg_gender_gr[est_marg_gender_gr$GEOLEV1 == regnr,]
  sex_tib1reg <- sex_tib1reg %>% select(-GEOLEV1)
  
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
  
  
  nr_individuals_per_regio <- c(nr_individuals_per_regio, nrow(syn_pop_reg))
  
  
  gc()
  
  
  syn_pop <- rbindlist(list(syn_pop, syn_pop_reg), use.names=TRUE)
  rm(syn_pop_reg)
  gc()
  
  
  
}



##########################################
### THE ERROR PART 2. Other part moved up, because we needed df1tot. 


read_freqs_LIS_regions <- function(txtpathfreq, beginline, endline, regionnumbers){
  
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
      df1 <- read_freqs_LIS_regions(txtpathfreq, beginlist_freq[i], endlist_freq[i], regionnumbers)
    } else {
      df2 <- read_freqs_LIS_regions(txtpathfreq, beginlist_freq[i], endlist_freq[i], regionnumbers)
      
      df1 <- rbind.data.frame(df1, df2)
      
    }
    
  }
  
}



nr_of_regions <- length(regionnumbers)

vec1tot_allreg <- rep(df1tot$National, nr_of_regions)
share_individuals_per_regio = nr_individuals_per_regio/sum(nr_individuals_per_regio)
share_individuals_per_regio_vec <- rep(share_individuals_per_regio, each = (length(vec1tot_allreg)/nr_of_regions)) #hier stond 11600/20

vec1tot_allreg <- vec1tot_allreg*share_individuals_per_regio_vec




##################################################
## THE ERROR WITH MARGINALS

# Marginals
marginals_beginlist = list('> print(ftable(incomemarg.table))', '> print(ftable(ruralmarg.table))', 
                           '> print(ftable(farmingmarg.table))', '> print(ftable(hhtypemarg.table))',
                           '> print(ftable(hhsizecatmarg.table))', '> print(ftable(ageindimarg.table))',
                           '> print(ftable(educindimarg.table))', '> print(ftable(sexindimarg.table))')
marginals_endlist = list('> print(ftable(ruralmarg.table))', '> print(ftable(farmingmarg.table))', 
                         '> print(ftable(hhtypemarg.table))', '> print(ftable(hhsizecatmarg.table))',
                         '> print(ftable(ageheadmarg.table))', '> print(ftable(educindimarg.table))',
                         '> print(ftable(sexindimarg.table))', '> # BIJNA EINDE')



incomemarg <- text_to_df(txtpath, marginals_beginlist[[1]], marginals_endlist[[1]], F)
incomemarg1 <- incomemarg %>% rename(INCOME = INCOMEQQ, GEOLEV1 = region_number)
incomemarg1$INCOME <- as.factor(incomemarg1$INCOME)
incomemarg1$GEOLEV1 <- as.factor(incomemarg1$GEOLEV1)
incomemarg1$Prob_INCOME <- incomemarg1$Frequency/sum(incomemarg1$Frequency)
incomemarg1 <- incomemarg1 %>% select(-Frequency)


farmmarg <- text_to_df(txtpath, marginals_beginlist[[3]], marginals_endlist[[3]], F)
farmmarg1 <- farmmarg %>% rename(FARMING = farming_number, GEOLEV1 = region_number)
farmmarg1$FARMING <- as.factor(farmmarg1$FARMING)
farmmarg1$GEOLEV1 <- as.factor(farmmarg1$GEOLEV1)
farmmarg1$Prob_FARMING <- farmmarg1$Frequency/sum(farmmarg1$Frequency)
farmmarg1 <- farmmarg1 %>% select(-Frequency)


hhtypemarg <- text_to_df(txtpath, marginals_beginlist[[4]], marginals_endlist[[4]], F)
hhtypemarg1 <- hhtypemarg %>% rename(HHTYPE = HHTYPE_CAT, GEOLEV1 = region_number)
hhtypemarg1$HHTYPE <- as.factor(as.integer(as.factor(hhtypemarg1$HHTYPE)))
hhtypemarg1$GEOLEV1 <- as.factor(hhtypemarg1$GEOLEV1)
hhtypemarg1$Prob_HHTYPE <- hhtypemarg1$Frequency/sum(hhtypemarg1$Frequency)
hhtypemarg1 <- hhtypemarg1 %>% select(-Frequency)



hhsizemarg <- text_to_df(txtpath, marginals_beginlist[[5]], marginals_endlist[[5]], F)
hhsizemarg1 <- hhsizemarg %>% rename(GEOLEV1 = region_number)
hhsizemarg1$HHSIZECAT <- as.factor(hhsizemarg1$HHSIZECAT)
hhsizemarg1$GEOLEV1 <- as.factor(hhsizemarg1$GEOLEV1)
hhsizemarg1$Prob_HHSIZECAT <- hhsizemarg1$Frequency/sum(hhsizemarg1$Frequency)
hhsizemarg1 <- hhsizemarg1 %>% select(-Frequency)


agemarg <- text_to_df(txtpath, marginals_beginlist[[6]], marginals_endlist[[6]], F)
agemarg1 <- agemarg %>% rename(GEOLEV1 = region_number)
agemarg1$AGECAT <- as.factor(agemarg1$AGECAT)
agemarg1$GEOLEV1 <- as.factor(agemarg1$GEOLEV1)
agemarg1$Prob_AGECAT <- agemarg1$Frequency/sum(agemarg1$Frequency)
agemarg1 <- agemarg1 %>% select(-Frequency)

edumarg <- text_to_df(txtpath, marginals_beginlist[[7]], marginals_endlist[[7]], F)
edumarg1 <- edumarg %>% rename(EDUCAT = education, GEOLEV1 = region_number)
edumarg1$EDUCAT <- as.factor(edumarg1$EDUCAT)
edumarg1$GEOLEV1 <- as.factor(edumarg1$GEOLEV1)
edumarg1$Prob_EDUCAT <- edumarg1$Frequency/sum(edumarg1$Frequency)
edumarg1 <- edumarg1 %>% select(-Frequency)


sexmarg <- text_to_df(txtpath, marginals_beginlist[[8]], marginals_endlist[[8]], F)
sexmarg1 <- sexmarg %>% rename(GENDER = sex_number, GEOLEV1 = region_number)
sexmarg1$GENDER <- as.factor(sexmarg1$GENDER)
sexmarg1$GEOLEV1 <- as.factor(sexmarg1$GEOLEV1)
sexmarg1$Prob_GENDER <- sexmarg1$Frequency/sum(sexmarg1$Frequency)
sexmarg1 <- sexmarg1 %>% select(-Frequency)


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

remove_rural_combi <- function(){
  
  
  combi1 <- data.frame(xtabs(~ RURAL + HHTYPE + GEOLEV1, data = syn_pophead)/nrow(syn_pophead))
  combi1 <- left_join(combi1, ruralmarg1, by = c('RURAL', 'GEOLEV1'))
  combi1 <- left_join(combi1, hhtypemarg1, by = c('HHTYPE', 'GEOLEV1'))
  combi1$Marg_freq <- combi1$Prob_RURAL * combi1$Prob_HHTYPE
  combi1[is.na(combi1)] <- 0
  combi1$Marg_freq <- combi1$Marg_freq / sum(combi1$Marg_freq)
  gc()
  
  combi2 <- data.frame(xtabs(~ RURAL + HHSIZECAT + GEOLEV1, data = syn_pophead)/nrow(syn_pophead))
  combi2 <- left_join(combi2, ruralmarg1, by = c('RURAL', 'GEOLEV1'))
  combi2 <- left_join(combi2, hhsizemarg1, by = c('HHSIZECAT', 'GEOLEV1'))
  combi2$Marg_freq <- combi2$Prob_RURAL * combi2$Prob_HHSIZECAT
  combi2[is.na(combi2)] <- 0
  combi2$Marg_freq <- combi2$Marg_freq / sum(combi2$Marg_freq)
  
  gc()
  combi3 <- data.frame(xtabs(~ RURAL + FARMING + GEOLEV1, data = syn_pophead)/nrow(syn_pophead))
  combi3 <- left_join(combi3, ruralmarg1, by = c('RURAL', 'GEOLEV1'))
  combi3 <- left_join(combi3, farmmarg1, by = c('FARMING', 'GEOLEV1'))
  combi3$Marg_freq <- combi3$Prob_RURAL * combi3$Prob_FARMING
  combi3[is.na(combi3)] <- 0
  combi3$Marg_freq <- combi3$Marg_freq / sum(combi3$Marg_freq)
  
  gc()
  combi4 <- data.frame(xtabs(~ RURAL + INCOME + GEOLEV1, data = syn_pophead)/nrow(syn_pophead))
  combi4 <- left_join(combi4, ruralmarg1, by = c('RURAL', 'GEOLEV1'))
  combi4 <- left_join(combi4, incomemarg1, by = c('INCOME', 'GEOLEV1'))
  combi4$Marg_freq <- combi4$Prob_RURAL * combi4$Prob_INCOME
  combi4[is.na(combi4)] <- 0
  combi4$Marg_freq <- combi4$Marg_freq / sum(combi4$Marg_freq)
  
  gc()
  combi5 <- data.frame(xtabs(~ RURAL + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
  combi5 <- left_join(combi5, ruralmarg1, by = c('RURAL', 'GEOLEV1'))
  combi5 <- left_join(combi5, agemarg1, by = c('AGECAT', 'GEOLEV1'))
  combi5$Marg_freq <- combi5$Prob_RURAL * combi5$Prob_AGECAT
  combi5[is.na(combi5)] <- 0
  combi5$Marg_freq <- combi5$Marg_freq / sum(combi5$Marg_freq)
  
  gc()
  combi6 <- data.frame(xtabs(~ RURAL + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
  combi6 <- left_join(combi6, ruralmarg1, by = c('RURAL', 'GEOLEV1'))
  combi6 <- left_join(combi6, edumarg1, by = c('EDUCAT', 'GEOLEV1'))
  combi6[is.na(combi6)] <- 0
  combi6$Marg_freq <- combi6$Prob_RURAL * combi6$Prob_EDUCAT
  combi6[is.na(combi6)] <- 0
  combi6$Marg_freq <- combi6$Marg_freq / sum(combi6$Marg_freq)
  
  gc()
  combi7 <- data.frame(xtabs(~ RURAL + GENDER + GEOLEV1, data = syn_pop)/nrow(syn_pop))
  combi7 <- left_join(combi7, ruralmarg1, by = c('RURAL', 'GEOLEV1'))
  combi7 <- left_join(combi7, sexmarg1, by = c('GENDER', 'GEOLEV1'))
  combi7$Marg_freq <- combi7$Prob_RURAL * combi7$Prob_GENDER
  combi7[is.na(combi7)] <- 0
  combi7$Marg_freq <- combi7$Marg_freq / sum(combi7$Marg_freq)
  
}

gc()


gc()
combi14 <- data.frame(xtabs(~ INCOME + HHTYPE + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi14 <- left_join(combi14, incomemarg1, by = c('INCOME', 'GEOLEV1'))
combi14 <- left_join(combi14, hhtypemarg1, by = c('HHTYPE', 'GEOLEV1'))
combi14$Marg_freq <- combi14$Prob_INCOME * combi14$Prob_HHTYPE
combi14[is.na(combi14)] <- 0
combi14$Marg_freq <- combi14$Marg_freq / sum(combi14$Marg_freq)

gc()
combi15 <- data.frame(xtabs(~ INCOME + HHSIZECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi15 <- left_join(combi15, incomemarg1, by = c('INCOME', 'GEOLEV1'))
combi15 <- left_join(combi15, hhsizemarg1, by = c('HHSIZECAT', 'GEOLEV1'))
combi15$Marg_freq <- combi15$Prob_INCOME * combi15$Prob_HHSIZECAT
combi15[is.na(combi15)] <- 0
combi15$Marg_freq <- combi15$Marg_freq / sum(combi15$Marg_freq)

gc()
combi16 <- data.frame(xtabs(~ INCOME + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi16 <- left_join(combi16, incomemarg1, by = c('INCOME', 'GEOLEV1'))
combi16 <- left_join(combi16, agemarg1, by = c('AGECAT', 'GEOLEV1'))
combi16$Marg_freq <- combi16$Prob_INCOME * combi16$Prob_AGECAT
combi16[is.na(combi16)] <- 0
combi16$Marg_freq <- combi16$Marg_freq / sum(combi16$Marg_freq)

gc()
combi17 <- data.frame(xtabs(~ INCOME + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi17 <- left_join(combi17, incomemarg1, by = c('INCOME', 'GEOLEV1'))
combi17 <- left_join(combi17, edumarg1, by = c('EDUCAT', 'GEOLEV1'))
combi17[is.na(combi17)] <- 0
combi17$Marg_freq <- combi17$Prob_INCOME * combi17$Prob_EDUCAT
combi17[is.na(combi17)] <- 0
combi17$Marg_freq <- combi17$Marg_freq / sum(combi17$Marg_freq)

gc()
combi18 <- data.frame(xtabs(~ INCOME + GENDER + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi18 <- left_join(combi18, incomemarg1, by = c('INCOME', 'GEOLEV1'))
combi18 <- left_join(combi18, sexmarg1, by = c('GENDER', 'GEOLEV1'))
combi18$Marg_freq <- combi18$Prob_INCOME * combi18$Prob_GENDER
combi18[is.na(combi18)] <- 0
combi18$Marg_freq <- combi18$Marg_freq / sum(combi18$Marg_freq)

gc()
combi19 <- data.frame(xtabs(~ HHSIZECAT + HHTYPE + GEOLEV1, data = syn_pophead)/nrow(syn_pophead))
combi19 <- left_join(combi19, hhsizemarg1, by = c('HHSIZECAT', 'GEOLEV1'))
combi19 <- left_join(combi19, hhtypemarg1, by = c('HHTYPE', 'GEOLEV1'))
combi19$Marg_freq <- combi19$Prob_HHSIZECAT * combi19$Prob_HHTYPE
combi19[is.na(combi19)] <- 0
combi19$Marg_freq <- combi19$Marg_freq / sum(combi19$Marg_freq)

gc()
combi20 <- data.frame(xtabs(~ HHSIZECAT + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi20 <- left_join(combi20, hhsizemarg1, by = c('HHSIZECAT', 'GEOLEV1'))
combi20 <- left_join(combi20, agemarg1, by = c('AGECAT', 'GEOLEV1'))
combi20$Marg_freq <- combi20$Prob_HHSIZECAT * combi20$Prob_AGECAT
combi20[is.na(combi20)] <- 0
combi20$Marg_freq <- combi20$Marg_freq / sum(combi20$Marg_freq)

gc()
combi21 <- data.frame(xtabs(~ HHSIZECAT + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi21 <- left_join(combi21, hhsizemarg1, by = c('HHSIZECAT', 'GEOLEV1'))
combi21 <- left_join(combi21, edumarg1, by = c('EDUCAT', 'GEOLEV1'))
combi21[is.na(combi21)] <- 0
combi21$Marg_freq <- combi21$Prob_HHSIZECAT * combi21$Prob_EDUCAT
combi21[is.na(combi21)] <- 0
combi21$Marg_freq <- combi21$Marg_freq / sum(combi21$Marg_freq)

gc()
combi22 <- data.frame(xtabs(~ HHSIZECAT + GENDER + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi22 <- left_join(combi22, hhsizemarg1, by = c('HHSIZECAT', 'GEOLEV1'))
combi22 <- left_join(combi22, sexmarg1, by = c('GENDER', 'GEOLEV1'))
combi22$Marg_freq <- combi22$Prob_HHSIZECAT * combi22$Prob_GENDER
combi22[is.na(combi22)] <- 0
combi22$Marg_freq <- combi22$Marg_freq / sum(combi22$Marg_freq)

gc()
combi23 <- data.frame(xtabs(~ HHTYPE + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi23 <- left_join(combi23, hhtypemarg1, by = c('HHTYPE', 'GEOLEV1'))
combi23 <- left_join(combi23, agemarg1, by = c('AGECAT', 'GEOLEV1'))
combi23$Marg_freq <- combi23$Prob_HHTYPE * combi23$Prob_AGECAT
combi23[is.na(combi23)] <- 0
combi23$Marg_freq <- combi23$Marg_freq / sum(combi23$Marg_freq)

gc()
combi24 <- data.frame(xtabs(~ HHTYPE + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi24 <- left_join(combi24, hhtypemarg1, by = c('HHTYPE', 'GEOLEV1'))
combi24 <- left_join(combi24, edumarg1, by = c('EDUCAT', 'GEOLEV1'))
combi24[is.na(combi24)] <- 0
combi24$Marg_freq <- combi24$Prob_HHTYPE * combi24$Prob_EDUCAT
combi24[is.na(combi24)] <- 0
combi24$Marg_freq <- combi24$Marg_freq / sum(combi24$Marg_freq)

gc()
combi25 <- data.frame(xtabs(~ HHTYPE + GENDER + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi25 <- left_join(combi25, hhtypemarg1, by = c('HHTYPE', 'GEOLEV1'))
combi25 <- left_join(combi25, sexmarg1, by = c('GENDER', 'GEOLEV1'))
combi25$Marg_freq <- combi25$Prob_HHTYPE * combi25$Prob_GENDER
combi25[is.na(combi25)] <- 0
combi25$Marg_freq <- combi25$Marg_freq / sum(combi25$Marg_freq)

gc()
combi26 <- data.frame(xtabs(~ GENDER + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi26 <- left_join(combi26, sexmarg1, by = c('GENDER', 'GEOLEV1'))
combi26 <- left_join(combi26, agemarg1, by = c('AGECAT', 'GEOLEV1'))
combi26$Marg_freq <- combi26$Prob_GENDER * combi26$Prob_AGECAT
combi26[is.na(combi26)] <- 0
combi26$Marg_freq <- combi26$Marg_freq / sum(combi26$Marg_freq)

gc()
combi27 <- data.frame(xtabs(~ GENDER + EDUCAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi27 <- left_join(combi27, sexmarg1, by = c('GENDER', 'GEOLEV1'))
combi27 <- left_join(combi27, edumarg1, by = c('EDUCAT', 'GEOLEV1'))
combi27[is.na(combi27)] <- 0
combi27$Marg_freq <- combi27$Prob_GENDER * combi27$Prob_EDUCAT
combi27[is.na(combi27)] <- 0
combi27$Marg_freq <- combi27$Marg_freq / sum(combi27$Marg_freq)

gc()
combi28 <- data.frame(xtabs(~ EDUCAT + AGECAT + GEOLEV1, data = syn_pop)/nrow(syn_pop))
combi28 <- left_join(combi28, edumarg1, by = c('EDUCAT', 'GEOLEV1'))
combi28 <- left_join(combi28, agemarg1, by = c('AGECAT', 'GEOLEV1'))
combi28[is.na(combi28)] <- 0
combi28$Marg_freq <- combi28$Prob_EDUCAT * combi28$Prob_AGECAT
combi28[is.na(combi28)] <- 0
combi28$Marg_freq <- combi28$Marg_freq / sum(combi28$Marg_freq)


if (farm_presence == 1){
  
  #combi1 / combi7 removed (rural)
  allcombi_freqs = c(combi8$Freq, combi9$Freq, combi10$Freq,
                     combi11$Freq, combi12$Freq, combi13$Freq, combi14$Freq, combi15$Freq, combi16$Freq, combi17$Freq, combi18$Freq, combi19$Freq, combi20$Freq,
                     combi21$Freq, combi22$Freq, combi23$Freq, combi24$Freq, combi25$Freq, combi26$Freq, combi27$Freq, combi28$Freq)
  
  allcombi_margfreqs = c(combi8$Marg_freq, combi9$Marg_freq, combi10$Marg_freq,
                         combi11$Marg_freq, combi12$Marg_freq, combi13$Marg_freq, combi14$Marg_freq, combi15$Marg_freq, combi16$Marg_freq, combi17$Marg_freq, combi18$Marg_freq, combi19$Marg_freq, combi20$Marg_freq,
                         combi21$Marg_freq, combi22$Marg_freq, combi23$Marg_freq, combi24$Marg_freq, combi25$Marg_freq, combi26$Marg_freq, combi27$Marg_freq, combi28$Marg_freq)
  
} else if (farm_presence == 0){
  
  allcombi_freqs = c(combi14$Freq, combi15$Freq, combi16$Freq, combi17$Freq, combi18$Freq, combi19$Freq, combi20$Freq,
                     combi21$Freq, combi22$Freq, combi23$Freq, combi24$Freq, combi25$Freq, combi26$Freq, combi27$Freq, combi28$Freq)
  
  allcombi_margfreqs = c(combi14$Marg_freq, combi15$Marg_freq, combi16$Marg_freq, combi17$Marg_freq, combi18$Marg_freq, combi19$Marg_freq, combi20$Marg_freq,
                         combi21$Marg_freq, combi22$Marg_freq, combi23$Marg_freq, combi24$Marg_freq, combi25$Marg_freq, combi26$Marg_freq, combi27$Marg_freq, combi28$Marg_freq)
  
  
}








df_ALLREG <- data.frame('Survey' = df1$Frequency, 'Synthetic' = allcombi_freqs, 'Marginal' = allcombi_margfreqs, 'National' = vec1tot_allreg)
df_ALLREG[is.na(df_ALLREG)] <- 0

check_sums_survey <- sum(df_ALLREG$Survey)
check_sums_synth <- sum(df_ALLREG$Synthetic)
check_sums_national <- sum(df_ALLREG$National)
check_sums_marginal <- sum(df_ALLREG$Marginal)




if ((check_sums_survey - check_sums_national) + (check_sums_synth - check_sums_marginal) <= 0.01){
  
  
  standardized_error_factor <- 100/sum(df_ALLREG$Synthetic)
  print('standerror (removed)')
  print(standardized_error_factor)
  standardized_error_factor = 1
  df_ALLREG$Survey <- df_ALLREG$Survey * standardized_error_factor
  df_ALLREG$Synthetic <- df_ALLREG$Synthetic * standardized_error_factor
  df_ALLREG$National <- df_ALLREG$National * standardized_error_factor
  df_ALLREG$Marginal <- df_ALLREG$Marginal * standardized_error_factor
  
  
  df_ALLREG['Sq_error_survey_synth'] <- (df_ALLREG$Survey - df_ALLREG$Synthetic)^2
  df_ALLREG['Sq_error_survey_national'] <- (df_ALLREG$Survey - df_ALLREG$National)^2
  df_ALLREG['Sq_error_survey_marginal'] <- (df_ALLREG$Survey - df_ALLREG$Marginal)^2
  df_ALLREG[is.na(df_ALLREG)] <- 0
  
  DF_errors[1,2] <- sum(df_ALLREG$Sq_error_survey_synth)
  DF_errors[1,3] <- sum(df_ALLREG$Sq_error_survey_national)
  DF_errors[1,4] <- sum(df_ALLREG$Sq_error_survey_marginal)
  
} else if ((check_sums_survey - check_sums_national) + (check_sums_synth - check_sums_marginal) > 0.01){
  DF_errors[1,2] <- -99
  DF_errors[1,3] <- -99
  DF_errors[1,4] <- -99
}



if (uselisdata == 'yes'){

  filename_dferrors <- paste0("synth_errors_estmarg_LIS_6aug24_", isocode, ".csv")
} 

if (uselisdata == 'replace'){
  
  filename_dferrors <- paste0("synth_errors_estmargsurvey_LIS_6aug24_", isocode, ".csv")
  
} 

write.table(DF_errors, filename_dferrors, row.names = FALSE, sep = ',')

print('saved errors')


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


