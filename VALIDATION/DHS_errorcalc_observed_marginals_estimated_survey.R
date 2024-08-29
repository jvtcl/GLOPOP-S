#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)



isocode = as.character(args[1])
print(isocode)


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
library(MASS)


#setwd('C:\\Users\\mtn308\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\Paper2\\DHSapril2024') 


DF_errors <- data.frame('iso_code' = isocode, 'Synth_sq_error' = 0, 'Marginal_sq_error' = 0, 'National_sq_error' = 0)

population <- read.csv('GHSpopulationGDLregions.csv')
population['Urban_percent'] <- population$Urbanpop/population$Population
population['Rural_percent'] <- population$Ruralpop/population$Population


dhsfilenames <- read.csv('DHSdatafiles.csv', sep = ';')

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



# ADD GDL REGIONS
merge_gdl_regions <- function(dhs, isocode){
  
  
  if (isocode %ni% c('AFG', 'GMB', 'IDN', 'MDV', 'PNG')){
    gdlregions_filename <- paste0(isocode, '_cluster_GDL.csv')
    gdlregions <- read.csv(gdlregions_filename)
    
    dhs <- left_join(dhs, gdlregions, by = 'DHSCLUST')
    
  }
  
  
  if (isocode == 'BDI'){
    dhs$GDLcode[dhs$GDLcode == ''] <- 'BDIr102'
  }
  
  if (isocode == 'PAK'){
    dhs$GDLcode[dhs$DHSCLUST == 535] <- 'PAKr107'
  }
  
  if (isocode == 'TJK'){
    dhs$GDLcode[dhs$ADMIN1 == 'dushanbe'] <- 'TJKr101'
    dhs$GDLcode[dhs$ADMIN1 == 'sughd'] <- 'TJKr103'
    dhs$GDLcode[dhs$ADMIN1 == 'khatlon'] <- 'TJKr102'
    dhs$GDLcode[dhs$ADMIN1 == 'drs'] <- 'TJKr104'
    dhs$GDLcode[dhs$ADMIN1 == 'gbao'] <- 'TJKr105'
  }
  
  if (isocode == 'BGD'){
    dhs$GDLcode[dhs$DHSCLUST == 21] <- 'BGDr202'
    dhs$GDLcode[dhs$DHSCLUST == 342] <- 'BGDr214'
    dhs$GDLcode[dhs$DHSCLUST == 544] <- 'BGDr223'
    dhs$GDLcode[dhs$DHSCLUST == 590] <- 'BGDr222'
    
  }
  
  # CONGO DEMOCRATIC REPUBLIC
  if (isocode == 'COD'){
    dhs$GDLcode[dhs$ADMIN1 == 'bandundu' & dhs$GDLcode == ''] <- 'CODr103'
    dhs$GDLcode[dhs$ADMIN1 == 'bas-congo' & dhs$GDLcode == ''] <- 'CODr102'
    dhs$GDLcode[dhs$ADMIN1 == 'equateur' & dhs$GDLcode == ''] <- 'CODr104'
    dhs$GDLcode[dhs$ADMIN1 == 'kasai-occidental' & dhs$GDLcode == ''] <- 'CODr111'
    dhs$GDLcode[dhs$ADMIN1 == 'kasai-oriental' & dhs$GDLcode == ''] <- 'CODr110'
    dhs$GDLcode[dhs$ADMIN1 == 'katanga' & dhs$GDLcode == ''] <- 'CODr109'
    dhs$GDLcode[dhs$ADMIN1 == 'maniema' & dhs$GDLcode == ''] <- 'CODr108'
    dhs$GDLcode[dhs$ADMIN1 == 'nord-kivu' & dhs$GDLcode == ''] <- 'CODr106'
    dhs$GDLcode[dhs$ADMIN1 == 'orientale' & dhs$GDLcode == ''] <- 'CODr105'
  }
  
  if (isocode == 'UGA'){
    dhs$GDLcode[dhs$ADMIN1 == 'south buganda' & dhs$GDLcode == ''] <- 'UGAr101'
    dhs$GDLcode[dhs$ADMIN1 == 'north buganda' & dhs$GDLcode == ''] <- 'UGAr102'
    dhs$GDLcode[dhs$ADMIN1 == 'bukedi' & dhs$GDLcode == ''] <- 'UGAr105'
    dhs$GDLcode[dhs$ADMIN1 == 'bugisu' & dhs$GDLcode == ''] <- 'UGAr105'
    dhs$GDLcode[dhs$ADMIN1 == 'ankole' & dhs$GDLcode == ''] <- 'UGAr109'
  }
  
  if (isocode == 'ETH'){
    dhs$GDLcode[dhs$ADMIN1 == 'somali' & dhs$GDLcode == ''] <- 'ETHr105'
    dhs$GDLcode[dhs$ADMIN1 == 'oromia' & dhs$GDLcode == ''] <- 'ETHr104'
    dhs$GDLcode[dhs$ADMIN1 == 'gambela' & dhs$GDLcode == ''] <- 'ETHr108'
    dhs$GDLcode[dhs$ADMIN1 == 'dire dawa' & dhs$GDLcode == ''] <- 'ETHr111'
  }
  
  if (isocode == 'ZMB'){
    dhs$GDLcode[dhs$ADMIN1 == 'eastern' & dhs$GDLcode == ''] <- 'ZMBr103'
    dhs$GDLcode[dhs$ADMIN1 == 'north western' & dhs$GDLcode == ''] <- 'ZMBr107'
    dhs$GDLcode[dhs$ADMIN1 == 'luapula'] <- 'ZMBr104'
  }
  
  if (isocode == 'PHL'){
    dhs$GDLcode[dhs$ADMIN1 == 'ilocos' & dhs$GDLcode == ''] <- 'PHLr103'
    dhs$GDLcode[dhs$ADMIN1 == 'cagayan valley' & dhs$GDLcode == ''] <- 'PHLr104'
    dhs$GDLcode[dhs$ADMIN1 == 'central luzon' & dhs$GDLcode == ''] <- 'PHLr105'
    dhs$GDLcode[dhs$ADMIN1 == 'calabarzon' & dhs$GDLcode == ''] <- 'PHLr106'
    dhs$GDLcode[dhs$ADMIN1 == 'bicol' & dhs$GDLcode == ''] <- 'PHLr108'
    dhs$GDLcode[dhs$ADMIN1 == 'western visayas' & dhs$GDLcode == ''] <- 'PHLr109'
    dhs$GDLcode[dhs$ADMIN1 == 'central visayas' & dhs$GDLcode == ''] <- 'PHLr110'
    dhs$GDLcode[dhs$ADMIN1 == 'eastern visayas' & dhs$GDLcode == ''] <- 'PHLr111'
    dhs$GDLcode[dhs$ADMIN1 == 'zamboanga peninsula' & dhs$GDLcode == ''] <- 'PHLr112'
    dhs$GDLcode[dhs$ADMIN1 == 'northern mindanao' & dhs$GDLcode == ''] <- 'PHLr113'
    dhs$GDLcode[dhs$ADMIN1 == 'davao' & dhs$GDLcode == ''] <- 'PHLr114'
    dhs$GDLcode[dhs$ADMIN1 == 'soccsksargen' & dhs$GDLcode == ''] <- 'PHLr115'
    dhs$GDLcode[dhs$ADMIN1 == 'national capital' & dhs$GDLcode == ''] <- 'PHLr101'
    dhs$GDLcode[dhs$ADMIN1 == 'cordillera' & dhs$GDLcode == ''] <- 'PHLr102'
    dhs$GDLcode[dhs$ADMIN1 == 'autonomous region in muslim mindanao' & dhs$GDLcode == ''] <- 'PHLr117'
    dhs$GDLcode[dhs$ADMIN1 == 'caraga' & dhs$GDLcode == ''] <- 'PHLr116'
    dhs$GDLcode[dhs$ADMIN1 == 'mimaropa' & dhs$GDLcode == ''] <- 'PHLr107'
    dhs$GDLcode[dhs$ADMIN1 == 'negros island region' & dhs$GDLcode == ''] <- 'PHLr105'
  }
  
  # INDIA
  if (isocode == 'IND'){
    dhs$GDLcode[dhs$ADMIN1 == 'andaman and nicobar islands'] <- 'INDr136'
    dhs$GDLcode[dhs$ADMIN1 == 'arunachal pradesh'] <- 'INDr125'
    dhs$GDLcode[dhs$ADMIN1 == 'andhra pradesh'] <- 'INDr101'
    dhs$GDLcode[dhs$ADMIN1 == 'assam'] <- 'INDr102'
    dhs$GDLcode[dhs$ADMIN1 == 'bihar'] <- 'INDr103'
    dhs$GDLcode[dhs$ADMIN1 == 'chandigarh'] <- 'INDr131'
    dhs$GDLcode[dhs$ADMIN1 == 'chhattisgarh'] <- 'INDr129'
    dhs$GDLcode[dhs$ADMIN1 == 'dadra and nagar haveli'] <- 'INDr132'
    dhs$GDLcode[dhs$ADMIN1 == 'daman and diu'] <- 'INDr134'
    dhs$GDLcode[dhs$ADMIN1 == 'goa'] <- 'INDr104'
    dhs$GDLcode[dhs$ADMIN1 == 'gujarat'] <- 'INDr105'
    dhs$GDLcode[dhs$ADMIN1 == 'haryana'] <- 'INDr106'
    dhs$GDLcode[dhs$ADMIN1 == 'himachal pradesh'] <- 'INDr107'
    dhs$GDLcode[dhs$ADMIN1 == 'jammu and kashmir'] <- 'INDr108'
    dhs$GDLcode[dhs$ADMIN1 == 'jharkhand'] <- 'INDr128'
    dhs$GDLcode[dhs$ADMIN1 == 'karnataka'] <- 'INDr109'
    dhs$GDLcode[dhs$ADMIN1 == 'kerala'] <- 'INDr110'
    dhs$GDLcode[dhs$ADMIN1 == 'lakshadweep'] <- 'INDr135'
    dhs$GDLcode[dhs$ADMIN1 == 'madhya pradesh'] <- 'INDr111'
    dhs$GDLcode[dhs$ADMIN1 == 'maharashtra'] <- 'INDr112'
    dhs$GDLcode[dhs$ADMIN1 == 'manipur'] <- 'INDr113'
    dhs$GDLcode[dhs$ADMIN1 == 'meghalaya'] <- 'INDr114'
    dhs$GDLcode[dhs$ADMIN1 == 'mizoram'] <- 'INDr115'
    dhs$GDLcode[dhs$ADMIN1 == 'nagaland'] <- 'INDr116'
    dhs$GDLcode[dhs$ADMIN1 == 'delhi'] <- 'INDr124'
    dhs$GDLcode[dhs$ADMIN1 == 'odisha'] <- 'INDr117'
    dhs$GDLcode[dhs$ADMIN1 == 'puducherry'] <- 'INDr133'
    dhs$GDLcode[dhs$ADMIN1 == 'punjab'] <- 'INDr118'
    dhs$GDLcode[dhs$ADMIN1 == 'rajasthan'] <- 'INDr119'
    dhs$GDLcode[dhs$ADMIN1 == 'sikkim'] <- 'INDr120'
    dhs$GDLcode[dhs$ADMIN1 == 'tamil nadu'] <- 'INDr121'
    dhs$GDLcode[dhs$ADMIN1 == 'tripura'] <- 'INDr126'
    dhs$GDLcode[dhs$ADMIN1 == 'uttar pradesh'] <- 'INDr123'
    dhs$GDLcode[dhs$ADMIN1 == 'uttarakhand'] <- 'INDr127'
    dhs$GDLcode[dhs$ADMIN1 == 'west bengal'] <- 'INDr122'
    dhs$GDLcode[dhs$ADMIN1 == 'telangana'] <- 'INDr130'
  }
  
  
  # EGYPT
  if (isocode == 'EGY'){
    dhs$GDLcode[dhs$DHSCLUST %in% c(440103, 440104)] <- 'EGYr101'
    dhs$GDLcode[dhs$DHSCLUST %in% c(950403, 950901, 1090102, 1090103)] <- 'EGYr102'
    dhs$GDLcode[dhs$DHSCLUST == 1130901] <- 'EGYr103'
    dhs$GDLcode[dhs$DHSCLUST == 1870103] <- 'EGYr105'
    dhs$GDLcode[dhs$DHSCLUST %in% c(3730102, 3730107, 3790202, 3790405)] <- 'EGYr110'
    dhs$GDLcode[dhs$DHSCLUST %in% c(6330104, 6330404, 6520202, 6520506)] <- 'EGYr117'
    dhs$GDLcode[dhs$DHSCLUST %in% c(6930302, 6930701)] <- 'EGYr118'
    dhs$GDLcode[dhs$DHSCLUST %in% c(8500108, 8510101, 8510103, 8700404)] <- 'EGYr122'
  }
  
  if (isocode == 'NGA'){
    dhs$GDLcode[dhs$DHSCLUST == 302] <- 'NGAr130'
    dhs$GDLcode[dhs$DHSCLUST == 373] <- 'NGAr111'
    dhs$GDLcode[dhs$DHSCLUST == 422] <- 'NGAr126'
    dhs$GDLcode[dhs$DHSCLUST == 514] <- 'NGAr102'
    dhs$GDLcode[dhs$DHSCLUST == 557] <- 'NGAr124'
    dhs$GDLcode[dhs$DHSCLUST == 569] <- 'NGAr124'
    dhs$GDLcode[dhs$DHSCLUST == 639] <- 'NGAr131'
    dhs$GDLcode[dhs$DHSCLUST == 727] <- 'NGAr120'
    dhs$GDLcode[dhs$DHSCLUST == 729] <- 'NGAr120'
  }
  
  
  if (isocode == 'GHA'){
    dhs$GDLcode[dhs$DHSCLUST == 123] <- 'GHAr103'
    dhs$GDLcode[dhs$DHSCLUST == 173] <- 'GHAr104'
    dhs$GDLcode[dhs$DHSCLUST == 204] <- 'GHAr109'
    dhs$GDLcode[dhs$DHSCLUST == 266] <- 'GHAr106'
    dhs$GDLcode[dhs$DHSCLUST == 347] <- 'GHAr109'
  }
  
  if (isocode == 'GTM'){
    dhs$GDLcode[dhs$DHSCLUST == 403] <- 'GTMr106'
    dhs$GDLcode[dhs$DHSCLUST %in% c(509, 512, 529)] <- 'GTMr107'
    dhs$GDLcode[dhs$DHSCLUST == 676] <- 'GTMr108'
    
  }
  
  if (isocode == 'KEN'){
    dhs$GDLcode[dhs$ADMIN1 == 'coast' & dhs$GDLcode == ''] <- 'KENr103'
    dhs$GDLcode[dhs$ADMIN1 == 'north eastern' & dhs$GDLcode == ''] <- 'KENr108'
    dhs$GDLcode[dhs$DHSCLUST == 791] <- 'KENr108'
    dhs$GDLcode[dhs$ADMIN1 == 'eastern' & dhs$GDLcode == ''] <- 'KENr104'
    dhs$GDLcode[dhs$ADMIN1 == 'central' & dhs$GDLcode == ''] <- 'KENr102'
    dhs$GDLcode[dhs$ADMIN1 == 'rift valley' & dhs$GDLcode == ''] <- 'KENr106'
    
    
  }
  
  if (isocode == 'ARM'){
    dhsarm <- dhs[dhs$GDLcode == 'TURr110',]
    dhs$GDLcode[dhs$DHSCLUST == 306] <- 'ARMr110'
    dhs$GDLcode[dhs$DHSCLUST == 81] <- 'ARMr102'
  }
  
  if (isocode == 'BEN'){
    dhs$GDLcode[dhs$DHSCLUST %in% c(485, 489)] <- 'BENr105'
    dhs$GDLcode[dhs$DHSCLUST == 162] <- 'BENr103'
  }
  
  if (isocode == 'DOM'){
    dhs$GDLcode[dhs$DHSCLUST %in% c(1056)] <- 'DOMr108'
  }
  
  if (isocode == 'LSO'){
    dhs$GDLcode[dhs$DHSCLUST %in% c(199, 393)] <- 'LSOr102'
  }
  
  
  if (isocode == 'MWI'){
    dhs$GDLcode[dhs$DHSCLUST == 453] <- 'MWIr108'
    dhs <- dhs[dhs$GDLcode != 'MOZr101',]
  }
  
  if (isocode == 'MLI'){
    dhs$GDLcode[dhs$DHSCLUST == 360] <- 'MLIr101'
  }
  
  if (isocode == 'MMR'){
    dhs$GDLcode[dhs$DHSCLUST == 414] <- 'MMRr103'
  }
  
  if (isocode == 'NAM'){
    dhs$GDLcode[dhs$DHSCLUST == 176] <- 'NAMr107'
    dhs$GDLcode[dhs$DHSCLUST == 183] <- 'NAMr104'
  }
  
  if (isocode == 'SEN'){
    dhs$GDLcode[dhs$DHSCLUST == 158] <- 'SENr110'
  }
  
  if (isocode == 'SLE'){
    dhs$GDLcode[dhs$DHSCLUST == 422] <- 'SLEr108' 
  }
  
  if (isocode == 'TCD'){
    dhs$GDLcode[dhs$DHSCLUST == 325] <- 'TCDr106'  
  }
  
  if (isocode == 'ZWE'){
    dhs$GDLcode[dhs$DHSCLUST == 269] <- 'ZWEr101' 
  }
  
  # AFGHANISTAN
  if (isocode == 'AFG'){
    afgr101 <- c('kabul', 'wardak', 'kapisa', 'parwan', 'logar', 'panjsher')
    afgr102 <- c('bamyan', 'daykundi')
    afgr103 <- c('nangarhar', 'laghman', 'nooristan', 'kunarha')
    afgr104 <- c('samangan', 'sar-e-pul', 'balkh', 'jawzjan', 'faryab')
    afgr105 <- c('baghlan', 'takhar', 'badakhshan', 'kunduz')
    afgr106 <- c('urozgan', 'helmand', 'zabul', 'nimroz', 'kandahar')
    afgr107 <- c('ghazni', 'paktika', 'paktya', 'khost')
    afgr108 <- c('ghor', 'herat', 'badghis', 'farah')
    
    dhs$GDLcode <- ''
    
    dhs$GDLcode[dhs$ADMIN1 %in% afgr101] <- 'AFGr101'
    dhs$GDLcode[dhs$ADMIN1 %in% afgr102] <- 'AFGr102'
    dhs$GDLcode[dhs$ADMIN1 %in% afgr103] <- 'AFGr103'
    dhs$GDLcode[dhs$ADMIN1 %in% afgr104] <- 'AFGr104'
    dhs$GDLcode[dhs$ADMIN1 %in% afgr105] <- 'AFGr105'
    dhs$GDLcode[dhs$ADMIN1 %in% afgr106] <- 'AFGr106'
    dhs$GDLcode[dhs$ADMIN1 %in% afgr107] <- 'AFGr107'
    dhs$GDLcode[dhs$ADMIN1 %in% afgr108] <- 'AFGr108'
  }
  
  # GAMBIA
  if (isocode == 'GMB'){
    gmbr101 <- c('Banjul')
    gmbr102 <- c('Kanifing')
    gmbr103 <- c('Brikama')
    gmbr104 <- c('Mansakonko')
    gmbr105 <- c('Kerewan')
    gmbr106 <- c('Kuntaur')
    gmbr107 <- c('Janjanbureh')
    gmbr108 <- c('Basse')
    
    dhs$GDLcode[dhs$ADMIN1 %in% gmbr101] <- 'GMBr101'
    dhs$GDLcode[dhs$ADMIN1 %in% gmbr102] <- 'GMBr102'
    dhs$GDLcode[dhs$ADMIN1 %in% gmbr103] <- 'GMBr103'
    dhs$GDLcode[dhs$ADMIN1 %in% gmbr104] <- 'GMBr104'
    dhs$GDLcode[dhs$ADMIN1 %in% gmbr105] <- 'GMBr105'
    dhs$GDLcode[dhs$ADMIN1 %in% gmbr106] <- 'GMBr106'
    dhs$GDLcode[dhs$ADMIN1 %in% gmbr107] <- 'GMBr107'
    dhs$GDLcode[dhs$ADMIN1 %in% gmbr108] <- 'GMBr108'
  }
  
  # INDONESIA
  if (isocode == 'IDN'){
    idnr101 <- c('aceh')
    idnr102 <- c('north sumatera')
    idnr103 <- c('west sumatera')
    idnr104 <- c('riau', 'riau islands')
    idnr105 <- c('jambi')
    idnr106 <- c('south sumatera')
    idnr107 <- c('bengkulu')
    idnr108 <- c('lampung')
    idnr109 <- c('bangka belitung')
    idnr110 <- c('jakarta')
    idnr111 <- c('west java')
    idnr112 <- c('central java')
    idnr113 <- c('yogyakarta')
    idnr114 <- c('east java')
    idnr115 <- c('banten')
    idnr116 <- c('bali')
    idnr117 <- c('west nusa tenggara')
    idnr118 <- c('east nusa tenggara')
    idnr119 <- c('???') #east timor is now timor leste. we can remove it. 
    idnr120 <- c('west kalimantan')
    idnr121 <- c('central kalimantan')
    idnr122 <- c('south kalimantan')
    idnr123 <- c('east kalimantan', 'north kalimantan')
    idnr124 <- c('north sulawesi')
    idnr125 <- c('central sulawesi')
    idnr126 <- c('south sulawesi', 'west sulawesi')
    idnr127 <- c('southeast sulawesi')
    idnr128 <- c('gorontalo')
    idnr129 <- c('maluku', 'north maluku')
    idnr130 <- c('papua', 'west papua')
    
    dhs <- dhs[dhs$ADMIN1 != 'east timor',]
    
    
    dhs['GDLcode'] <- ''
    
    dhs$GDLcode[dhs$ADMIN1 %in% idnr101] <- 'IDNr101'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr102] <- 'IDNr102'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr103] <- 'IDNr103'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr104] <- 'IDNr104'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr105] <- 'IDNr105'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr106] <- 'IDNr106'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr107] <- 'IDNr107'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr108] <- 'IDNr108'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr109] <- 'IDNr109'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr110] <- 'IDNr110'
    
    dhs$GDLcode[dhs$ADMIN1 %in% idnr111] <- 'IDNr111'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr112] <- 'IDNr112'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr113] <- 'IDNr113'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr114] <- 'IDNr114'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr115] <- 'IDNr115'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr116] <- 'IDNr116'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr117] <- 'IDNr117'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr118] <- 'IDNr118'
    #dhs$GDLcode[dhs$ADMIN1 %in% idnr119] <- 'IDNr119'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr120] <- 'IDNr120'
    
    dhs$GDLcode[dhs$ADMIN1 %in% idnr121] <- 'IDNr121'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr122] <- 'IDNr122'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr123] <- 'IDNr123'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr124] <- 'IDNr124'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr125] <- 'IDNr125'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr126] <- 'IDNr126'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr127] <- 'IDNr127'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr128] <- 'IDNr128'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr129] <- 'IDNr129'
    dhs$GDLcode[dhs$ADMIN1 %in% idnr130] <- 'IDNr130'
  }
  
  # MALDIVES
  if (isocode == 'MDV'){
    mdvr101 <- c('malé')
    mdvr102 <- c('north region')
    mdvr103 <- c('north central')
    mdvr104 <- c('central region')
    mdvr105 <- c('south central')
    mdvr106 <- c('south region')
    
    dhs['GDLcode'] <- ''
    
    dhs$GDLcode[dhs$ADMIN1 %in% mdvr101] <- 'MDVr101'
    dhs$GDLcode[dhs$ADMIN1 %in% mdvr102] <- 'MDVr102'
    dhs$GDLcode[dhs$ADMIN1 %in% mdvr103] <- 'MDVr103'
    dhs$GDLcode[dhs$ADMIN1 %in% mdvr104] <- 'MDVr104'
    dhs$GDLcode[dhs$ADMIN1 %in% mdvr105] <- 'MDVr105'
    dhs$GDLcode[dhs$ADMIN1 %in% mdvr106] <- 'MDVr106'
    
  }
  
  # PAPUA NEW GUINEA
  if (isocode == 'PNG'){
    highlands <- c(110, 111, 108, 121, 122, 107, 109)
    islands <- c(120, 118, 116, 117, 119)
    momase <- c(114, 113, 112, 115)
    southern <- c(103, 101, 102, 106, 104, 105)
    
    pngr101 <- c('highlands region')
    pngr102 <- c('islands region')
    pngr103 <- c('momase region')
    pngr104 <- c('southern region')
    
    dhs['GDLcode'] <- ''
    
    dhs$GDLcode[dhs$ADMIN1 %in% pngr101] <- 'PNGr101'
    dhs$GDLcode[dhs$ADMIN1 %in% pngr102] <- 'PNGr102'
    dhs$GDLcode[dhs$ADMIN1 %in% pngr103] <- 'PNGr103'
    dhs$GDLcode[dhs$ADMIN1 %in% pngr104] <- 'PNGr104'
  }
  
  return(dhs)
}

# HARMONIZE DATA
dhs_survey2 <- function(dhs){
  dataindi <- dhs %>% dplyr::select(HID, WEIGHT, URBANHH, SEX, HHAGE, EDSUMM, HHRELATE, HHMEMBERS, WEALTHQHH, AGLANDYN, WALL, ROOF, FLOOR, GDLcode) #changed hv106 to hv109, because more categories in hv109.
  
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
  
  dataindi <- subset(dataindi, rowSums(is.na(dataindi)) != ncol(dataindi))
  
  missage <- dataindi[dataindi$HHAGE >= 98,]
  missgender <- dataindi[dataindi$SEX >= 9,]
  
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
  dataindi$AGECAT[dataindi$HHAGE %in% (0:4)] <- 1
  dataindi$AGECAT[dataindi$HHAGE %in% (5:14)] <- 2
  dataindi$AGECAT[dataindi$HHAGE %in% (15:24)] <- 3
  dataindi$AGECAT[dataindi$HHAGE %in% (25:34)] <- 4
  dataindi$AGECAT[dataindi$HHAGE %in% (35:44)] <- 5
  dataindi$AGECAT[dataindi$HHAGE %in% (45:54)] <- 6
  dataindi$AGECAT[dataindi$HHAGE %in% (55:64)] <- 7
  dataindi$AGECAT[dataindi$HHAGE > 64] <- 8 #missing and don't know eruit. 
  
  #hv109 = highest attained education. don't know and missing go to no education category. 
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
  
  data_EDUCAT <- dataindi[dataindi$EDUCAT == 0,]
  
  if (nrow(dataindi) == nrow(data_EDUCAT)){
    print(paste0("Check DHS EDUCAT for potential error ", isocode))
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
    print(paste0("Minimal DHS wealth error ", isocode))
    dataindi <- dataindi[dataindi$WEALTH != 0,]
  }
  
  if ((nrow(data_WEALTH)/nrow(dataindi) > 0.005 & nrow(data_WEALTH)/nrow(dataindi) <= 0.01)){
    print(paste0("Less than 1% DHS wealth missing", isocode))
    dataindi <- dataindi[dataindi$WEALTH != 0,]
  }
  
  if ((nrow(data_WEALTH)/nrow(dataindi) > 0.01 & nrow(data_WEALTH)/nrow(dataindi) <= 0.1)){
    print(paste0("Substantial DHS wealth error", isocode))
    dataindi <- dataindi[dataindi$WEALTH != 0,]
  }
  
  if ((nrow(data_WEALTH)/nrow(dataindi) > 0.1)){
    print(paste0("DHS wealth error too big", isocode))
    dataindi <- dataindi[dataindi$WEALTH != 0,]
  }
  
  dataindi['FARMING'] <- 0
  dataindi$FARMING[dataindi$AGLANDYN == 'Yes'] <- 1
  dataindi$FARMING[dataindi$AGLANDYN == 'yes'] <- 1
  dataindi$FARMING[dataindi$AGLANDYN == 1] <- 1
  
  data_FARM <- dataindi[dataindi$FARMING == 0,]
  
  if (nrow(data_FARM) == nrow(dataindi)){
    print(paste0("Check DHS farming for error ", isocode))
    dataindi$FARMING = -1
    
  }
  
  #relationship to HH head
  
  dataindi$HHRELATE <- tolower(dataindi$HHRELATE)
  
  dataindi["RELATE"] <- 4
  
  dataindi$RELATE[dataindi$HHRELATE == "wife or husband"] <- 2
  dataindi$RELATE[dataindi$HHRELATE == 2] <- 2
  dataindi$RELATE[dataindi$HHRELATE == "co-spouse"] <- 2
  dataindi$RELATE[dataindi$HHRELATE == 9] <- 2
  
  dataindi$RELATE[dataindi$HHRELATE == 1] <- 1
  dataindi$RELATE[dataindi$HHRELATE == "head"] <- 1
  
  
  dataindi$RELATE[dataindi$HHRELATE == "son/daughter"] <- 3
  dataindi$RELATE[dataindi$HHRELATE == 3] <- 3
  dataindi$RELATE[dataindi$HHRELATE == "son/daughter-in-law"] <- 3
  dataindi$RELATE[dataindi$HHRELATE == 4] <- 3
  dataindi$RELATE[dataindi$HHRELATE == "adopted/foster child/stepchild"] <- 3
  dataindi$RELATE[dataindi$HHRELATE == "adopted/foster/step child"] <- 3
  
  dataindi$RELATE[dataindi$HHRELATE == "adopted/foster child"] <- 3
  dataindi$RELATE[dataindi$HHRELATE == 11] <- 3
  dataindi$RELATE[dataindi$HHRELATE == "stepson/daughter"] <- 3
  
  
  dataindi$RELATE[dataindi$HHRELATE == "not related"] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "domestic employee (cs)"] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "herdboy (cs)"] <- 5
  dataindi$RELATE[dataindi$HHRELATE %in% c(12, 15,40,41, 42, 97, 98, 99)] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "don't know"] <- 5
  dataindi$RELATE[dataindi$HHRELATE == "dk"] <- 5
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
  
  
  dataindi <- dataindi %>% dplyr::select(-RELATE_2, -RELATE_3, -RELATE_4, -RELATE_5)
  
  dataindi['HHTYPE'] <- 8
  dataindi$HHTYPE[dataindi$HHMEMBERS == 1] <- 1
  dataindi$HHTYPE[(dataindi$HHMEMBERS == 2 & dataindi$Partnerdummy >= 1)] <- 2
  dataindi$HHTYPE[(dataindi$HHMEMBERS >= 3 & dataindi$Partnerdummy >= 1 & dataindi$Childdummy >= 1 & dataindi$Otherdummy == 0)] <- 3
  dataindi$HHTYPE[(dataindi$HHMEMBERS >= 2 & dataindi$Partnerdummy == 0 & dataindi$Childdummy >= 1 & dataindi$Otherdummy == 0)] <- 4
  dataindi$HHTYPE[(dataindi$HHMEMBERS >= 3 & dataindi$Partnerdummy >= 1 & dataindi$Childdummy == 0 & dataindi$Otherdummy >= 1)] <- 5
  dataindi$HHTYPE[(dataindi$HHMEMBERS >= 3 & dataindi$Partnerdummy >= 1 & dataindi$Childdummy >= 1 & dataindi$Otherdummy >= 1)] <- 6
  dataindi$HHTYPE[(dataindi$HHMEMBERS >= 2 & dataindi$Partnerdummy == 0 & dataindi$Childdummy >= 1 & dataindi$Otherdummy >= 1)] <- 7 #moet partnerdummy niet 0 zijn?? 
  
  


  dataindi['PID'] = (1:nrow(dataindi))
  dataindi['HID'] <- as.integer(as.factor(dataindi$HID))
  
  datadhs <- dataindi %>% dplyr::select(HID, WEIGHT, PID, HHSIZECAT, RURAL, HHTYPE, GENDER, AGECAT, EDUCAT, WEALTH, FARMING, RELATE, GDLcode)
  
  
  #checknan = anyNA(datadhs)
  #print(checknan)
  #table(datadhs$WEALTH)
  #table(datadhs$EDUCAT)
  #table(datadhs$ROOFCAT)
  #table(datadhs$WALLCAT)
  #table(datadhs$FLOORCAT)
  #table(datadhs$FARMING)
  
  return(datadhs)
}


# REPLACE SURVEY


similarcountries <- read.csv('similar_countries_5august24.csv')

similarcountries_row <- similarcountries[similarcountries$ISOcode == isocode,]

isocode1 <- similarcountries_row$Replace1
isocode2 <- similarcountries_row$Replace2
isocode3 <- similarcountries_row$Replace3

print(isocode1)
print(isocode2)
print(isocode3)


# Country 1

dhsfilenames1row <- dhsfilenames[dhsfilenames$isocode == isocode1,]$DHSdatasetname

dhsfilename <- paste0(dhsfilenames1row, '.DTA')


dhs <- read.dta13(dhsfilename)

print('DHSdata imported')

dhs <- dhs %>% dplyr::select(hhid, hv001, hv005, hv009, hv024, hv025, hv101, hv104, hv105, hv109, hv244, hv270, hv213, hv214, hv215)

dhs <- dhs %>% rename(HID = hhid, WEIGHT = hv005, DHSCLUST = hv001, HHMEMBERS = hv009, ADMIN1 = hv024, URBANHH = hv025, HHRELATE = hv101, 
                      SEX = hv104, HHAGE = hv105, EDSUMM = hv109, AGLANDYN = hv244,
                      WEALTHQHH = hv270, FLOOR = hv213, WALL = hv214, ROOF = hv215)


dhs['WEIGHT'] = dhs['WEIGHT']/1000000

dhsgdl <- merge_gdl_regions(dhs, isocode1)

reference_sample_1 <- dhs_survey2(dhsgdl) #dhsgdl

reference_sample_1 <- as.data.table(reference_sample_1)
print(colnames(reference_sample_1))
reference_sample_1[, Replacecountry := 1]

print('length ref1')
print(nrow(reference_sample_1))

# Country 2

dhsfilenames1row <- dhsfilenames[dhsfilenames$isocode == isocode2,]$DHSdatasetname

dhsfilename <- paste0(dhsfilenames1row, '.DTA')


dhs <- read.dta13(dhsfilename)

print('DHSdata imported')

dhs <- dhs %>% dplyr::select(hhid, hv001, hv005, hv009, hv024, hv025, hv101, hv104, hv105, hv109, hv244, hv270, hv213, hv214, hv215)

dhs <- dhs %>% rename(HID = hhid, WEIGHT = hv005, DHSCLUST = hv001, HHMEMBERS = hv009, ADMIN1 = hv024, URBANHH = hv025, HHRELATE = hv101, 
                      SEX = hv104, HHAGE = hv105, EDSUMM = hv109, AGLANDYN = hv244,
                      WEALTHQHH = hv270, FLOOR = hv213, WALL = hv214, ROOF = hv215)


dhs['WEIGHT'] = dhs['WEIGHT']/1000000

dhsgdl <- merge_gdl_regions(dhs, isocode2)

reference_sample_2 <- dhs_survey2(dhsgdl) #dhsgdl

reference_sample_2 <- as.data.table(reference_sample_2)
reference_sample_2[, HID := HID+max(reference_sample_1$HID)]
print(colnames(reference_sample_2))
reference_sample_2[, Replacecountry := 2]

print('length ref2')
print(nrow(reference_sample_2))

# Country 3

dhsfilenames1row <- dhsfilenames[dhsfilenames$isocode == isocode3,]$DHSdatasetname

dhsfilename <- paste0(dhsfilenames1row, '.DTA')


dhs <- read.dta13(dhsfilename)

print('DHSdata imported')

dhs <- dhs %>% dplyr::select(hhid, hv001, hv005, hv009, hv024, hv025, hv101, hv104, hv105, hv109, hv244, hv270, hv213, hv214, hv215)

dhs <- dhs %>% rename(HID = hhid, WEIGHT = hv005, DHSCLUST = hv001, HHMEMBERS = hv009, ADMIN1 = hv024, URBANHH = hv025, HHRELATE = hv101, 
                      SEX = hv104, HHAGE = hv105, EDSUMM = hv109, AGLANDYN = hv244,
                      WEALTHQHH = hv270, FLOOR = hv213, WALL = hv214, ROOF = hv215)

dhs['WEIGHT'] = dhs['WEIGHT']/1000000

dhsgdl <- merge_gdl_regions(dhs, isocode3)

reference_sample_3 <- dhs_survey2(dhsgdl) #dhsgdl

reference_sample_3 <- as.data.table(reference_sample_3)
reference_sample_3[, HID := HID+max(reference_sample_2$HID)]
print(colnames(reference_sample_3))
reference_sample_3[, Replacecountry := 3]

print('length ref3')
print(nrow(reference_sample_3))

#jointhead100small <- rbind(jointhead100small_1, jointhead100small_2)
#jointhead100small <- rbind(jointhead100small, jointhead100small_3)

jointhead100small <- rbindlist(list(reference_sample_1, reference_sample_2), use.names=TRUE)
jointhead100small <- rbindlist(list(jointhead100small, reference_sample_3), use.names=TRUE)

rm(reference_sample_1)
rm(reference_sample_2)
rm(reference_sample_3)
gc()

#correct weights

weights1 <- sum(jointhead100small[jointhead100small$Replacecountry == 1,]$WEIGHT)
weights2 <- sum(jointhead100small[jointhead100small$Replacecountry == 2,]$WEIGHT)
weights3 <- sum(jointhead100small[jointhead100small$Replacecountry == 3,]$WEIGHT)


print(weights1)
print(weights2)
print(weights3)

maxweight <- max(weights1, weights2, weights3)

print('maxweight')
print(maxweight)

jointhead100small$Replacecountry[jointhead100small$Replacecountry == 1] <- weights1
jointhead100small$Replacecountry[jointhead100small$Replacecountry == 2] <- weights2
jointhead100small$Replacecountry[jointhead100small$Replacecountry == 3] <- weights3

jointhead100small$Maxweight <- maxweight

jointhead100small$Weightcorrection <- jointhead100small$Maxweight / jointhead100small$Replacecountry
print(table(jointhead100small$Weightcorrection))


jointhead100small$WEIGHT <- jointhead100small$Weightcorrection * jointhead100small$WEIGHT

jointhead100small$WEIGHT <- jointhead100small$WEIGHT * 1000


jointhead100small$WEIGHT <- round(jointhead100small$WEIGHT)

print('maxweight WEIGHT')
print(max(jointhead100small$WEIGHT))

jointhead100small <- as.data.table(jointhead100small)


print('income ref')
print(table(jointhead100small$WEALTH))

print('length jointhead100small 1')
print(nrow(jointhead100small))




############# ORIGINAL DATA #################


dhsfilenames1row <- dhsfilenames[dhsfilenames$isocode == isocode,]$DHSdatasetname

dhsfilename <- paste0(dhsfilenames1row, '.DTA')


dhs <- read.dta13(dhsfilename)

print('DHSdata imported')

dhs <- dhs %>% dplyr::select(hhid, hv001, hv005, hv009, hv024, hv025, hv101, hv104, hv105, hv109, hv244, hv270, hv213, hv214, hv215)

dhs <- dhs %>% rename(HID = hhid, WEIGHT = hv005, DHSCLUST = hv001, HHMEMBERS = hv009, ADMIN1 = hv024, URBANHH = hv025, HHRELATE = hv101, 
                      SEX = hv104, HHAGE = hv105, EDSUMM = hv109, AGLANDYN = hv244,
                      WEALTHQHH = hv270, FLOOR = hv213, WALL = hv214, ROOF = hv215)


# check dhs wealth

dhs['WEIGHT'] = dhs['WEIGHT']/1000000

dhsgdl <- merge_gdl_regions(dhs, isocode)

reference_sample <- dhs_survey2(dhsgdl) #dhsgdl




########################################################################
############################# MARGINALS ################################

#hhsize
hhsize_marg <- read.csv('DHS_observedmarginals_hhsizecat_april24.csv', sep = ',', header = TRUE)
hhsize_marg['iso_code'] <- substr(hhsize_marg$GDLcode, 1, 3)
hhsize_marg <- hhsize_marg[hhsize_marg$iso_code == isocode,]
hhsize_marg <- hhsize_marg %>% dplyr::select(-iso_code)

hhsize_marg <- pivot_longer(hhsize_marg, cols = c(X1, X2, X3, X4, X5, X6), names_to = "HHSIZECAT", values_to = "Frequency")
hhsize_marg$HHSIZECAT <- as.integer(as.factor(hhsize_marg$HHSIZECAT))

hhsize_tib <- as_tibble(hhsize_marg)

#hhtype
hhtype_marg <- read.csv('DHS_observedmarginals_hhtype_april24.csv', sep = ',', header = TRUE)
hhtype_marg['iso_code'] <- substr(hhtype_marg$GDLcode, 1, 3)
hhtype_marg <- hhtype_marg[hhtype_marg$iso_code == isocode,]
hhtype_marg <- hhtype_marg %>% dplyr::select(-iso_code)

hhtype_marg <- pivot_longer(hhtype_marg, cols = c(X1, X2, X3, X4, X5, X6, X7, X8), names_to = "HHTYPE", values_to = "Frequency")
hhtype_marg$HHTYPE <- as.integer(as.factor(hhtype_marg$HHTYPE))

hhtype_tib <- as_tibble(hhtype_marg)

# age
age_marg <- read.csv('DHS_observedmarginals_age_april24.csv', sep = ',', header = TRUE)
age_marg['iso_code'] <- substr(age_marg$GDLcode, 1, 3)
age_marg <- age_marg[age_marg$iso_code == isocode,]
age_marg <- age_marg %>% dplyr::select(-iso_code)

age_marg <- pivot_longer(age_marg, cols = c(X1, X2, X3, X4, X5, X6, X7, X8), names_to = "AGECAT", values_to = "Frequency")
age_marg$AGECAT <- as.integer(as.factor(age_marg$AGECAT))

age_tib <- as_tibble(age_marg)

# edu
edu_marg <- read.csv('DHS_observedmarginals_edu_april24.csv', sep = ',', header = TRUE)
edu_marg['iso_code'] <- substr(edu_marg$GDLcode, 1, 3)
edu_marg <- edu_marg[edu_marg$iso_code == isocode,]
edu_marg <- edu_marg %>% dplyr::select(-iso_code)

edu_marg <- pivot_longer(edu_marg, cols = c(X1, X2, X3, X4, X5), names_to = "EDUCAT", values_to = "Frequency")
edu_marg$EDUCAT <- as.integer(as.factor(edu_marg$EDUCAT))

edu_tib <- as_tibble(edu_marg)

# gender
gender_marg <- read.csv('DHS_observedmarginals_gender_april24.csv', sep = ',', header = TRUE)
gender_marg['iso_code'] <- substr(gender_marg$GDLcode, 1, 3)
gender_marg <- gender_marg[gender_marg$iso_code == isocode,]
gender_marg <- gender_marg %>% dplyr::select(-iso_code)

gender_marg <- pivot_longer(gender_marg, cols = c(X1, X2), names_to = "GENDER", values_to = "Frequency")
gender_marg$GENDER <- as.integer(as.factor(gender_marg$GENDER))-1

gender_tib <- as_tibble(gender_marg)

# wealth
wealth_marg <- read.csv('DHS_observedmarginals_wealth_april24.csv', sep = ',', header = TRUE)
wealth_marg['iso_code'] <- substr(wealth_marg$GDLcode, 1, 3)
wealth_marg <- wealth_marg[wealth_marg$iso_code == isocode,]
wealth_marg <- wealth_marg %>% dplyr::select(-iso_code)

wealth_marg <- pivot_longer(wealth_marg, cols = c(X1, X2, X3, X4, X5), names_to = "WEALTH", values_to = "Frequency")
wealth_marg$WEALTH <- as.integer(as.factor(wealth_marg$WEALTH))

wealth_tib <- as_tibble(wealth_marg)


# rural
rural_marg <- population[population$iso_code == isocode,]
rural_marg <- rural_marg %>% dplyr::select(GDLcode, Ruralpop, Urbanpop)
rural_marg <- pivot_longer(rural_marg, cols = c(Ruralpop, Urbanpop), names_to = "RURAL", values_to = "Frequency")
rural_marg$RURAL <- abs(as.integer(as.factor(rural_marg$RURAL))-2) #1 is rural and 0 is urban

rural_tib <- as_tibble(rural_marg)


# NIET OP MARGINALS RUNNEN ALS ZE MISSEN. if statement maken. Op -1 zetten als roof of floor ofzo mist. 

# in de marginals zitten altijd alle opties. 
# check of die ook in reference sample zitten. 
rurallevelssample <- unique(reference_sample$RURAL)
hhtypelevelssample <- unique(reference_sample$HHTYPE)
hhsizelevelssample <- unique(reference_sample$HHSIZECAT)
wealthlevelssample <- unique(reference_sample$WEALTH)
agelevelssample <- unique(reference_sample$AGECAT)
edulevelssample <- unique(reference_sample$EDUCAT)
genderlevelssample <- unique(reference_sample$GENDER)


GEOLEV1regions <- unique(rural_tib$GDLcode)

nr_individuals_per_regio <- c()

syn_pop <- data.frame()


for (regnr in GEOLEV1regions){
  
  print(regnr)
  
  popsize <- population[population$GDLcode == regnr,]$Population
  
  rural_tib1reg <- rural_tib[rural_tib$GDLcode == regnr,]
  rural_tib1reg <- rural_tib1reg %>% dplyr::select(-GDLcode)
  
  rural_tib1reg <- rural_tib1reg[rural_tib1reg$RURAL %in% rurallevelssample,]
  
  print('rural_tib1reg')
  print(rural_tib1reg)
  
  hhtype_tib1reg <- hhtype_tib[hhtype_tib$GDLcode == regnr,]
  hhtype_tib1reg <- hhtype_tib1reg %>% dplyr::select(-GDLcode)
  
  hhtype_tib1reg <- hhtype_tib1reg[hhtype_tib1reg$HHTYPE %in% hhtypelevelssample,]
  
  print('hhtype_tib1reg')
  print(hhtype_tib1reg)
  
  hhsize_tib1reg <- hhsize_tib[hhsize_tib$GDLcode == regnr,]
  hhsize_tib1reg <- hhsize_tib1reg %>% dplyr::select(-GDLcode)
  
  hhsize_tib1reg <- hhsize_tib1reg[hhsize_tib1reg$HHSIZECAT %in% hhsizelevelssample,]
  
  print('hhsize_tib1reg')
  print(hhsize_tib1reg)
  
  wealth_tib1reg <- wealth_tib[wealth_tib$GDLcode == regnr,]
  wealth_tib1reg <- wealth_tib1reg %>% dplyr::select(-GDLcode)
  
  wealth_tib1reg <- wealth_tib1reg[wealth_tib1reg$WEALTH %in% wealthlevelssample,]
  
  print('wealth_tib1reg')
  print(wealth_tib1reg)
  
  age_tib1reg <- age_tib[age_tib$GDLcode == regnr,]
  age_tib1reg <- age_tib1reg %>% dplyr::select(-GDLcode)
  
  age_tib1reg <- age_tib1reg[age_tib1reg$AGECAT %in% agelevelssample,]
  
  print('age_tib1reg')
  print(age_tib1reg)
  
  edu_tib1reg <- edu_tib[edu_tib$GDLcode == regnr,]
  edu_tib1reg <- edu_tib1reg %>% dplyr::select(-GDLcode)
  
  edu_tib1reg <- edu_tib1reg[edu_tib1reg$EDUCAT %in% edulevelssample,]
  
  print('edu_tib1reg')
  print(edu_tib1reg)
  
  gender_tib1reg <- gender_tib[gender_tib$GDLcode == regnr,]
  gender_tib1reg <- gender_tib1reg %>% dplyr::select(-GDLcode)
  
  gender_tib1reg <- gender_tib1reg[gender_tib1reg$GENDER %in% genderlevelssample,]
  
  print('gender_tib1reg')
  print(gender_tib1reg)
  
  
  group_control <- list()
  individual_control <- list(rural_tib1reg, hhtype_tib1reg, hhsize_tib1reg, wealth_tib1reg, 
                             age_tib1reg, edu_tib1reg, gender_tib1reg)
  
  names(group_control) <- c() 
  names(individual_control) <- c('RURAL','HHTYPE', 'HHSIZECAT', 'WEALTH', 'AGECAT',
                                 'EDUCAT', 'GENDER')
  
  
  fitting_problem <- ml_problem(
    ref_sample = jointhead100small, 
    controls = list(
      individual = individual_control,
      group = group_control
    ), prior_weights = jointhead100small$WEIGHT,
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
  syn_pop_reg$FARMING <- as.integer(syn_pop_reg$FARMING)
  syn_pop_reg$WEALTH <- as.integer(syn_pop_reg$WEALTH)
  syn_pop_reg$HID <- as.integer(syn_pop_reg$HID)
  syn_pop_reg$RELATE <- as.integer(syn_pop_reg$RELATE)
  syn_pop_reg$HHTYPE <- as.integer(syn_pop_reg$HHTYPE)
  
  
  print(nrow(syn_pop_reg))
  
  nr_individuals_per_regio <- append(nr_individuals_per_regio, nrow(syn_pop_reg))
  
  syn_pop_reg <- syn_pop_reg %>% dplyr::select(-GDLcode)
  syn_pop_reg <- syn_pop_reg %>% add_column(GDLcode = regnr)
  gc()
  syn_pop <- rbindlist(list(syn_pop, syn_pop_reg), use.names=TRUE)
  rm(syn_pop_reg)
  
  
  gc()
  
}


df_nr_individuals_per_region <- data.frame(GDLcode = GEOLEV1regions, Nr_individuals = nr_individuals_per_regio)

nr_individuals_per_regio_share <- nr_individuals_per_regio/sum(nr_individuals_per_regio) 

gc()



# SPATIAL ERROR

freq11 <- data.frame(xtabs(WEIGHT ~ HHTYPE + HHSIZECAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
freq12 <- data.frame(xtabs(WEIGHT ~ HHTYPE + WEALTH + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
freq13 <- data.frame(xtabs(WEIGHT ~ HHTYPE + AGECAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
freq14 <- data.frame(xtabs(WEIGHT ~ HHTYPE + EDUCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
freq15 <- data.frame(xtabs(WEIGHT ~ HHTYPE + GENDER + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))

freq20 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + WEALTH + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
freq21 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + AGECAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
freq22 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + EDUCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
freq23 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + GENDER + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))

freq28 <- data.frame(xtabs(WEIGHT ~ WEALTH + AGECAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
freq29 <- data.frame(xtabs(WEIGHT ~ WEALTH + EDUCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
freq30 <- data.frame(xtabs(WEIGHT ~ WEALTH + GENDER + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))

freq35 <- data.frame(xtabs(WEIGHT ~ AGECAT + EDUCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
freq36 <- data.frame(xtabs(WEIGHT ~ AGECAT + GENDER + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))

freq41 <- data.frame(xtabs(WEIGHT ~ EDUCAT + GENDER + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))


freq11tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + HHSIZECAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
freq12tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + WEALTH, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
freq13tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + AGECAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
freq14tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + EDUCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
freq15tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + GENDER, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)

freq20tot <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + WEALTH, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
freq21tot <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + AGECAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
freq22tot <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + EDUCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
freq23tot <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + GENDER, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)

freq28tot <- data.frame(xtabs(WEIGHT ~ WEALTH + AGECAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
freq29tot <- data.frame(xtabs(WEIGHT ~ WEALTH + EDUCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
freq30tot <- data.frame(xtabs(WEIGHT ~ WEALTH + GENDER, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)

freq35tot <- data.frame(xtabs(WEIGHT ~ AGECAT + EDUCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
freq36tot <- data.frame(xtabs(WEIGHT ~ AGECAT + GENDER, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)

freq41tot <- data.frame(xtabs(WEIGHT ~ EDUCAT + GENDER, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)



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
freq20tot <- rep(freq20tot, length(nr_individuals_per_regio))
freq20tot <- freq20tot * rep(nr_individuals_per_regio_share, each = length(freq20tot)/length(nr_individuals_per_regio))
freq21tot <- rep(freq21tot, length(nr_individuals_per_regio))
freq21tot <- freq21tot * rep(nr_individuals_per_regio_share, each = length(freq21tot)/length(nr_individuals_per_regio))
freq22tot <- rep(freq22tot, length(nr_individuals_per_regio))
freq22tot <- freq22tot * rep(nr_individuals_per_regio_share, each = length(freq22tot)/length(nr_individuals_per_regio))
freq23tot <- rep(freq23tot, length(nr_individuals_per_regio))
freq23tot <- freq23tot * rep(nr_individuals_per_regio_share, each = length(freq23tot)/length(nr_individuals_per_regio))
freq28tot <- rep(freq28tot, length(nr_individuals_per_regio))
freq28tot <- freq28tot * rep(nr_individuals_per_regio_share, each = length(freq28tot)/length(nr_individuals_per_regio))
freq29tot <- rep(freq29tot, length(nr_individuals_per_regio))
freq29tot <- freq29tot * rep(nr_individuals_per_regio_share, each = length(freq29tot)/length(nr_individuals_per_regio))
freq30tot <- rep(freq30tot, length(nr_individuals_per_regio))
freq30tot <- freq30tot * rep(nr_individuals_per_regio_share, each = length(freq30tot)/length(nr_individuals_per_regio))
freq35tot <- rep(freq35tot, length(nr_individuals_per_regio))
freq35tot <- freq35tot * rep(nr_individuals_per_regio_share, each = length(freq35tot)/length(nr_individuals_per_regio))
freq36tot <- rep(freq36tot, length(nr_individuals_per_regio))
freq36tot <- freq36tot * rep(nr_individuals_per_regio_share, each = length(freq36tot)/length(nr_individuals_per_regio))
freq41tot <- rep(freq41tot, length(nr_individuals_per_regio))
freq41tot <- freq41tot * rep(nr_individuals_per_regio_share, each = length(freq41tot)/length(nr_individuals_per_regio))

# MARGINAL ERROR
# rural, hhtype, hhsize, wealth, age, edu, gender, farming, floor, wall, roof

#marg1 <- data.frame(xtabs(WEIGHT ~ RURAL + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
#names(marg1)[names(marg1) == 'Freq'] <- 'Prob_RURAL'

marg2 <- data.frame(xtabs(WEIGHT ~ HHTYPE + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
names(marg2)[names(marg2) == 'Freq'] <- 'Prob_HHTYPE'

marg3 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
names(marg3)[names(marg3) == 'Freq'] <- 'Prob_HHSIZECAT'

marg4 <- data.frame(xtabs(WEIGHT ~ WEALTH + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
names(marg4)[names(marg4) == 'Freq'] <- 'Prob_WEALTH'

marg5 <- data.frame(xtabs(WEIGHT ~ AGECAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
names(marg5)[names(marg5) == 'Freq'] <- 'Prob_AGECAT'

marg6 <- data.frame(xtabs(WEIGHT ~ EDUCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
names(marg6)[names(marg6) == 'Freq'] <- 'Prob_EDUCAT'

marg7 <- data.frame(xtabs(WEIGHT ~ GENDER + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
names(marg7)[names(marg7) == 'Freq'] <- 'Prob_GENDER'



#rural, hhtype, hhsize, wealth, age, edu, gender
joinmarg_forerror <- function(freq1, marg1, marg2, nr_individuals_per_regio_share){
  
  freq1 <- left_join(freq1, marg1, by = c(colnames(freq1)[1], 'GDLcode'))
  freq1 <- left_join(freq1, marg2, by = c(colnames(freq1)[2], 'GDLcode'))
  freq1[is.na(freq1)] <- 0
  freq1$Marg_freq <- freq1[,5] * freq1[,6]
  sumfreq_perregio <- (freq1 %>% group_by(GDLcode) %>% summarize(sumfreq_perregio = sum(Marg_freq)))$sumfreq_perregio
  correctfactor <- nr_individuals_per_regio_share/sumfreq_perregio
  freq1$Marg_freq <- freq1$Marg_freq * rep(correctfactor, each = nrow(freq1)/length(nr_individuals_per_regio_share))
  
  return(freq1)
}





freq11 <- joinmarg_forerror(freq11, marg2, marg3, nr_individuals_per_regio_share)
freq12 <- joinmarg_forerror(freq12, marg2, marg4, nr_individuals_per_regio_share)
freq13 <- joinmarg_forerror(freq13, marg2, marg5, nr_individuals_per_regio_share)
freq14 <- joinmarg_forerror(freq14, marg2, marg6, nr_individuals_per_regio_share)
freq15 <- joinmarg_forerror(freq15, marg2, marg7, nr_individuals_per_regio_share)

freq20 <- joinmarg_forerror(freq20, marg3, marg4, nr_individuals_per_regio_share)
freq21 <- joinmarg_forerror(freq21, marg3, marg5, nr_individuals_per_regio_share)
freq22 <- joinmarg_forerror(freq22, marg3, marg6, nr_individuals_per_regio_share)
freq23 <- joinmarg_forerror(freq23, marg3, marg7, nr_individuals_per_regio_share)

freq28 <- joinmarg_forerror(freq28, marg4, marg5, nr_individuals_per_regio_share)
freq29 <- joinmarg_forerror(freq29, marg4, marg6, nr_individuals_per_regio_share)
freq30 <- joinmarg_forerror(freq30, marg4, marg7, nr_individuals_per_regio_share)

freq35 <- joinmarg_forerror(freq35, marg5, marg6, nr_individuals_per_regio_share)
freq36 <- joinmarg_forerror(freq36, marg5, marg7, nr_individuals_per_regio_share)

freq41 <- joinmarg_forerror(freq41, marg6, marg7, nr_individuals_per_regio_share)


freq11['National'] <- freq11tot
freq12['National'] <- freq12tot
freq13['National'] <- freq13tot
freq14['National'] <- freq14tot
freq15['National'] <- freq15tot
freq20['National'] <- freq20tot
freq21['National'] <- freq21tot
freq22['National'] <- freq22tot
freq23['National'] <- freq23tot
freq28['National'] <- freq28tot
freq29['National'] <- freq29tot
freq30['National'] <- freq30tot
freq35['National'] <- freq35tot
freq36['National'] <- freq36tot
freq41['National'] <- freq41tot


freq11 <- freq11 %>% dplyr::select(Freq, Marg_freq, National)
freq12 <- freq12 %>% dplyr::select(Freq, Marg_freq, National)
freq13 <- freq13 %>% dplyr::select(Freq, Marg_freq, National)
freq14 <- freq14 %>% dplyr::select(Freq, Marg_freq, National)
freq15 <- freq15 %>% dplyr::select(Freq, Marg_freq, National)
freq20 <- freq20 %>% dplyr::select(Freq, Marg_freq, National)
freq21 <- freq21 %>% dplyr::select(Freq, Marg_freq, National)
freq22 <- freq22 %>% dplyr::select(Freq, Marg_freq, National)
freq23 <- freq23 %>% dplyr::select(Freq, Marg_freq, National)
freq28 <- freq28 %>% dplyr::select(Freq, Marg_freq, National)
freq29 <- freq29 %>% dplyr::select(Freq, Marg_freq, National)
freq30 <- freq30 %>% dplyr::select(Freq, Marg_freq, National)
freq35 <- freq35 %>% dplyr::select(Freq, Marg_freq, National)
freq36 <- freq36 %>% dplyr::select(Freq, Marg_freq, National)
freq41 <- freq41 %>% dplyr::select(Freq, Marg_freq, National)





df_allfreqs <- bind_rows(freq11, freq12, freq13, freq14, freq15,
                         freq20, freq21, freq22, freq23, freq28, freq29,
                         freq30, freq35, freq36, 
                         freq41)


names(df_allfreqs)[names(df_allfreqs) == 'Freq'] <- 'Data_freq'

df_allfreqs['Sq_error_survey_national'] <- (df_allfreqs$Data_freq - df_allfreqs$National)^2
df_allfreqs['Sq_error_survey_marginal'] <- (df_allfreqs$Data_freq - df_allfreqs$Marg_freq)^2

# Synthetic combinations. 


freq11synth <- data.frame(xtabs( ~ HHTYPE + HHSIZECAT + GDLcode, data = syn_pop)/nrow(syn_pop))
freq12synth <- data.frame(xtabs( ~ HHTYPE + WEALTH + GDLcode, data = syn_pop)/nrow(syn_pop))
freq13synth <- data.frame(xtabs( ~ HHTYPE + AGECAT + GDLcode, data = syn_pop)/nrow(syn_pop))
freq14synth <- data.frame(xtabs( ~ HHTYPE + EDUCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
freq15synth <- data.frame(xtabs( ~ HHTYPE + GENDER + GDLcode, data = syn_pop)/nrow(syn_pop))

freq20synth <- data.frame(xtabs( ~ HHSIZECAT + WEALTH + GDLcode, data = syn_pop)/nrow(syn_pop))
freq21synth <- data.frame(xtabs( ~ HHSIZECAT + AGECAT + GDLcode, data = syn_pop)/nrow(syn_pop))
freq22synth <- data.frame(xtabs( ~ HHSIZECAT + EDUCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
freq23synth <- data.frame(xtabs( ~ HHSIZECAT + GENDER + GDLcode, data = syn_pop)/nrow(syn_pop))


freq28synth <- data.frame(xtabs( ~ WEALTH + AGECAT + GDLcode, data = syn_pop)/nrow(syn_pop))
freq29synth <- data.frame(xtabs( ~ WEALTH + EDUCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
freq30synth <- data.frame(xtabs( ~ WEALTH + GENDER + GDLcode, data = syn_pop)/nrow(syn_pop))


freq35synth <- data.frame(xtabs( ~ AGECAT + EDUCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
freq36synth <- data.frame(xtabs( ~ AGECAT + GENDER + GDLcode, data = syn_pop)/nrow(syn_pop))


freq41synth <- data.frame(xtabs( ~ EDUCAT + GENDER + GDLcode, data = syn_pop)/nrow(syn_pop))



names(freq11synth)[names(freq11synth) == 'Freq'] <- 'Freq_synth'
names(freq12synth)[names(freq12synth) == 'Freq'] <- 'Freq_synth'
names(freq13synth)[names(freq13synth) == 'Freq'] <- 'Freq_synth'
names(freq14synth)[names(freq14synth) == 'Freq'] <- 'Freq_synth'
names(freq15synth)[names(freq15synth) == 'Freq'] <- 'Freq_synth'


names(freq20synth)[names(freq20synth) == 'Freq'] <- 'Freq_synth'
names(freq21synth)[names(freq21synth) == 'Freq'] <- 'Freq_synth'
names(freq22synth)[names(freq22synth) == 'Freq'] <- 'Freq_synth'
names(freq23synth)[names(freq23synth) == 'Freq'] <- 'Freq_synth'

names(freq28synth)[names(freq28synth) == 'Freq'] <- 'Freq_synth'
names(freq29synth)[names(freq29synth) == 'Freq'] <- 'Freq_synth'
names(freq30synth)[names(freq30synth) == 'Freq'] <- 'Freq_synth'

names(freq35synth)[names(freq35synth) == 'Freq'] <- 'Freq_synth'
names(freq36synth)[names(freq36synth) == 'Freq'] <- 'Freq_synth'

names(freq41synth)[names(freq41synth) == 'Freq'] <- 'Freq_synth'



df_synthfreqs <- bind_rows(freq11synth, freq12synth, freq13synth, freq14synth, freq15synth,
                           freq20synth, freq21synth, freq22synth, freq23synth, freq28synth, freq29synth,
                           freq30synth, freq35synth, freq36synth, freq41synth)

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



filename_dferrors <- paste0("synth_errors_estsurvey_DHS_aug24_", isocode, ".csv") #was oct, nov. 

write.table(DF_errors, filename_dferrors, row.names = FALSE, sep = ',')

print('saved output')














