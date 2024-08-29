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








# IMPORT SURVEY DATA 

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
  dataindi <- dhs %>% dplyr::select(HID, WEIGHT, URBANHH, SEX, HHAGE, EDSUMM, HHRELATE, HHMEMBERS, WEALTHQHH) #changed hv106 to hv109, because more categories in hv109.
  
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
  
  datadhs <- dataindi %>% dplyr::select(HID, WEIGHT, PID, HHSIZECAT, RURAL, HHTYPE, GENDER, AGECAT, EDUCAT, WEALTH, RELATE)
  
  
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




similarcountries <- read.csv('similar_countries_5august24.csv')

similarcountries_row <- similarcountries[similarcountries$ISOcode == isocode,]

isocode1 <- similarcountries_row$Replace1
isocode2 <- similarcountries_row$Replace2
isocode3 <- similarcountries_row$Replace3



# Country 1

dhsfilenames1row <- dhsfilenames[dhsfilenames$isocode == isocode1,]$DHSdatasetname

dhsfilename <- paste0(dhsfilenames1row, '.DTA')


dhs <- read.dta13(dhsfilename)

print('DHSdata imported')

dhs <- dhs %>% dplyr::select(hhid, hv001, hv005, hv009, hv024, hv025, hv101, hv104, hv105, hv109, hv270)

dhs <- dhs %>% rename(HID = hhid, WEIGHT = hv005, DHSCLUST = hv001, HHMEMBERS = hv009, ADMIN1 = hv024, URBANHH = hv025, HHRELATE = hv101, 
                      SEX = hv104, HHAGE = hv105, EDSUMM = hv109, WEALTHQHH = hv270)


dhs['WEIGHT'] = dhs['WEIGHT']/1000000


reference_sample_1 <- dhs_survey2(dhs) #dhsgdl

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

dhs <- dhs %>% dplyr::select(hhid, hv001, hv005, hv009, hv024, hv025, hv101, hv104, hv105, hv109, hv270)

dhs <- dhs %>% rename(HID = hhid, WEIGHT = hv005, DHSCLUST = hv001, HHMEMBERS = hv009, ADMIN1 = hv024, URBANHH = hv025, HHRELATE = hv101, 
                      SEX = hv104, HHAGE = hv105, EDSUMM = hv109, WEALTHQHH = hv270)


dhs['WEIGHT'] = dhs['WEIGHT']/1000000

reference_sample_2 <- dhs_survey2(dhs) #dhsgdl

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

dhs <- dhs %>% dplyr::select(hhid, hv001, hv005, hv009, hv024, hv025, hv101, hv104, hv105, hv109, hv270)

dhs <- dhs %>% rename(HID = hhid, WEIGHT = hv005, DHSCLUST = hv001, HHMEMBERS = hv009, ADMIN1 = hv024, URBANHH = hv025, HHRELATE = hv101, 
                      SEX = hv104, HHAGE = hv105, EDSUMM = hv109, WEALTHQHH = hv270)

dhs['WEIGHT'] = dhs['WEIGHT']/1000000

reference_sample_3 <- dhs_survey2(dhs) #dhsgdl

reference_sample_3 <- as.data.table(reference_sample_3)
reference_sample_3[, HID := HID+max(reference_sample_2$HID)]
print(colnames(reference_sample_3))
reference_sample_3[, Replacecountry := 3]

print('length ref3')
print(nrow(reference_sample_3))


if (isocode != 'SYR'){

  jointhead100small <- rbindlist(list(reference_sample_1, reference_sample_2), use.names=TRUE)
  jointhead100small <- rbindlist(list(jointhead100small, reference_sample_3), use.names=TRUE)
  
  
  #correct weights
  
  weights1 <- sum(jointhead100small[jointhead100small$Replacecountry == 1,]$WEIGHT)
  weights2 <- sum(jointhead100small[jointhead100small$Replacecountry == 2,]$WEIGHT)
  weights3 <- sum(jointhead100small[jointhead100small$Replacecountry == 3,]$WEIGHT)
  
  maxweight <- max(weights1, weights2, weights3)
  
  
  jointhead100small$Replacecountry[jointhead100small$Replacecountry == 1] <- weights1
  jointhead100small$Replacecountry[jointhead100small$Replacecountry == 2] <- weights2
  jointhead100small$Replacecountry[jointhead100small$Replacecountry == 3] <- weights3
  
  jointhead100small$Maxweight <- maxweight
  
  jointhead100small$Weightcorrection <- jointhead100small$Maxweight / jointhead100small$Replacecountry
  
  jointhead100small$WEIGHT <- jointhead100small$Weightcorrection * jointhead100small$WEIGHT
  
  
  jointhead100small$WEIGHT <- jointhead100small$WEIGHT * 1000
  
  jointhead100small$WEIGHT <- round(jointhead100small$WEIGHT)
  
}

if (isocode == 'SYR'){
  print('1 ref sample')
  jointhead100small <- copy(reference_sample_2)
}
  
  
rm(reference_sample_1)
rm(reference_sample_2)
rm(reference_sample_3)
gc()




jointhead100small <- as.data.table(jointhead100small)



print('income ref')
print(table(jointhead100small$WEALTH))

print('length jointhead100small 1')
print(nrow(jointhead100small))


jointhead100small <- as.data.table(jointhead100small)


print('sum weights')
print(sum(jointhead100small$WEIGHT))
########################################################################
############################# MARGINALS ################################

#hhsize
hhsize_marg <- read.csv('Est_marginals_HHSIZECAT_june24.csv', sep = ',', header = TRUE)
hhsize_marg <- hhsize_marg[hhsize_marg$ISOcode == isocode,]

hhsize_marg <- hhsize_marg %>% 
  pivot_longer(
    cols = X1est:X6est,   
    names_to = "HHSIZECAT",    
    values_to = "Frequency"       
  )

hhsize_marg$HHSIZECAT <- as.integer(as.factor(hhsize_marg$HHSIZECAT))

hhsize_marg <- hhsize_marg %>% dplyr::select(GDLcode, HHSIZECAT, Frequency)

hhsize_tib <- as_tibble(hhsize_marg)

#hhtype
hhtype_marg <- read.csv('Est_marginals_HHTYPE_june24.csv', sep = ',', header = TRUE)
hhtype_marg <- hhtype_marg[hhtype_marg$ISOcode == isocode,]

hhtype_marg <- hhtype_marg %>% 
  pivot_longer(
    cols = X1est:X8est,   
    names_to = "HHTYPE",    
    values_to = "Frequency"       
  )

hhtype_marg$HHTYPE <- as.integer(as.factor(hhtype_marg$HHTYPE))

hhtype_marg <- hhtype_marg %>% dplyr::select(GDLcode, HHTYPE, Frequency)

hhtype_tib <- as_tibble(hhtype_marg)

# age
age_marg <- read.csv('Est_marginals_AGECAT_june24.csv', sep = ',', header = TRUE)
age_marg <- age_marg[age_marg$ISOcode == isocode,]


age_marg <- age_marg %>% 
  pivot_longer(
    cols = X1est:X8est,   
    names_to = "AGECAT",    
    values_to = "Frequency"       
  )

age_marg$AGECAT <- as.integer(as.factor(age_marg$AGECAT))

age_marg <- age_marg %>% dplyr::select(GDLcode, AGECAT, Frequency)

age_tib <- as_tibble(age_marg)

# edu
edu_marg <- read.csv('Est_marginals_EDUCAT_june24.csv', sep = ',', header = TRUE)
edu_marg <- edu_marg[edu_marg$ISOcode == isocode,]

edu_marg <- edu_marg %>% 
  pivot_longer(
    cols = X1est:X5est,   
    names_to = "EDUCAT",    
    values_to = "Frequency"       
  )

edu_marg$EDUCAT <- as.integer(as.factor(edu_marg$EDUCAT))

edu_marg <- edu_marg %>% dplyr::select(GDLcode, EDUCAT, Frequency)

edu_tib <- as_tibble(edu_marg)

# gender
gender_marg <- read.csv('Est_marginals_GENDER_june24.csv', sep = ',', header = TRUE)
gender_marg <- gender_marg[gender_marg$ISOcode == isocode,]

gender_marg <- gender_marg %>% 
  pivot_longer(
    cols = X1est:X2est,   
    names_to = "GENDER",    
    values_to = "Frequency"       
  )

gender_marg$GENDER <- as.integer(as.factor(gender_marg$GENDER))-1

gender_marg <- gender_marg %>% dplyr::select(GDLcode, GENDER, Frequency)

gender_tib <- as_tibble(gender_marg)

# wealth
wealth_marg <- read.csv('Est_marginals_INCOME_june24.csv', sep = ',', header = TRUE)
wealth_marg <- wealth_marg[wealth_marg$ISOcode == isocode,]


wealth_marg <- wealth_marg %>% 
  pivot_longer(
    cols = X1est:X5est,   
    names_to = "WEALTH",    
    values_to = "Frequency"       
  )
wealth_marg$WEALTH <- as.integer(as.factor(wealth_marg$WEALTH))

wealth_marg <- wealth_marg %>% dplyr::select(GDLcode, WEALTH, Frequency)

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
rurallevelssample <- unique(jointhead100small$RURAL)
hhtypelevelssample <- unique(jointhead100small$HHTYPE)
hhsizelevelssample <- unique(jointhead100small$HHSIZECAT)
wealthlevelssample <- unique(jointhead100small$WEALTH)
agelevelssample <- unique(jointhead100small$AGECAT)
edulevelssample <- unique(jointhead100small$EDUCAT)
genderlevelssample <- unique(jointhead100small$GENDER)


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
  
  print('length syn_pop_reg')
  print(syn_pop_reg)
  
  syn_pop_reg$HHSIZECAT <- as.integer(syn_pop_reg$HHSIZECAT)
  syn_pop_reg$RURAL <- as.integer(syn_pop_reg$RURAL)
  syn_pop_reg$GENDER <- as.integer(syn_pop_reg$GENDER)
  syn_pop_reg$AGECAT <- as.integer(syn_pop_reg$AGECAT)
  syn_pop_reg$EDUCAT <- as.integer(syn_pop_reg$EDUCAT)
  syn_pop_reg$FARMING <- -1
  syn_pop_reg$WEALTH <- as.integer(syn_pop_reg$WEALTH)
  syn_pop_reg$INCOME <- -1
  syn_pop_reg$FLOORCAT <- -1
  syn_pop_reg$WALLCAT <- -1
  syn_pop_reg$ROOFCAT <- -1
  syn_pop_reg$HID <- as.integer(syn_pop_reg$HID)
  syn_pop_reg$RELATE <- as.integer(syn_pop_reg$RELATE)
  syn_pop_reg$HHTYPE <- as.integer(syn_pop_reg$HHTYPE)
  
  syn_pop_reg$INCOME <- as.integer(syn_pop_reg$INCOME)
  syn_pop_reg$FLOORCAT <- as.integer(syn_pop_reg$FLOORCAT)
  syn_pop_reg$WALLCAT <- as.integer(syn_pop_reg$WALLCAT)
  syn_pop_reg$FARMING <- as.integer(syn_pop_reg$FARMING)
  syn_pop_reg$ROOFCAT <- as.integer(syn_pop_reg$ROOFCAT)
  syn_pop_reg$SOURCE <- 6
  syn_pop_reg$SOURCE <- as.integer(syn_pop_reg$SOURCE)
  
  # HID RELATE INCOME WEALTH RURAL AGECAT GENDER EDUCAT HHTYPE HHSIZECAT FARMING FLOORCAT WALLCAT ROOFCAT DATA SOURCE
  
  
  name = paste0('synthpop_unknown2_DHS_', regnr, '.dat') 
  
  con = file(name, "wb")
  
  writeBin(c(syn_pop_reg$HID, syn_pop_reg$RELATE, syn_pop_reg$INCOME, syn_pop_reg$WEALTH, syn_pop_reg$RURAL, syn_pop_reg$AGECAT, syn_pop_reg$GENDER, 
             syn_pop_reg$EDUCAT, syn_pop_reg$HHTYPE, syn_pop_reg$HHSIZECAT, syn_pop_reg$FARMING, syn_pop_reg$FLOORCAT, syn_pop_reg$WALLCAT, syn_pop_reg$ROOFCAT, syn_pop_reg$SOURCE), con)
  
  
  close(con)
  
  
  print(nrow(syn_pop_reg))
  
  nr_individuals_per_regio <- append(nr_individuals_per_regio, nrow(syn_pop_reg))
  
  #syn_pop <- rbindlist(list(syn_pop, syn_pop_reg), use.names=TRUE)
  rm(syn_pop_reg)
  
  
  gc()
  
}


DF_nr_individuals_per_region <- data.frame(GDLcode = GEOLEV1regions, Nr_individuals = nr_individuals_per_regio)

filename_indiperregion <- paste0("Individuals_per_region_DHS_", isocode, ".csv")
write.table(DF_nr_individuals_per_region, filename_indiperregion, row.names = FALSE, sep = ',')











