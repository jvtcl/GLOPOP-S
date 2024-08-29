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


dhsfilenames <- read.csv('dhsdatafiles.csv', sep = ';')

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
    dhs$GDLcode[dhs$ADMIN1 == 'punjab'] <- 'PAKr101'
    dhs$GDLcode[dhs$ADMIN1 == 'sindh'] <- 'PAKr102'
    dhs$GDLcode[dhs$ADMIN1 == 'kpk'] <- 'PAKr103'
    dhs$GDLcode[dhs$ADMIN1 == 'balochistan'] <- 'PAKr104'
    dhs$GDLcode[dhs$ADMIN1 == 'gb'] <- 'PAKr106'
    dhs$GDLcode[dhs$ADMIN1 == 'ict'] <- 'PAKr105'
    dhs$GDLcode[dhs$ADMIN1 == 'ajk'] <- 'PAKr107'
    dhs$GDLcode[dhs$ADMIN1 == 'fata'] <- 'PAKr108'
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
    
    # regio's in GDL zijn groter dan in DHS
    
    highlands <- c(110, 111, 108, 121, 122, 107, 109)
    islands <- c(120, 118, 116, 117, 119)
    momase <- c(114, 113, 112, 115)
    southern <- c(103, 101, 102, 106, 104, 105)
    
    pngr101 <- c('highlands region')
    pngr102 <- c('islands region')
    pngr103 <- c('momase region')
    pngr104 <- c('southern region')
    
    dhs['GDLcode'] <- ''
    
    dhs$GDLcode[dhs$ADMIN1 %in% pngr101] <- 'PNGr107'
    dhs$GDLcode[dhs$ADMIN1 %in% pngr102] <- 'PNGr116'
    dhs$GDLcode[dhs$ADMIN1 %in% pngr103] <- 'PNGr112'
    dhs$GDLcode[dhs$ADMIN1 %in% pngr104] <- 'PNGr101'
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
  
  
  
  floorcategories <- levels(dataindi$FLOOR)
  floorcategories <- tolower(floorcategories)
  dataindi$FLOOR <- tolower(dataindi$FLOOR)
  if ('natural' %in% floorcategories){
    begin_pos_nat <- grep('natural', floorcategories, fixed = FALSE)
    end_pos_nat <- grep('rudimentary', floorcategories, fixed = FALSE)
    naturalfloor <- floorcategories[begin_pos_nat:(end_pos_nat-1)]
    
    begin_pos_rud <- grep('rudimentary', floorcategories, fixed = FALSE)
    end_pos_rud <- grep('finished', floorcategories, fixed = FALSE)
    rudimentfloor <- floorcategories[begin_pos_rud:(end_pos_rud-1)]
    
    begin_pos_fin <- grep('finished', floorcategories, fixed = FALSE)
    catother <- floorcategories[length(floorcategories)]
    
    if (catother == 'other'){
      finishedfloor <- floorcategories[begin_pos_fin:(length(floorcategories)-1)]  
    }
    if (catother != 'other'){
      finishedfloor <- floorcategories[begin_pos_fin:(length(floorcategories))]  
    }
    
    
    dataindi["FLOORCAT"] <- 0
    dataindi$FLOORCAT[dataindi$FLOOR %in% naturalfloor] <- 1
    dataindi$FLOORCAT[dataindi$FLOOR %in% rudimentfloor] <- 2
    dataindi$FLOORCAT[dataindi$FLOOR %in% finishedfloor] <- 3
    dataindi$FLOORCAT[dataindi$FLOOR == "other"] <- 1
    
  } else if ('natural' %ni% floorcategories){
    naturalfloor <- c('earth-based', 'earth, sand', 'earth, mud', 'earth, mud, clay', 'sand', 'dirt/earth', 'earth/sand',
                      'mud and hay', 'dung-based', 'dung', 'earth and dung', 'mud, dung, sand', 'natural', 'other',
                      'natural floor - earth/sand', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '96', '99')
    rudimentfloor <- c('wood', 'wood planks', 'wood and tile', 'wood/palm/bamboo', 'palm/bamboo', 
                       'palm/bamboo/leaves', 'other rudimentary', 'brocken bricks', 'adobe', 'unfinished stone',
                       'rudimentary', 'rudimentary floor - wood planks', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29') 
    finishedfloor <- c('parquet/polished wood', 'polished wood/vinyl/tiles', 'vinyl/asphalt strips/linoleum',
                       'linoleum', 'vinyl', 'tiles/mosaic', 'ceramic tiles', 'cement tiles', 'ceramic/terrazo tiles', 
                       'ceramic/marble tiles', 'tiles/brick', 'cement/concrete', 'carpet', 'mat', 'terrazo', 'stone', 
                       'bricks', 'other finished', 'plaster', 'finished', 'finished floor - carpeted', 'finished floor - cement',
                       'finished floor - ceramic or marble tiles', 'finished floor - parquet or polished wood or laminate', 
                       'finished floor - vinyl or linoleum', 'cement', 'ceramic/marble/porcelain tiles / terrazo', 'linoleum/rubber carpet',
                       'parquet, polished wood', 'woolen carpets/ synthetic carpet', 'vinyl, asphalt strips', 'wall to wall carpet',
                       '30', '31', '32', '33', '34', '35', '36', '37', '38', '39')
    
    dataindi["FLOORCAT"] <- 0
    dataindi$FLOORCAT[dataindi$FLOOR %in% naturalfloor] <- 1
    dataindi$FLOORCAT[dataindi$FLOOR %in% rudimentfloor] <- 2
    dataindi$FLOORCAT[dataindi$FLOOR %in% finishedfloor] <- 3
    
    dataindi0 = dataindi[dataindi$FLOORCAT == 0,]
    
    
  }
  
  
  wallcategories <- levels(dataindi$WALL)
  wallcategories <- tolower(wallcategories)
  dataindi$WALL <- tolower(dataindi$WALL)
  if ('natural' %in% wallcategories){
    begin_pos_nat <- grep('natural', wallcategories, fixed = FALSE)
    end_pos_nat <- grep('rudimentary', wallcategories, fixed = FALSE)
    naturalwalls <- wallcategories[begin_pos_nat:(end_pos_nat-1)]
    
    begin_pos_rud <- grep('rudimentary', wallcategories, fixed = FALSE)
    end_pos_rud <- grep('finished', wallcategories, fixed = FALSE)
    rudimentwalls <- wallcategories[begin_pos_rud:(end_pos_rud-1)]
    
    begin_pos_fin <- grep('finished', wallcategories, fixed = FALSE)
    catother <- wallcategories[length(wallcategories)]
    
    if (catother == 'other'){
      finishedwalls <- wallcategories[begin_pos_fin:(length(wallcategories)-1)]  
    }
    if (catother != 'other'){
      finishedwalls <- wallcategories[begin_pos_fin:(length(wallcategories))]  
    }
    
    dataindi["WALLCAT"] <- 0
    dataindi$WALLCAT[dataindi$WALL %in% naturalwalls] <- 1
    dataindi$WALLCAT[dataindi$WALL %in% rudimentwalls] <- 2
    dataindi$WALLCAT[dataindi$WALL %in% finishedwalls] <- 3
    
    
    dataindi$WALLCAT[dataindi$WALL == "other"] <- 1
    
  } else if ('natural' %ni% wallcategories){
    naturalwalls <- c('other', 'cane/palm/trunks/grass/sticks', 'cane/palm/trunks', 
                      'cane/trunks', 'palm branches', 'sticks', 'shells', 'earth/mud/dirt/dung',
                      'unbaked brick, mud, or earth', 'earth', 'mud', 'mud, dung', 'cane / palm / trunks',
                      'dirt', 'hay with mud', 'animal dung', 'thatch/mat/leaves/straw/reeds', 
                      'grass', 'thatch or straw', 'natural', 'no walls', 'without walls', 'bamboo without plaster',
                      'bamboo(guadua), straw, other plants', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '96', '99')
    rudimentwalls <- c('cardboard', 'plywood', 'reused wood', 'stone with mud', 
                       'uncovered adobe', 'wood planks/shingles', 'bamboo with mud', 
                       'bamboo/wood with mud', 'rough wood', 'timber', 'wood/metal planks', 
                       'poles and mud', 'trunks with mud', 'tin/cardboard/paper/bags', 'carton', 
                       'corrugated metal', 'canvas/tent', 'unburnt bricks', 'unburnt brick and plaster', 
                       'unburnt brick with cement', 'rudimentary', 'adobe (mud bricks)', 'bamboo with mud plaster',
                       'planks', 'zinc, canvas, plastics', 'prefabricated material', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29') 
    finishedwalls <- c('bricks', 'cement', 'cement blocks', 'covered adobe', 'stone with lime/cement', 'stone with lime / cement',
                       'cement/concrete', 'semi-dur (cement and sand blocks)', 'finished/burnt bricks', 
                       'burnt bricks with mud', 'burnt bricks with cement', 'sundried bricks', 'wood planks/shingles', 
                       'wood and grass', 'stone', 'other finished', 'metal or asbestos sheets', 'tin', 
                       't-iron/wood/brick', 'iron or zinc sheets', 'corrugated asbestos', 'metal, unspecified', 
                       'prefab', 'finished', 'cement blocks or panels', 'cement/monolit', 'wood planks / shingles',
                       'bricks/polished wood/pre-manufactured material', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39')
    
    dataindi["WALLCAT"] <- 0
    dataindi$WALLCAT[dataindi$WALL %in% naturalwalls] <- 1
    dataindi$WALLCAT[dataindi$WALL %in% rudimentwalls] <- 2
    dataindi$WALLCAT[dataindi$WALL %in% finishedwalls] <- 3
    
  }
  
  
  roofcategories <- levels(dataindi$ROOF)
  roofcategories <- tolower(roofcategories)
  dataindi$ROOF <- tolower(dataindi$ROOF)
  if ('natural' %in% roofcategories){
    begin_pos_nat <- grep('natural', roofcategories, fixed = FALSE)
    end_pos_nat <- grep('rudimentary', roofcategories, fixed = FALSE)
    naturalroof <- roofcategories[begin_pos_nat:(end_pos_nat-1)]
    
    begin_pos_rud <- grep('rudimentary', roofcategories, fixed = FALSE)
    end_pos_rud <- grep('finished', roofcategories, fixed = FALSE)
    rudimentroof <- roofcategories[begin_pos_rud:(end_pos_rud-1)]
    
    begin_pos_fin <- grep('finished', roofcategories, fixed = FALSE)
    catother <- roofcategories[length(roofcategories)]
    
    if (catother == 'other'){
      finishedroof <- roofcategories[begin_pos_fin:(length(roofcategories)-1)]  
    }
    if (catother != 'other'){
      finishedroof <- roofcategories[begin_pos_fin:(length(roofcategories))]  
    }
    
    dataindi["ROOFCAT"] <- 0
    dataindi$ROOFCAT[dataindi$ROOF %in% naturalroof] <- 1
    dataindi$ROOFCAT[dataindi$ROOF %in% rudimentroof] <- 2
    dataindi$ROOFCAT[dataindi$ROOF %in% finishedroof] <- 3
    dataindi$ROOFCAT[dataindi$ROOF == "other"] <- 1
  } else if ('natural' %ni% roofcategories){
    naturalroofs <- c('no roof', 'thatch/palm leaf/grass/makuti', 'thatch/palm leaf', 'grass, thatch', 
                      'grass, thatch, makuti', 'thatch', 'thatch/mat/leaves', 'leaves', 'grass/leaves/mud',
                      'sticks/sticks with mud or dung', 'earth', 'mud', 'dung, mud', 'earth, mud', 'sod', 'thatch / palm leaf / leaf',
                      'sod/mud and grass mixture', 'mud and hay', 'straw', 'other',  'natural', 'palm / bamboo', 'thatch / palm leaf',
                      '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '96', '99')
    rudimentroofs <- c('rustic mat', 'rustic mat, plastic sheets', 'plastic/polythene sheet', 'palm/bamboo', 
                       'palm, bamboo, grass', 'reed, bamboo', 'wood planks', 'wooden tiles, planks', 
                       'wood, mulch', 'cardboard', 'rudimentary-metal', 'rudimentary - metal', 'tin cans', 
                       'iron sheets', 'other rudimentary', 'mobile roofs of nomads', 'skin', 'waste materials', 
                       'rudimentary', 'asbestos/slate roofing sheets', 'metal / iron sheet', 'rustic mat/plastic',
                       '20', '21', '22', '23', '24', '25', '26', '27', '28', '29')
    
    finishedroofs <- c('metal', 'metal, zinc', 'corrugated metal sheet, asbestos', 'corrugated iron', 'tin', 
                       'asbestos', 'wood', 'cement/concrete', 'cement', 'concrete', 'reinforced brick cement', 
                       'tiles', 'ceramic tiles', 'iron and tiles', 'tiles/slate', 'mud tiles', 'reinforced brick cement/rcc',
                       'ceramic tiles, harvey (steel) tiles', 'cement fiber', 'calamine/cement fiber', 'asbestos, cement fiber', 
                       'zinc/cement fiber', 'roofing shingles', 'bricks', 'unburnt bricks', 'burnt bricks', 'stone', 
                       'loosely packed stone', 'stone slabs', 'slate', 'finished', 'reinforced concrete', 'taule', 
                       'roofing shingles/shifer', 'calamine / cement fiber',
                       '30', '31', '32', '33', '34', '35', '36', '37', '38', '39')
    
    dataindi["ROOFCAT"] <- 0
    dataindi$ROOFCAT[dataindi$ROOF %in% naturalroofs] <- 1
    dataindi$ROOFCAT[dataindi$ROOF %in% rudimentroofs] <- 2
    dataindi$ROOFCAT[dataindi$ROOF %in% finishedroofs] <- 3
    
  }
  
  
  
  floor0 = dataindi[dataindi$FLOORCAT == 0,]
  wall0 = dataindi[dataindi$WALLCAT == 0,]
  roof0 = dataindi[dataindi$ROOFCAT == 0,]
  
  # regressie urban/rural, wealth, floor, wall, roof. 
  
  no0dwelling <- dataindi[(dataindi$FLOORCAT > 0 & dataindi$ROOFCAT > 0 & dataindi$WALLCAT > 0),]
  
  wallcats = as.character(sort(unique(no0dwelling$WALLCAT)))
  roofcats = as.character(sort(unique(no0dwelling$ROOFCAT)))
  floorcats = as.character(sort(unique(no0dwelling$FLOORCAT)))
  
  
  
  no0dwelling$WALLCAT = factor(no0dwelling$WALLCAT, levels = wallcats, ordered = TRUE) 
  no0dwelling$ROOFCAT = factor(no0dwelling$ROOFCAT, levels = roofcats, ordered = TRUE) 
  no0dwelling$FLOORCAT = factor(no0dwelling$FLOORCAT, levels = floorcats, ordered = TRUE)
  
  
  if (nrow(floor0) == nrow(dataindi)){
    dataindi$FLOORCAT = -1
  } else if (nrow(floor0) > 0){  
    missingFLOOR <- dataindi[(dataindi$FLOORCAT == 0 & dataindi$ROOFCAT > 0 & dataindi$WALLCAT > 0),] 
    
    if (nrow(missingFLOOR) > 0){
      
      if (length(floorcats) > 2){
        
        floormodel= polr(FLOORCAT ~ ROOFCAT + WALLCAT + WEALTHQHH + URBANHH, data = no0dwelling, Hess = TRUE)
        missingFLOOR$WALLCAT = factor(missingFLOOR$WALLCAT, levels = wallcats, ordered = TRUE) 
        missingFLOOR$ROOFCAT = factor(missingFLOOR$ROOFCAT, levels = roofcats, ordered = TRUE) 
        missingFLOOR$FLOORCAT = factor(missingFLOOR$FLOORCAT, levels = floorcats, ordered = TRUE)
        
        test <- predict(floormodel, newdata = missingFLOOR, type="class") 
        
        dataindi$FLOORCAT[dataindi$FLOORCAT == 0 & dataindi$WALLCAT > 0 & dataindi$ROOFCAT > 0] <- test
        
      } else if (length(floorcats) == 2){
        floormodel= glm(FLOORCAT ~ ROOFCAT + WALLCAT + WEALTHQHH + URBANHH, family = binomial, data = no0dwelling)
        missingFLOOR$WALLCAT = factor(missingFLOOR$WALLCAT, levels = wallcats, ordered = TRUE) 
        missingFLOOR$ROOFCAT = factor(missingFLOOR$ROOFCAT, levels = roofcats, ordered = TRUE) 
        missingFLOOR$FLOORCAT = factor(missingFLOOR$FLOORCAT, levels = floorcats, ordered = TRUE)
        test <- predict(floormodel, newdata = missingFLOOR, type = 'response')
        test <- ifelse(test >= 0.5, floorcats[2], floorcats[1])
        
        dataindi$FLOORCAT[dataindi$FLOORCAT == 0 & dataindi$WALLCAT > 0 & dataindi$ROOFCAT > 0] <- test
        
      }
      
    }
    
    
    missingFLOOR2 <- dataindi[(dataindi$FLOORCAT == 0 & dataindi$ROOFCAT == 0 & dataindi$WALLCAT > 0),] 
    
    if (nrow(missingFLOOR2) > 0){
      
      if (length(floorcats) > 2){
        
        floormodel= polr(FLOORCAT ~ WALLCAT + WEALTHQHH + URBANHH, data = no0dwelling, Hess = TRUE)
        missingFLOOR2$WALLCAT = factor(missingFLOOR2$WALLCAT, levels = wallcats, ordered = TRUE) 
        missingFLOOR2$FLOORCAT = factor(missingFLOOR2$FLOORCAT, levels = floorcats, ordered = TRUE)
        
        test <- predict(floormodel, newdata = missingFLOOR2, type="class") 
        
        dataindi$FLOORCAT[dataindi$FLOORCAT == 0 & dataindi$WALLCAT > 0 & dataindi$ROOFCAT == 0] <- test
      } else if (length(floorcats) == 2){
        floormodel= glm(FLOORCAT ~ WALLCAT + WEALTHQHH + URBANHH, family = binomial, data = no0dwelling)
        missingFLOOR2$WALLCAT = factor(missingFLOOR2$WALLCAT, levels = wallcats, ordered = TRUE) 
        missingFLOOR2$FLOORCAT = factor(missingFLOOR2$FLOORCAT, levels = floorcats, ordered = TRUE)
        
        test <- predict(floormodel, newdata = missingFLOOR2, type="response") 
        test <- ifelse(test >= 0.5, floorcats[2], floorcats[1])
        
        dataindi$FLOORCAT[dataindi$FLOORCAT == 0 & dataindi$WALLCAT > 0 & dataindi$ROOFCAT == 0] <- test
        
        
        
      }
    }
    
    missingFLOOR3 <- dataindi[(dataindi$FLOORCAT == 0 & dataindi$ROOFCAT > 0 & dataindi$WALLCAT == 0),]  
    
    if (nrow(missingFLOOR3) > 0){
      if (length(floorcats) > 2){
        floormodel= polr(FLOORCAT ~ ROOFCAT + WEALTHQHH + URBANHH, data = no0dwelling, Hess = TRUE)
        missingFLOOR3$ROOFCAT = factor(missingFLOOR3$ROOFCAT, levels = roofcats, ordered = TRUE) 
        missingFLOOR3$FLOORCAT = factor(missingFLOOR3$FLOORCAT, levels = floorcats, ordered = TRUE)
        
        test <- predict(floormodel, newdata = missingFLOOR3, type="class") 
        
        dataindi$FLOORCAT[dataindi$FLOORCAT == 0 & dataindi$WALLCAT == 0 & dataindi$ROOFCAT > 0] <- test
      } else if (length(floorcats) == 2){
        floormodel= glm(FLOORCAT ~ ROOFCAT + WEALTHQHH + URBANHH, family = binomial, data = no0dwelling)
        missingFLOOR3$ROOFCAT = factor(missingFLOOR3$ROOFCAT, levels = roofcats, ordered = TRUE) 
        missingFLOOR3$FLOORCAT = factor(missingFLOOR3$FLOORCAT, levels = floorcats, ordered = TRUE)
        
        test <- predict(floormodel, newdata = missingFLOOR3, type="response") 
        test <- ifelse(test >= 0.5, floorcats[2], floorcats[1])
        
        dataindi$FLOORCAT[dataindi$FLOORCAT == 0 & dataindi$WALLCAT == 0 & dataindi$ROOFCAT > 0] <- test
        
      }
      
    }
    
  }
  
  
  if (nrow(wall0) == nrow(dataindi)){
    dataindi$WALLCAT = -1
  } else if (nrow(wall0) > 0){  
    #summary(floormodel)
    missingWALL <- dataindi[(dataindi$FLOORCAT > 0 & dataindi$ROOFCAT > 0 & dataindi$WALLCAT == 0),] 
    
    if (nrow(missingWALL) > 0){
      
      wallmodel= polr(WALLCAT ~ ROOFCAT + FLOORCAT + WEALTHQHH + URBANHH, data = no0dwelling, Hess = TRUE)
      missingWALL$WALLCAT = factor(missingWALL$WALLCAT, levels = wallcats, ordered = TRUE) 
      missingWALL$ROOFCAT = factor(missingWALL$ROOFCAT, levels = roofcats, ordered = TRUE) 
      missingWALL$FLOORCAT = factor(missingWALL$FLOORCAT, levels = floorcats, ordered = TRUE)
      
      test <- predict(wallmodel, newdata = missingWALL, type="class") 
      
      dataindi$WALLCAT[dataindi$FLOORCAT > 0 & dataindi$WALLCAT == 0 & dataindi$ROOFCAT > 0] <- test
      
    }
    
    
    missingWALL2 <- dataindi[(dataindi$FLOORCAT > 0 & dataindi$ROOFCAT == 0 & dataindi$WALLCAT == 0),] #test met >=. dus al die shit wel loshalen. 
    
    if (nrow(missingWALL2) > 0){
      wallmodel= polr(WALLCAT ~ FLOORCAT + WEALTHQHH + URBANHH, data = no0dwelling, Hess = TRUE)
      missingWALL2$WALLCAT = factor(missingWALL2$WALLCAT, levels = wallcats, ordered = TRUE) 
      missingWALL2$FLOORCAT = factor(missingWALL2$FLOORCAT, levels = floorcats, ordered = TRUE)
      
      test <- predict(wallmodel, newdata = missingWALL2, type="class") 
      
      dataindi$WALLCAT[dataindi$FLOORCAT > 0 & dataindi$WALLCAT == 0 & dataindi$ROOFCAT == 0] <- test
      
    }
    
    missingWALL3 <- dataindi[(dataindi$FLOORCAT == 0 & dataindi$ROOFCAT > 0 & dataindi$WALLCAT == 0),] #test met >=. dus al die shit wel loshalen. 
    
    if (nrow(missingWALL3) > 0){
      wallmodel= polr(WALLCAT ~ ROOFCAT + WEALTHQHH + URBANHH, data = no0dwelling, Hess = TRUE)
      missingWALL3$WALLCAT = factor(missingWALL3$WALLCAT, levels = wallcats, ordered = TRUE) 
      missingWALL3$ROOFCAT = factor(missingWALL3$ROOFCAT, levels = roofcats, ordered = TRUE)
      
      test <- predict(wallmodel, newdata = missingWALL3, type="class") 
      
      dataindi$WALLCAT[dataindi$FLOORCAT == 0 & dataindi$WALLCAT == 0 & dataindi$ROOFCAT > 0] <- test
      
    }
    
  }
  
  
  
  if (nrow(roof0) == nrow(dataindi)){
    dataindi$ROOFCAT = -1
  } else if(nrow(roof0) > 0){
    missingROOF <- dataindi[(dataindi$FLOORCAT > 0 & dataindi$ROOFCAT == 0 & dataindi$WALLCAT > 0),] 
    
    if (nrow(missingROOF) > 0){
      
      roofmodel= polr(ROOFCAT ~ WALLCAT + FLOORCAT + WEALTHQHH + URBANHH, data = no0dwelling, Hess = TRUE)
      missingROOF$WALLCAT = factor(missingROOF$WALLCAT, levels = wallcats, ordered = TRUE) 
      missingROOF$ROOFCAT = factor(missingROOF$ROOFCAT, levels = roofcats, ordered = TRUE) 
      missingROOF$FLOORCAT = factor(missingROOF$FLOORCAT, levels = floorcats, ordered = TRUE)
      
      test <- predict(roofmodel, newdata = missingROOF, type="class") 
      
      dataindi$ROOFCAT[dataindi$FLOORCAT > 0 & dataindi$WALLCAT > 0 & dataindi$ROOFCAT == 0] <- test
      
    }
    
    
    missingROOF2 <- dataindi[(dataindi$FLOORCAT > 0 & dataindi$ROOFCAT == 0 & dataindi$WALLCAT == 0),]
    
    if (nrow(missingROOF2) > 0){
      roofmodel= polr(ROOFCAT ~ FLOORCAT + WEALTHQHH + URBANHH, data = no0dwelling, Hess = TRUE)
      missingROOF2$ROOFCAT = factor(missingROOF2$ROOFCAT, levels = roofcats, ordered = TRUE) 
      missingROOF2$FLOORCAT = factor(missingROOF2$FLOORCAT, levels = floorcats, ordered = TRUE)
      
      test <- predict(roofmodel, newdata = missingROOF2, type="class") 
      
      dataindi$ROOFCAT[dataindi$FLOORCAT > 0 & dataindi$WALLCAT == 0 & dataindi$ROOFCAT == 0] <- test
      
    }
    
    missingROOF3 <- dataindi[(dataindi$FLOORCAT == 0 & dataindi$ROOFCAT == 0 & dataindi$WALLCAT > 0),]  
    
    if (nrow(missingROOF3) > 0){
      roofmodel= polr(ROOFCAT ~ WALLCAT + WEALTHQHH + URBANHH, data = no0dwelling, Hess = TRUE)
      missingROOF3$WALLCAT = factor(missingROOF3$WALLCAT, levels = wallcats, ordered = TRUE) 
      missingROOF3$ROOFCAT = factor(missingROOF3$ROOFCAT, levels = roofcats, ordered = TRUE)
      
      test <- predict(roofmodel, newdata = missingROOF3, type="class") 
      
      dataindi$ROOFCAT[dataindi$FLOORCAT == 0 & dataindi$WALLCAT > 0 & dataindi$ROOFCAT == 0] <- test
      
    }
    
  }
  
  zerodwelling = dataindi[(dataindi$WALLCAT <= 0 & dataindi$FLOORCAT <= 0 & dataindi$ROOFCAT <= 0),]
  if (nrow(zerodwelling) > 0){
    
    if (nrow(dataindi[dataindi$ROOFCAT == 0,]) > 0){
      no0dwelling = dataindi[dataindi$ROOFCAT > 0,]
      no0dwelling$ROOFCAT = factor(no0dwelling$ROOFCAT, levels = roofcats, ordered = TRUE)
      roofmodel= polr(ROOFCAT ~ WEALTHQHH + URBANHH, data = no0dwelling, Hess = TRUE)
      test <- predict(roofmodel, newdata = zerodwelling, type="class") 
      dataindi$ROOFCAT[dataindi$ROOFCAT == 0] <- test
    }
    
    if (nrow(dataindi[dataindi$WALLCAT == 0,]) > 0){ 
      no0dwelling = dataindi[dataindi$WALLCAT > 0,]
      no0dwelling$WALLCAT = factor(no0dwelling$WALLCAT, levels = wallcats, ordered = TRUE)
      wallmodel= polr(WALLCAT ~ WEALTHQHH + URBANHH, data = no0dwelling, Hess = TRUE)
      test <- predict(wallmodel, newdata = zerodwelling, type="class") 
      dataindi$WALLCAT[dataindi$WALLCAT == 0] <- test
    }
    
    if (nrow(dataindi[dataindi$FLOORCAT == 0,]) > 0){ 
      if (isocode == 'EGY'){
        floorcats = c('1', '2', '3')
      }
      no0dwelling = dataindi[dataindi$FLOORCAT > 0,]
      no0dwelling$FLOORCAT = factor(no0dwelling$FLOORCAT, levels = floorcats, ordered = TRUE)
      floormodel= polr(FLOORCAT ~ WEALTHQHH + URBANHH, data = no0dwelling, Hess = TRUE)
      test <- predict(floormodel, newdata = zerodwelling, type="class") 
      dataindi$FLOORCAT[dataindi$FLOORCAT == 0] <- test
    }
  }
  
  
  
  dataindi <- dataindi %>% dplyr::select(-AGLANDYN, -ROOF, -FLOOR, -WALL)
  
  
  dataindi <- subset(dataindi, rowSums(is.na(dataindi)) != ncol(dataindi))
  
  dataindi <- dataindi[complete.cases(dataindi),]
  
  
  floor0 = dataindi[dataindi$FLOORCAT == 0,]
  wall0 = dataindi[dataindi$WALLCAT == 0,]
  roof0 = dataindi[dataindi$ROOFCAT == 0,]
  
  
  
  if (nrow(floor0) > 0){
    dataindi$FLOORCAT[dataindi$FLOORCAT == 0] <- dataindi$WALLCAT[dataindi$FLOORCAT == 0]
  }
  
  if (nrow(wall0) > 0){
    dataindi$WALLCAT[dataindi$WALLCAT == 0] <- dataindi$ROOFCAT[dataindi$WALLCAT == 0]
  }
  
  if (nrow(roof0) > 0){
    dataindi$ROOFCAT[dataindi$ROOFCAT == 0] <- dataindi$WALLCAT[dataindi$ROOFCAT == 0]
  }
  
  dataindi['PID'] = (1:nrow(dataindi))
  dataindi['HID'] <- as.integer(as.factor(dataindi$HID))
  
  datadhs <- dataindi %>% dplyr::select(HID, WEIGHT, PID, HHSIZECAT, RURAL, HHTYPE, GENDER, AGECAT, EDUCAT, WEALTH, FARMING, RELATE, ROOFCAT, WALLCAT, FLOORCAT, GDLcode)
  
  
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



dhsgdl <- merge_gdl_regions(dhs, isocode)

reference_sample <- dhs_survey2(dhsgdl) #dhsgdl

if (isocode == 'PAK'){
  reference_sample$WEIGHT[reference_sample$GDLcode == 'PAKr106'] <- 1
  reference_sample$WEIGHT[reference_sample$GDLcode == 'PAKr107'] <- 1
  
}

if (isocode == 'MLI'){
  reference_sample106 <- reference_sample[reference_sample$GDLcode == 'MLIr105',]
  reference_sample106$GDLcode <- 'MLIr106'
  reference_sample <- rbindlist(list(reference_sample, reference_sample106), use.names=TRUE)
  
  reference_sample104 <- reference_sample[reference_sample$GDLcode == 'MLIr104',]
  reference_sample104$GDLcode <- 'MLIr107'
  reference_sample <- rbindlist(list(reference_sample, reference_sample104), use.names=TRUE)
  
}

if (isocode == 'PNG'){
  dhs1 <- copy(reference_sample)
  dhs1$GDLcode[dhs1$GDLcode ==  'PNGr107'] <- 'PNGr108'
  dhs1$GDLcode[dhs1$GDLcode == 'PNGr116'] <- 'PNGr117'
  dhs1$GDLcode[dhs1$GDLcode == 'PNGr112'] <- 'PNGr113'
  dhs1$GDLcode[dhs1$GDLcode == 'PNGr101'] <- 'PNGr102'
  
  dhs2 <- copy(reference_sample)
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr107'] <- 'PNGr109'
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr116'] <- 'PNGr118'
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr112'] <- 'PNGr114'
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr101'] <- 'PNGr103'
  
  dhs1 <- rbindlist(list(dhs1, dhs2), use.names=TRUE)
  
  dhs2 <- copy(reference_sample)
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr107'] <- 'PNGr110'
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr116'] <- 'PNGr119'
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr112'] <- 'PNGr115'
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr101'] <- 'PNGr104'
  
  dhs1 <- rbindlist(list(dhs1, dhs2), use.names=TRUE)
  
  dhs2 <- copy(reference_sample)
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr107'] <- 'PNGr111'
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr116'] <- 'PNGr120'
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr101'] <- 'PNGr105'
  
  dhs2 <- dhs2[dhs2$GDLcode %in% c('PNGr111', 'PNGr120', 'PNGr105'),]
  
  dhs1 <- rbindlist(list(dhs1, dhs2), use.names=TRUE)
  
  dhs2 <- copy(reference_sample)
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr116'] <- 'PNGr121'
  dhs2$GDLcode[dhs2$GDLcode ==  'PNGr101'] <- 'PNGr106'
  
  dhs2 <- dhs2[dhs2$GDLcode %in% c('PNGr121', 'PNGr106'),]
  
  dhs1 <- rbindlist(list(dhs1, dhs2), use.names=TRUE)
  
  dhs2 <- copy(reference_sample)
  dhs2$GDLcode[dhs2$GDLcode == 'PNGr116'] <- 'PNGr122'
  
  dhs2 <- dhs2[dhs2$GDLcode %in% c('PNGr122'),]
  
  dhs1 <- rbindlist(list(dhs1, dhs2), use.names=TRUE)
  
  reference_sample <- rbindlist(list(reference_sample, dhs1), use.names=TRUE)
}


########################################################################
############################# MARGINALS ################################

#hhsize
hhsize_marg <- read.csv('DHS_observedmarginals_hhsizecat_aug24.csv', sep = ',', header = TRUE)
hhsize_marg['iso_code'] <- substr(hhsize_marg$GDLcode, 1, 3)
hhsize_marg <- hhsize_marg[hhsize_marg$iso_code == isocode,]
hhsize_marg <- hhsize_marg %>% dplyr::select(-iso_code)

hhsize_marg <- pivot_longer(hhsize_marg, cols = c(X1, X2, X3, X4, X5, X6), names_to = "HHSIZECAT", values_to = "Frequency")
hhsize_marg$HHSIZECAT <- as.integer(as.factor(hhsize_marg$HHSIZECAT))

hhsize_tib <- as_tibble(hhsize_marg)

#hhtype
hhtype_marg <- read.csv('DHS_observedmarginals_hhtype_aug24.csv', sep = ',', header = TRUE)
hhtype_marg['iso_code'] <- substr(hhtype_marg$GDLcode, 1, 3)
hhtype_marg <- hhtype_marg[hhtype_marg$iso_code == isocode,]
hhtype_marg <- hhtype_marg %>% dplyr::select(-iso_code)

hhtype_marg <- pivot_longer(hhtype_marg, cols = c(X1, X2, X3, X4, X5, X6, X7, X8), names_to = "HHTYPE", values_to = "Frequency")
hhtype_marg$HHTYPE <- as.integer(as.factor(hhtype_marg$HHTYPE))

hhtype_tib <- as_tibble(hhtype_marg)

# age
age_marg <- read.csv('DHS_observedmarginals_age_aug24.csv', sep = ',', header = TRUE)
age_marg['iso_code'] <- substr(age_marg$GDLcode, 1, 3)
age_marg <- age_marg[age_marg$iso_code == isocode,]
age_marg <- age_marg %>% dplyr::select(-iso_code)

age_marg <- pivot_longer(age_marg, cols = c(X1, X2, X3, X4, X5, X6, X7, X8), names_to = "AGECAT", values_to = "Frequency")
age_marg$AGECAT <- as.integer(as.factor(age_marg$AGECAT))

age_tib <- as_tibble(age_marg)

# edu
edu_marg <- read.csv('DHS_observedmarginals_edu_aug24.csv', sep = ',', header = TRUE)
edu_marg['iso_code'] <- substr(edu_marg$GDLcode, 1, 3)
edu_marg <- edu_marg[edu_marg$iso_code == isocode,]
edu_marg <- edu_marg %>% dplyr::select(-iso_code)

edu_marg <- pivot_longer(edu_marg, cols = c(X1, X2, X3, X4, X5), names_to = "EDUCAT", values_to = "Frequency")
edu_marg$EDUCAT <- as.integer(as.factor(edu_marg$EDUCAT))

edu_tib <- as_tibble(edu_marg)

# gender
gender_marg <- read.csv('DHS_observedmarginals_gender_aug24.csv', sep = ',', header = TRUE)
gender_marg['iso_code'] <- substr(gender_marg$GDLcode, 1, 3)
gender_marg <- gender_marg[gender_marg$iso_code == isocode,]
gender_marg <- gender_marg %>% dplyr::select(-iso_code)

gender_marg <- pivot_longer(gender_marg, cols = c(X1, X2), names_to = "GENDER", values_to = "Frequency")
gender_marg$GENDER <- as.integer(as.factor(gender_marg$GENDER))-1

gender_tib <- as_tibble(gender_marg)

# wealth
wealth_marg <- read.csv('DHS_observedmarginals_wealth_aug24.csv', sep = ',', header = TRUE)
wealth_marg['iso_code'] <- substr(wealth_marg$GDLcode, 1, 3)
wealth_marg <- wealth_marg[wealth_marg$iso_code == isocode,]
wealth_marg <- wealth_marg %>% dplyr::select(-iso_code)

wealth_marg <- pivot_longer(wealth_marg, cols = c(X1, X2, X3, X4, X5), names_to = "WEALTH", values_to = "Frequency")
wealth_marg$WEALTH <- as.integer(as.factor(wealth_marg$WEALTH))

wealth_tib <- as_tibble(wealth_marg)

# farming
farming_marg <- read.csv('DHS_observedmarginals_farming_aug24.csv', sep = ',', header = TRUE)
farming_marg['iso_code'] <- substr(farming_marg$GDLcode, 1, 3)
farming_marg <- farming_marg[farming_marg$iso_code == isocode,]
farming_marg <- farming_marg %>% dplyr::select(-iso_code)

farming_marg <- pivot_longer(farming_marg, cols = c(X1, X2), names_to = "FARMING", values_to = "Frequency")
farming_marg$FARMING <- as.integer(as.factor(farming_marg$FARMING))-1

farming_tib <- as_tibble(farming_marg)

# roofcat
roofcat_marg <- read.csv('DHS_observedmarginals_roofcat_aug24.csv', sep = ',', header = TRUE)
roofcat_marg['iso_code'] <- substr(roofcat_marg$GDLcode, 1, 3)
roofcat_marg <- roofcat_marg[roofcat_marg$iso_code == isocode,]
roofcat_marg <- roofcat_marg %>% dplyr::select(-iso_code)

roofcat_marg <- pivot_longer(roofcat_marg, cols = c(X1, X2, X3), names_to = "ROOFCAT", values_to = "Frequency")
roofcat_marg$ROOFCAT <- as.integer(as.factor(roofcat_marg$ROOFCAT))

roofcat_tib <- as_tibble(roofcat_marg)

# wallcat
wallcat_marg <- read.csv('DHS_observedmarginals_wallcat_aug24.csv', sep = ',', header = TRUE)
wallcat_marg['iso_code'] <- substr(wallcat_marg$GDLcode, 1, 3)
wallcat_marg <- wallcat_marg[wallcat_marg$iso_code == isocode,]
wallcat_marg <- wallcat_marg %>% dplyr::select(-iso_code)

wallcat_marg <- pivot_longer(wallcat_marg, cols = c(X1, X2, X3), names_to = "WALLCAT", values_to = "Frequency")
wallcat_marg$WALLCAT <- as.integer(as.factor(wallcat_marg$WALLCAT))

wallcat_tib <- as_tibble(wallcat_marg)

# floorcat
floorcat_marg <- read.csv('DHS_observedmarginals_floorcat_aug24.csv', sep = ',', header = TRUE)
floorcat_marg['iso_code'] <- substr(floorcat_marg$GDLcode, 1, 3)
floorcat_marg <- floorcat_marg[floorcat_marg$iso_code == isocode,]
floorcat_marg <- floorcat_marg %>% dplyr::select(-iso_code)

floorcat_marg <- pivot_longer(floorcat_marg, cols = c(X1, X2, X3), names_to = "FLOORCAT", values_to = "Frequency")
floorcat_marg$FLOORCAT <- as.integer(as.factor(floorcat_marg$FLOORCAT))

floorcat_tib <- as_tibble(floorcat_marg)

# rural
rural_marg <- population[population$iso_code == isocode,]
rural_marg <- rural_marg %>% dplyr::select(GDLcode, Ruralpop, Urbanpop)
rural_marg <- pivot_longer(rural_marg, cols = c(Ruralpop, Urbanpop), names_to = "RURAL", values_to = "Frequency")
rural_marg$RURAL <- abs(as.integer(as.factor(rural_marg$RURAL))-2) #1 is rural and 0 is urban

rural_tib <- as_tibble(rural_marg)


rurallevelssample <- unique(reference_sample$RURAL)
hhtypelevelssample <- unique(reference_sample$HHTYPE)
hhsizelevelssample <- unique(reference_sample$HHSIZECAT)
wealthlevelssample <- unique(reference_sample$WEALTH)
agelevelssample <- unique(reference_sample$AGECAT)
edulevelssample <- unique(reference_sample$EDUCAT)
genderlevelssample <- unique(reference_sample$GENDER)
farminglevelssample <- unique(reference_sample$FARMING)
floorlevelssample <- unique(reference_sample$FLOORCAT)
walllevelssample <- unique(reference_sample$WALLCAT)
rooflevelssample <- unique(reference_sample$ROOFCAT)

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
  
  hhsize_tib1reg <- hhsize_tib[hhsize_tib$GDLcode == regnr,]
  hhsize_tib1reg <- hhsize_tib1reg %>% dplyr::select(-GDLcode)
  
  hhsize_tib1reg <- hhsize_tib1reg[hhsize_tib1reg$HHSIZECAT %in% hhsizelevelssample,]
  
  wealth_tib1reg <- wealth_tib[wealth_tib$GDLcode == regnr,]
  wealth_tib1reg <- wealth_tib1reg %>% dplyr::select(-GDLcode)
  
  wealth_tib1reg <- wealth_tib1reg[wealth_tib1reg$WEALTH %in% wealthlevelssample,]
  
  age_tib1reg <- age_tib[age_tib$GDLcode == regnr,]
  age_tib1reg <- age_tib1reg %>% dplyr::select(-GDLcode)
  
  age_tib1reg <- age_tib1reg[age_tib1reg$AGECAT %in% agelevelssample,]
  
  edu_tib1reg <- edu_tib[edu_tib$GDLcode == regnr,]
  edu_tib1reg <- edu_tib1reg %>% dplyr::select(-GDLcode)
  
  edu_tib1reg <- edu_tib1reg[edu_tib1reg$EDUCAT %in% edulevelssample,]
  
  print('edu_tib1reg')
  print(edu_tib1reg)
  
  gender_tib1reg <- gender_tib[gender_tib$GDLcode == regnr,]
  gender_tib1reg <- gender_tib1reg %>% dplyr::select(-GDLcode)
  
  gender_tib1reg <- gender_tib1reg[gender_tib1reg$GENDER %in% genderlevelssample,]
  
  farming_tib1reg <- farming_tib[farming_tib$GDLcode == regnr,]
  farming_tib1reg <- farming_tib1reg %>% dplyr::select(-GDLcode)
  
  if (nrow(farming_tib1reg) == 0){
    farming_tib1reg <- tibble(FARMING = -1, Frequency = popsize)
  }
  
  print('farming_tib1reg')
  print(farming_tib1reg)
  
  floorcat_tib1reg <- floorcat_tib[floorcat_tib$GDLcode == regnr,]
  floorcat_tib1reg <- floorcat_tib1reg %>% dplyr::select(-GDLcode)
  
  floorcat_tib1reg <- floorcat_tib1reg[floorcat_tib1reg$FLOORCAT %in% floorlevelssample,]
  
  if (nrow(floorcat_tib1reg) == 0){
    floorcat_tib1reg <- tibble(FLOORCAT = -1, Frequency = popsize)
  }
  
  print('floorcat_tib1reg')
  print(floorcat_tib1reg)
  
  wallcat_tib1reg <- wallcat_tib[wallcat_tib$GDLcode == regnr,]
  wallcat_tib1reg <- wallcat_tib1reg %>% dplyr::select(-GDLcode)
  
  wallcat_tib1reg <- wallcat_tib1reg[wallcat_tib1reg$WALLCAT %in% walllevelssample,]
  
  if (nrow(wallcat_tib1reg) == 0){
    wallcat_tib1reg <- tibble(WALLCAT = -1, Frequency = popsize)
  }
  
  print('wallcat_tib1reg')
  print(wallcat_tib1reg)
  
  roofcat_tib1reg <- roofcat_tib[roofcat_tib$GDLcode == regnr,]
  roofcat_tib1reg <- roofcat_tib1reg %>% dplyr::select(-GDLcode)
  
  roofcat_tib1reg <- roofcat_tib1reg[roofcat_tib1reg$ROOFCAT %in% rooflevelssample,]
  
  if (nrow(roofcat_tib1reg) == 0){
    roofcat_tib1reg <- tibble(ROOFCAT = -1, Frequency = popsize)
  }
  
  print('roofcat_tib1reg')
  print(roofcat_tib1reg)
  
  group_control <- list()
  individual_control <- list(rural_tib1reg, hhtype_tib1reg, hhsize_tib1reg, wealth_tib1reg, 
                             age_tib1reg, edu_tib1reg, gender_tib1reg, farming_tib1reg, 
                             floorcat_tib1reg, wallcat_tib1reg, roofcat_tib1reg)
  
  names(group_control) <- c() 
  names(individual_control) <- c('RURAL','HHTYPE', 'HHSIZECAT', 'WEALTH', 'AGECAT',
                                 'EDUCAT', 'GENDER', 'FARMING', 'FLOORCAT', 'WALLCAT', 'ROOFCAT')
  
  
  fitting_problem <- ml_problem(
    ref_sample = reference_sample, 
    controls = list(
      individual = individual_control,
      group = group_control
    ), prior_weights = reference_sample$WEIGHT,
    field_names = special_field_names(
      groupId = "HID", 
      individualId = "PID", 
      count = "Frequency"
    )
  )
  
  fit <- ml_fit(ml_problem = fitting_problem, algorithm = "ipu", maxiter = 150) #was 50
  
  syn_pop_reg <- ml_replicate(fit, algorithm = "trs")
  
  print('syn_pop_reg')
  print(head(syn_pop_reg, 3))
  
  #syn_pop_reg[] <- lapply(syn_pop_reg, as.integer)
  
  syn_pop_reg$HHSIZECAT <- as.integer(syn_pop_reg$HHSIZECAT)
  syn_pop_reg$RURAL <- as.integer(syn_pop_reg$RURAL)
  syn_pop_reg$GENDER <- as.integer(syn_pop_reg$GENDER)
  syn_pop_reg$AGECAT <- as.integer(syn_pop_reg$AGECAT)
  syn_pop_reg$EDUCAT <- as.integer(syn_pop_reg$EDUCAT)
  syn_pop_reg$FARMING <- as.integer(syn_pop_reg$FARMING)
  syn_pop_reg$WEALTH <- as.integer(syn_pop_reg$WEALTH)
  syn_pop_reg$FLOORCAT <- as.integer(syn_pop_reg$FLOORCAT)
  syn_pop_reg$WALLCAT <- as.integer(syn_pop_reg$WALLCAT)
  syn_pop_reg$ROOFCAT <- as.integer(syn_pop_reg$ROOFCAT)
  syn_pop_reg$HID <- as.integer(syn_pop_reg$HID)
  syn_pop_reg$RELATE <- as.integer(syn_pop_reg$RELATE)
  syn_pop_reg$HHTYPE <- as.integer(syn_pop_reg$HHTYPE)
  
  syn_pop_reg$INCOME <- -1
  syn_pop_reg$SOURCE <- 5
  
  syn_pop_reg$INCOME <- as.integer(syn_pop_reg$INCOME)
  syn_pop_reg$SOURCE <- as.integer(syn_pop_reg$SOURCE)
  
  
  
  name = paste0('synthpop_DHS_', regnr, '.dat') 
  
  con = file(name, "wb")
  
  writeBin(c(syn_pop_reg$HID, syn_pop_reg$RELATE,syn_pop_reg$INCOME, syn_pop_reg$WEALTH, syn_pop_reg$RURAL, syn_pop_reg$AGECAT, syn_pop_reg$GENDER, 
             syn_pop_reg$EDUCAT, syn_pop_reg$HHTYPE, syn_pop_reg$HHSIZECAT, syn_pop_reg$FARMING, syn_pop_reg$FLOORCAT, syn_pop_reg$WALLCAT, syn_pop_reg$ROOFCAT, syn_pop_reg$SOURCE), con)
  
  
  close(con)
  
  
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

filename_indiperregion <- paste0("Individuals_per_region_DHS_april24_", isocode, ".csv") #was oct, nov. 

write.table(df_nr_individuals_per_region, filename_indiperregion, row.names = FALSE, sep = ',')

noerrorcalc <- function(){

  # SPATIAL ERROR
  #freq1 <- data.frame(xtabs(WEIGHT ~ RURAL + HHTYPE + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  #freq2 <- data.frame(xtabs(WEIGHT ~ RURAL + HHSIZECAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  #freq3 <- data.frame(xtabs(WEIGHT ~ RURAL + WEALTH + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  #freq4 <- data.frame(xtabs(WEIGHT ~ RURAL + AGECAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  #freq5 <- data.frame(xtabs(WEIGHT ~ RURAL + EDUCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  #freq6 <- data.frame(xtabs(WEIGHT ~ RURAL + GENDER + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  #freq7 <- data.frame(xtabs(WEIGHT ~ RURAL + FARMING + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  #freq8 <- data.frame(xtabs(WEIGHT ~ RURAL + FLOORCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  #freq9 <- data.frame(xtabs(WEIGHT ~ RURAL + WALLCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  #freq10 <- data.frame(xtabs(WEIGHT ~ RURAL + ROOFCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  
  freq11 <- data.frame(xtabs(WEIGHT ~ HHTYPE + HHSIZECAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq12 <- data.frame(xtabs(WEIGHT ~ HHTYPE + WEALTH + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq13 <- data.frame(xtabs(WEIGHT ~ HHTYPE + AGECAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq14 <- data.frame(xtabs(WEIGHT ~ HHTYPE + EDUCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq15 <- data.frame(xtabs(WEIGHT ~ HHTYPE + GENDER + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq16 <- data.frame(xtabs(WEIGHT ~ HHTYPE + FARMING + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq17 <- data.frame(xtabs(WEIGHT ~ HHTYPE + FLOORCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq18 <- data.frame(xtabs(WEIGHT ~ HHTYPE + WALLCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq19 <- data.frame(xtabs(WEIGHT ~ HHTYPE + ROOFCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  
  freq20 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + WEALTH + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq21 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + AGECAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq22 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + EDUCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq23 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + GENDER + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq24 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + FARMING + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq25 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + FLOORCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq26 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + WALLCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq27 <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + ROOFCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  
  freq28 <- data.frame(xtabs(WEIGHT ~ WEALTH + AGECAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq29 <- data.frame(xtabs(WEIGHT ~ WEALTH + EDUCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq30 <- data.frame(xtabs(WEIGHT ~ WEALTH + GENDER + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq31 <- data.frame(xtabs(WEIGHT ~ WEALTH + FARMING + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq32 <- data.frame(xtabs(WEIGHT ~ WEALTH + FLOORCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq33 <- data.frame(xtabs(WEIGHT ~ WEALTH + WALLCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq34 <- data.frame(xtabs(WEIGHT ~ WEALTH + ROOFCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  
  freq35 <- data.frame(xtabs(WEIGHT ~ AGECAT + EDUCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq36 <- data.frame(xtabs(WEIGHT ~ AGECAT + GENDER + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq37 <- data.frame(xtabs(WEIGHT ~ AGECAT + FARMING + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq38 <- data.frame(xtabs(WEIGHT ~ AGECAT + FLOORCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq39 <- data.frame(xtabs(WEIGHT ~ AGECAT + WALLCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq40 <- data.frame(xtabs(WEIGHT ~ AGECAT + ROOFCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  
  freq41 <- data.frame(xtabs(WEIGHT ~ EDUCAT + GENDER + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq42 <- data.frame(xtabs(WEIGHT ~ EDUCAT + FARMING + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq43 <- data.frame(xtabs(WEIGHT ~ EDUCAT + FLOORCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq44 <- data.frame(xtabs(WEIGHT ~ EDUCAT + WALLCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq45 <- data.frame(xtabs(WEIGHT ~ EDUCAT + ROOFCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  
  freq46 <- data.frame(xtabs(WEIGHT ~ GENDER + FARMING + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq47 <- data.frame(xtabs(WEIGHT ~ GENDER + FLOORCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq48 <- data.frame(xtabs(WEIGHT ~ GENDER + WALLCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq49 <- data.frame(xtabs(WEIGHT ~ GENDER + ROOFCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  
  freq50 <- data.frame(xtabs(WEIGHT ~ FARMING + FLOORCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq51 <- data.frame(xtabs(WEIGHT ~ FARMING + WALLCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq52 <- data.frame(xtabs(WEIGHT ~ FARMING + ROOFCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  
  freq53 <- data.frame(xtabs(WEIGHT ~ FLOORCAT + WALLCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  freq54 <- data.frame(xtabs(WEIGHT ~ FLOORCAT + ROOFCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  
  freq55 <- data.frame(xtabs(WEIGHT ~ WALLCAT + ROOFCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  
  #freq1tot <- data.frame(xtabs(WEIGHT ~ RURAL + HHTYPE, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  #freq2tot <- data.frame(xtabs(WEIGHT ~ RURAL + HHSIZECAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  #freq3tot <- data.frame(xtabs(WEIGHT ~ RURAL + WEALTH, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  #freq4tot <- data.frame(xtabs(WEIGHT ~ RURAL + AGECAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  #freq5tot <- data.frame(xtabs(WEIGHT ~ RURAL + EDUCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  #freq6tot <- data.frame(xtabs(WEIGHT ~ RURAL + GENDER, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  #freq7tot <- data.frame(xtabs(WEIGHT ~ RURAL + FARMING, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  #freq8tot <- data.frame(xtabs(WEIGHT ~ RURAL + FLOORCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  #freq9tot <- data.frame(xtabs(WEIGHT ~ RURAL + WALLCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  #freq10tot <- data.frame(xtabs(WEIGHT ~ RURAL + ROOFCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  
  freq11tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + HHSIZECAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq12tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + WEALTH, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq13tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + AGECAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq14tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + EDUCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq15tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + GENDER, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq16tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + FARMING, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq17tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + FLOORCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq18tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + WALLCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq19tot <- data.frame(xtabs(WEIGHT ~ HHTYPE + ROOFCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  
  freq20tot <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + WEALTH, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq21tot <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + AGECAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq22tot <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + EDUCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq23tot <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + GENDER, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq24tot <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + FARMING, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq25tot <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + FLOORCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq26tot <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + WALLCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq27tot <- data.frame(xtabs(WEIGHT ~ HHSIZECAT + ROOFCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  
  freq28tot <- data.frame(xtabs(WEIGHT ~ WEALTH + AGECAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq29tot <- data.frame(xtabs(WEIGHT ~ WEALTH + EDUCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq30tot <- data.frame(xtabs(WEIGHT ~ WEALTH + GENDER, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq31tot <- data.frame(xtabs(WEIGHT ~ WEALTH + FARMING, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq32tot <- data.frame(xtabs(WEIGHT ~ WEALTH + FLOORCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq33tot <- data.frame(xtabs(WEIGHT ~ WEALTH + WALLCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq34tot <- data.frame(xtabs(WEIGHT ~ WEALTH + ROOFCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  
  freq35tot <- data.frame(xtabs(WEIGHT ~ AGECAT + EDUCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq36tot <- data.frame(xtabs(WEIGHT ~ AGECAT + GENDER, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq37tot <- data.frame(xtabs(WEIGHT ~ AGECAT + FARMING, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq38tot <- data.frame(xtabs(WEIGHT ~ AGECAT + FLOORCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq39tot <- data.frame(xtabs(WEIGHT ~ AGECAT + WALLCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq40tot <- data.frame(xtabs(WEIGHT ~ AGECAT + ROOFCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  
  freq41tot <- data.frame(xtabs(WEIGHT ~ EDUCAT + GENDER, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq42tot <- data.frame(xtabs(WEIGHT ~ EDUCAT + FARMING, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq43tot <- data.frame(xtabs(WEIGHT ~ EDUCAT + FLOORCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq44tot <- data.frame(xtabs(WEIGHT ~ EDUCAT + WALLCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq45tot <- data.frame(xtabs(WEIGHT ~ EDUCAT + ROOFCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  
  freq46tot <- data.frame(xtabs(WEIGHT ~ GENDER + FARMING, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq47tot <- data.frame(xtabs(WEIGHT ~ GENDER + FLOORCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq48tot <- data.frame(xtabs(WEIGHT ~ GENDER + WALLCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq49tot <- data.frame(xtabs(WEIGHT ~ GENDER + ROOFCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  
  freq50tot <- data.frame(xtabs(WEIGHT ~ FARMING + FLOORCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq51tot <- data.frame(xtabs(WEIGHT ~ FARMING + WALLCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq52tot <- data.frame(xtabs(WEIGHT ~ FARMING + ROOFCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  
  freq53tot <- data.frame(xtabs(WEIGHT ~ FLOORCAT + WALLCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  freq54tot <- data.frame(xtabs(WEIGHT ~ FLOORCAT + ROOFCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  
  freq55tot <- data.frame(xtabs(WEIGHT ~ WALLCAT + ROOFCAT, data = reference_sample))$Freq/sum(reference_sample$WEIGHT)
  
  
  
  #freq1tot <- rep(freq1tot, length(nr_individuals_per_regio))
  #freq1tot <- freq1tot * rep(nr_individuals_per_regio_share, each = length(freq1tot)/length(nr_individuals_per_regio))
  #freq2tot <- rep(freq2tot, length(nr_individuals_per_regio))
  #freq2tot <- freq2tot * rep(nr_individuals_per_regio_share, each = length(freq2tot)/length(nr_individuals_per_regio))
  #freq3tot <- rep(freq3tot, length(nr_individuals_per_regio))
  #freq3tot <- freq3tot * rep(nr_individuals_per_regio_share, each = length(freq3tot)/length(nr_individuals_per_regio))
  #freq4tot <- rep(freq4tot, length(nr_individuals_per_regio))
  #freq4tot <- freq4tot * rep(nr_individuals_per_regio_share, each = length(freq4tot)/length(nr_individuals_per_regio))
  #freq5tot <- rep(freq5tot, length(nr_individuals_per_regio))
  #freq5tot <- freq5tot * rep(nr_individuals_per_regio_share, each = length(freq5tot)/length(nr_individuals_per_regio))
  #freq6tot <- rep(freq6tot, length(nr_individuals_per_regio))
  #freq6tot <- freq6tot * rep(nr_individuals_per_regio_share, each = length(freq6tot)/length(nr_individuals_per_regio))
  #freq7tot <- rep(freq7tot, length(nr_individuals_per_regio))
  #freq7tot <- freq7tot * rep(nr_individuals_per_regio_share, each = length(freq7tot)/length(nr_individuals_per_regio))
  #freq8tot <- rep(freq8tot, length(nr_individuals_per_regio))
  #freq8tot <- freq8tot * rep(nr_individuals_per_regio_share, each = length(freq8tot)/length(nr_individuals_per_regio))
  #freq9tot <- rep(freq9tot, length(nr_individuals_per_regio))
  #freq9tot <- freq9tot * rep(nr_individuals_per_regio_share, each = length(freq9tot)/length(nr_individuals_per_regio))
  #freq10tot <- rep(freq10tot, length(nr_individuals_per_regio))
  #freq10tot <- freq10tot * rep(nr_individuals_per_regio_share, each = length(freq10tot)/length(nr_individuals_per_regio))
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
  freq16tot <- rep(freq16tot, length(nr_individuals_per_regio))
  freq16tot <- freq16tot * rep(nr_individuals_per_regio_share, each = length(freq16tot)/length(nr_individuals_per_regio))
  freq17tot <- rep(freq17tot, length(nr_individuals_per_regio))
  freq17tot <- freq17tot * rep(nr_individuals_per_regio_share, each = length(freq17tot)/length(nr_individuals_per_regio))
  freq18tot <- rep(freq18tot, length(nr_individuals_per_regio))
  freq18tot <- freq18tot * rep(nr_individuals_per_regio_share, each = length(freq18tot)/length(nr_individuals_per_regio))
  freq19tot <- rep(freq19tot, length(nr_individuals_per_regio))
  freq19tot <- freq19tot * rep(nr_individuals_per_regio_share, each = length(freq19tot)/length(nr_individuals_per_regio))
  freq20tot <- rep(freq20tot, length(nr_individuals_per_regio))
  freq20tot <- freq20tot * rep(nr_individuals_per_regio_share, each = length(freq20tot)/length(nr_individuals_per_regio))
  freq21tot <- rep(freq21tot, length(nr_individuals_per_regio))
  freq21tot <- freq21tot * rep(nr_individuals_per_regio_share, each = length(freq21tot)/length(nr_individuals_per_regio))
  freq22tot <- rep(freq22tot, length(nr_individuals_per_regio))
  freq22tot <- freq22tot * rep(nr_individuals_per_regio_share, each = length(freq22tot)/length(nr_individuals_per_regio))
  freq23tot <- rep(freq23tot, length(nr_individuals_per_regio))
  freq23tot <- freq23tot * rep(nr_individuals_per_regio_share, each = length(freq23tot)/length(nr_individuals_per_regio))
  freq24tot <- rep(freq24tot, length(nr_individuals_per_regio))
  freq24tot <- freq24tot * rep(nr_individuals_per_regio_share, each = length(freq24tot)/length(nr_individuals_per_regio))
  freq25tot <- rep(freq25tot, length(nr_individuals_per_regio))
  freq25tot <- freq25tot * rep(nr_individuals_per_regio_share, each = length(freq25tot)/length(nr_individuals_per_regio))
  freq26tot <- rep(freq26tot, length(nr_individuals_per_regio))
  freq26tot <- freq26tot * rep(nr_individuals_per_regio_share, each = length(freq26tot)/length(nr_individuals_per_regio))
  freq27tot <- rep(freq27tot, length(nr_individuals_per_regio))
  freq27tot <- freq27tot * rep(nr_individuals_per_regio_share, each = length(freq27tot)/length(nr_individuals_per_regio))
  freq28tot <- rep(freq28tot, length(nr_individuals_per_regio))
  freq28tot <- freq28tot * rep(nr_individuals_per_regio_share, each = length(freq28tot)/length(nr_individuals_per_regio))
  freq29tot <- rep(freq29tot, length(nr_individuals_per_regio))
  freq29tot <- freq29tot * rep(nr_individuals_per_regio_share, each = length(freq29tot)/length(nr_individuals_per_regio))
  freq30tot <- rep(freq30tot, length(nr_individuals_per_regio))
  freq30tot <- freq30tot * rep(nr_individuals_per_regio_share, each = length(freq30tot)/length(nr_individuals_per_regio))
  freq31tot <- rep(freq31tot, length(nr_individuals_per_regio))
  freq31tot <- freq31tot * rep(nr_individuals_per_regio_share, each = length(freq31tot)/length(nr_individuals_per_regio))
  freq32tot <- rep(freq32tot, length(nr_individuals_per_regio))
  freq32tot <- freq32tot * rep(nr_individuals_per_regio_share, each = length(freq32tot)/length(nr_individuals_per_regio))
  freq33tot <- rep(freq33tot, length(nr_individuals_per_regio))
  freq33tot <- freq33tot * rep(nr_individuals_per_regio_share, each = length(freq33tot)/length(nr_individuals_per_regio))
  freq34tot <- rep(freq34tot, length(nr_individuals_per_regio))
  freq34tot <- freq34tot * rep(nr_individuals_per_regio_share, each = length(freq34tot)/length(nr_individuals_per_regio))
  freq35tot <- rep(freq35tot, length(nr_individuals_per_regio))
  freq35tot <- freq35tot * rep(nr_individuals_per_regio_share, each = length(freq35tot)/length(nr_individuals_per_regio))
  freq36tot <- rep(freq36tot, length(nr_individuals_per_regio))
  freq36tot <- freq36tot * rep(nr_individuals_per_regio_share, each = length(freq36tot)/length(nr_individuals_per_regio))
  freq37tot <- rep(freq37tot, length(nr_individuals_per_regio))
  freq37tot <- freq37tot * rep(nr_individuals_per_regio_share, each = length(freq37tot)/length(nr_individuals_per_regio))
  freq38tot <- rep(freq38tot, length(nr_individuals_per_regio))
  freq38tot <- freq38tot * rep(nr_individuals_per_regio_share, each = length(freq38tot)/length(nr_individuals_per_regio))
  freq39tot <- rep(freq39tot, length(nr_individuals_per_regio))
  freq39tot <- freq39tot * rep(nr_individuals_per_regio_share, each = length(freq39tot)/length(nr_individuals_per_regio))
  freq40tot <- rep(freq40tot, length(nr_individuals_per_regio))
  freq40tot <- freq40tot * rep(nr_individuals_per_regio_share, each = length(freq40tot)/length(nr_individuals_per_regio))
  freq41tot <- rep(freq41tot, length(nr_individuals_per_regio))
  freq41tot <- freq41tot * rep(nr_individuals_per_regio_share, each = length(freq41tot)/length(nr_individuals_per_regio))
  freq42tot <- rep(freq42tot, length(nr_individuals_per_regio))
  freq42tot <- freq42tot * rep(nr_individuals_per_regio_share, each = length(freq42tot)/length(nr_individuals_per_regio))
  freq43tot <- rep(freq43tot, length(nr_individuals_per_regio))
  freq43tot <- freq43tot * rep(nr_individuals_per_regio_share, each = length(freq43tot)/length(nr_individuals_per_regio))
  freq44tot <- rep(freq44tot, length(nr_individuals_per_regio))
  freq44tot <- freq44tot * rep(nr_individuals_per_regio_share, each = length(freq44tot)/length(nr_individuals_per_regio))
  freq45tot <- rep(freq45tot, length(nr_individuals_per_regio))
  freq45tot <- freq45tot * rep(nr_individuals_per_regio_share, each = length(freq45tot)/length(nr_individuals_per_regio))
  freq46tot <- rep(freq46tot, length(nr_individuals_per_regio))
  freq46tot <- freq46tot * rep(nr_individuals_per_regio_share, each = length(freq46tot)/length(nr_individuals_per_regio))
  freq47tot <- rep(freq47tot, length(nr_individuals_per_regio))
  freq47tot <- freq47tot * rep(nr_individuals_per_regio_share, each = length(freq47tot)/length(nr_individuals_per_regio))
  freq48tot <- rep(freq48tot, length(nr_individuals_per_regio))
  freq48tot <- freq48tot * rep(nr_individuals_per_regio_share, each = length(freq48tot)/length(nr_individuals_per_regio))
  freq49tot <- rep(freq49tot, length(nr_individuals_per_regio))
  freq49tot <- freq49tot * rep(nr_individuals_per_regio_share, each = length(freq49tot)/length(nr_individuals_per_regio))
  freq50tot <- rep(freq50tot, length(nr_individuals_per_regio))
  freq50tot <- freq50tot * rep(nr_individuals_per_regio_share, each = length(freq50tot)/length(nr_individuals_per_regio))
  freq51tot <- rep(freq51tot, length(nr_individuals_per_regio))
  freq51tot <- freq51tot * rep(nr_individuals_per_regio_share, each = length(freq51tot)/length(nr_individuals_per_regio))
  freq52tot <- rep(freq52tot, length(nr_individuals_per_regio))
  freq52tot <- freq52tot * rep(nr_individuals_per_regio_share, each = length(freq52tot)/length(nr_individuals_per_regio))
  freq53tot <- rep(freq53tot, length(nr_individuals_per_regio))
  freq53tot <- freq53tot * rep(nr_individuals_per_regio_share, each = length(freq53tot)/length(nr_individuals_per_regio))
  freq54tot <- rep(freq54tot, length(nr_individuals_per_regio))
  freq54tot <- freq54tot * rep(nr_individuals_per_regio_share, each = length(freq54tot)/length(nr_individuals_per_regio))
  freq55tot <- rep(freq55tot, length(nr_individuals_per_regio))
  freq55tot <- freq55tot * rep(nr_individuals_per_regio_share, each = length(freq55tot)/length(nr_individuals_per_regio))
  
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
  
  marg8 <- data.frame(xtabs(WEIGHT ~ FARMING + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  names(marg8)[names(marg8) == 'Freq'] <- 'Prob_FARMING'
  
  marg9 <- data.frame(xtabs(WEIGHT ~ FLOORCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  names(marg9)[names(marg9) == 'Freq'] <- 'Prob_FLOORCAT'
  
  marg10 <- data.frame(xtabs(WEIGHT ~ WALLCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  names(marg10)[names(marg10) == 'Freq'] <- 'Prob_WALLCAT'
  
  marg11 <- data.frame(xtabs(WEIGHT ~ ROOFCAT + GDLcode, data = reference_sample)/sum(reference_sample$WEIGHT))
  names(marg11)[names(marg11) == 'Freq'] <- 'Prob_ROOFCAT'
  
  
  #rural, hhtype, hhsize, wealth, age, edu, gender, farming, floor, wall, roof
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
  
  
  
  
  #freq1 <- joinmarg_forerror(freq1, marg1, marg2, nr_individuals_per_regio_share)
  #freq2 <- joinmarg_forerror(freq2, marg1, marg3, nr_individuals_per_regio_share)
  #freq3 <- joinmarg_forerror(freq3, marg1, marg4, nr_individuals_per_regio_share)
  #freq4 <- joinmarg_forerror(freq4, marg1, marg5, nr_individuals_per_regio_share)
  #freq5 <- joinmarg_forerror(freq5, marg1, marg6, nr_individuals_per_regio_share)
  #freq6 <- joinmarg_forerror(freq6, marg1, marg7, nr_individuals_per_regio_share)
  #freq7 <- joinmarg_forerror(freq7, marg1, marg8, nr_individuals_per_regio_share)
  #freq8 <- joinmarg_forerror(freq8, marg1, marg9, nr_individuals_per_regio_share)
  #freq9 <- joinmarg_forerror(freq9, marg1, marg10, nr_individuals_per_regio_share)
  #freq10 <- joinmarg_forerror(freq10, marg1, marg11, nr_individuals_per_regio_share)
  
  freq11 <- joinmarg_forerror(freq11, marg2, marg3, nr_individuals_per_regio_share)
  freq12 <- joinmarg_forerror(freq12, marg2, marg4, nr_individuals_per_regio_share)
  freq13 <- joinmarg_forerror(freq13, marg2, marg5, nr_individuals_per_regio_share)
  freq14 <- joinmarg_forerror(freq14, marg2, marg6, nr_individuals_per_regio_share)
  freq15 <- joinmarg_forerror(freq15, marg2, marg7, nr_individuals_per_regio_share)
  freq16 <- joinmarg_forerror(freq16, marg2, marg8, nr_individuals_per_regio_share)
  freq17 <- joinmarg_forerror(freq17, marg2, marg9, nr_individuals_per_regio_share)
  freq18 <- joinmarg_forerror(freq18, marg2, marg10, nr_individuals_per_regio_share)
  freq19 <- joinmarg_forerror(freq19, marg2, marg11, nr_individuals_per_regio_share)
  
  freq20 <- joinmarg_forerror(freq20, marg3, marg4, nr_individuals_per_regio_share)
  freq21 <- joinmarg_forerror(freq21, marg3, marg5, nr_individuals_per_regio_share)
  freq22 <- joinmarg_forerror(freq22, marg3, marg6, nr_individuals_per_regio_share)
  freq23 <- joinmarg_forerror(freq23, marg3, marg7, nr_individuals_per_regio_share)
  freq24 <- joinmarg_forerror(freq24, marg3, marg8, nr_individuals_per_regio_share)
  freq25 <- joinmarg_forerror(freq25, marg3, marg9, nr_individuals_per_regio_share)
  freq26 <- joinmarg_forerror(freq26, marg3, marg10, nr_individuals_per_regio_share)
  freq27 <- joinmarg_forerror(freq27, marg3, marg11, nr_individuals_per_regio_share)
  
  freq28 <- joinmarg_forerror(freq28, marg4, marg5, nr_individuals_per_regio_share)
  freq29 <- joinmarg_forerror(freq29, marg4, marg6, nr_individuals_per_regio_share)
  freq30 <- joinmarg_forerror(freq30, marg4, marg7, nr_individuals_per_regio_share)
  freq31 <- joinmarg_forerror(freq31, marg4, marg8, nr_individuals_per_regio_share)
  freq32 <- joinmarg_forerror(freq32, marg4, marg9, nr_individuals_per_regio_share)
  freq33 <- joinmarg_forerror(freq33, marg4, marg10, nr_individuals_per_regio_share)
  freq34 <- joinmarg_forerror(freq34, marg4, marg11, nr_individuals_per_regio_share)
  
  freq35 <- joinmarg_forerror(freq35, marg5, marg6, nr_individuals_per_regio_share)
  freq36 <- joinmarg_forerror(freq36, marg5, marg7, nr_individuals_per_regio_share)
  freq37 <- joinmarg_forerror(freq37, marg5, marg8, nr_individuals_per_regio_share)
  freq38 <- joinmarg_forerror(freq38, marg5, marg9, nr_individuals_per_regio_share)
  freq39 <- joinmarg_forerror(freq39, marg5, marg10, nr_individuals_per_regio_share)
  freq40 <- joinmarg_forerror(freq40, marg5, marg11, nr_individuals_per_regio_share)
  
  freq41 <- joinmarg_forerror(freq41, marg6, marg7, nr_individuals_per_regio_share)
  freq42 <- joinmarg_forerror(freq42, marg6, marg8, nr_individuals_per_regio_share)
  freq43 <- joinmarg_forerror(freq43, marg6, marg9, nr_individuals_per_regio_share)
  freq44 <- joinmarg_forerror(freq44, marg6, marg10, nr_individuals_per_regio_share)
  freq45 <- joinmarg_forerror(freq45, marg6, marg11, nr_individuals_per_regio_share)
  
  freq46 <- joinmarg_forerror(freq46, marg7, marg8, nr_individuals_per_regio_share)
  freq47 <- joinmarg_forerror(freq47, marg7, marg9, nr_individuals_per_regio_share)
  freq48 <- joinmarg_forerror(freq48, marg7, marg10, nr_individuals_per_regio_share)
  freq49 <- joinmarg_forerror(freq49, marg7, marg11, nr_individuals_per_regio_share)
  
  freq50 <- joinmarg_forerror(freq50, marg8, marg9, nr_individuals_per_regio_share)
  freq51 <- joinmarg_forerror(freq51, marg8, marg10, nr_individuals_per_regio_share)
  freq52 <- joinmarg_forerror(freq52, marg8, marg11, nr_individuals_per_regio_share)
  
  freq53 <- joinmarg_forerror(freq53, marg9, marg10, nr_individuals_per_regio_share)
  freq54 <- joinmarg_forerror(freq54, marg9, marg11, nr_individuals_per_regio_share)
  
  freq55 <- joinmarg_forerror(freq55, marg10, marg11, nr_individuals_per_regio_share)
  
  
  #freq1['National'] <- freq1tot
  #freq2['National'] <- freq2tot
  #freq3['National'] <- freq3tot
  #freq4['National'] <- freq4tot
  #freq5['National'] <- freq5tot
  #freq6['National'] <- freq6tot
  #freq7['National'] <- freq7tot
  #freq8['National'] <- freq8tot
  #freq9['National'] <- freq9tot
  #freq10['National'] <- freq10tot
  freq11['National'] <- freq11tot
  freq12['National'] <- freq12tot
  freq13['National'] <- freq13tot
  freq14['National'] <- freq14tot
  freq15['National'] <- freq15tot
  freq16['National'] <- freq16tot
  freq17['National'] <- freq17tot
  freq18['National'] <- freq18tot
  freq19['National'] <- freq19tot
  freq20['National'] <- freq20tot
  freq21['National'] <- freq21tot
  freq22['National'] <- freq22tot
  freq23['National'] <- freq23tot
  freq24['National'] <- freq24tot
  freq25['National'] <- freq25tot
  freq26['National'] <- freq26tot
  freq27['National'] <- freq27tot
  freq28['National'] <- freq28tot
  freq29['National'] <- freq29tot
  freq30['National'] <- freq30tot
  freq31['National'] <- freq31tot
  freq32['National'] <- freq32tot
  freq33['National'] <- freq33tot
  freq34['National'] <- freq34tot
  freq35['National'] <- freq35tot
  freq36['National'] <- freq36tot
  freq37['National'] <- freq37tot
  freq38['National'] <- freq38tot
  freq39['National'] <- freq39tot
  freq40['National'] <- freq40tot
  freq41['National'] <- freq41tot
  freq42['National'] <- freq42tot
  freq43['National'] <- freq43tot
  freq44['National'] <- freq44tot
  freq45['National'] <- freq45tot
  freq46['National'] <- freq46tot
  freq47['National'] <- freq47tot
  freq48['National'] <- freq48tot
  freq49['National'] <- freq49tot
  freq50['National'] <- freq50tot
  freq51['National'] <- freq51tot
  freq52['National'] <- freq52tot
  freq53['National'] <- freq53tot
  freq54['National'] <- freq54tot
  freq55['National'] <- freq55tot
  
  #freq1 <- freq1 %>% dplyr::select(Freq, Marg_freq, National)
  #freq2 <- freq2 %>% dplyr::select(Freq, Marg_freq, National)
  #freq3 <- freq3 %>% dplyr::select(Freq, Marg_freq, National)
  #freq4 <- freq4 %>% dplyr::select(Freq, Marg_freq, National)
  #freq5 <- freq5 %>% dplyr::select(Freq, Marg_freq, National)
  #freq6 <- freq6 %>% dplyr::select(Freq, Marg_freq, National)
  #freq7 <- freq7 %>% dplyr::select(Freq, Marg_freq, National)
  #freq8 <- freq8 %>% dplyr::select(Freq, Marg_freq, National)
  #freq9 <- freq9 %>% dplyr::select(Freq, Marg_freq, National)
  #freq10 <- freq10 %>% dplyr::select(Freq, Marg_freq, National)
  freq11 <- freq11 %>% dplyr::select(Freq, Marg_freq, National)
  freq12 <- freq12 %>% dplyr::select(Freq, Marg_freq, National)
  freq13 <- freq13 %>% dplyr::select(Freq, Marg_freq, National)
  freq14 <- freq14 %>% dplyr::select(Freq, Marg_freq, National)
  freq15 <- freq15 %>% dplyr::select(Freq, Marg_freq, National)
  freq16 <- freq16 %>% dplyr::select(Freq, Marg_freq, National)
  freq17 <- freq17 %>% dplyr::select(Freq, Marg_freq, National)
  freq18 <- freq18 %>% dplyr::select(Freq, Marg_freq, National)
  freq19 <- freq19 %>% dplyr::select(Freq, Marg_freq, National)
  freq20 <- freq20 %>% dplyr::select(Freq, Marg_freq, National)
  freq21 <- freq21 %>% dplyr::select(Freq, Marg_freq, National)
  freq22 <- freq22 %>% dplyr::select(Freq, Marg_freq, National)
  freq23 <- freq23 %>% dplyr::select(Freq, Marg_freq, National)
  freq24 <- freq24 %>% dplyr::select(Freq, Marg_freq, National)
  freq25 <- freq25 %>% dplyr::select(Freq, Marg_freq, National)
  freq26 <- freq26 %>% dplyr::select(Freq, Marg_freq, National)
  freq27 <- freq27 %>% dplyr::select(Freq, Marg_freq, National)
  freq28 <- freq28 %>% dplyr::select(Freq, Marg_freq, National)
  freq29 <- freq29 %>% dplyr::select(Freq, Marg_freq, National)
  freq30 <- freq30 %>% dplyr::select(Freq, Marg_freq, National)
  freq31 <- freq31 %>% dplyr::select(Freq, Marg_freq, National)
  freq32 <- freq32 %>% dplyr::select(Freq, Marg_freq, National)
  freq33 <- freq33 %>% dplyr::select(Freq, Marg_freq, National)
  freq34 <- freq34 %>% dplyr::select(Freq, Marg_freq, National)
  freq35 <- freq35 %>% dplyr::select(Freq, Marg_freq, National)
  freq36 <- freq36 %>% dplyr::select(Freq, Marg_freq, National)
  freq37 <- freq37 %>% dplyr::select(Freq, Marg_freq, National)
  freq38 <- freq38 %>% dplyr::select(Freq, Marg_freq, National)
  freq39 <- freq39 %>% dplyr::select(Freq, Marg_freq, National)
  freq40 <- freq40 %>% dplyr::select(Freq, Marg_freq, National)
  freq41 <- freq41 %>% dplyr::select(Freq, Marg_freq, National)
  freq42 <- freq42 %>% dplyr::select(Freq, Marg_freq, National)
  freq43 <- freq43 %>% dplyr::select(Freq, Marg_freq, National)
  freq44 <- freq44 %>% dplyr::select(Freq, Marg_freq, National)
  freq45 <- freq45 %>% dplyr::select(Freq, Marg_freq, National)
  freq46 <- freq46 %>% dplyr::select(Freq, Marg_freq, National)
  freq47 <- freq47 %>% dplyr::select(Freq, Marg_freq, National)
  freq48 <- freq48 %>% dplyr::select(Freq, Marg_freq, National)
  freq49 <- freq49 %>% dplyr::select(Freq, Marg_freq, National)
  freq50 <- freq50 %>% dplyr::select(Freq, Marg_freq, National)
  freq51 <- freq51 %>% dplyr::select(Freq, Marg_freq, National)
  freq52 <- freq52 %>% dplyr::select(Freq, Marg_freq, National)
  freq53 <- freq53 %>% dplyr::select(Freq, Marg_freq, National)
  freq54 <- freq54 %>% dplyr::select(Freq, Marg_freq, National)
  freq55 <- freq55 %>% dplyr::select(Freq, Marg_freq, National)
  
  
  
  df_allfreqs <- bind_rows(freq11, freq12, freq13, freq14, freq15, freq16, freq17, freq18, freq19,
                           freq20, freq21, freq22, freq23, freq24, freq25, freq26, freq27, freq28, freq29,
                           freq30, freq31, freq32, freq33, freq34, freq35, freq36, freq37, freq38, freq39,
                           freq40, freq41, freq42, freq43, freq44, freq45, freq46, freq47, freq48, freq49,
                           freq50, freq51, freq52, freq53, freq54, freq55)
  
  
  names(df_allfreqs)[names(df_allfreqs) == 'Freq'] <- 'Data_freq'
  
  df_allfreqs['Sq_error_survey_national'] <- (df_allfreqs$Data_freq - df_allfreqs$National)^2
  df_allfreqs['Sq_error_survey_marginal'] <- (df_allfreqs$Data_freq - df_allfreqs$Marg_freq)^2
  
  # Synthetic combinations. 
  
  #freq1synth <- data.frame(xtabs( ~ RURAL + HHTYPE + GDLcode, data = syn_pop)/nrow(syn_pop))
  #freq2synth <- data.frame(xtabs( ~ RURAL + HHSIZECAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  #freq3synth <- data.frame(xtabs( ~ RURAL + WEALTH + GDLcode, data = syn_pop)/nrow(syn_pop))
  #freq4synth <- data.frame(xtabs( ~ RURAL + AGECAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  #freq5synth <- data.frame(xtabs( ~ RURAL + EDUCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  #freq6synth <- data.frame(xtabs( ~ RURAL + GENDER + GDLcode, data = syn_pop)/nrow(syn_pop))
  #freq7synth <- data.frame(xtabs( ~ RURAL + FARMING + GDLcode, data = syn_pop)/nrow(syn_pop))
  #freq8synth <- data.frame(xtabs( ~ RURAL + FLOORCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  #freq9synth <- data.frame(xtabs( ~ RURAL + WALLCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  #freq10synth <- data.frame(xtabs( ~ RURAL + ROOFCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  
  freq11synth <- data.frame(xtabs( ~ HHTYPE + HHSIZECAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq12synth <- data.frame(xtabs( ~ HHTYPE + WEALTH + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq13synth <- data.frame(xtabs( ~ HHTYPE + AGECAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq14synth <- data.frame(xtabs( ~ HHTYPE + EDUCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq15synth <- data.frame(xtabs( ~ HHTYPE + GENDER + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq16synth <- data.frame(xtabs( ~ HHTYPE + FARMING + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq17synth <- data.frame(xtabs( ~ HHTYPE + FLOORCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq18synth <- data.frame(xtabs( ~ HHTYPE + WALLCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq19synth <- data.frame(xtabs( ~ HHTYPE + ROOFCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  
  freq20synth <- data.frame(xtabs( ~ HHSIZECAT + WEALTH + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq21synth <- data.frame(xtabs( ~ HHSIZECAT + AGECAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq22synth <- data.frame(xtabs( ~ HHSIZECAT + EDUCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq23synth <- data.frame(xtabs( ~ HHSIZECAT + GENDER + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq24synth <- data.frame(xtabs( ~ HHSIZECAT + FARMING + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq25synth <- data.frame(xtabs( ~ HHSIZECAT + FLOORCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq26synth <- data.frame(xtabs( ~ HHSIZECAT + WALLCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq27synth <- data.frame(xtabs( ~ HHSIZECAT + ROOFCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  
  freq28synth <- data.frame(xtabs( ~ WEALTH + AGECAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq29synth <- data.frame(xtabs( ~ WEALTH + EDUCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq30synth <- data.frame(xtabs( ~ WEALTH + GENDER + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq31synth <- data.frame(xtabs( ~ WEALTH + FARMING + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq32synth <- data.frame(xtabs( ~ WEALTH + FLOORCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq33synth <- data.frame(xtabs( ~ WEALTH + WALLCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq34synth <- data.frame(xtabs( ~ WEALTH + ROOFCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  
  freq35synth <- data.frame(xtabs( ~ AGECAT + EDUCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq36synth <- data.frame(xtabs( ~ AGECAT + GENDER + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq37synth <- data.frame(xtabs( ~ AGECAT + FARMING + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq38synth <- data.frame(xtabs( ~ AGECAT + FLOORCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq39synth <- data.frame(xtabs( ~ AGECAT + WALLCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq40synth <- data.frame(xtabs( ~ AGECAT + ROOFCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  
  freq41synth <- data.frame(xtabs( ~ EDUCAT + GENDER + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq42synth <- data.frame(xtabs( ~ EDUCAT + FARMING + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq43synth <- data.frame(xtabs( ~ EDUCAT + FLOORCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq44synth <- data.frame(xtabs( ~ EDUCAT + WALLCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq45synth <- data.frame(xtabs( ~ EDUCAT + ROOFCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  
  freq46synth <- data.frame(xtabs( ~ GENDER + FARMING + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq47synth <- data.frame(xtabs( ~ GENDER + FLOORCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq48synth <- data.frame(xtabs( ~ GENDER + WALLCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq49synth <- data.frame(xtabs( ~ GENDER + ROOFCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  
  freq50synth <- data.frame(xtabs( ~ FARMING + FLOORCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq51synth <- data.frame(xtabs( ~ FARMING + WALLCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq52synth <- data.frame(xtabs( ~ FARMING + ROOFCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  
  freq53synth <- data.frame(xtabs( ~ FLOORCAT + WALLCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  freq54synth <- data.frame(xtabs( ~ FLOORCAT + ROOFCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  
  freq55synth <- data.frame(xtabs( ~ WALLCAT + ROOFCAT + GDLcode, data = syn_pop)/nrow(syn_pop))
  
  
  
  
  #names(freq1synth)[names(freq1synth) == 'Freq'] <- 'Freq_synth'
  #names(freq2synth)[names(freq2synth) == 'Freq'] <- 'Freq_synth'
  #names(freq3synth)[names(freq3synth) == 'Freq'] <- 'Freq_synth'
  #names(freq4synth)[names(freq4synth) == 'Freq'] <- 'Freq_synth'
  #names(freq5synth)[names(freq5synth) == 'Freq'] <- 'Freq_synth'
  #names(freq6synth)[names(freq6synth) == 'Freq'] <- 'Freq_synth'
  #names(freq7synth)[names(freq7synth) == 'Freq'] <- 'Freq_synth'
  #names(freq8synth)[names(freq8synth) == 'Freq'] <- 'Freq_synth'
  #names(freq9synth)[names(freq9synth) == 'Freq'] <- 'Freq_synth'
  #names(freq10synth)[names(freq10synth) == 'Freq'] <- 'Freq_synth'
  names(freq11synth)[names(freq11synth) == 'Freq'] <- 'Freq_synth'
  names(freq12synth)[names(freq12synth) == 'Freq'] <- 'Freq_synth'
  names(freq13synth)[names(freq13synth) == 'Freq'] <- 'Freq_synth'
  names(freq14synth)[names(freq14synth) == 'Freq'] <- 'Freq_synth'
  names(freq15synth)[names(freq15synth) == 'Freq'] <- 'Freq_synth'
  names(freq16synth)[names(freq16synth) == 'Freq'] <- 'Freq_synth'
  names(freq17synth)[names(freq17synth) == 'Freq'] <- 'Freq_synth'
  names(freq18synth)[names(freq18synth) == 'Freq'] <- 'Freq_synth'
  names(freq19synth)[names(freq19synth) == 'Freq'] <- 'Freq_synth'
  names(freq20synth)[names(freq20synth) == 'Freq'] <- 'Freq_synth'
  names(freq21synth)[names(freq21synth) == 'Freq'] <- 'Freq_synth'
  names(freq22synth)[names(freq22synth) == 'Freq'] <- 'Freq_synth'
  names(freq23synth)[names(freq23synth) == 'Freq'] <- 'Freq_synth'
  names(freq24synth)[names(freq24synth) == 'Freq'] <- 'Freq_synth'
  names(freq25synth)[names(freq25synth) == 'Freq'] <- 'Freq_synth'
  names(freq26synth)[names(freq26synth) == 'Freq'] <- 'Freq_synth'
  names(freq27synth)[names(freq27synth) == 'Freq'] <- 'Freq_synth'
  names(freq28synth)[names(freq28synth) == 'Freq'] <- 'Freq_synth'
  names(freq29synth)[names(freq29synth) == 'Freq'] <- 'Freq_synth'
  names(freq30synth)[names(freq30synth) == 'Freq'] <- 'Freq_synth'
  names(freq31synth)[names(freq31synth) == 'Freq'] <- 'Freq_synth'
  names(freq32synth)[names(freq32synth) == 'Freq'] <- 'Freq_synth'
  names(freq33synth)[names(freq33synth) == 'Freq'] <- 'Freq_synth'
  names(freq34synth)[names(freq34synth) == 'Freq'] <- 'Freq_synth'
  names(freq35synth)[names(freq35synth) == 'Freq'] <- 'Freq_synth'
  names(freq36synth)[names(freq36synth) == 'Freq'] <- 'Freq_synth'
  names(freq37synth)[names(freq37synth) == 'Freq'] <- 'Freq_synth'
  names(freq38synth)[names(freq38synth) == 'Freq'] <- 'Freq_synth'
  names(freq39synth)[names(freq39synth) == 'Freq'] <- 'Freq_synth'
  names(freq40synth)[names(freq40synth) == 'Freq'] <- 'Freq_synth'
  names(freq41synth)[names(freq41synth) == 'Freq'] <- 'Freq_synth'
  names(freq42synth)[names(freq42synth) == 'Freq'] <- 'Freq_synth'
  names(freq43synth)[names(freq43synth) == 'Freq'] <- 'Freq_synth'
  names(freq44synth)[names(freq44synth) == 'Freq'] <- 'Freq_synth'
  names(freq45synth)[names(freq45synth) == 'Freq'] <- 'Freq_synth'
  names(freq46synth)[names(freq46synth) == 'Freq'] <- 'Freq_synth'
  names(freq47synth)[names(freq47synth) == 'Freq'] <- 'Freq_synth'
  names(freq48synth)[names(freq48synth) == 'Freq'] <- 'Freq_synth'
  names(freq49synth)[names(freq49synth) == 'Freq'] <- 'Freq_synth'
  names(freq50synth)[names(freq50synth) == 'Freq'] <- 'Freq_synth'
  names(freq51synth)[names(freq51synth) == 'Freq'] <- 'Freq_synth'
  names(freq52synth)[names(freq52synth) == 'Freq'] <- 'Freq_synth'
  names(freq53synth)[names(freq53synth) == 'Freq'] <- 'Freq_synth'
  names(freq54synth)[names(freq54synth) == 'Freq'] <- 'Freq_synth'
  names(freq55synth)[names(freq55synth) == 'Freq'] <- 'Freq_synth'
  
  df_synthfreqs <- bind_rows(freq11synth, freq12synth, freq13synth, freq14synth, freq15synth, freq16synth, freq17synth, freq18synth, freq19synth,
                             freq20synth, freq21synth, freq22synth, freq23synth, freq24synth, freq25synth, freq26synth, freq27synth, freq28synth, freq29synth,
                             freq30synth, freq31synth, freq32synth, freq33synth, freq34synth, freq35synth, freq36synth, freq37synth, freq38synth, freq39synth,
                             freq40synth, freq41synth, freq42synth, freq43synth, freq44synth, freq45synth, freq46synth, freq47synth, freq48synth, freq49synth,
                             freq50synth, freq51synth, freq52synth, freq53synth, freq54synth, freq55synth)
  
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
  
  
  
  filename_dferrors <- paste0("synth_errors_DHS_april24_", isocode, ".csv") #was oct, nov. 
  
  
  write.table(DF_errors, filename_dferrors, row.names = FALSE, sep = ',')
  
  
  print('saved output')
  
} 
  










