# Download synthpop_data from https://doi.org/10.7910/DVN/KJC3RH

gdlcode = 'AFGr102'
isocode = substr(gdlcode, 1,3)

filename_synth <- 'AFG_unknown2_sept23_synthpop_AFGr102.dat'


# Be aware:  
# Household ID (HHID) are unique within a region, not within a country.   


# import file with nr of individuals per region
filename_indi <- paste0('Nr_individuals_data_availability.csv')

individuals_per_region <- read.csv(filename_indi, sep = ',', header = TRUE) 

l = individuals_per_region[individuals_per_region$GDLCODE == gdlcode,]$Nr_individuals

n_columns = 10 

con = file(filename_synth, "rb")

bindata = readBin(con, integer(), n = l*n_columns)

close(con)
  
# reshape bindata
bindata <- array(bindata, dim=c(l, n_columns))
bindata <- as.data.frame(bindata)

colnames(bindata) <- c('ECON', 'RURAL', 'FARM', 'AGE', 'GENDER', 
                         'EDUC', 'HHTYPE', 'HH_ID', 'REL_HEAD', 'HHSIZE_CAT')


# To add gdlcode region number. 
if (nchar(gdlcode) == 7){
  gdlcodenr <- substr(gdlcode, 5,7)
  print(gdlcodenr)
} else if (nchar(gdlcode) == 4){
  gdlcodenr <- 1
  print(gdlcodenr)
}  

bindata['GDLCODE'] <- as.integer(gdlcodenr)

# column names explained:
# ECON: Economic situation. 1: poorest 20% households, 5: richest 20% households. 
# RURAL: Settlement type. 1: rural. 0: urban. 
# FARM: Farmer. 1: yes. 0: no. Not available for all countries. When unavailable, FARM is random generated 0 or 1. 
# AGE: Age groups. 1:0-15, 2: 16-25, 3: 26-35, 4: 36-45, 5: 46-55, 6: 56-65, 7:65+
# GENDER: gender. 1: male. 0: female. 
# EDUC: highest achieved education level. 1: less than primary. 2: primary completed. 
# 3: less than secondary. 4: secondary completed. 5: higher education
# HHTYPE: household types. 1: single. 2: couple. 3: couple with children. 4: one parent with children
# 5: couple with (non-)relatives. 6: couple with children and (non-)relatives. 
# 7: one parent with children and (non-)relatives. 8: other
# HH_ID: household ID. Be aware than household ID's are unique within a region, not within a country. 
# REL_HEAD: relationship to household head. 1: head. 2: partner. 3: child. 4: relative. 5: non-relative. 
# HHSIZE_CAT: household size categories. 1: 1 person. 2: 2 persons. 3: 3-4 persons. 4: 5-6. 5:7-10. 6: 10+. 
