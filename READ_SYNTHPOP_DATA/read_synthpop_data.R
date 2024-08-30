library(R.utils)
library(Rcpp)


gdlcode = '' #example: 'AFGr101' see Nr_individuals_data_availability.csv

filename <- paste0('synthpop_', gdlcode, '.dat.gz')


# Be aware:  
# Household ID (HHID) are unique within a region, not within a country.   


# import file with nr of individuals per region
filename_indi <- paste0('Nr_individuals_data_availability.csv')

individuals_per_region <- read.csv(filename_indi, sep = ',', header = TRUE) 

l = individuals_per_region[individuals_per_region$GDLcode == gdlcode,]$Nr_individuals

n_columns = 15


con <- gzcon(file(filename, "rb"))

bindata = readBin(con, integer(), n = l*n_columns)

close(con)


# reshape bindata
bindata <- array(bindata, dim=c(l, n_columns))
bindata <- as.data.frame(bindata)

colnames(bindata) <- c('HID', 'RELATE_HEAD', 'INCOME', 'WEALTH', 'RURAL', 'AGE', 'GENDER', 
                         'EDUC', 'HHTYPE', 'HHSIZE_CAT','AGRI_OWNERSHIP', 'FLOOR', 'WALL', 'ROOF', 'SOURCE')



# column names explained:
# HID: household ID. Be aware that household ID's are unique within a region, not within a country. 
# RELATE_HEAD: relationship to household head. 1: head. 2: partner. 3: child. 4: relative. 5: non-relative. 
# INCOME: 1: poorest 20% individuals, 2: poorer 20%, 3: middle 20%, 4: richer 20%, 5: richest 20% individuals, -1: unavailable for country 
# WEALTH: 1: poorest 20% individuals, 2: poorer 20%, 3: middle 20%, 4: richer 20%, 5: richest 20% individuals, -1: unavailable for country 
# RURAL: settlement type. 1: rural. 0: urban. 
# AGE: age groups. 1:0-4, 2: 5-14, 3: 15-24, 4: 25-34, 5: 35-44, 6: 45-54, 7:55-64, 8:65+
# GENDER: gender. 1: male. 0: female. 
# EDUC: highest achieved education level. 1: less than primary. 2: primary completed. 
# 3: less than secondary. 4: secondary completed. 5: higher education
# HHTYPE: household types. 1: single. 2: couple. 3: couple with children. 4: one parent with children
# 5: couple with (non-)relatives. 6: couple with children and (non-)relatives. 
# 7: one parent with children and (non-)relatives. 8: other
# HHSIZE_CAT: household size categories. 1: 1 person. 2: 2 persons. 3: 3-4 persons. 4: 5-6. 5:7-10. 6: 10+. 
# AGRI_OWNERSHIP: Household owns land for agriculture. 1: yes. 0: no, -1: unavailable for country
# FLOOR: floor material. 1: natural, 2: rudimentary, 3: finished, -1: unavailable for country
# WALL: wall material. 1: natural, 2: rudimentary, 3: finished, -1: unavailable for country
# ROOF: roof material. 1: natural, 2: rudimentary, 3: finished, -1: unavailable for country
# SOURCE: data source of synthetic population. 1: LIS, 2: LIS survey, 3: LIS marginals, 
# 4: Modeled by LIS data, 5: DHS, 6: Modeled by DHS data


