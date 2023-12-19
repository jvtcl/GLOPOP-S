import numpy as np
import pandas as pd

# Data available on  https://doi.org/10.7910/DVN/KJC3RH

# Filename .dat file.
filename = 'AFG_unknown2_sept23_synthpop_AFGr102.dat' 
data_np = np.fromfile(filename, dtype=np.int32)

n_columns = 10

n_people = data_np.size// n_columns
# reshapa data
data_reshaped = np.reshape(data_np, (n_columns, n_people)).transpose()

attribute_names = ['ECON', 'RURAL', 'FARM', 'AGE', 'GENDER', 'EDUC', 'HHTYPE', 'HH_ID', 'REL_HEAD', 'HHSIZE_CAT']
df = pd.DataFrame(data_reshaped, columns=attribute_names)

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
