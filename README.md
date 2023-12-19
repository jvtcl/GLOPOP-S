# GLOPOP-S
This repository provides code that was used to generate a synthetic population for each country in the world. 
This results in a global synthetic population database, GLOPOP-S, that is stored on …. The database contains .. households and individuals for the year 2015 and includes the following attributes: age, education, economic situation, gender, settlement type (urban/rural), household size and household type. 
The synthetic population is based on national microdata and scaled to regional statistics to account for socio-economic and demographic differences within a country. The synthetic household data can be downloaded per country or administrative unit, see 

The DATA folder contains the data files needed to generate the database. We use microdata from the Luxembourg Income Study (LIS), Integrated Public Use Microdata Series (IPUMS) and Demographic and Health Surveys (DHS). (We do not publish the original data files extracted from the LIS, IPUMS and DHS servers, for more information contact marijn.ton@vu.nl.). Data from the Global Data Lab (GDL) are used to generate a synthetic population when no microdata from LIS, IPUMS or DHS were available. 
The SCRIPTS folder contains the scripts to generate the database. Depending on which data were available, we created separate scripts. Filename provides details about the data that was available for each country. There are several options: LIS survey + regional statistics (marginals), LIS survey, LIS regional statistics, IPUMS&DHS survey + regional statistics, IPUMS marginals, no data.  To assess the goodness of fit of the synthetic population database, we evaluate how well the synthetic population resembles the observed survey data. 
The VALIDATION folder contains all the files necessary to evaluate the performance of our methods in handling missing data (missing micro data and/or missing regional statistics). 
The FIGURES folder contains the figures as published in -link to paper-.
The IMPORT folder contains an R file and a python file that can be used to read the synthetic population data that is saved in binary format on https://doi.org/10.7910/DVN/KJC3RH  


