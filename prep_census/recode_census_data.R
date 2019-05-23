## Recode census data
## data recode
library('data.table')
library('memisc')
library('dplyr')
library('stringr')

setwd('/well/nichols/users/bwj567/')

source('/well/nichols/users/bwj567/mini-project-1/prep_census/recode_functions_census.R')

var_codings_file = '/well/nichols/users/bwj567/mini-project-1/variable_codings.csv'
census_file = '_data/recodev12.csv'
census_recoded_file = 'data/census11_recoded.csv'


# read in list of vars we want
variables_census = fread(var_codings_file)
variables_census[, census_var := tolower(census_var)]
variables_census = variables_census[census_var != "",]$census_var

# get census data header
#census_data_header = names(fread(census_file, nrows = 0))

####### READ IN DATA
census_data = fread(census_file    
    #, nrows = 500    # for testing
    , select = unique(variables_census))

census_data <- as_tibble(census_data)

###### DO RECODE
census_data_recoded <- doCensusRecode(census_data)
rm(census_data)

########### WRITE OUT TO FILE
# select only the demo cols
census_data_recoded <- census_data_recoded %>% select(., c('caseno', names(census_data_recoded)[grep('demo_', names(census_data_recoded))]))

# write out recoded data
write.csv(census_data_recoded, census_recoded_file, row.names = F)

rm()


