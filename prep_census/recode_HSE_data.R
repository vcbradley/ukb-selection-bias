### recode hse

library('data.table')
library('memisc')
library('dplyr')
library('stringr')

setwd('/well/nichols/users/bwj567/')

#source('/well/nichols/users/bwj567/mini-project-1/prep_hse/recode_functions_hse.R')



var_codings_file = '/well/nichols/users/bwj567/mini-project-1/variable_codings.csv'
hse_file = '_data/hse2016_eul.tab'
hse_recoded_file = 'data/hse16_recoded.csv'


# read in list of vars we want
variables_hse = fread(var_codings_file)
#variables_hse[, hse16_var := tolower(hse16_var)]
variables_hse = variables_hse[hse16_var != "",]$hse16_var
variables_hse = unlist(lapply(variables_hse, function(x) unlist(strsplit(x, ', '))))


####### READ IN DATA
hse_data = fread(hse_file    
    , nrows = 500    # for testing
    , select = unique(variables_hse)
    )

hse_data <- as_tibble(hse_data)

hse_data %>% str(.)


##### GENDER

hse_data <- hse_data %>% mutate(demo_sex = case_when(
    Sex == 1 ~ 'Male'
    , Sex == 2 ~ 'Female'
    , TRUE ~ '99-DNK/Refused'
    ))

hse_data %>% group_by(Sex, demo_sex) %>% tally()


#### AGE

hse_data <- hse_data %>% mutate(demo_age_bucket = case_when(
    Age35g == 1.0 ~ '00 to 01'
    , Age35g == 2.0 ~ '02 to 04'
    , Age35g == 3.0 ~ '05 to 07'
    , Age35g == 4.0 ~ '08 to 10'
    , Age35g == 5.0 ~ '11 to 12'
    , Age35g == 6.0 ~ '13 to 15'
    , Age35g == 7.0 ~ '16 to 19'
    , Age35g == 8.0 ~ '20 to 24'
    , Age35g == 9.0 ~ '25 to 29'
    , Age35g == 10.0 ~ '30 to 34'
    , Age35g == 11.0 ~ '35 to 39'
    , Age35g == 12.0 ~ '40 to 44'
    , Age35g == 13.0 ~ '45 to 49'
    , Age35g == 14.0 ~ '50 to 54'
    , Age35g == 15.0 ~ '55 to 59'
    , Age35g == 16.0 ~ '60 to 64'
    , Age35g == 17.0 ~ '65 to 69'
    , Age35g == 18.0 ~ '70 to 74'
    , Age35g == 19.0 ~ '75 to 79'
    , Age35g == 20.0 ~ '80 to 84'
    , Age35g == 21.0 ~ '85 to 89'
    , Age35g == 22.0 ~ '90+'
    ))


hse_data %>% group_by(Age35g, demo_age_bucket) %>% tally()
















