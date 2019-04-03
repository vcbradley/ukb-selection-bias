### recode hse

library('data.table')
library('memisc')
library('dplyr')
library('stringr')

setwd('/well/nichols/users/bwj567/')

source('/well/nichols/users/bwj567/mini-project-1/prep_hse/recode_functions_hse.R')



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
    #, nrows = 500    # for testing
    , select = unique(variables_hse)
    )

hse_data <- as_tibble(hse_data)

hse_data %>% str(.)


######### DO RECODES
hse_data_recoded <- doHSERecodes(hse_data)

names(hse_data_recoded)


########### WRITE OUT TO FILE
# select only the demo cols
hse_data_recoded <- hse_data_recoded %>% select(., c('SerialA', names(hse_data_recoded)[grep('demo_|health_', names(hse_data_recoded))]))

# write out recoded data
write.csv(hse_data_recoded, hse_recoded_file, row.names = F)


rm()




