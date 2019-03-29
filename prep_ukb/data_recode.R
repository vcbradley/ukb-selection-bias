## data recode
library('data.table')
library('memisc')
library('dplyr')
library('stringr')
setwd('/well/nichols/users/bwj567/data')

#source code with recode functions
source('/well/nichols/users/bwj567/mini-project-1/prep_ukb/recode_functions.R')


# set params for testing
full_baseline_file = 'ukb25120_5k_baseline.tsv'
full_imaging_file = 'ukb25120_5k_imaging.tsv'
all_UKB_vars_file = 'ukb25120_allvars.csv'
var_codings_file = '/well/nichols/users/bwj567/mini-project-1/variable_codings.csv'
new_data_file = 'ukb25120_weighting.csv'



####################
# SET UP VARIABLES #
####################

# read in data header to get all names
## ONLY DO ONCE
#all_UKB_vars = names(fread('/well/nichols/projects/UKB/SMS/ukb25120.csv', nrows = 0))
#write.csv(all_UKB_vars, file = all_UKB_vars_file, row.names=F)

all_UKB_vars = as.vector(fread(all_UKB_vars_file)$x)

# read in list of variables
variables_raw = fread(var_codings_file)
variables_raw = unique(variables_raw[, .(cat, var, `biobank code`)])
setnames(variables_raw, old = 'biobank code', new = 'biobank_code')


# grab list of actual variable names in the data (at baseline)
variables_baseline = lapply(variables_raw[, biobank_code], function(c){
    all_UKB_vars[grepl(paste0('^',c,'-0'), all_UKB_vars)]
    })
names(variables_baseline) = variables_raw[, var]
variables_baseline = unlist(variables_baseline)
variables_baseline = data.table(var = names(variables_baseline), code = variables_baseline)


# grab list of actual variable names in the data (at baseline)
variables_imaging = lapply(variables_raw[, biobank_code], function(c){
    all_UKB_vars[grepl(paste0('^',c,'-2'), all_UKB_vars)]
    })
names(variables_imaging) = variables_raw[, var]
variables_imaging = unlist(variables_imaging)
variables_imaging = data.table(var = names(variables_imaging), code = variables_imaging)


variables_all = merge(variables_baseline, variables_imaging, all = T, by = 'var')
setnames(variables_all, c('var','baseline', 'imaging'))

variables_all[is.na(imaging),var]



##############
# DO RECODES #
##############


#### BASELINE ####

# read in ALL the imaging data
data_base = fread(full_baseline_file
    , nrows = 5000     #for testing
    , select = c('eid', variables_all[!is.na(baseline),]$baseline)
    , col.names = c('eid', variables_all[!is.na(baseline),]$var)
    , na.strings = ''
    )
data_base <- as_tibble(data_base)


#do recodes
data_base_recoded <- doRecode(data_base)
rm(data_base)
data_base_recoded %>% str(.)

# select the vars we need
data_base_recoded <- data_base_recoded %>% select(., grep('eid|^demo_|^health_|^age|^bmi|addr_east_recent|addr_north_recent', names(data_base_recoded)))



#### IMAGING ####

## read in data
data_imaging = fread(full_imaging_file
    , nrows = 5000     #for testing
    , select = c('eid', variables_all[!is.na(imaging),]$imaging)
    , col.names = c('eid', variables_all[!is.na(imaging),]$var)
    , na.strings = ''
    )
data_imaging <- as_tibble(data_imaging)


## do recodes
data_imaging_recoded <- doRecode(data_imaging)
rm(data_imaging)
data_imaging_recoded %>% str(.)

# select the vars we need
data_imaging_recoded <- data_imaging_recoded %>% select(., grep('eid|^demo_|^health_|^age|^bmi|addr_east_recent|addr_north_recent', names(data_imaging_recoded)))



#############################
# MERGE INTO WEIGHTING FILE #
#############################

# rename cols with prefix 'base'
data_base_recoded <- data_base_recoded %>% rename_at(vars(-contains('eid')), funs(paste0('base_', .)))

# rename cols with prefix 'img'
data_imaging_recoded <- data_imaging_recoded %>% rename_at(vars(-contains('eid')), funs(paste0('img_', .)))

#merge
data_weighting <- merge(data_base_recoded, data_imaging_recoded, by = 'eid')

names(data_weighting)


#####################
# WRITE OUT TO FILE #
#####################

write.csv(data_weighting, new_data_file, row.names = F)





