## data recode
library('data.table')
library('memisc')
library('dplyr')
setwd('/well/nichols/users/bwj567')

#source code with recode functions
source('/well/nichols/users/bwj567/mini-project-1/recode_functions.R')


# set params for testing
full_baseline_file = 'data/ukb25120_5k_baseline.tsv'
all_UKB_vars_file = 'data/ukb25120_allvars.csv'
var_codings_file = '/well/nichols/users/bwj567/mini-project-1/variable_codings.csv'
new_data_file = 'ukb25120_weighting.csv'

## set params from commandline args
# args = commandArgs(trailingOnly=TRUE)
# if(is.na(args[1])){
#     full_data_file = 'ukb25120.csv'
# } else{
#     full_data_file = args[1]
# }

# if(is.na(args[2])){
#     instance = 'baseline'
# } else {
#     instance = args[2]
# }

# if(is.na(args[3])){
#     new_data_file = paste0('ukb25120-weighting-', instance, '.csv')
# } else {
#     new_data_file = args[3]
# }


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


# read in ALL the imaging data
data = fread(full_baseline_file
    , nrows = 5000     #for testing
    , select = c('eid', variables_all[!is.na(baseline),]$baseline)
    , col.names = c('eid', variables_all[!is.na(baseline),]$var)
    , na.strings = ''
    )
data <- as_tibble(data)

data %>% group_by(year(assessment_date)) %>% tally()

# write out sample for UKBparse testing
#write.csv(data,'/well/nichols/users/bwj567/data/ukb25120_sample5k.csv', row.names = F)



###########
# RECODES #
###########

data_recoded <- doRecode(data)

data_recoded %>% str(.)

data_recoded %>% select(., grep('eid|^demo_|^educ_|^health_|age|bmi|addr_east_recent|addr_north_recent', names(data_recoded)))
names(data_recoded)


#############
# RECODE QC #
#############











