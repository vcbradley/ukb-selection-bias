## data recode
library('data.table')
library('memisc')
library('dplyr')
library('stringr')
library('MESS')
setwd('/well/nichols/users/bwj567/data')

#source code with recode functions
source('/well/nichols/users/bwj567/mini-project-1/prep_ukb/recode_functions.R')


# set params for testing
full_baseline_file = 'ukb25120_raw_baseline.tsv'
full_imaging_file = 'ukb25120_raw_imaging.tsv'  
all_UKB_vars_file = 'ukb25120_allvars.csv'
apoe_file = 'ApoE.dat'
var_codings_file = '/well/nichols/users/bwj567/mini-project-1/variable_codings.csv'
new_file_prefix = 'ukb25120_weighting'

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

# Read in Apoe data
apoe = data.table(read.table(apoe_file, header = T))
setnames(apoe, old = names(apoe), new = c('e3', 'e4', 'eid'))



#### BASELINE ####

# read in ALL the imaging data
data_base = fread(full_baseline_file
    #, nrows = 5000     #for testing
    , select = c('eid', variables_all[!is.na(baseline),]$baseline)
    , col.names = c('eid', variables_all[!is.na(baseline),]$var)
    , na.strings = ''
    )

# merge in ApoE data
data_base = merge(data_base, apoe, by = 'eid', all.x = T)

data_base <- as_tibble(data_base)

### IMPUTE DOB
#impute bday as the middle of the month in which they were born
data_base <- data_base %>% mutate(dob_imputed = as.Date(paste0(yob,'-',mob,'-','15')))

#check error
data_base %>% mutate(age_same = (age(dob_imputed, assessment_date) - age)) %>% group_by(age_same) %>% tally()





#do recodes
data_base_recoded <- doRecode(data_base)
#rm(data_base)
data_base_recoded %>% str(.)

# select the vars we need
data_base_recoded <- data_base_recoded %>% select(., grep('eid|^demo_|^health_|^age|^dob|^bmi|addr_east_recent|addr_north_recent|^assessment', names(data_base_recoded)))


data_base_recoded %>% group_by(health_apoe_phenotype, health_apoe_level) %>% tally()


#### IMAGING ####

## read in data
data_imaging = fread(full_imaging_file
    #, nrows = 10000     #for testing
    , select = c('eid', variables_all[!is.na(imaging),]$imaging)
    , col.names = c('eid', variables_all[!is.na(imaging),]$var)
    , na.strings = ''
    )

# merge in ApoE data
data_imaging = merge(data_imaging, apoe, by = 'eid', all.x = T)  # LEFT JOIN BECASE NOT ALL IN APOE DATA!!

data_imaging <- as_tibble(data_imaging)

# limit to only imaging subjects
data_imaging %>% filter(., !is.na(MRI_t1_struct)) %>% count()
data_imaging <- data_imaging %>% filter(., !is.na(MRI_t1_struct))

# join in impt columns from the baseliine data
data_imaging <- left_join(data_imaging, data_base %>% dplyr::select(eid, dob_imputed, sex, base_age = age, base_assessment_date = assessment_date, base_ethnicity = ethnicity))

# calculate age at imaging
data_imaging <- data_imaging %>% mutate(age = age(dob_imputed, assessment_date))

# check dist of diff between age at base and age at imaging
data_imaging %>% mutate(age_diff = age - base_age) %>% group_by(age_diff) %>% tally()
data_imaging %>% mutate(date_diff = age(base_assessment_date, assessment_date)) %>% group_by(date_diff) %>% tally()


# impute missing ethnicities with ethnicity at base
data_imaging <- data_imaging %>% mutate(ethnicity = ifelse(is.na(ethnicity) | ethnicity < 0, base_ethnicity, ethnicity))
data_imaging %>% group_by(ethnicity) %>% tally() %>% data.table


## do recodes
data_imaging_recoded <- doRecode(data_imaging)
#rm(data_imaging)

# select the vars we need
data_imaging_recoded <- data_imaging_recoded %>% dplyr::select(., grep('eid|^demo_|^health_|^age|^bmi|^MRI|has_t1_MRI|addr_east_recent|addr_north_recent|^assessment', names(data_imaging_recoded)))


data_imaging %>% dplyr::select(., MRI_t1_struct) %>% summary(.)

data_imaging_recoded %>% group_by(is.na(MRI_t1_struct), has_t1_MRI) %>% tally()

data_imaging_recoded %>% group_by(health_apoe_phenotype, health_apoe_level) %>% tally()



#############################
# MERGE INTO WEIGHTING FILE #
#############################

# rename cols with prefix 'base'
data_base_recoded <- data_base_recoded %>% rename_at(vars(-contains('eid')), funs(paste0('base_', .)))

# rename cols with prefix 'img'
data_imaging_recoded <- data_imaging_recoded %>% rename_at(vars(-contains('eid')), funs(paste0('img_', .)))


# merge imaging flags onto baseline data
data_base_recoded <- merge(data_base_recoded, select(data_imaging_recoded,c('eid', 'img_MRI_completed', 'img_MRI_method', 'img_MRI_safe', 'img_has_t1_MRI')), by = 'eid', all.x = T)
data_base_recoded <- data_base_recoded %>% mutate(img_has_t1_MRI = ifelse(is.na(img_has_t1_MRI), 0, img_has_t1_MRI))

length(unique(data_base_recoded$eid))
length((data_base_recoded$eid))

data_base_recoded %>% group_by(img_has_t1_MRI) %>% tally()
data_imaging_recoded %>% group_by(img_has_t1_MRI) %>% tally()
data_imaging_recoded %>% group_by(img_MRI_completed,img_has_t1_MRI, is.na(img_MRI_t1_struct)) %>% tally()

#####################
# WRITE OUT TO FILE #
#####################

write.csv(data_base_recoded, paste0(new_file_prefix,'_base.csv'), row.names = F)
write.csv(data_imaging_recoded, paste0(new_file_prefix,'_img.csv'), row.names = F)





