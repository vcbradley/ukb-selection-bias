## data recode
library('data.table')
library('memisc')
library('dplyr')
setwd('/well/nichols/projects/UKB/SMS')

#source code with recode functions
source('/well/nichols/users/bwj567/mini-project-1/recode_functions.R')


# set params for testing
full_data_file = 'ukb25120.csv'
instance = 'baseline'
new_data_file = paste0('ukb25120-weighting-', instance, '.csv')

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
data_head = names(fread(full_data_file, nrows = 0))

# read in list of variables
variables_raw = fread('/well/nichols/users/bwj567/mini-project-1/variable_codings.csv')
variables_raw = unique(variables_raw[, .(cat, var, `biobank code`)])
setnames(variables_raw, old = 'biobank code', new = 'biobank_code')


# grab list of actual variable names in the data (at baseline)
variables = lapply(variables_raw[, biobank_code], function(c){
    data_head[grepl(paste0('^',c,'-', ifelse(instance == 'imaging', '2', '0')), data_head)]
    })
names(variables) = variables_raw[, var]
variables = unlist(variables)
variables = data.table(var = names(variables), code = variables)


# read in ALL the imaging data
data = fread(full_data_file
    , nrows = 5000     #for testing
    , select = c('eid', variables$code)
    , col.names = c('eid', variables$var)
    , na.strings = ''
    )
data

data[, .N, .(year(assessment_date))][order(year)]




###########
# RECODES #
###########

#### GENDER
data[, gender_male := as.numeric(sex)]
data[, demo_sex := ifelse(sex == 0, 'M', 'F')]

#### AGE
data[, max(age_baseline)]
#data[, age := age_baseline]
data[, demo_age_bucket := cases(
    age < 45 -> '40 to 44'
    , age < 50 -> '44 to 49'
    , age < 55 -> '50 to 54'
    , age < 60 -> '55 to 59'
    , age < 65 -> '60 to 64'
    , age <= 70 -> '65 to 70'        # note this is different from the census - grouping 70 in with 65-69
    #, age == 70 -> '70plus'
    )]
# check age dist
data[, .(min_age = min(age)
    , max_age = max(age)
    , .N
    , dist = .N/nrow(data))
, demo_age_bucket][order(demo_age_bucket)]


#### ETHNICITY
data[, demo_ethnicity_full := cases(
    '02-White Irish' = (ethnicity == 1002)
    ,'03-White Other' = (ethnicity == 1003)
    ,'01-White' = (substr(ethnicity, 1,1) == 1)
    ,'04-Mixed' = (substr(ethnicity, 1,1) == 2)
    ,'05-Asian Indian' = (ethnicity == 3001)
    ,'05-Asian Pakistani' = (ethnicity == 3002)
    ,'06-Asian Bangladeshi' = (ethnicity == 3003)
    ,'07-Asian Other' = (substr(ethnicity, 1,1) == 3)
    ,'08-Black Carribean' = (ethnicity == 4001)
    ,'09-Black African' = (ethnicity == 4002)
    ,'10-Black Other' = (substr(ethnicity, 1,1) == 4)
    , '11-Other' = TRUE
    )]
data[, .N, .(ethnicity, demo_ethnicity_full)][order(demo_ethnicity_full)]

# 4-way ethnicity
data[, demo_ethnicity_4way := cases(
    '01-White' = substr(ethnicity,1,1) == 1
    , '03-Asian' = substr(ethnicity,1,1) == 3
    , '04-Black' = substr(ethnicity,1,1) == 4
    , '02-Mixed/Other' = TRUE
    )]
data[, .(.N), .(demo_ethnicity_4way, ethnicity)][order(demo_ethnicity_4way)]

# white
data[, demo_white := 'Non-white']
data[substr(ethnicity, 1,1) == 1, demo_white := 'White']


#### EMPLOYMENT STATUS

data = cbind(data
    , rbindlist(apply(data[, .(job_status1, job_status2, job_status3, job_status4, job_status5, job_status6, job_status7)]
        , 1, doEmplRecode)))

# check overlap
data[, .N, .(employed, retired, homemaker, disabled, unemployed, volunteer, student)]


#### OCCUPATION
table(unlist(lapply(data[, job_code], length))) # all the same length

#coalesce variables
data[, job_code_full := ifelse(is.na(job_code_deduced), job_code, job_code_deduced)]
data[, job_code_major := substr(job_code_full,1,1)]

#recode
data[, demo_occupation := cases(
	'01-manager' = (job_code_major == 1)
	, '02-professional' = (job_code_major == 2)
	, '03-assoc professional' = (job_code_major == 3)
	, '04-admin' = (job_code_major == 4)
	, '05-skilled trades' = (job_code_major == 5)
	, '06-personal service' = (job_code_major == 6)
	, '07-sales customer service' = (job_code_major == 7)
	, '08-industrial' = (job_code_major == 8)
	, '09-elementary' = (job_code_major == 9)
	, '10-unemployed/DNK' = TRUE
	)]
#fill NAs
data[is.na(demo_occupation), demo_occupation := '10-unemployed/DNK']

# check occupation distribution
data[, .(.N, dist = .N/nrow(data)), .(job_code_major, demo_occupation)][order(demo_occupation)]


#### EDUCATION

## binary indicators
data = cbind(data
    , rbindlist(apply(data[, .(educ1, educ2, educ3, educ4, educ5, educ6)], 1, doEducRecode)))

## highest qualification
data[, demo_educ_highest := cases(
            '01-College plus' = (educ1 == 1)
            , '02-A Levels' = (educ1 == 2)
            , '03-O Levels' = (educ1 == 3)
            , '04-CSEs' = (educ1 == 4)
            , '05-Vocational' = (educ1 == 5)
            , '06-Other professional' = (educ1 == 6)
            , '07-None' = TRUE
            )]

data[, .N, .(demo_educ_highest, educ1)][order(demo_educ_highest)]

        
#### INCOME
data[, .N, hh_income_cat]
data[, demo_income_bucket := cases(
    '01-Under 18k' = (hh_income_cat == 1)
    , '02-18k to 31k' = (hh_income_cat == 2)
    , '03-31k to 52k' = (hh_income_cat == 3)
    , '04-52k to 100k' = (hh_income_cat == 4)
    , '05-Over 100k' = (hh_income_cat == 5)
    , '06-DNK' = (hh_income_cat == -1)
    , '07-Refused' = TRUE
    )]
data[, .N, .(hh_income_cat, demo_income_bucket)][order(demo_income_bucket)]



#### RECENT ADDRESS
addr_dates = data[,variables[grepl('addr_firstdate', var), var][15:1], with = F]

# CHECK: the most recent date is in the last populated field
# sum(apply(addr_dates,1, function(r) {
#     r[!is.na(r)][1]
#     })==
# apply(addr_dates,1, function(r) {
#     max(r, na.rm = T)
#     })
# , na.rm = T)

#which means that we can apply the same logic to find most recent location
# EASTING of most recent address
addr_east_all = data[, variables[grepl('addr_east', var), var][15:1], with = F]
data[, 'addr_east_recent' := apply(addr_east_all, 1, function(r){
    r[!is.na(r)][1]
    })]

# NORTHING of most recent address
addr_north_all = data[, variables[grepl('addr_north', var), var][15:1], with = F]
data[, 'addr_north_recent' := apply(addr_north_all, 1, function(r){
    r[!is.na(r)][1]
    })]

#some checks that it worked
data[is.na(addr_east2), sum(addr_east1 == addr_east_recent, na.rm = T)/sum(is.na(addr_east2))]
data[is.na(addr_east6) & !is.na(addr_east5), .(sum(addr_east5 == addr_east_recent, na.rm = T), .N)]
data[is.na(addr_north6) & !is.na(addr_north5), .(sum(addr_north5 == addr_north_recent, na.rm = T), .N)]


## NOTE: other location recodes will be more complex, should happen elsewhere
# Place of birth

#### YEAR IMMIGRATED
data[, demo_year_immigrated := cases(
	'01-Before 1941' = (year_immigrated < 1941)
	, '02-1941 to 1950' = (year_immigrated <= 1950)
	, '03-1951 to 1960' = (year_immigrated <= 1960)
	, '04-1961 to 1970' = (year_immigrated <= 1970)
	, '05-1971 to 1980' = (year_immigrated <= 1980)
	, '06-1981 to 1990' = (year_immigrated <= 1990)
	, '07-1991 to 2000' = (year_immigrated <= 2000)
	, '08-2001 to 2003' = (year_immigrated <= 2003)
	, '09-2004 to 2006' = (year_immigrated <= 2006)
	, '10-2007 to 2009' = (year_immigrated <= 2009)
	)]
data[is.na(year_immigrated), demo_year_immigrated := '00-Born in UK']  #this seems safe to assume data[, .N, .(is.na(pob_uk_east), is.na(year_immigrated))]

data[, .N, demo_year_immigrated][order(demo_year_immigrated)]

#### HOUSEHOLD VARS

## SIZE
data[, .N, hh_size]

data[, demo_hh_size := cases(
	'99-DNK/Refused' = (hh_size == -3)
	, '1' = (hh_size == 1)
	, '2' = (hh_size == 2)
	, '3' = (hh_size == 3)
	, '4' = (hh_size == 4)
	, '5 or more' = (hh_size > 4)
	)]
data[is.na(demo_hh_size), demo_hh_size := '99-DNK/Refused']

data[, .N, demo_hh_size][order(demo_hh_size)]

## RENT OR OWN HOME
data[, .N, hh_ownrent][order(hh_ownrent)]
data[, demo_hh_ownrent := cases(
	'01-Own outright' = (hh_ownrent == 1)
	, '02-Own with mortgage' = (hh_ownrent == 2)
	, '03-Rent from LA' = (hh_ownrent == 3)
	, '04-Rent private' = (hh_ownrent == 4)
	, '05-Shared' = (hh_ownrent == 5)
	, '06-Rent free' = (hh_ownrent == 6)
	)]
data[is.na(demo_hh_ownrent), demo_hh_ownrent := '99-DNK/Refused']

data[, .N, demo_hh_ownrent][order(demo_hh_ownrent)]

## TYPE
data[, .N, hh_type]
data[, demo_hh_type := cases(
	'01-House' = (hh_type == 1)
	, '02-Flat or apartment' = (hh_type <= 4)
	)]
data[is.na(demo_hh_type), demo_hh_type := '03-Other']


#### HEALTH VARS

# smoking
data[, health_smoking_current := cases(
	'01-Current' = (smoking_status == 2)
	, '02-Previous' = (smoking_status == 1)
	, '03-Never' = (smoking_status == 0)
	)]
data[is.na(health_smoking_current), health_smoking_current := '99-DNK/Refused']

# ever smoked
data[, health_smoking_ever := ifelse(health_smoking_current == '01-Current' | health_smoking_current == '02-Previous', '01-Yes', '02-No')]
data[, .N, .(health_smoking_current, health_smoking_ever)]

## BMI
# https://www.nhs.uk/common-health-questions/lifestyle/what-is-the-body-mass-index-bmi/
data[, health_BMI_bucket := cases(
	'01-Underweight' = (bmi < 18.5)
	, '02-Healthy' = (bmi < 24.9)
	, '03-Overweight' = (bmi < 29.9)
	, '04-Obese' = (bmi >= 29.9)
	)]
data[is.na(health_BMI_bucket), health_BMI_bucket := '99-DNK/Refused']
data[, .N, health_BMI_bucket][order(health_BMI_bucket)]


data[, health_alc_freq := cases(
	'01-Every day or almost' = (alc_freq == 1)
	, '02-Three to four times pw' = (alc_freq == 2)
	, '03-Once or twice pw' = (alc_freq == 3)
	, '04-One to three times pm' = (alc_freq == 4) # NOT QUITE THE SAME AS CENSUS
	, '05-Special occasions' = (alc_freq == 5)
	, '06-Never' = (alc_freq == 6)
	)]
data[is.na(health_alc_freq), health_alc_freq := '99-DNK/Refused']

data[, .N, .(alc_freq, health_alc_freq)]



# Total alc consumption
setNumeric(data, col.names = c('alc_weekly_beer', 'alc_weekly_champ', 'alc_weekly_fortwine', 'alc_weekly_other', 'alc_weekly_redwine', 'alc_weekly_spirits'))
replaceNA(data, col.names = c('alc_weekly_beer', 'alc_weekly_champ', 'alc_weekly_fortwine', 'alc_weekly_other', 'alc_weekly_redwine', 'alc_weekly_spirits'))

for(i in c('alc_weekly_beer', 'alc_weekly_champ', 'alc_weekly_fortwine', 'alc_weekly_other', 'alc_weekly_redwine', 'alc_weekly_spirits'))
	data[, (i) := ifelse(get(i) < 0, 0, get(i))]

data[, health_alc_weekly_total := alc_weekly_beer + alc_weekly_champ
	+ alc_weekly_fortwine
	+ alc_weekly_other
	+ alc_weekly_redwine
	+ alc_weekly_spirits]

data[health_alc_weekly_total > 0, .N, cut(health_alc_weekly_total, breaks = quantile(health_alc_weekly_total,probs = seq(0,1,length=5)))][order(cut)]


# Blood pressure
# https://www.acc.org/latest-in-cardiology/articles/2017/11/08/11/47/mon-5pm-bp-guideline-aha-2017

data[, health_bp_cat := cases(
	'01-Normal' = (bp_systolic_auto < 120 & bp_diastolic_auto < 80)
	, '02-Elevated' = (bp_systolic_auto >= 120 & bp_systolic_auto < 130 & bp_diastolic_auto < 80)
	, '03-Stage 1 HBP' = ((bp_systolic_auto >= 130 & bp_systolic_auto < 140) | (bp_diastolic_auto >= 80 & bp_diastolic_auto < 90))
	, '04-Stage 2 HBP' = (bp_systolic_auto >= 140 | bp_diastolic_auto >= 90)
	)]
data[is.na(health_bp_cat), health_bp_cat := '99-DNK/Refused']

data[, .(.N, min(bp_systolic_auto), max(bp_systolic_auto)
	, min(bp_diastolic_auto), max(bp_diastolic_auto)
	), health_bp_cat]



#############
# RECODE QC #
#############

data[, .(health_alc_weekly_total)]


