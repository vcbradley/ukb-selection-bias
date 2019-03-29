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

doRecode <- function(data, instance = 'baseline'){
    #### GENDER
    if(exists('sex', data)){
        data <- data %>% mutate(demo_sex = ifelse(sex == 0, 'M', 'F'))

        #checks
        data %>% group_by(demo_sex) %>% tally()
    }
    

    #### AGE
    if(exists('age', data)){
        data <- data %>% mutate(demo_age_bucket = case_when(
            age < 45 ~ '40 to 44'
            , age < 50 ~ '44 to 49'
            , age < 55 ~ '50 to 54'
            , age < 60 ~ '55 to 59'
            , age < 65 ~ '60 to 64'
            , age <= 70 ~ '65 to 70'  
            , TRUE ~ '70 plus' 
            ))

        # check age dist
        data %>% group_by(demo_age_bucket) %>% tally()
        data %>% group_by(demo_age_bucket) %>% summarize(min(age), max(age))
    }


    #### ETHNICITY
    if(exists('ethnity'), data){

        # full
        data <- data %>% mutate(demo_ethnicity_full = case_when(
            (ethnicity == 1002) ~ '02-White Irish'
            ,(ethnicity == 1003) ~ '03-White Other'
            ,(substr(ethnicity, 1,1) == 1) ~ '01-White'
            ,(substr(ethnicity, 1,1) == 2) ~ '04-Mixed'
            ,(ethnicity == 3001) ~ '05-Asian Indian'
            ,(ethnicity == 3002) ~ '05-Asian Pakistani'
            ,(ethnicity == 3003) ~ '06-Asian Bangladeshi'
            ,(substr(ethnicity, 1,1) == 3) ~ '07-Asian Other'
            ,(ethnicity == 4001) ~ '08-Black Carribean'
            ,(ethnicity == 4002) ~ '09-Black African'
            ,(substr(ethnicity, 1,1) == 4) ~ '10-Black Other'
            ,TRUE ~  '11-Other'
            ))

        #4-way
        data <- data %>% mutate(demo_ethnicity_4way = case_when(
             substr(ethnicity,1,1) == 1 ~ '01-White'
            , substr(ethnicity,1,1) == 3 ~  '03-Asian'
            , substr(ethnicity,1,1) == 4 ~  '04-Black'
            , TRUE ~  '02-Mixed/Other'
            ))

        #white/non-white
        data <- data %>% mutate(demo_white = ifelse(substr(ethnicity, 1,1) == 1, '01-White', '02-Non-white'))

        #checks
        data %>% group_by(demo_ethnicity_full) %>% tally()
        data %>% group_by(demo_ethnicity_4way) %>% tally()
        data %>% group_by(demo_white) %>% tally()
        data %>% group_by(ethnicity, demo_ethnicity_full, demo_ethnicity_4way, demo_white) %>% tally()

    }


    #### EMPLOYMENT STATUS
    if(exists('job_status1', data)){
        data <- cbind(data, 
            data %>% 
                select(.,grep('job_status', names(data))) %>%
                transmute(
                    employed = rowSums(. == 1, na.rm = T)
                    , retired = rowSums(. == 2, na.rm = T)
                    , homemaker = rowSums(. == 3, na.rm = T)
                    , disabled = rowSums(. == 4, na.rm = T)
                    , unemployed = rowSums(. == 5, na.rm = T)
                    , volunteer = rowSums(. == 6, na.rm = T)
                    , student = rowSums(. == 7, na.rm = T)
                    )
            )

        data %>% group_by(employed, retired, homemaker, disabled, unemployed, volunteer, student) %>% tally()

    }


    #### OCCUPATION
    if(exists('job_code', data)){

        #coalesce variables
        data <- data %>% 
            mutate(job_code_full = ifelse(is.na(job_code_deduced), job_code, job_code_deduced)
                , job_code_major = substr(job_code_full,1,1)
            )

        data <- data %>% mutate(demo_occupation = case_when(
            (job_code_major == 1) ~ '01-manager'
            , (job_code_major == 2) ~ '02-professional'
            , (job_code_major == 3) ~ '03-assoc professional'
            , (job_code_major == 4) ~ '04-admin'
            , (job_code_major == 5) ~ '05-skilled trades'
            , (job_code_major == 6) ~ '06-personal service'
            , (job_code_major == 7) ~ '07-sales customer service'
            , (job_code_major == 8) ~ '08-industrial'
            , (job_code_major == 9) ~ '09-elementary'
            , TRUE ~ '10-unemployed/DNK'
            ))

        # check
        data %>% group_by(demo_occupation, job_code_major) %>% tally()

    }  

    

    #### EDUCATION
    if(exists('educ1', data)){

        # binary indicators
        data <- cbind(data, 
            data %>% 
                select(.,grep('educ', names(data))) %>%
                transmute(
                    educ_collegeplus = rowSums(. == 1, na.rm = T)
                    , educ_alevels = rowSums(. == 2, na.rm = T)
                    , educ_olevels = rowSums(. == 3, na.rm = T)
                    , educ_cses = rowSums(. == 4, na.rm = T)
                    , educ_vocational = rowSums(. == 5, na.rm = T)
                    , educ_profesh = rowSums(. == 6, na.rm = T)
                    )
            )

        #check
        data %>% group_by(educ_collegeplus, educ_alevels, educ_olevels, educ_cses, educ_vocational, educ_profesh) %>% tally()


        ## highest qualification
        data <- data %>% mutate(demo_educ_highest = case_when(
                                  (educ1 == 1) ~ '01-College plus'
                                , (educ1 == 2) ~ '02-A Levels'
                                , (educ1 == 3) ~ '03-O Levels'
                                , (educ1 == 4) ~ '04-CSEs'
                                , (educ1 == 5) ~ '05-Vocational'
                                , (educ1 == 6) ~ '06-Other professional'
                                , TRUE ~ '07-None'
            ))

        data %>% group_by(demo_educ_highest, educ1) %>% tally()

    }
  

            
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
    data[, .N, hh_accom_type]
    data[, demo_hh_accom_type := cases(
        '01-House' = (hh_accom_type == 1)
        , '02-Flat or apartment' = (hh_accom_type <= 4)
        )]
    data[is.na(demo_hh_accom_type), demo_hh_accom_type := '03-Other']


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
    data[, names(data)[grep('bp_', names(data))], with = F]

    # BP Category
    data <- as_tibble(data)
    data <- data %>% 
        mutate_at(as.numeric, .vars = c('bp_systolic_auto1', 'bp_systolic_auto2', 'bp_systolic_man1', 'bp_systolic_man2')) %>%
        mutate(bp_systolic = rowMeans(select(., c('bp_systolic_auto1', 'bp_systolic_auto2', 'bp_systolic_man1', 'bp_systolic_man2')), na.rm = T))

    data <- data %>% 
        mutate_at(as.numeric, .vars = c('bp_diastolic_auto1', 'bp_diastolic_auto2', 'bp_diastolic_man1', 'bp_diastolic_man2')) %>%
        mutate(bp_diastolic = rowMeans(select(., c('bp_diastolic_auto1', 'bp_diastolic_auto2', 'bp_diastolic_man1', 'bp_diastolic_man2')), na.rm = T))


    data <- data %>%
        mutate(health_bp_cat = 
            case_when(
            (bp_systolic < 120 & bp_diastolic < 80) ~ '01-Normal',
            (bp_systolic >= 120 & bp_systolic < 130 & bp_diastolic < 80) ~ '02-Elevated',
            ((bp_systolic >= 130 & bp_systolic < 140) | (bp_diastolic >= 80 & bp_diastolic < 90)) ~ '03-Stage 1 HBP',
            (bp_systolic >= 140 | bp_diastolic >= 90) ~ '04-Stage 2 HBP',
            TRUE ~ '99-DNK/Refused'
            )
        )

    data %>% group_by(health_bp_cat) %>% tally()
    data %>% group_by(health_bp_cat) %>% summarize(min_sis = min(bp_systolic), max_sis = max(bp_systolic), min_dia = min(bp_diastolic), max_dia = max(bp_diastolic))


    # ever diagnosed
    data <- data %>%
        mutate(health_bp_high_ever = 
            case_when(
                (bp_age_diagnosed >=18) ~ '01-Yes'
                , bp_age_diagnosed < 0 ~ '03-DNK/Refused'
                , is.na(bp_age_diagnosed) ~ '02-No'
                )
            )
    data %>% group_by(health_bp_high_ever) %>% tally()

    # HBP MEDS

    data$health_bp_meds_current <- data %>%
        select(bp_current_meds1,bp_current_meds2,bp_current_meds3) %>%
        mutate(health_bp_meds_current_TF = rowSums(. == 2, na.rm = T)
            , health_bp_meds_current = ifelse(health_bp_meds_current_TF == 1, '01-Yes', '02-No')) %>%
        pull()

    data %>% group_by(health_bp_meds_current, bp_current_meds1,bp_current_meds2,bp_current_meds3) %>% tally()


    ## DIABETES
    data %>% select(., contains('diabetes'))
    data %>% group_by(diabetes) %>% tally()

    data$health_diabetes <- data %>% 
        select(.,diabetes) %>% 
        mutate(health_diabetes = ifelse(diabetes != 1 | is.na(diabetes), '02-No', '01-Yes')) %>% 
        pull()

    data %>% group_by(diabetes, health_diabetes) %>% tally()


    return(data)
}



##### BRAINS

if(instance == 'imaging'){
	data <- data %>% mutate(in_imaging_cohort = ifelse(!is.na(MRI_t1_struct) & MRI_completed == 1, 1, 0))
	data %>% group_by(is.na(MRI_t1_struct), MRI_completed, in_imaging_cohort) %>% tally()

}




#############
# RECODE QC #
#############











