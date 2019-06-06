#recode_functions.R

#function to pull last val that isn't NA
getLast = function(r) {
    r = r[length(r):1]
    r[!is.na(r)][1]
    }


# function to do all UKB recodes
doRecode <- function(data){

    #### GENDER
    if(exists('sex', data)){
        data <- data %>% mutate(demo_sex = case_when(
            sex == 0 ~ 'Female'
            , sex == 1 ~ 'Male'
            , TRUE ~ '99-DNK/Refused'))

        #checks
        data %>% group_by(demo_sex) %>% tally()
    }
    

    #### AGE
    if(exists('age', data)){

        if(data %>% select(age) %>% max > 75){
            data <- data %>% mutate(demo_age_bucket = case_when(
                age < 50 ~ '45 to 49'
                , age < 55 ~ '50 to 54'
                , age < 60 ~ '55 to 59'
                , age < 65 ~ '60 to 64'
                , age < 70 ~ '65 to 69'
                , age < 75 ~ '70 to 74'
                , TRUE ~ '75 to 79'
            ))
        }else{
            data <- data %>% mutate(demo_age_bucket = case_when(
                age < 45 ~ '40 to 44'
                , age < 50 ~ '45 to 49'
                , age < 55 ~ '50 to 54'
                , age < 60 ~ '55 to 59'
                , age < 65 ~ '60 to 64' 
                , TRUE ~ '65 to 69'  
            ))
        }
        

        # check age dist
        data %>% group_by(demo_age_bucket) %>% tally()
        data %>% group_by(demo_age_bucket) %>% summarize(min(age), max(age))
    }


    #### ETHNICITY
    if(exists('ethnicity', data)){

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
            ,(ethnicity == 4001) ~ '08-Black Caribbean'
            ,(ethnicity == 4002) ~ '09-Black African'
            ,(substr(ethnicity, 1,1) == 4) ~ '10-Black Other'
            , ethnicity > 0 ~ '11-Other'
            ,TRUE ~  '99-DNK/Refused'
            ))

        #4-way
        data <- data %>% mutate(demo_ethnicity_4way = case_when(
             substr(ethnicity,1,1) == 1 ~ '01-White'
            , substr(ethnicity,1,1) == 3 ~  '03-Asian'
            , substr(ethnicity,1,1) == 4 ~  '04-Black'
            , ethnicity > 0 ~ '02-Mixed/Other'
            , TRUE ~ '99-DNK/Refused'
            ))

        #white/non-white
        data <- data %>% mutate(demo_white = case_when(
            substr(ethnicity,1,1) == 1 ~ '01-White'
            , ethnicity < 0 ~ '99-DNK/Refused'
            , TRUE ~ '02-Non-white'
            ))

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
                    demo_empl_employed = rowSums(. == 1, na.rm = T)
                    , demo_empl_retired = rowSums(. == 2, na.rm = T)
                    , demo_empl_homemaker = rowSums(. == 3, na.rm = T)
                    , demo_empl_disabled = rowSums(. == 4, na.rm = T)
                    , demo_empl_unemployed = rowSums(. == 5, na.rm = T)
                    , demo_empl_volunteer = rowSums(. == 6, na.rm = T)
                    , demo_empl_student = rowSums(. == 7, na.rm = T)
                    )
            )

        data %>% group_by(demo_empl_employed, demo_empl_retired, demo_empl_homemaker, demo_empl_disabled, demo_empl_unemployed, demo_empl_volunteer, demo_empl_student) %>% tally()

    }


    #### OCCUPATION
    if(exists('job_code', data)){

        #coalesce variables
        if(exists('job_code_deduced', data)){
            data <- data %>% mutate(job_code_full = ifelse(is.na(job_code_deduced), job_code, job_code_deduced))
        } else {
            data <- data %>% mutate(job_code_full = job_code)
        }
        data <- data %>% mutate(job_code_major = substr(job_code_full,1,1))


        data <- data %>% mutate(demo_occupation = case_when(
            demo_empl_employed == 0 ~ '10-unemployed'
            , (job_code_major == 1) ~ '01-manager'
            , (job_code_major == 2) ~ '02-professional'
            , (job_code_major == 3) ~ '03-assoc professional'
            , (job_code_major == 4) ~ '04-admin'
            , (job_code_major == 5) ~ '05-skilled trades'
            , (job_code_major == 6) ~ '06-personal service'
            , (job_code_major == 7) ~ '07-sales customer service'
            , (job_code_major == 8) ~ '08-industrial'
            , (job_code_major == 9) ~ '09-elementary'
            , TRUE ~ '99-DNK/Refused'
            ))

        # check
        data %>% group_by(demo_occupation, job_code_major) %>% tally()

    }  

    

    #### EDUCATION
    if(exists('educ1', data)){

        # binary indicators
        data <- cbind(data, 
            data %>% 
                select(., grep('educ', names(data))) %>%
                transmute(
                    demo_educ_collegeplus = rowSums(. == 1, na.rm = T)
                    , demo_educ_alevels = rowSums(. == 2, na.rm = T)
                    , demo_educ_olevels = rowSums(. == 3, na.rm = T)
                    , demo_educ_cses = rowSums(. == 4, na.rm = T)
                    , demo_educ_vocational = rowSums(. == 5, na.rm = T)
                    , demo_educ_profesh = rowSums(. == 6, na.rm = T)
                    )
            )

        #check
        data %>% group_by(demo_educ_collegeplus, demo_educ_alevels, demo_educ_olevels, demo_educ_cses, demo_educ_vocational, demo_educ_profesh) %>% tally()


        ## highest qualification
        data <- data %>% mutate(demo_educ_highest_full = case_when(
                                  (educ1 == 1) ~ '01-College plus'
                                , (educ1 == 2) ~ '02-A Levels'
                                , (educ1 == 3) ~ '03-O Levels'
                                , (educ1 == 4) ~ '04-CSEs'
                                , (educ1 == 5) ~ '05-Vocational'
                                , (educ1 == 6) ~ '06-Other professional'
                                , (educ1 == -7) ~ '07-None'
                                , TRUE ~ '99-DNK/Refused'
            ))

        data <- data %>% mutate(demo_educ_highest = case_when(
                                  (educ1 %in% c(1,6)) ~ '01-College plus/profesh'
                                , (educ1 == 2) ~ '02-A Levels'
                                , (educ1 %in% c(3,4)) ~ '03-O Levels/CSEs'
                                , (educ1 > 0) ~ '04-Vocational/Other'
                                , (educ1 == -7) ~ '05-None'
                                , TRUE ~ '99-DNK/Refused'
            ))

        data %>% group_by(demo_educ_highest, educ1) %>% tally()

    }
  

            
    #### INCOME

    if(exists('hh_income_cat', data)){
        data <- data %>% mutate(demo_income_bucket = case_when(
            (hh_income_cat == 1) ~ '01-Under 18k'
            , (hh_income_cat == 2) ~ '02-18k to 31k'
            , (hh_income_cat == 3) ~ '03-31k to 52k'
            , (hh_income_cat == 4) ~ '04-52k to 100k'
            , (hh_income_cat == 5) ~ '05-Over 100k'
            , TRUE ~ '99-DNK/Refused'
            ))

        data %>% group_by(demo_income_bucket) %>% tally()
    }

    #### RECENT ADDRESS
    if(exists('addr_east1', data)){

        ## Check that logic works
        addr_dates <- data %>% select(., grep('addr_firstdate', names(data))) %>% mutate_all(as.Date)

        # check that max date is the last one listed
        all.equal(
            addr_dates %>% apply(1, max, na.rm = T)
            , addr_dates %>% apply(1, getLast)
            )

        # apply same logic to locations instead of dates
        data$addr_east_recent <- data %>% select(., grep('addr_east', names(data))) %>% apply(1, getLast)
        data$addr_north_recent <- data %>% select(., grep('addr_north', names(data))) %>% apply(1, getLast)

        #check that ir worked
        data %>% select(addr_east_recent, addr_north_recent) %>% glimpse(.)
        data %>% filter(is.na(addr_east2)) %>% summarize(prop_equal = mean(addr_east1 == addr_east_recent))
        data %>% filter(is.na(addr_east6) & !is.na(addr_east5)) %>% summarize(prop_equal = mean(addr_east5 == addr_east_recent))
        data %>% filter(is.na(addr_north6) & !is.na(addr_north5)) %>% summarize(prop_equal = mean(addr_north5 == addr_north_recent))

    }

    ## NOTE: other location recodes will be more complex, should happen elsewhere
    # Place of birth

    #### YEAR IMMIGRATED
    if(exists('year_immigrated', data)){
        data <- data %>% mutate(demo_year_immigrated = case_when(
            (year_immigrated < 1941) ~ '01-Before 1941'
            , (year_immigrated <= 1950) ~ '02-1941 to 1950'
            , (year_immigrated <= 1960) ~ '03-1951 to 1960'
            , (year_immigrated <= 1970) ~ '04-1961 to 1970'
            , (year_immigrated <= 1980) ~ '05-1971 to 1980'
            , (year_immigrated <= 1990) ~ '06-1981 to 1990'
            , (year_immigrated <= 2000) ~ '07-1991 to 2000'
            , (year_immigrated <= 2003) ~ '08-2001 to 2003'
            , (year_immigrated <= 2006) ~ '09-2004 to 2006'
            , (year_immigrated <= 2009) ~ '10-2007 to 2009'
            , TRUE ~ '00-Born in UK'
            ))

        data %>% group_by(demo_year_immigrated) %>% tally()
    }

    #### HOUSEHOLD VARS

    ## SIZE
    if(exists('hh_size', data)){
        data <- data %>% mutate(demo_hh_size = case_when(
                    (hh_size == 1) ~ '1'
                    , (hh_size == 2) ~ '2'
                    , (hh_size == 3) ~ '3'
                    , (hh_size == 4) ~ '4'
                    , (hh_size > 4) ~ '5 or more'
                    , (hh_size == -3) ~ '99-DNK/Refused'
            ))

        data %>% group_by(demo_hh_size, hh_size) %>% tally()
    }

    ## RENT OR OWN HOME
    if(exists('hh_ownrent', data)){
        data <- data %>% mutate(demo_hh_ownrent = case_when(
            (hh_ownrent == 1) ~ '01-Own outright'
            , (hh_ownrent == 2) ~ '02-Own with mortgage'
            , (hh_ownrent == 3) ~ '03-Rent from LA'
            , (hh_ownrent == 4) ~ '04-Rent private'
            , (hh_ownrent == 5) ~ '05-Shared'
            , (hh_ownrent == 6) ~ '06-Rent free'
            , TRUE ~ '99-DNK/Refused'
            ))

        data %>% group_by(demo_hh_ownrent, hh_ownrent) %>% tally()
    }

    ## TYPE
    if(exists('hh_accom_type', data)){
        data <- data %>% mutate(demo_hh_accom_type = case_when(
            (hh_accom_type == 1) ~ '01-House'
            , (hh_accom_type <= 4) ~ '02-Flat, apartment or temp'
            , hh_accom_type %in% c(5,-7) ~ '03-Other'
            , TRUE ~ '99-DNK/Refused'
            ))

        data %>% group_by(demo_hh_accom_type, hh_accom_type) %>% tally()
    }


    #### HEALTH VARS

    # smoking
    if(exists('smoking_status', data)){
        data <- data %>% mutate(health_smoking_status = case_when(
            (smoking_status == 2) ~ '01-Current'
            , (smoking_status == 1) ~ '02-Previous'
            , (smoking_status == 0) ~ '03-Never'
            , TRUE ~ '99-DNK/Refused'
            ))

        data <- data %>% mutate(health_smoking_current = ifelse(health_smoking_status == '01-Current', '01-Current', '02-Not current'))

        data %>% group_by(health_smoking_current, health_smoking_status, smoking_status) %>% tally()
    }


    ## BMI
    # https://www.nhs.uk/common-health-questions/lifestyle/what-is-the-body-mass-index-bmi/
    if(exists('bmi', data)){
        data <- data %>% mutate(health_BMI_bucket = case_when(
            (bmi < 18.5) ~ '01-Underweight'
            , (bmi < 24.9) ~ '02-Healthy'
            , (bmi < 29.9) ~ '03-Overweight'
            , (bmi >= 29.9) ~ '04-Obese'
            , TRUE ~ '99-DNK/Refused'
            ))

        data %>% group_by(health_BMI_bucket) %>% summarize(length(bmi), min(bmi), max(bmi))
    }

    # alc consumption
    if(exists('alc_freq', data)){
        data <- data %>% mutate(health_alc_freq = case_when(
            (alc_freq == 1) ~ '01-Every day or almost'
            , (alc_freq == 2) ~ '02-Three to four times pw'
            , (alc_freq == 3) ~ '03-Once or twice pw'
            , (alc_freq == 4) ~ '04-One to three times pm' # NOT QUITE THE SAME AS CENSUS 
            , (alc_freq == 5) ~ '05-Special occasions'
            , (alc_freq == 6) ~ '06-Never'
            , TRUE ~ '99-DNK/Refused'
            ))

        data %>% group_by(health_alc_freq, alc_freq) %>% tally()
    }


    # Total alc consumption
    if(exists('alc_weekly_beer', data)){
        data$health_alc_weekly_total <-  data %>% 
            select(., grep('alc_weekly', names(data))) %>% 
            mutate(health_alc_weekly_total = rowSums(., na.rm = T)) %>% pull()

        data %>% select(health_alc_weekly_total) %>% glimpse(.)
        data %>% select(health_alc_weekly_total) %>% summary()
    }
 

    ### Blood pressure
    # https://www.acc.org/latest-in-cardiology/articles/2017/11/08/11/47/mon-5pm-bp-guideline-aha-2017

    # BP Category
    if(exists('bp_systolic_auto1', data)){

        # coalesce and average the measurements
        data <- data %>% 
            mutate_at(as.numeric, .vars = c('bp_systolic_auto1', 'bp_systolic_auto2', 'bp_systolic_man1', 'bp_systolic_man2')) %>%
            mutate(bp_systolic = rowMeans(select(., c('bp_systolic_auto1', 'bp_systolic_auto2', 'bp_systolic_man1', 'bp_systolic_man2')), na.rm = T))

        data <- data %>% 
            mutate_at(as.numeric, .vars = c('bp_diastolic_auto1', 'bp_diastolic_auto2', 'bp_diastolic_man1', 'bp_diastolic_man2')) %>%
            mutate(bp_diastolic = rowMeans(select(., c('bp_diastolic_auto1', 'bp_diastolic_auto2', 'bp_diastolic_man1', 'bp_diastolic_man2')), na.rm = T))

        # categories
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

        # checks
        data %>% group_by(health_bp_cat) %>% tally()
        data %>% group_by(health_bp_cat) %>% summarize(min_sis = min(bp_systolic), max_sis = max(bp_systolic), min_dia = min(bp_diastolic), max_dia = max(bp_diastolic))

    }

    #ever diagnosed with high blood pressure
    if(exists('bp_age_diagnosed', data)){
        data <- data %>%
        mutate(health_bp_high_ever = 
            case_when(
                (bp_age_diagnosed >=18) ~ '01-Yes'
                , bp_age_diagnosed < 0 ~ '03-DNK/Refused'
                , is.na(bp_age_diagnosed) ~ '02-No'
                )
            )
        data %>% group_by(health_bp_high_ever) %>% tally()
    }

    # take BP meds
    if(exists('bp_current_meds1', data)){

        data$health_bp_meds_current <- data %>%
            select(bp_current_meds1,bp_current_meds2,bp_current_meds3) %>%
            mutate(health_bp_meds_current_TF = (rowSums(. == 2, na.rm = T) > 0)
                , health_bp_meds_current_REFUSED = (rowSums(. == -1 | . == -3, na.rm = T) > 0)
                , health_bp_meds_current = case_when(
                    health_bp_meds_current_TF ~ '01-Yes'
                    , health_bp_meds_current_REFUSED ~ '99-DNK/Refused'
                    , TRUE ~ '02-No'
                    )) %>%
            pull()

        data %>% group_by(health_bp_meds_current, bp_current_meds1,bp_current_meds2,bp_current_meds3) %>% tally()
    }

    ## DIABETES
    if(exists('diabetes', data)){

        data$health_diabetes <- data %>% 
            select(.,diabetes) %>% 
            mutate(health_diabetes = ifelse(diabetes != 1 | is.na(diabetes), '02-No', '01-Yes')) %>% 
            pull()

        data %>% group_by(diabetes, health_diabetes) %>% tally()
    }


    ##### IMAGING
    if(exists('MRI_t1_struct', data)){
        data <- data %>% mutate(
            has_t1_MRI = as.numeric(!is.na(MRI_t1_struct))
            )
    }

    ##### Apoe
    if(exists('e3', data)){
        data <- data %>% mutate(
            health_apoe_phenotype = case_when(
                (e4 == 1) ~ '01-e4/e4'
                , (e3 == 1) ~ '02-e3/e4'
                , TRUE ~ '03-other'
                )
            )

        data  <- data %>% mutate(health_apoe_level = (2 * e4) + e3)

        data %>% count(e3, e4, health_apoe_phenotype)
    }


    
    return(data)
}








