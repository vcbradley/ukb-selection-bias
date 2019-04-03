#############################
# HSE 2016-specific recodes #
#############################

doHSERecodes <- function(data){
    ##### GENDER
    data <- data %>% mutate(demo_sex = case_when(
        Sex == 1 ~ 'Male'
        , Sex == 2 ~ 'Female'
        , TRUE ~ '99-DNK/Refused'
        ))

    data %>% group_by(Sex, demo_sex) %>% tally()


    #### AGE

    data <- data %>% mutate(demo_age_bucket = case_when(
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
        , TRUE ~ '99-DNK/Refused'
        ))


    data %>% group_by(Age35g, demo_age_bucket) %>% tally()


    #### ETHNICITY

    data <- data %>% mutate(demo_ethnicity_4way = case_when(
            Origin2 == 1.0 ~ '01-White'
            , Origin2 == 3.0 ~ '03-Asian'
            , Origin2 == 2.0 ~ '04-Black'
            , Origin2 >= 4 ~ '02-Mixed/Other'
            , TRUE ~ '99-DNK/Refused'
            ))

    data <- data %>% mutate(demo_white = case_when(
            Origin2 == 1.0 ~ '01-White'
            , Origin2 < 0 ~ '99-DNK/Refused'
            , TRUE ~ '02-Non-white'
            ))

    data %>% group_by(Origin2, demo_ethnicity_4way, demo_white) %>% tally()


    #### EMPLOYMENT STATUS

    data <- cbind(data, 
                data %>% 
                    select(.,grep('Activb2', names(data))) %>%
                    transmute(
                        demo_empl_employed = rowSums(. >= 2 & . <= 4, na.rm = T)
                        , demo_empl_retired = rowSums(. == 8, na.rm = T)
                        , demo_empl_homemaker = rowSums(. == 9, na.rm = T)
                        , demo_empl_disabled = rowSums(. == 7, na.rm = T)
                        , demo_empl_unemployed = rowSums(. == 5, na.rm = T)
                        , demo_empl_volunteer = 0#rowSums(. == 100, na.rm = T)
                        , demo_empl_student = rowSums(. == 1, na.rm = T)
                        )
                )

    data %>% group_by(Activb2, demo_empl_employed, demo_empl_retired, demo_empl_homemaker, demo_empl_disabled, demo_empl_unemployed, demo_empl_volunteer, demo_empl_student) %>% tally()


    #### OCCUPATION

    data <- data %>% mutate(demo_occupation = case_when(
                (substr(SOC2010B, 1, 1) == 1) ~ '01-manager'
                , (substr(SOC2010B, 1, 1) == 2) ~ '02-professional'
                , (substr(SOC2010B, 1, 1) == 3) ~ '03-assoc professional'
                , (substr(SOC2010B, 1, 1) == 4) ~ '04-admin'
                , (substr(SOC2010B, 1, 1) == 5) ~ '05-skilled trades'
                , (substr(SOC2010B, 1, 1) == 6) ~ '06-personal service'
                , (substr(SOC2010B, 1, 1) == 7) ~ '07-sales customer service'
                , (substr(SOC2010B, 1, 1) == 8) ~ '08-industrial'
                , (substr(SOC2010B, 1, 1) == 9) ~ '09-elementary'
                , TRUE ~ '10-unemployed/DNK'
                ))

    # check
    data %>% group_by(demo_occupation, SOC2010B) %>% tally()


    #### Education

    data <- data %>% mutate(demo_educ_highest = case_when(
            (topqual3 == 1) ~ '01-College plus/profesh'
            , (topqual3 %in% c(2,3)) ~ '02-A Levels'
            , (topqual3 %in% c(4,5)) ~ '03-O Levels/CSEs'
            , (topqual3 == 7) ~ '05-None'
            , (topqual3 < 0) ~ '99-DNK/Refused'
            , TRUE ~ '04-Vocational/Other'
            ))


    data %>% group_by_(.dots = c('topqual3', names(data)[grepl('Degree', names(data))])) %>% tally()

    data %>% group_by(demo_educ_highest, topqual3) %>% tally()


    #### INCOME

    data <- data %>% mutate(demo_income_bucket = case_when(
                (HHInc2 <= 4) ~ '01-Under 18k'
                , (HHInc2 <= 9) ~ '02-18k to 31k'
                , (HHInc2 <= 14) ~ '03-31k to 52k'
                , (HHInc2 <= 19) ~ '04-52k to 100k'
                , (HHInc2 <= 21) ~ '05-Over 100k'
                , TRUE ~ '06-DNK/Refused'
                ))

    data %>% group_by(HHInc2, demo_income_bucket) %>% tally()



    ##### HOUSEHOLD

    ## TYPE
    data <- data %>% mutate(demo_hh_ownrent = case_when(
                (TenureB2 == 1) ~ '01-Own outright'
                , (TenureB2 == 2) ~ '02-Own with mortgage'
                , (TenureB2 == 4 & LandLord %in% c(1,2)) ~ '03-Rent from LA'
                , (TenureB2 == 4) ~ '04-Rent private'
                , (TenureB2 == 3) ~ '05-Shared'
                , (TenureB2 == 5) ~ '06-Rent free'
                , TRUE ~ '99-DNK/Refused'
                ))

    data %>% group_by(demo_hh_ownrent, TenureB2, LandLord) %>% tally()



    #### HEALTH

    ## SMOKING - NOT ASKED OF CHILDREN
    data %>% filter(demo_age_bucket >= '16 to 19') %>% group_by(cignow, cigst1) %>% tally()

    data <- data %>% mutate(health_smoking_status = case_when(
                (cigst1 == 4) ~ '01-Current'
                , (cigst1 == 1) ~ '03-Never'
                , (cigst1 < 0 ) ~ '99-DNK/Refused'
                , TRUE ~ '02-Previous'
        ))

    data %>% group_by(demo_age_bucket, health_smoking_status, cignow, cigst1) %>% tally()


    ## BMI

    data <- data %>% mutate(health_BMI_bucket = case_when(
                (BMIval < 0) ~ '99-DNK/Refused'
                , (BMIval < 18.5) ~ '01-Underweight'
                , (BMIval < 24.9) ~ '02-Healthy'
                , (BMIval < 29.9) ~ '03-Overweight'
                , (BMIval >= 29.9) ~ '04-Obese'
                , TRUE ~ '99-DNK/Refused'
                ))

    data %>% group_by(health_BMI_bucket) %>% summarize(length(BMIval), min(BMIval), max(BMIval))


    ### ALC CONSUMPTION

    ## frequency
    data <- data %>% mutate(health_alc_freq = case_when(
                (dnoft3 < 0) ~ '99-DNK/Refused'
                , (dnoft3 <= 2) ~ '01-Every day or almost'
                , (dnoft3 == 3) ~ '02-Three to four times pw'
                , (dnoft3 == 4) ~ '03-Once or twice pw'
                , (dnoft3 == 5) ~ '04-One to three times pm' # NOT QUITE THE SAME AS CENSUS 
                , (dnoft3 <= 7) ~ '05-Special occasions'
                , (dnoft3 == 8) ~ '06-Never'
                , TRUE ~ '99-DNK/Refused'
                ))

    data %>% group_by(health_alc_freq, dnoft3) %>% tally()

    ## weekly amount
    data <- data %>% mutate(health_alc_weekly_total = ifelse(totalwu < 0, 0, totalwu))

    data %>% select(health_alc_weekly_total) %>% glimpse(.)
    data %>% select(health_alc_weekly_total) %>% summary()




    #### BLOOD PRESSURE

    ## BP cat
    data <- data %>%
                mutate(health_bp_cat = 
                    case_when(
                        (omsysval < 0 | omdiaval < 0) ~ '99-DNK/Refused',
                        (omsysval < 120 & omdiaval < 80) ~ '01-Normal',
                        (omsysval >= 120 & omsysval < 130 & omdiaval < 80) ~ '02-Elevated',
                        ((omsysval >= 130 & omsysval < 140) | (omdiaval >= 80 & omdiaval < 90)) ~ '03-Stage 1 HBP',
                        (omsysval >= 140 | omdiaval >= 90) ~ '04-Stage 2 HBP',
                        TRUE ~ '99-DNK/Refused'
                    )
                )

    # checks
    data %>% group_by(health_bp_cat) %>% tally()
    data %>% group_by(health_bp_cat) %>% summarize(count = n(), min_sis = min(omsysval), max_sis = max(omsysval), min_dia = min(omdiaval), max_dia = max(omdiaval))


    ## ever diagnosed with high BP
    data <- data %>%
            mutate(health_bp_high_ever = 
                case_when(
                    DocBP == 1 ~ '01-Yes'
                    , (DocBP == 2 | EverBP == 2) ~ '01-No'
                    , TRUE ~ '03-DNK/Refused'
                    )
                )
    data %>% group_by(health_bp_high_ever) %>% tally()


    ## take medicine for BP
    data <- data %>% mutate(health_bp_meds_current = case_when(
        bpmedc2 == 1 ~ '01-Yes'
        , bpmedc2 == 0 ~ '02-No'
        , TRUE ~ '99-DNK/Refused'
        ))
    data %>% group_by(health_bp_meds_current,bpmedc2) %>% tally()



    #### DIABETES

    data <- data %>% mutate(health_diabetes = case_when(
        (EverDi == 1 & Diabetes == 1) ~ '01-Yes'
        , EverDi < 0 ~ '99-DNK/Refused'
        , TRUE ~ '02-No'
        ))

    data %>% group_by(EverDi, Diabetes, health_diabetes) %>% tally()


    return(data)
}









