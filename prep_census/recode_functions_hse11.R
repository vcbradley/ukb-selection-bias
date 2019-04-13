#############################
# HSE 2011-specific recodes #
#############################

doHSE11Recode <- function(data){
        
    ## GENDER
    data <- data %>% mutate(demo_sex = case_when(
            Sex == 1 ~ 'Male'
            , Sex == 2 ~ 'Female'
            , TRUE ~ '99-DNK/Refused'
            ))

    data %>% group_by(Sex, demo_sex) %>% tally()


    ## AGE BUCKET
    data <- data %>% mutate(demo_age_bucket = case_when(
        Age < 40 ~ 'Under 40'
        , Age < 45 ~ '40 to 44'
        , Age < 50 ~ '45 to 49'
        , Age < 55 ~ '50 to 54'
        , Age < 60 ~ '55 to 59'
        , Age < 65 ~ '60 to 64'
        , Age <= 70 ~ '65 to 69'  
        , TRUE ~ 'Over 70'
        ))

    data %>% group_by(demo_age_bucket) %>% tally()


    ## ETHNICITY
    # Full
    data <- data %>% mutate(demo_ethnicity_full = case_when(
        Origin == 2 ~ '02-White Irish'
        , Origin %in% c(3,4) ~ '03-White Other'
        , Origin == 1 ~ '01-White'
        , Origin >=5 & Origin <= 8 ~ '04-Mixed'
        , Origin == 9 ~ '05-Asian Indian'
        , Origin == 10 ~ '05-Asian Pakistani'
        , Origin == 11 ~ '06-Asian Bangladeshi'
        , Origin %in% c(12,13) ~ '07-Asian Other'
        , Origin == 15 ~ '08-Black Caribbean'
        , Origin == 14 ~ '09-Black African'
        , Origin == 16 ~ '10-Black Other'
        , Origin > 0 ~ '11-Other'
        ,TRUE ~  '99-DNK/Refused'
                ))

    data %>% group_by(Origin, demo_ethnicity_full) %>% tally()

    # 4-way
    data <- data %>% mutate(demo_ethnicity = case_when(
        Origin < 0 ~ '99-DNK/Refused'
        , Origin <= 4 ~ '01-White'
        , Origin <= 8 | Origin >= 17 ~ '02-Mixed/Other'
        , Origin <= 13 ~  '03-Asian'
        , Origin <= 16 ~  '04-Black'
        ))

    data %>% group_by(Origin, demo_ethnicity) %>% tally()

    # White v. non-white
    data <- data %>% mutate(demo_white = case_when(
        Origin < 0 ~ '99-DNK/Refused'
        , Origin <= 4 ~ '01-White'
        , TRUE ~ '02-Non-white'
         ))
    data %>% group_by(Origin, demo_white) %>% tally()


    ## EMPLOYMENT STATUS
    data <- cbind(data, 
                    data %>% 
                        transmute(
                            demo_empl_employed = as.numeric((activb >= 2 & activb <= 4) | hrpstwk == 1)
                            , demo_empl_retired = as.numeric(activb == 9)
                            , demo_empl_homemaker = as.numeric(activb == 10)
                            , demo_empl_disabled = as.numeric(activb == 8)
                            , demo_empl_unemployed = as.numeric(activb == 5)
                            , demo_empl_volunteer = 0#as.numeric(activb == 100)
                            , demo_empl_student = as.numeric(activb == 1)
                            )
                    )

    data %>% group_by(activb, hrpstwk, demo_empl_employed
        , demo_empl_retired
        , demo_empl_homemaker
        , demo_empl_disabled
        , demo_empl_unemployed
        , demo_empl_volunteer
        , demo_empl_student) %>% tally()


    ## OCCUPATION
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
        , demo_empl_employed == 0 ~ '10-unemployed'
        , TRUE ~ '99-DNK/Refused'
        ))

    data %>% 
    #filter(demo_occupation >= 10) %>% 
    group_by(demo_occupation, SOC2010B) %>% tally() %>% data.table(.)


    #### Education

    data <- data %>% mutate(demo_educ_highest = case_when(
                (topqual3 == 1) ~ '01-College plus/profesh'
                , (topqual3 %in% c(2,3)) ~ '02-A Levels'
                , (topqual3 %in% c(4,5)) ~ '03-O Levels/CSEs'
                , (topqual3 == 7) ~ '05-None'
                , (topqual3 < 0) ~ '99-DNK/Refused'
                , TRUE ~ '04-Vocational/Other'
                ))

    data %>% group_by(topqual3, demo_educ_highest) %>% tally()



    #### INCOME
    data <- data %>% mutate(demo_income_bucket = case_when(
                    JntInc < 0 ~ '99-DNK/Refused'
                    , (JntInc <= 10) ~ '01-Under 18k'
                    , (JntInc <= 15) ~ '02-18k to 31k'
                    , (JntInc <= 20) ~ '03-31k to 52k'
                    , (JntInc <= 25) ~ '04-52k to 100k'
                    , (JntInc <= 31) ~ '05-Over 100k'
                    , TRUE ~ '99-DNK/Refused'
                    ))

    data %>% group_by(demo_income_bucket, JntInc) %>% tally() %>% data.table


    #### HOUSEHOLD
    data <- data %>% mutate(demo_hh_ownrent = case_when(
                        (tenureb == 1) ~ '01-Own outright'
                    , (tenureb == 2) ~ '02-Own with mortgage'
                    , (tenureb == 4 & LandLord %in% c(1,2)) ~ '03-Rent from LA'
                    , (tenureb == 4) ~ '04-Rent private'
                    , (tenureb == 3) ~ '05-Shared'
                    , (tenureb == 5 | tenureb == 6) ~ '06-Rent free'
                    , TRUE ~ '99-DNK/Refused'
                    ))

    data %>% group_by(tenureb, demo_hh_ownrent, LandLord) %>% tally()


    #### HEALTH

    ## SMOKING

    data %>% filter(demo_age_bucket >= '16 to 19') %>% group_by(cignow, cigst1) %>% tally()

    data <- data %>% mutate(health_smoking_status = case_when(
                (cigst1 == 4) ~ '01-Current'
                , (cigst1 == 1) ~ '03-Never'
                , (cigst1 < 0 ) ~ '99-DNK/Refused'
                , TRUE ~ '02-Previous'
        ))

    data %>% filter(demo_age_bucket >= '16 to 19') %>% group_by(health_smoking_status, cignow, cigst1) %>% tally()


   ## BMI

    data <- data %>% mutate(health_BMI_bucket = case_when(
                (bmival < 0) ~ '99-DNK/Refused'
                , (bmival < 18.5) ~ '01-Underweight'
                , (bmival < 24.9) ~ '02-Healthy'
                , (bmival < 29.9) ~ '03-Overweight'
                , (bmival >= 29.9) ~ '04-Obese'
                , TRUE ~ '99-DNK/Refused'
                ))

    data %>% group_by(health_BMI_bucket) %>% summarize(length(bmival), min(bmival), max(bmival))


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
                    docnurbp == 1 ~ '01-Yes'
                    , (docnurbp == 2 | everbp == 2) ~ '02-No'
                    , TRUE ~ '03-DNK/Refused'
                    )
                )
    data %>% group_by(health_bp_high_ever) %>% tally()


    ## take medicine for BP
    data <- data %>% mutate(health_bp_meds_current = case_when(
        medcinbp == 1 ~ '01-Yes'
        , medcinbp == 0 ~ '02-No'
        , TRUE ~ '99-DNK/Refused'
        ))
    data %>% group_by(health_bp_meds_current,medcinbp) %>% tally()


    data <- data %>% mutate(health_diabetes = case_when(
        (everdi == 1) ~ '01-Yes'
        , everdi < 0 ~ '99-DNK/Refused'
        , TRUE ~ '02-No'
        ))

    data %>% group_by(everdi, docinfo1, health_diabetes) %>% tally()

return(data)
}