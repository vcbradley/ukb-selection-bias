#### CENSUS Recode function


doCensusRecode <- function(data){

    census_data <- census_data %>% mutate(demo_sex = case_when(
	    sex == 1 ~ 'Male'
        , sex == 2 ~ 'Female'
        , TRUE ~ '99-DNK/Refused'
        ))

    census_data %>% group_by(demo_sex, sex) %>% tally()


    #### AGE
    census_data <- census_data %>% mutate(demo_age_bucket = case_when(        
                ageh == 1 ~ '00 to 4'        
                , ageh == 2 ~ '05 to 9'        
                , ageh == 3 ~ '10 to 15'        
                , ageh == 4 ~ '16 to 18'        
                , ageh == 5 ~ '19 to 24'        
                , ageh == 6 ~ '25 to 29'        
                , ageh == 7 ~ '30 to 34'        
                , ageh == 8 ~ '35 to 39'            
                , ageh == 9 ~ '40 to 44'
                , ageh == 10 ~ '44 to 49'
                , ageh == 11 ~ '50 to 54'
                , ageh == 12 ~ '55 to 59'
                , ageh == 13 ~ '60 to 64'
                , ageh == 14 ~ '65 to 69'
                , ageh == 15 ~ '70 to 74'
                , ageh == 16 ~ '75 to 79'
                , ageh == 17 ~ '80 to 84'  
                , ageh == 18 ~ '85 to 89'
                , ageh == 19 ~ '90 plus'
                , TRUE ~ '99 UNK'     
                ))

    census_data %>% group_by(demo_age_bucket) %>% tally()



    #### ETHNICITY

    ## FULL
    census_data <- census_data %>% mutate(demo_ethnicity_full = case_when(
                    (ethnicityew == 1) ~ '01-White'
                    , (ethnicityew == 2) ~ '02-White Irish'
                    , (ethnicityew == 3) ~ '03-White Other'
                    , (ethnicityew %in% c(4,5)) ~ '04-Mixed'
                    , (ethnicityew == 6) ~ '05-Asian Indian'
                    , (ethnicityew == 7) ~ '05-Asian Pakistani'
                    , (ethnicityew == 8) ~ '06-Asian Bangladeshi'
                    , (ethnicityew %in% c(9,10)) ~ '07-Asian Other'
                    , (ethnicityew == 11) ~ '09-Black African'
                    , (ethnicityew == 12) ~ '08-Black Carribean/Other'
                    , TRUE ~  '11-Other'
        ))
    census_data %>% group_by(ethnicityew, demo_ethnicity_full) %>% tally()


    ## 4-WAY
    census_data <- census_data %>% mutate(demo_ethnicity_4way = case_when(
                     ethnicityew %in% c(1,2,3) ~ '01-White'
                    , ethnicityew %in% c(6,7,8,9,10) ~  '03-Asian'
                    , ethnicityew %in% c(11,12) ~  '04-Black'
                    , TRUE ~  '02-Mixed/Other'
        ))

    census_data %>% group_by(demo_ethnicity_full, demo_ethnicity_4way, ethnicityew) %>% tally()


    census_data <- census_data %>% mutate(demo_white = case_when(
        ethnicityew <= 3 ~ '01-White'
        , ethnicityew > 3 ~ '02-Non-white'
        , TRUE ~ '99-DNK/Refused'
        ))

    census_data %>% group_by(demo_ethnicity_full, demo_ethnicity_4way, demo_white, ethnicityew) %>% tally()



    #### EMPLOYMENT STATUS

    ## Employment status
    census_data <- census_data %>% 
        mutate(
            demo_empl_employed = as.numeric(ecopuk11 >=1 & ecopuk11 <= 8)
            , demo_empl_retired = as.numeric(ecopuk11 == 10)
            , demo_empl_disabled = as.numeric(ecopuk11 == 13)
            , demo_empl_unemployed = as.numeric(ecopuk11 %in% c(7,9))
            , demo_empl_student = as.numeric(ecopuk11 %in% c(8,9,11))
            )

    census_data %>% group_by(ecopuk11, demo_empl_employed, demo_empl_retired
    , demo_empl_disabled
    , demo_empl_unemployed
    , demo_empl_student) %>% tally()

    ## Occupation
    census_data <- census_data %>% mutate(demo_occupation = case_when(
                (substr(occg,1,1) == 1) ~ '01-manager'
                , (substr(occg,1,1) == 2) ~ '02-professional'
                , (substr(occg,1,1) == 3) ~ '03-assoc professional'
                , (substr(occg,1,1) == 4) ~ '04-admin'
                , (substr(occg,1,1) == 5) ~ '05-skilled trades'
                , (substr(occg,1,1) == 6) ~ '06-personal service'
                , (substr(occg,1,1) == 7) ~ '07-sales customer service'
                , (substr(occg,1,1) == 8) ~ '08-industrial'
                , (substr(occg,1,1) == 9) ~ '09-elementary'
                , TRUE ~ '10-unemployed/DNK'

        ))

    census_data %>% group_by(occg, demo_occupation) %>% tally()


    ##### EDUCATION

    census_data <- census_data %>% mutate(demo_educ_highest = case_when(
        hlqupuk11 == 15 ~ '01-College plus/profesh'
        , hlqupuk11 == 14 ~ '02-A Levels'
        , hlqupuk11 %in% c(11,12) ~ '03-Levels/CES'
        , hlqupuk11 %in% c(13,16) ~ '04-Vocational/Other'
        , hlqupuk11 == 10 ~ '05-None'
        , TRUE ~ '00-DNK/Refused'
        ))


    census_data %>% group_by(hlqupuk11, demo_educ_highest) %>% tally()


    #### YEAR IMMIGRATED
    census_data <- census_data %>% mutate(demo_year_immigrated = case_when(
                   yrarr_yearg == 1 ~ '01-Before 1941'
                , yrarr_yearg == 2 ~ '02-1941 to 1950'
                , yrarr_yearg == 3 ~ '03-1951 to 1960'
                , yrarr_yearg == 4 ~ '04-1961 to 1970'
                , yrarr_yearg == 5 ~ '05-1971 to 1980'
                , yrarr_yearg == 6 ~ '06-1981 to 1990'
                , yrarr_yearg == 7 ~ '07-1991 to 2000'
                , yrarr_yearg == 8 ~ '08-2001 to 2003'
                , yrarr_yearg == 9 ~ '09-2004 to 2006'
                , yrarr_yearg == 10 ~ '10-2007 to 2009'
                , yrarr_yearg == 11 ~ '11-2010-2011'
                , TRUE ~ '00-Born in UK'
                ))


    census_data %>% group_by(yrarr_yearg, demo_year_immigrated) %>% tally()



    #### HOUSEHOLD VARS

    ## SIZE

    census_data <- census_data %>% mutate(demo_hh_size = case_when(
                            (sizhuk11 <= 1) ~ '1'
                        , (sizhuk11 == 2) ~ '2'
                        , (sizhuk11 == 3) ~ '3'
                        , (sizhuk11 == 4) ~ '4'
                        , (sizhuk11 > 4) ~ '5 or more'
                        , TRUE ~ '99-DNK/Refused'
        ))

    ## RENT OR OWN
    census_data <- census_data %>% mutate(demo_hh_ownrent = case_when(
                tenure == 0 ~ '01-Own outright'
                , tenure == 1 ~ '02-Own with mortgage'
                , tenure %in% c(3,4) ~ '03-Rent from LA'
                , tenure %in% c(5,6,7,8) ~ '04-Rent private'
                , tenure == 2 ~ '05-Shared'
                , tenure == 9 ~ '06-Rent free'
                , TRUE ~ '99-DNK/Refused'))

    census_data %>% group_by(demo_hh_ownrent, tenure) %>% tally()

    ## TYPE

    census_data <- census_data %>% mutate(demo_hh_accom_type = case_when(
                (typaccom %in% c(1,2,3)) ~ '01-House'
                , (typaccom == 4) ~ '02-Flat or apartment'
                , TRUE ~ '03-Other'
                ))


    ## CHECK LA
    #census_data %>% select(la_group) %>% pull()

    return(census_data)
}