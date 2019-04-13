## Recode HSE 2011

library('data.table')
library('memisc')
library('dplyr')
library('stringr')

setwd('/well/nichols/users/bwj567/')

#source('/well/nichols/users/bwj567/mini-project-1/prep_census/recode_functions_hse.R')



var_codings_file = '/well/nichols/users/bwj567/mini-project-1/variable_codings.csv'
hse_file = '_data/hse2011ai.tab'
hse_recoded_file = 'data/hse11_recoded.csv'


# read in list of vars we want
variables_hse = fread(var_codings_file)
#variables_hse[, hse16_var := tolower(hse16_var)]
variables_hse = variables_hse[hse_var != "",]$hse_var
variables_hse = unlist(lapply(variables_hse, function(x) unlist(strsplit(x, ', '))))


####### READ IN DATA
hse_data = fread(hse_file    
    , nrows = 500    # for testing
    #, select = unique(variables_hse)
    )

hse_data <- as_tibble(hse_data)

hse_data %>% str(.)



## GENDER
hse_data <- hse_data %>% mutate(demo_sex = case_when(
        Sex == 1 ~ 'Male'
        , Sex == 2 ~ 'Female'
        , TRUE ~ '99-DNK/Refused'
        ))

hse_data %>% group_by(Sex, demo_sex) %>% tally()


## AGE BUCKET
hse_data <- hse_data %>% mutate(demo_age_bucket = case_when(
    Age < 40 ~ 'Under 40'
	, Age < 45 ~ '40 to 44'
	, Age < 50 ~ '45 to 49'
	, Age < 55 ~ '50 to 54'
	, Age < 60 ~ '55 to 59'
	, Age < 65 ~ '60 to 64'
    , Age <= 70 ~ '65 to 69'  
    , TRUE ~ 'Over 70'
    ))

hse_data %>% group_by(demo_age_bucket) %>% tally()


## ETHNICITY
# Full
hse_data <- hse_data %>% mutate(demo_ethnicity_full = case_when(
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

hse_data %>% group_by(Origin, demo_ethnicity_full) %>% tally()

# 4-eay
hse_data <- hse_data %>% mutate(demo_ethnicity = case_when(
    Origin < 0 ~ '99-DNK/Refused'
    , Origin <= 4 ~ '01-White'
    , Origin <= 8 | Origin >= 17 ~ '02-Mixed/Other'
    , Origin <= 13 ~  '03-Asian'
    , Origin <= 16 ~  '04-Black'
    ))

hse_data %>% group_by(Origin, demo_ethnicity) %>% tally()

# White v. non-white
hse_data <- hse_data %>% mutate(demo_white = case_when(
	Origin < 0 ~ '99-DNK/Refused'
	, Origin <= 4 ~ '01-White'
    , TRUE ~ '02-Non-white'
     ))
hse_data %>% group_by(Origin, demo_white) %>% tally()

hse_data <- cbind(hse_data, 
                hse_data %>% 
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

hse_data %>% group_by(activb, hrpstwk, demo_empl_employed
	, demo_empl_retired
    , demo_empl_homemaker
    , demo_empl_disabled
    , demo_empl_unemployed
    , demo_empl_volunteer
    , demo_empl_student) %>% tally()

hse_data <- hse_data %>% mutate(demo_occupation = case_when(
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

hse_data %>% 
#filter(demo_occupation >= 10) %>% 
group_by(demo_occupation, SOC2010B) %>% tally() %>% data.table(.)


hse_data %>% 


######### DO RECODES
hse_data_recoded <- doHSERecodes(hse_data)

names(hse_data_recoded)


########### WRITE OUT TO FILE
# select only the demo cols
hse_data_recoded <- hse_data_recoded %>% select(., c('SerialA', names(hse_data_recoded)[grep('demo_|health_|wt_', names(hse_data_recoded))]))

# write out recoded data
write.csv(hse_data_recoded, hse_recoded_file, row.names = F)


rm()




