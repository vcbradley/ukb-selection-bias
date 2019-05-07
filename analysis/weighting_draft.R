#############
# Weighting #
#############


library(data.table)
library(memisc)
library(dplyr)
library(stringr)
library(survey)
library(MatrixModels)
library(Matrix)
library(glmnet)
library(lazyeval)

setwd('/well/nichols/users/bwj567')
source('mini-project-1/weighting/weighting_functions.R')

# Read in UKB data
ukbdata = fread('data/ukb25120_weighting_img.csv')
ukbdata <- as_tibble(ukbdata)

# drop img prefix
ukbdata <- ukbdata %>% rename_at(vars(starts_with("img_")), funs(str_replace(., "img_", "")))

# temp recodes
ukbdata = ukbdata %>% mutate(demo_white = ifelse(demo_white > 2, '01-White', demo_white))

# read in HSE data
hsedata = fread('data/hse16_recoded.csv')
hsedata <- as_tibble(hsedata)
# limit HSe data to relevant age range
hsedata <- hsedata %>% filter(demo_age_bucket >= '45 to 49') %>% filter(demo_age_bucket <= '75 to 79')

hsedata = hsedata %>% mutate(demo_white = ifelse(demo_white > 2, '01-White', demo_white))
hsedata = hsedata %>% mutate(demo_income_bucket = ifelse(demo_income_bucket == '06-DNK/Refused', '99-DNK/Refused', demo_income_bucket))

hsedata %>% count(demo_occupation) %>% mutate(n/nrow(hsedata))
ukbdata %>% count(demo_occupation) %>% mutate(n/nrow(ukbdata))


strat_vars = c('demo_sex'
, 'demo_age_bucket'
, 'demo_white'
, 'demo_empl_employed'
, 'demo_income_bucket'
, 'demo_hh_ownrent'
#, 'demo_occupation'
)



getPopframe(hsedata, vars = strat_vars, weight_col = 'wt_blood') %>% data.frame



ukbweighted = doPostStrat(svydata = ukbdata, popdata = hsedata, vars = c('demo_sex','demo_white','demo_age_bucket'), pop_weight_col = 'wt_blood')
hsedata %>% group_by(health_diabetes) %>% summarize(n()/nrow(hsedata))
ukbweighted %>% group_by(health_diabetes) %>% summarize(raw =n()/nrow(ukbweighted), weighted = sum(weight)/sum(ukbweighted$weight))

names(ukbweighted)




ukbweighted = doRaking(svydata = ukbdata, popdata = hsedata, vars = strat_vars, pop_weight_col = 'wt_blood')

names(ukbweighted)


ukbweighted = doRaking(svydata = ukbdata, popdata = hsedata, vars = strat_vars, pop_weight_col = 'wt_blood')
hsedata %>% group_by(health_diabetes) %>% summarize(n()/nrow(hsedata))
ukbweighted %>% group_by(health_diabetes) %>% summarize(raw =n()/nrow(ukbweighted), weighted = sum(weight)/sum(ukbweighted$weight))

ukbdata %>% count(demo_income_bucket)
hsedata %>% group_by(demo_income_bucket) %>% summarize(sum(wt_blood, na.rm = T)/sum(hsedata$wt_blood, na.rm = T))



# create full data set
ukbdata = ukbdata %>% mutate(pop_weight = 1, selected = 1)
hsedata = hsedata %>% mutate(selected = 0)
fulldata = rbind(
    ukbdata %>% select(id = eid, strat_vars, selected, pop_weight)
    , hsedata %>% filter(!is.na(wt_blood)) %>% select(id = SerialA, strat_vars, selected, pop_weight = wt_blood)
    )




ukbweighted = doLassoRake(data = fulldata
    , strat_vars = strat_vars
    , selected_ind = 'selected'
    , outcome = 'MRI_brain_vol'
    , pop_weight_col = 'pop_weight'
    , n_interactions = 2)
ukbdata = ukbdata %>% select(.,-weight)
ukbdata = ukbdata %>% left_join(ukbweighted[, .(id, weight)], by = c('eid' = 'id'))


summary(ukbweighted$weight)
hsedata %>% group_by(health_diabetes) %>% summarize(n()/nrow(hsedata))
ukbdata %>% group_by(health_diabetes) %>% summarize(raw =n()/nrow(ukbdata), weighted = sum(weight)/sum(ukbdata$weight))









