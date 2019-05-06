#############
# Weighting #
#############


library('data.table')
library('memisc')
library('dplyr')
library('stringr')
library(survey)
library(MatrixModels)
library(Matrix)
library(glmnet)
library(lazyeval)

setwd('/well/nichols/users/bwj567')

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

# function to create popframe

getPopframe = function(data, vars, weight_col = NULL){
    popframe = data %>% 
            group_by_at(vars(vars)) %>% 
            summarize(
                n = n()
                , prop = n()/nrow(data)
                )

    if(!is.null(weight_col)){
        data = data %>% mutate(weight = .data[[weight_col]])

        weighted = data %>% 
            group_by_at(vars(vars)) %>% 
            summarize(
                n_wt = sum(weight, na.rm = T)
                , prop_wt = sum(weight, na.rm = T)/sum(data$weight, na.rm = T)
                )

        popframe = left_join(popframe, weighted, by = vars)
    }

    # formula = as.formula(paste0('~', paste(vars, collapse = '+')))

    # if(!is.null(weight_col)){
    #     data = data %>% filter(!is.na(get(weight_col)) & get(weight_col) > 0)
    #     weights = as.formula(paste0('~', weight_col))
    # }else{
    #     weights = as.formula('~1')
    # }

    # svydata = svydesign(id=~1, weights = weights, data = data)

    # popframe = svytable(formula, svydata)

    popframe = popframe %>% mutate(Freq = get(ifelse(!is.null(weight_col), 'prop_wt', 'prop')))

    return(popframe %>% select(-c(n,prop,n_wt,prop_wt)))
}


getPopframe(hsedata, vars = strat_vars, weight_col = 'wt_blood') %>% data.frame

#### Function to post-stratify survey
doPostStrat = function(svydata, popdata, vars, pop_weight_col = NULL, prior_weight_col = NULL){
    # get population frame
    popframe  = getPopframe(popdata, vars = vars, weight_col = pop_weight_col)

    # make survey data
    prior_weight = as.formula(ifelse(!is.null(prior_weight_col), paste0('~', prior_weight_col), '~1'))
    svydata = svydesign(id = ~1, weights = prior_weight, data = svydata)

    # define strata for post-stratification
    strata = as.formula(paste0('~', paste(vars, collapse = '+')))

    # weight
    weighted = postStratify(svydata, strata = strata, population = popframe)
    weighted = cbind(weighted$variables, weight = (1/weighted$prob)/mean(1/weighted$prob, na.rm = T))

    #check mean
    mean(weighted$weight)

    return(weighted)
}

ukbweighted = doPostStrat(svydata = ukbdata, popdata = hsedata, vars = c('demo_sex','demo_white','demo_age_bucket'), pop_weight_col = 'wt_blood')
hsedata %>% group_by(health_diabetes) %>% summarize(n()/nrow(hsedata))
ukbweighted %>% group_by(health_diabetes) %>% summarize(raw =n()/nrow(ukbweighted), weighted = sum(weight)/sum(ukbweighted$weight))

names(ukbweighted)


doRaking = function(svydata, popdata, vars
    , pop_weight_col = NULL
    , prior_weight_col = NULL
    , control = list(maxit = 100, epsilon = 10e-4, verbose=FALSE)
    ){
    # get population frame
    popmargins = lapply(vars, getPopframe, data = popdata, weight_col = pop_weight_col)

    strata = lapply(vars, function(x) as.formula(paste("~", x)))

    prior_weight = as.formula(ifelse(!is.null(prior_weight_col), paste0('~', prior_weight_col), '~1'))
    svydata = svydesign(id = ~1, weights = prior_weight, data = svydata)

    # do weighting
    weighted = rake(svydata, sample.margins = strata, population.margins = popmargins, control = control)
    weighted = cbind(weighted$variables, weight = (1/weighted$prob)/mean(1/weighted$prob, na.rm = T))

    return(weighted)
}

ukbweighted = doRaking(svydata = ukbdata, popdata = hsedata, vars = strat_vars, pop_weight_col = 'wt_blood')

names(ukbweighted)



ukbweighted = doRaking(svydata = ukbdata, popdata = hsedata, vars = strat_vars, pop_weight_col = 'wt_blood')
hsedata %>% group_by(health_diabetes) %>% summarize(n()/nrow(hsedata))
ukbweighted %>% group_by(health_diabetes) %>% summarize(raw =n()/nrow(ukbweighted), weighted = sum(weight)/sum(ukbweighted$weight))



ukbdata %>% count(demo_income_bucket)
hsedata %>% group_by(demo_income_bucket) %>% summarize(sum(wt_blood, na.rm = T)/sum(hsedata$wt_blood, na.rm = T))

# create full data set
ukbdata = ukbdata %>% mutate(weight = 1, selected = 1)
hsedata = hsedata %>% mutate(selected = 0)
nrdata = rbind(
    ukbdata %>% select(id = eid, strat_vars, selected, weight)
    , hsedata %>% filter(!is.na(wt_blood)) %>% select(id = SerialA, strat_vars, selected, weight = wt_blood)
    )



n_interactions = 2
outcome = 'selected'
formula = as.formula(paste0('~ -1 + (', paste(strat_vars, collapse = ' + '), ')^', n_interactions))

nrdata_modmat = model.Matrix(formula, nrdata)

# drop levels with to few observations
head(nrdata_modmat)
colnames(nrdata_modmat)

samp = apply(nrdata_modmat[nrdata%>% select_(outcome) == 1, ], 2, sum)
pop = apply(nrdata_modmat, 2, sum)

# create var data table with variable codes
vars = data.table(var_name = names(pop), var_code = paste0('v', 1:length(pop)), n_pop = pop, n_samp = samp)

# rename columns to variable codes
colnames(nrdata_modmat) = vars$var_code

# calc distributions
vars[, dist_pop := n_pop/nrow(nrdata_modmat)]
vars[, dist_samp := n_samp/sum(nrdata%>% select_(outcome))]

# figure out which vars to drop
drop_samp = which(vars$dist_samp < 0.02 | vars$dist_samp > 0.98)
drop_pop = which(vars$dist_pop < 0.02 | vars$dist_pop > 0.98)

# drop variables from vars and data
vars = vars[-unique(drop_pop, drop_samp)]
nrdata_modmat = nrdata_modmat[, -unique(drop_pop, drop_samp)]

# fit nonresponse lasso
fit_nr = cv.glmnet(y = nrdata$selected
    , x = nrdata_modmat
    , weights = nrdata$weight  #because the population data is weighted, include this
    , family = 'binomial'
    , nfolds = 10)

fit_out = cv.glmnet(y = nrdata %>% select_(outcome)
    , x = nrdata_modmat[]
    , weights = nrdata %>% filter_(interp(~outcome==1, v=as.name(outcome)))$weight  #because the population data is weighted, include this
    , family = 'binomial'
    , nfolds = 10)

coef_nr = rownames(coef(fit_nr, lambda = 'lambda.1se'))[as.numeric(coef(fit_nr, lambda = 'lambda.1se')) != 0][-1]
coef_out = rownames(coef(fit_ut, lambda = 'lambda.1se'))[as.numeric(coef(fit_ut, lambda = 'lambda.1se')) != 0][-1]












