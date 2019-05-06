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
ukbdata = ukbdata %>% mutate(pop_weight = 1, selected = 1)
hsedata = hsedata %>% mutate(selected = 0)
fulldata = rbind(
    ukbdata %>% select(id = eid, strat_vars, selected, pop_weight)
    , hsedata %>% filter(!is.na(wt_blood)) %>% select(id = SerialA, strat_vars, selected, pop_weight = wt_blood)
    )


doLassoRake = function(
    data
    , strat_vars
    , selected_ind 
    , outcome
    , pop_weight_col
    , n_interactions = 2
){

    formula = as.formula(paste0('~ -1 + (', paste(strat_vars, collapse = ' + '), ')^', n_interactions))

    #moodmat for nr data
    data_modmat = model.matrix(formula, data)
    outdata_modmat = model.matrix(formula, data %>% filter_(paste0(selected_ind, ' == 1')))

    ##### PREP VARIABLES #####
    samp = apply(outdata_modmat, 2, sum)
    pop = apply(data_modmat, 2, sum)

    # create var data table with variable codes
    vars = data.table(var_name = names(pop), var_code = paste0('v', 1:length(pop)), n_pop = pop, n_samp = samp)

    # rename columns to variable codes
    colnames(data_modmat) = vars$var_code
    colnames(outdata_modmat) = vars$var_code

    # calc distributions
    vars[, dist_pop := n_pop/nrow(data_modmat)]
    vars[, dist_samp := n_samp/nrow(outdata_modmat)]

    # figure out which vars to drop
    drop_samp = which(vars$dist_samp < 0.02 | vars$dist_samp > 0.98)
    drop_pop = which(vars$dist_pop < 0.02 | vars$dist_pop > 0.98)

    # drop variables from vars and data
    vars = vars[-unique(drop_pop, drop_samp)]
    data_modmat = data_modmat[, -unique(drop_pop, drop_samp)]


    ###### FIT MODELS ######
    # fit nonresponse lasso
    fit_nr = cv.glmnet(y = data %>% select_(selected_ind) %>% pull
        , x = data_modmat
        , weights = data %>% select_(pop_weight_col) %>% pull  #because the population data is weighted, include this
        , family = 'binomial'
        , nfolds = 10)


    ##### RANK COEFS #####
    fit_out = cv.glmnet(y = ukbdata %>% select_(outcome) %>% pull
        , x = outdata_modmat
        , nfolds = 10)

    coef_nr = data.frame(var_code = rownames(coef(fit_nr, lambda = 'lambda.1se')), coef_nr = coef(fit_nr, lambda = 'lambda.1se')[,1])[-1,]
    coef_out = data.frame(var_code = rownames(coef(fit_out, lambda = 'lambda.1se')), coef_out = coef(fit_out, lambda = 'lambda.1se')[,1])[-1,]

    vars[coef_nr, on = 'var_code', coef_nr := i.coef_nr]
    vars[coef_out, on = 'var_code', coef_out := i.coef_out]

    ##### RANK VARIABLES BY IMPORTANCE -- SHOULD REEVALUATE
    vars[order(abs(coef_nr), decreasing = T), rank_nr := .I]
    vars[coef_nr == 0, rank_nr := NA]
    vars[order(abs(coef_out), decreasing = T), rank_out := .I]
    vars[coef_out == 0, rank_out := NA]

    vars[,as.numeric(!is.na(rank_nr)) + as.numeric(!is.na(rank_out))]
    vars[order(as.numeric(is.na(rank_nr)) + as.numeric(is.na(rank_out)), as.numeric(rank_nr + rank_out)), rank_total := .I]

    vars[rank_total < 15, ]

    #create data table for weighting
    data = cbind(data, as.matrix(data_modmat))

    # get variable subsets for raking
    vars = vars[!is.na(rank_nr) | !is.na(rank_out)]

    vars[, subset := floor(rank_total/20)]
    vars[, subset := (max(subset) + 1)- subset]

    svydata = data.table(data %>% filter(selected == 1))
    popdata = data.table(data %>% filter(selected == 0))



    ##### DO RAKING THROUGH SUBSETS #####
    for(s in 1:max(vars$subset)){
        vars_for_raking <- vars[subset == s, var_code]

        # set prior weight for first subset
        if(s == 1){
            svydata[, prior_weight := 1]
        }

        # do raking
        rakeddata = doRaking(svydata = svydata
                                , popdata = popdata
                                , vars = vars_for_raking
                                , pop_weight_col = 'pop_weight'
                                , prior_weight_col = 'prior_weight'
                                , control = list(maxit = 100, epsilon = 10e-4, verbose=FALSE)
                                )
        # replace prior weight with new weight
        svydata$prior_weight = rakeddata$weight
        rm(rakeddata)

    }

    #set final weight
    svydata$weight = svydata$prior_weight

    return(svydata)
}



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









