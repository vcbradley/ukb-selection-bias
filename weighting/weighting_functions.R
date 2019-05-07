## WEIGHTING FUNCTIONS
library(survey)
library(dplyr)
library(data.table)
library(MatrixModels)
library(Matrix)
library(glmnet)
library(lazyeval)

#Utility function for creating a design matrix with all levels of factor variables included, rather than omitting a reference level
#as one should generally do when passing a design matrix to glmnet/cv.glmnet for a lasso (or elastic net) model
modmat_all_levs=function(formula, data, sparse = F) UseMethod("modmat_all_levs",data)

modmat_all_levs.data.frame=function(formula, data, sparse = F){
  #data.frame method: data should be a data.frame containing the variables referred to in formula, which determines the model matrix
  terms_data=data.frame(lapply(data[,all.vars(formula),drop=F],function(x)if (is.character(x)) as.factor(x) else x)) #convert character to factor #need drop=F to maintain a dataframe if only one factor!
  
  if(sparse){
    require(Matrix)
    sparse.model.matrix(formula,terms_data,contrasts.arg = lapply(terms_data[,sapply(terms_data,function(x)is.factor(x)),drop=F], contrasts, contrasts=FALSE)) 
  }else{
    model.matrix(formula,terms_data,contrasts.arg = lapply(terms_data[,sapply(terms_data,function(x)is.factor(x)),drop=F], contrasts, contrasts=FALSE)) 
  }
  
}
modmat_all_levs.data.table=function(formula, data, sparse = F){
  #data.table method: data should be a data.table containing the variables referred to in formula, which determines the model matrix
  require(data.table)
  terms_data=setDT(lapply(data[,all.vars(formula),with=F],function(x)if (is.character(x)) as.factor(x) else x))
  
  if(sparse){
    require(Matrix)
    sparse.model.matrix(formula,terms_data,contrasts.arg = lapply(terms_data[,sapply(terms_data,function(x)is.factor(x)),with=F,drop=F], contrasts, contrasts=FALSE)) #need drop=F to maintain a dataframe if only one factor!
  }else{
    model.matrix(formula,terms_data,contrasts.arg = lapply(terms_data[,sapply(terms_data,function(x)is.factor(x)),with=F,drop=F], contrasts, contrasts=FALSE)) #need drop=F to maintain a dataframe if only one factor!
  }
}


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

    #drop extra cols
    popframe = popframe %>% select(-c(n, prop))
    if(!is.null(weight_col)){
    	popframe = popframe %>% select(-c(n_wt, prop_wt))
    }
    return(popframe)
}





#### Function to post-stratify survey
doPostStrat = function(svydata, popdata, vars, pop_weight_col = NULL, prior_weight_col = NULL){
    # get population frame

    popframe  = getPopframe(popdata, vars = vars, weight_col = ifelse(is.null(pop_weight_col), NULL, pop_weight_col))

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


## Raking function
doRaking = function(svydata
    , popdata
    , vars
    , pop_weight_col = NULL
    , prior_weight_col = NULL
    , control = list(maxit = 100, epsilon = 10e-4, verbose=FALSE)
    ){
    # get population frame
    if(is.null(pop_weight_col)){
    	popmargins = lapply(vars, getPopframe, data = popdata, weight_col = NULL)
    }else{
    	popmargins = lapply(vars, getPopframe, data = popdata, weight_col = pop_weight_col)
    }
    

    #print(popmargins)

    strata = lapply(vars, function(x) as.formula(paste("~", x)))
    #print(strata)

    prior_weight = as.formula(ifelse(!is.null(prior_weight_col), paste0('~', prior_weight_col), '~1'))
    svydata = svydesign(id = ~1, weights = prior_weight, data = svydata)

    # do weighting
    weighted = rake(svydata, sample.margins = strata, population.margins = popmargins, control = control)
    weighted = cbind(weighted$variables, weight = (1/weighted$prob)/mean(1/weighted$prob, na.rm = T))

    return(weighted)
}


## LAsso rake function
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
    data_modmat = modmat_all_levs(formula, data)
    outdata_modmat = modmat_all_levs(formula, data %>% filter_(paste0(selected_ind, ' == 1')))

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
