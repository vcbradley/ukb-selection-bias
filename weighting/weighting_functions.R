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
doPostStrat = function(svydata, popdata, vars, pop_weight_col = NULL, prior_weight_col = NULL, partial=FALSE){
    
    popframe  = data.table(getPopframe(popdata, vars = vars, weight_col = NULL))
	setnames(popframe, old = 'Freq', new = 'pop_prop')
    popframe = popframe[, pop_N := pop_prop * nrow(popdata)]
    
    sampframe  = data.table(getPopframe(sample, vars = vars, weight_col = NULL))
    setnames(sampframe, old = 'Freq', new = 'samp_prop')
    sampframe = sampframe[, samp_N := samp_prop * nrow(svydata)]

    # DO weighting
    weighted = merge(popframe, sampframe, by = vars)
    weighted[, weight := (pop_prop/samp_prop)]
    weighted = merge(svydata, weighted[, c(vars, 'weight'), with = F], by = vars, all.x = T)

    #fix col sorting
    weighted = weighted[, c(names(svydata), 'weight'), with = F]

    # normalize
    weighted = weighted[, weight := weight/mean(weight, na.rm = T)]

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

    if(is.null(pop_weight_col)){
        popdata[, pop_weight := 1]
    }else{
        popdata[, pop_weight := get(pop_weight_col)]
    }

    if(is.null(prior_weight_col)){
        svydata[, prior_weight := 1]
    }else{
        svydata[, prior_weight := get(prior_weight_col)]
    }

    drop_pop = 1
    drop_samp = 1
    while(length(drop_samp)>0 | length(drop_pop)>0){
        drop_samp = unlist(lapply(vars, function(v){
            popmargin = popdata[, .(pop_prop = sum(pop_weight)/sum(popdata$pop_weight)), by = v]
            sampmargin = svydata[, .(samp_prop = sum(prior_weight)/sum(svydata$prior_weight)), by = v]
            margins = merge(popmargin, sampmargin, all = T)
            margins = cbind(var = v, margins)

            drop_samp = margins[is.na(pop_prop)]

            if(nrow(drop_samp) > 0){
                print("WARNING DROPPING strata because null in population")
                print(drop_samp)
            }

            drop = which(svydata[,get(as.character(v))] %in% drop_samp[, 2])

            return(drop)
            }))

        if(length(drop_samp)>0){
            svydata = svydata[-drop_samp, ]
        }

        drop_pop = unlist(lapply(vars, function(v){
            popmargin = popdata[, .(pop_prop = sum(pop_weight)/sum(popdata$pop_weight)), by = v]
            sampmargin = svydata[, .(samp_prop = sum(prior_weight)/sum(svydata$prior_weight)), by = v]
            margins = merge(popmargin, sampmargin, all = T)
            margins = cbind(var = v, margins)

            drop_pop = margins[is.na(samp_prop)]

            if(nrow(drop_pop) > 0){
                print("WARNING DROPPING strata because null in sample")
                print(drop_pop)
            }

            drop = which(popdata[,get(as.character(v))] %in% drop_pop[, 2])

            return(drop)
            }))

        if(length(drop_pop) > 0){
            popdata = popdata[-drop_pop, ]
        }
        
    }

    popmargins = lapply(vars, getPopframe, data = popdata, weight_col = 'pop_weight')
   

    strata = lapply(vars, function(x) as.formula(paste("~", x)))

    prior_weight = as.formula(ifelse(!is.null(prior_weight_col), paste0('~', prior_weight_col), '~1'))
    svydata = svydesign(id = ~1, weights = prior_weight, data = svydata)


    # do weighting
    weighted = rake(svydata, sample.margins = strata, population.margins = popmargins, control = control)
    weighted = cbind(weighted$variables, weight = (1/(weighted$prob + 0.00000001))/mean(1/(weighted$prob + 0.00000001), na.rm = T))

    return(weighted)
}


## LAsso rake function
doLassoRake = function(
    data
    , vars
    , selected_ind 
    , outcome
    , pop_weight_col = NULL
    , n_interactions = 2
){

    formula = as.formula(paste0('~ -1 + (', paste(vars, collapse = ' + '), ')^', n_interactions))

    #moodmat for nr data
    data_modmat = modmat_all_levs(formula, data, sparse = T)
    outdata_modmat = modmat_all_levs(formula, data[get(selected_ind) == 1, ], sparse = T)

    ##### PREP VARIABLES #####
    samp = apply(outdata_modmat, 2, sum)
    pop = apply(data_modmat, 2, sum)

    # create var data table with variable codes
    lasso_vars = data.table(var_name = names(pop), var_code = paste0('v', str_pad(1:length(pop), width = 4, side = 'left', pad = '0')), n_pop = pop)
    samp = data.table(var_name = names(samp), n_samp = samp)
    lasso_vars = merge(lasso_vars, samp, by = 'var_name', all = T)

    lasso_vars[order(var_code),]

    # drop strata that are null in the pop or the sample
    strata_missing = lasso_vars[(is.na(n_pop) | is.na(n_samp)),]
    if(nrow(strata_missing) > 0){
    	print("WARNING dropping strata because missing in sample or pop")
    	print(strata_missing)
    	lasso_vars = lasso_vars[!(is.na(n_pop) | is.na(n_samp)),]
    }

    data_modmat = data_modmat[, which(colnames(data_modmat) %in% lasso_vars$var_name)]
    outdata_modmat = outdata_modmat[, which(colnames(outdata_modmat) %in% lasso_vars$var_name)]

    ### rename columns to variable codes
    # mean(colnames(data_modmat) == lasso_vars[order(var_code), var_name])
    # mean(colnames(outdata_modmat) == lasso_vars[order(var_code), var_name])
    colnames(data_modmat) = lasso_vars[order(var_code), var_name]
    colnames(outdata_modmat) = lasso_vars[order(var_code), var_name]

    # calc distributions
    lasso_vars[, dist_pop := n_pop/nrow(data_modmat)]
    lasso_vars[, dist_samp := n_samp/nrow(outdata_modmat)]

    # figure out which lasso_vars to drop
    drop_samp = which(lasso_vars$dist_samp < 0.01 | lasso_vars$dist_samp > 0.99)
    drop_pop = which(lasso_vars$dist_pop < 0.01 | lasso_vars$dist_pop > 0.99)

    # drop variables from lasso_vars and data
    lasso_vars = lasso_vars[-unique(drop_pop, drop_samp)]
    data_modmat = data_modmat[, -unique(drop_pop, drop_samp)]


    ###### FIT MODELS ######
    # fit nonresponse lasso
    if(is.null(pop_weight_col)){
    	data[, pop_weight := 1]
    }else{
    	data[, pop_weight := get(pop_weight_col)]
    }

    fit_nr = cv.glmnet(y = as.numeric(data[, get(selected_ind)])
        , x = data_modmat
        , weights = as.numeric(data[, pop_weight])  #because the population data is weighted, include this
        , family = 'binomial'
        , nfolds = 10)

    ##### RANK COEFS #####
    fit_out = cv.glmnet(y = as.numeric(data[, get(outcome)])
        , x = outdata_modmat
        , nfolds = 10)

    coef_nr = data.frame(var_code = rownames(coef(fit_nr, lambda = 'lambda.1se')), coef_nr = coef(fit_nr, lambda = 'lambda.1se')[,1])[-1,]
    coef_out = data.frame(var_code = rownames(coef(fit_out, lambda = 'lambda.1se')), coef_out = coef(fit_out, lambda = 'lambda.1se')[,1])[-1,]

    lasso_vars[coef_nr, on = 'var_code', coef_nr := i.coef_nr]
    lasso_vars[coef_out, on = 'var_code', coef_out := i.coef_out]

    ##### RANK VARIABLES BY IMPORTANCE -- SHOULD REEVALUATE
    lasso_vars[order(abs(coef_nr), decreasing = T), rank_nr := .I]
    lasso_vars[coef_nr == 0, rank_nr := NA]
    lasso_vars[order(abs(coef_out), decreasing = T), rank_out := .I]
    lasso_vars[coef_out == 0, rank_out := NA]

    lasso_vars[,as.numeric(!is.na(rank_nr)) + as.numeric(!is.na(rank_out))]
    lasso_vars[order(as.numeric(is.na(rank_nr)) + as.numeric(is.na(rank_out)), as.numeric(rank_nr + rank_out)), rank_total := .I]

    lasso_vars[rank_total < 15, ]

    #create data table for weighting
    data = cbind(data, as.matrix(data_modmat))

    # get variable subsets for raking
    lasso_vars = lasso_vars[!is.na(rank_nr) | !is.na(rank_out)]

    lasso_vars[, subset := floor(rank_total/20)]
    lasso_vars[, subset := (max(subset) + 1)- subset]

    svydata = data[get(selected_ind) == 1, ]
    popdata = data[get(selected_ind) == 0, ]



    ##### DO RAKING THROUGH SUBSETS #####
    for(s in 1:max(lasso_vars$subset)){
        vars_for_raking <- lasso_vars[subset == s, var_code]

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


doCalibration = function(svydata, popdata, vars, epsilon = 1){

    # make model matricies
    formula_modmat = as.formula(paste0('~ ', paste(vars, collapse = '+')))
    pop_modmat = modmat_all_levs(formula_modmat, popdata, sparse = T)
    samp_modmat = modmat_all_levs(formula_modmat, svydata, sparse = T)

    ### DROP strata that are entirely missing
    drop_pop_levels = which(!colnames(pop_modmat) %in% colnames(samp_modmat))

    if(length(drop_pop_levels) > 0){
        print("WARNING dropping population levels because not in sample\n")
        apply(pop_modmat[,drop_pop_levels], 2, mean)
        pop_modmat = pop_modmat[, -drop_pop_levels]
    }
    

    ### rename variables so calibrate doesn't hate us
    cal_vars = data.table(var_name = colnames(pop_modmat), var_code = paste0('v', str_pad(1:ncol(pop_modmat), width = 3, side = 'left', pad='0')))

    colnames(pop_modmat) = cal_vars$var_code
    colnames(samp_modmat) = cal_vars$var_code[which(cal_vars$var_name %in% colnames(samp_modmat))]

    ## calculate sample and population totals
    pop_totals = apply(pop_modmat, 2, sum)
    samp_totals = apply(samp_modmat, 2, sum)

    ### DROP more levels that are too small
    small_pop_strata = pop_totals/nrow(pop_modmat)
    small_pop_strata = small_pop_strata[small_pop_strata < 0.01 | (small_pop_strata > 0.99 & small_pop_strata < 1)]
    small_pop_strata = data.table(var_code = names(small_pop_strata), pop_prop = small_pop_strata)

    small_samp_strata = samp_totals/nrow(samp_modmat)
    small_samp_strata = small_samp_strata[small_samp_strata < 0.01 | (small_samp_strata > 0.99 & small_samp_strata < 1)]
    small_samp_strata = data.table(var_code = names(small_samp_strata), samp_prop = small_samp_strata)

    small_strata = merge(merge(cal_vars, small_pop_strata, all = T), small_samp_strata, all = T)
    small_strata = small_strata[!is.na(pop_prop) | !is.na(samp_prop)]

    if(nrow(small_strata) > 0){
        print('WARNING dropping strata because <1% of pop or sample')
        print(small_strata)
        pop_modmat = pop_modmat[, -which(colnames(pop_modmat) %in% small_strata$var_code)]
    	samp_modmat = samp_modmat[, -which(colnames(samp_modmat) %in% small_strata$var_code)]
    }
    
    ## re-calc pop totals
    pop_totals = apply(pop_modmat, 2, sum)

    ## make survey data
    svydata = svydesign(id = ~1, data = data.frame(samp_modmat))

    ## make formula
    formula_cal = as.formula(paste0('~ -1 +', paste(names(pop_totals), collapse = '+')))

    ## DO calibration
    weighted = calibrate(design = svydata
            , formula = formula_cal
            , population = pop_totals
            , calfun = 'raking' #'logit', 'linear'
            , maxit = 1000
            , epsilon = epsilon #THIS IS KEY
            )

    weighted = cbind(weighted$variables, weight = (1/(weighted$prob + 0.00000001))/mean(1/(weighted$prob + 0.00000001), na.rm = T))

    return(weighted)
}
