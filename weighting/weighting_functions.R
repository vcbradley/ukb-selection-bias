## WEIGHTING FUNCTIONS
library(survey)
library(dplyr)
library(data.table)
library(MatrixModels)
library(Matrix)
library(glmnet)
library(lazyeval)
library(randomForest)
library(stringr)
library(BayesTree)
library(biglasso)
library(bigmemory)

# according to BART documentation, set this before loading bartMachine to avoid mem limit errors
#options(java.parameters = "-Xmx5g" ) 
#library(bartMachine)


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

    data_new = as_tibble(copy(data))

    popframe = data_new %>% 
            group_by_at(vars(vars)) %>% 
            summarize(
                n = n()
                , prop = n()/nrow(data_new)
                )

    if(!is.null(weight_col)){
        data_new = data_new %>% mutate(weight = data_new[[weight_col]])

        weighted = data_new %>% 
            group_by_at(vars(vars)) %>% 
            summarize(
                n_wt = sum(weight, na.rm = T)
                , prop_wt = sum(weight, na.rm = T)/sum(data_new$weight, na.rm = T)
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
    return(data.table(popframe))
}





#### Function to post-stratify survey
doPostStrat = function(svydata, popdata, vars, pop_weight_col = NULL, prior_weight_col = NULL, partial=FALSE){
    
    cat(vars)
    cat("\n")

    popframe  = getPopframe(popdata, vars = vars, weight_col = pop_weight_col)
	setnames(popframe, old = 'Freq', new = 'pop_prop')
    popframe = popframe[, pop_N := pop_prop * nrow(popdata)]

    print(head(popframe))
    
    sampframe  = data.table(getPopframe(svydata, vars = vars, weight_col = NULL))
    setnames(sampframe, old = 'Freq', new = 'samp_prop')
    sampframe = sampframe[, samp_N := samp_prop * nrow(svydata)]

    print(head(sampframe))

    # DO weighting
    weighted = merge(popframe, sampframe, by = vars)
    weighted[, weight := (pop_prop/samp_prop)]
    weighted = merge(svydata, weighted[, c(vars, 'weight'), with = F], by = vars, all.x = T)
    weighted[is.na(weight), weight := 1]
    print(weighted[,.N,is.na(weight)])

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
    , control = list(maxit = 5000, epsilon = 3e-4, verbose=FALSE)
    ){
     popdata_rake <- copy(popdata)
     svydata_rake <- copy(svydata)

    if(is.null(pop_weight_col)){
        popdata_rake[, pop_weight := 1]
    }else{
        popdata_rake[, pop_weight := get(pop_weight_col)]
        popdata_rake = popdata_rake[pop_weight > 0 & !is.na(pop_weight),]
    }

    if(is.null(prior_weight_col)){
        svydata_rake[, prior_weight := 1]
    }else{
        svydata_rake[, prior_weight := get(prior_weight_col)]
    }

    drop_pop = 1
    drop_samp = 1
    while(length(drop_samp)>0 | length(drop_pop)>0){
        drop_samp = unlist(lapply(vars, function(v){
            popmargin = popdata_rake[, .(pop_prop = sum(pop_weight)/sum(popdata_rake$pop_weight)), by = v]
            sampmargin = svydata_rake[, .(samp_prop = sum(prior_weight)/sum(svydata_rake$prior_weight)), by = v]
            margins = merge(popmargin, sampmargin, all = T)
            margins = cbind(var = v, margins)

            drop_samp = margins[is.na(pop_prop)]

            if(nrow(drop_samp) > 0){
                print("WARNING DROPPING strata because null in population")
                print(drop_samp)
            }

            drop = which(svydata_rake[,get(as.character(v))] %in% drop_samp[, 2])

            return(drop)
            }))

        if(length(drop_samp)>0){
            svydata_rake = svydata_rake[-unique(drop_samp), ]
        }

        drop_pop = unlist(lapply(vars, function(v){
            popmargin = popdata_rake[, .(pop_prop = sum(pop_weight)/sum(popdata_rake$pop_weight)), by = v]
            sampmargin = svydata_rake[, .(samp_prop = sum(prior_weight)/sum(svydata_rake$prior_weight)), by = v]
            margins = merge(popmargin, sampmargin, all = T)
            margins = cbind(var = v, margins)

            drop_pop = margins[is.na(samp_prop)]

            if(nrow(drop_pop) > 0){
                print("WARNING DROPPING strata because null in sample")
                print(drop_pop)
            }

            drop = which(popdata_rake[,get(as.character(v))] %in% unlist(drop_pop[, 2]))

            return(drop)
            }))

        if(length(drop_pop) > 0){
            popdata_rake = popdata_rake[-unique(drop_pop), ]
        }
        
    }

    cat(vars)

    popmargins = lapply(vars, getPopframe, data = popdata_rake, weight_col = 'pop_weight')
   
    strata = lapply(vars, function(x) as.formula(paste("~", x)))

    prior_weight = as.formula(ifelse(!is.null(prior_weight_col), paste0('~', prior_weight_col), '~1'))
    svydata_rake = svydesign(id = ~1, weights = prior_weight, data = svydata_rake)


    # do weighting
    weighted = rake(svydata_rake, sample.margins = strata, population.margins = popmargins, control = control)
    weighted = cbind(weighted$variables, weight = (1/(weighted$prob + 0.00000001))/mean(1/(weighted$prob + 0.00000001), na.rm = T))

    svydata[weighted, on = 'eid', weight := i.weight]
    svydata[is.na(weight), weight := 1]

    return(svydata)
}

## Wrapper for stratification that adds in a variable select function based on importance from a random forest
doPostStratVarSelect = function(data, vars, selected_ind, pop_weight_col = NULL){

    popdata = copy(data)

    if(is.null(pop_weight_col)){
        popdata[, pop_weight := 1]
    }else{
        popdata[, pop_weight := get(pop_weight_col)]
    }


    # make model matrix with categorical vars for random forest
    ps_modmat = copy(popdata[, vars, with = F])
    ps_modmat[,(vars):=lapply(.SD, as.factor), .SDcols=vars]

    # fit random forest and calc variable importance
    ps_fit = randomForest(y = as.factor(data[, get(selected_ind)]), x = ps_modmat, importance = T, ntree = 100)
    ps_vars = sort(ps_fit$importance[, 1], decreasing = T)
    cat(names(ps_vars))

    # get sample and popdata
    sample = popdata[get(selected_ind) == 1, ]
    popdata = popdata[!is.na(pop_weight) & pop_weight > 0, ]

    ### Increase number of stratification variables until we lose too much of the pop
    prop_pop_dropped = 0
    n_vars = 2
    # implicit tolerance threshold for dropped strata
    while(prop_pop_dropped < 0.01){
        # get strat vars
        strat_vars = names(ps_vars[1:n_vars])

        # calculate number in sample and number in populatioin
        drop_pop = merge(popdata[, .(prop_pop = sum(pop_weight)/sum(popdata$pop_weight)), by = strat_vars]
            , sample[, .(n_samp = .N, prop_samp = .N/nrow(sample)), by = strat_vars], all.x = T)

        # caluclate the pct of the sample that will be dropped
        prop_pop_dropped = drop_pop[is.na(n_samp), sum(prop_pop)]

        #increment number of variables
        n_vars = n_vars + 1
    }
    # take one fewer than the number it took to go over the tol threshold
    n_vars = n_vars - 1
    strat_vars = names(ps_vars[1:n_vars])

    ## DO POST STRAT
    weighted = doPostStrat(svydata = sample
            , popdata = popdata
            , vars = strat_vars
            , pop_weight_col = pop_weight_col)

    return(list(weighted, strat_vars))
}

# load('data.rda')
# load('data_modmat.rda')
# sample = unlist(fread('sample_00001.csv')[,1])

# data = ukbdata[,selected := sample]

## LAsso rake function
doLassoRake = function(
    data
    , vars
    , selected_ind 
    , outcome
    , pop_weight_col = NULL
    , n_interactions = 2
){
    # deep copy
    data_lasso = copy(data)

    if(is.null(pop_weight_col)){
        data_lasso[, pop_weight := 1]
    }else{
        data_lasso[, pop_weight := get(pop_weight_col)]
    }

    # check numbers of levels and drop vars with only 1
    samp_levels = apply(data_lasso[get(selected_ind) == 1, vars, with = F], 2, function(x) length(unique(x)))
    pop_levels = apply(data_lasso[, vars, with = F], 2, function(x) length(unique(x)))
    single_level = unique(names(c(samp_levels[which(samp_levels == 1)], pop_levels[which(samp_levels == 1)] )))
    vars = vars[!vars %in% single_level]

    formula = as.formula(paste0('~ -1 + (', paste(vars, collapse = ' + '), ')^', n_interactions))

    #moodmat for nr data
    data_modmat = modmat_all_levs(formula, data_lasso, sparse = T)
    outdata_modmat = modmat_all_levs(formula, data_lasso[get(selected_ind) == 1, ], sparse = T)

    ##### PREP VARIABLES #####
    samp = apply(outdata_modmat, 2, sum)
    pop = apply(data_modmat, 2, function(x, pop_weight) sum(x * pop_weight), pop_weight = data_lasso$pop_weight)

    # create var data table with variable codes
    lasso_vars = data.table(var_name = names(pop), var_code = paste0('v', str_pad(1:length(pop), width = 4, side = 'left', pad = '0')), n_pop = pop)
    samp = data.table(var_name = names(samp), n_samp = samp)
    lasso_vars = merge(lasso_vars, samp, by = 'var_name', all = T)

    lasso_vars[order(var_code),]

    # drop strata that are null in the pop or the sample
    strata_missing = lasso_vars[,which(is.na(n_pop) | is.na(n_samp) | n_samp == 0 | n_pop == 0)]
    if(length(strata_missing) > 0){
    	print("WARNING dropping strata because missing in sample or pop")
    	print(strata_missing)
    	lasso_vars = lasso_vars[-strata_missing,]
    }
    lasso_vars = lasso_vars[order(var_code)]

    cat(paste0(Sys.time(), "\t\t Creating modmat....\n"))
    data_modmat = data_modmat[, which(colnames(data_modmat) %in% lasso_vars$var_name)]
    outdata_modmat = outdata_modmat[, which(colnames(outdata_modmat) %in% lasso_vars$var_name)]

    ### rename columns to variable codes
    # mean(colnames(data_modmat) == lasso_vars[order(var_code), var_name])
    # mean(colnames(outdata_modmat) == lasso_vars[order(var_code), var_name])
    colnames(data_modmat) = lasso_vars[order(var_code), var_code]
    colnames(outdata_modmat) = lasso_vars[order(var_code), var_code]

    # calc distributions
    lasso_vars[, dist_pop := n_pop/nrow(data_modmat)]
    lasso_vars[, dist_samp := n_samp/nrow(outdata_modmat)]

    # figure out which lasso_vars to drop
    drop_samp = which(lasso_vars$dist_samp < 0.01 | lasso_vars$dist_samp > 0.99)
    drop_pop = which(lasso_vars$dist_pop < 0.01 | lasso_vars$dist_pop > 0.99)

    # drop variables from lasso_vars and data
    lasso_vars = lasso_vars[-unique(c(drop_pop, drop_samp)),]
    data_modmat = data_modmat[, colnames(data_modmat) %in% lasso_vars$var_code]
    outdata_modmat = outdata_modmat[, colnames(data_modmat) %in% lasso_vars$var_code]



    ##### DEFINE LAMBDAS
    #https://github.com/lmweber/glmnet-error-example/blob/master/glmnet_error_example.R

    # nonresponse
    y_mat_nr = matrix(as.numeric(data_lasso[, get(selected_ind)]), ncol = 1)
    lambda_max_nr = max(t(y_mat_nr) %*% data_modmat)/nrow(data_modmat)
    lambda_nr <- exp(seq(log(lambda_max_nr * 0.001), log(lambda_max_nr), length.out=20)) 

    # outcome
    y_mat_out = matrix(as.numeric(data_lasso[get(selected_ind) == 1, get(selected_ind)]), ncol = 1)
    lambda_max_out = max(t(y_mat_out) %*% outdata_modmat)/nrow(outdata_modmat)
    lambda_out <- exp(seq(log(lambda_max_out * 0.001), log(lambda_max_out), length.out=20)) 


    ###### FIT MODELS ######
    # fit nonresponse lasso

    cat(paste0(Sys.time(), "\t\t Fitting NR model....\n"))
    fit_nr = cv.glmnet(y = as.numeric(data_lasso[, get(selected_ind)])
        , x = data_modmat
        , weights = as.numeric(data_lasso[, ifelse(pop_weight == 0, 1, pop_weight)])  #because the population data is weighted, include this
        , family = 'binomial'
        , nfolds = 5
        , lambda = lambda_nr
        )

    print(summary(fit_nr))

    
    cat(paste0(Sys.time(), "\t\t Fitting outcome model....\n"))

    fit_out = cv.glmnet(y = as.numeric(data_lasso[get(selected_ind) == 1, get(outcome)])
        , x = outdata_modmat
        , nfolds = 5
        , lambda = lambda_out
        )

    print(summary(fit_nr))

    ##### RANK COEFS #####
    coef_nr = data.table(var_code = rownames(coef(fit_nr, s = 'lambda.min')), coef_nr = coef(fit_nr, s = 'lambda.min')[,1])[-1,]
    coef_out = data.table(var_code = rownames(coef(fit_out, s = 'lambda.1se')), coef_out = coef(fit_out, s = 'lambda.1se')[,1])[-1,]

    lasso_vars[coef_nr, on = 'var_code', coef_nr := i.coef_nr]
    lasso_vars[coef_out, on = 'var_code', coef_out := i.coef_out]
    lasso_vars[coef_nr != 0 | coef_out != 0]

    ##### RANK VARIABLES BY IMPORTANCE -- SHOULD REEVALUATE
    lasso_vars[order(abs(coef_nr), decreasing = T), rank_nr := .I]
    lasso_vars[coef_nr == 0, rank_nr := 9999]
    lasso_vars[order(abs(coef_out), decreasing = T), rank_out := .I]
    lasso_vars[coef_out == 0, rank_out := 9999]

    #lasso_vars[,as.numeric(!is.na(rank_nr)) + as.numeric(!is.na(rank_out))]
    lasso_vars[, has_nr_coef := as.numeric(rank_nr < 9999)]
    lasso_vars[, has_out_coef := as.numeric(rank_out < 9999)]
    lasso_vars[order(2 - (has_out_coef + has_nr_coef), as.numeric(rank_nr + rank_out)), rank_total := .I]

    print(lasso_vars[rank_total < 15, ][order(rank_total)])

    #create data table for weighting
    data_modmat_allvars = cbind(data_lasso, as.matrix(data_modmat))

    # get variable subsets for raking
    # only take top 50 vars if there are more than 50
    lasso_vars = lasso_vars[rank_total <= 50]

    lasso_vars[, subset := floor(rank_total/20)]
    lasso_vars[, subset := (max(subset) + 1)- subset]

    svydata = data_modmat_allvars[get(selected_ind) == 1, ]
    popdata = data_modmat_allvars[pop_weight > 0 & !is.na(pop_weight),]



    ##### DO RAKING THROUGH SUBSETS #####
    cat(paste0(Sys.time(), "\t\t Weighting....\n"))
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
    weighted = copy(data_lasso[get(selected_ind) == 1, ])
    weighted$weight = svydata$prior_weight

    return(list(weighted, lasso_vars$var_name))
}


doCalibration = function(svydata, popdata, vars, epsilon = 1, calfun = 'raking', pop_weight_col = NULL){

    if(is.null(pop_weight_col)){
        popdata[, pop_weight := 1]
    }else{
        popdata[, pop_weight := get(pop_weight_col)]
    }

    # check levels and get rid of those vars with only 1 so model matrix works
    samp_levels = apply(svydata[, vars, with = F], 2, function(x) length(unique(x)))
    vars_subset = vars[!vars %in% names(samp_levels)[samp_levels < 2]]

    # make model matricies
    cat(paste0(Sys.time(), "\t\t Making model matricies....\n"))

    formula_modmat = as.formula(paste0('~ ', paste(vars_subset, collapse = '+')))

    # use model matrix that drops a level for each categorical var so that result is not singular
    pop_modmat = model.matrix(formula_modmat, popdata, sparse = T)
    samp_modmat = model.matrix(formula_modmat, svydata, sparse = T)

    cat(paste0(Sys.time(), "\t\t Dropping levels....\n"))
    ### DROP strata that are entirely missing
    drop_pop_levels = which(!colnames(pop_modmat) %in% colnames(samp_modmat))

    if(length(drop_pop_levels) > 0){
        print("WARNING dropping population levels because not in sample\n")
        if(length(drop_pop_levels) == 1){
        	print(paste(colnames(pop_modmat)[drop_pop_levels],mean(pop_modmat[,drop_pop_levels])))
        }else{
        	print(apply(pop_modmat[,drop_pop_levels], 2, mean))
        }
        
        pop_modmat = pop_modmat[, -drop_pop_levels]
    }

    drop_samp_levels = which(!colnames(samp_modmat) %in% colnames(pop_modmat))

    if(length(drop_samp_levels) > 0){
        print("WARNING dropping population levels because not in sample\n")
        if(length(drop_samp_levels) == 1){
            print(paste(colnames(samp_modmat)[drop_samp_levels],mean(samp_modmat[,drop_samp_levels])))
        }else{
            print(apply(samp_modmat[,drop_samp_levels], 2, mean))
        }
        
        samp_modmat = samp_modmat[, -drop_samp_levels]
    }
    

    ### rename variables so calibrate doesn't hate us
    cal_vars = data.table(var_name = colnames(pop_modmat), var_code = paste0('v', str_pad(1:ncol(pop_modmat), width = 3, side = 'left', pad='0')))

    colnames(pop_modmat) = cal_vars$var_code
    colnames(samp_modmat) = cal_vars$var_code[which(cal_vars$var_name %in% colnames(samp_modmat))]

    ## calculate sample and population totals
    pop_totals = apply(pop_modmat, 2, function(x, pop_weight) {sum(x * pop_weight, na.rm = T)}, pop_weight = popdata$pop_weight)
    samp_totals = apply(samp_modmat, 2, sum)
    pop_levels = apply(pop_modmat, 2, function(x) length(unique(x)))

    # re-scale continuous vars
    cols = which(pop_totals > sum(popdata$pop_weight))
    if(length(cols) > 0){
        if(length(cols) == 1){
            pop_modmat[,cols] <- pop_modmat[,cols] / sum(pop_modmat[,cols]) * sum(popdata$pop_weight)
            samp_modmat[,cols] <- samp_modmat[,cols] / sum(samp_modmat[,cols]) * nrow(svydata)
        }else{
            pop_modmat[,cols] <- apply(pop_modmat[,cols], 2, function(x) x / sum(x) * sum(popdata$pop_weight))
            samp_modmat[,cols] <- apply(samp_modmat[,cols], 2, function(x) x / sum(x) * nrow(svydata))
        }

        # drop sample totals since they're implicit in the continuous vars now
        #pop_modmat = pop_modmat[, -1]
        #samp_modmat = samp_modmat[, -1]
    }


    # recalc totals
    pop_totals = apply(pop_modmat, 2, function(x, pop_weight) {sum(x * pop_weight, na.rm = T)}, pop_weight = popdata$pop_weight)
    samp_totals = apply(samp_modmat, 2, sum)
    pop_levels = apply(pop_modmat, 2, function(x) length(unique(x)))

    ### DROP more levels that are too small
    small_pop_strata = pop_totals/sum(popdata$pop_weight)
    small_pop_strata = small_pop_strata[(small_pop_strata < 0.025 | small_pop_strata > 0.98 & small_pop_strata < 1) & pop_levels <= 2]
    small_pop_strata = data.table(var_code = names(small_pop_strata), pop_prop = small_pop_strata)

    small_samp_strata = samp_totals/nrow(samp_modmat)
    small_samp_strata = small_samp_strata[(small_samp_strata < 0.025 | small_samp_strata > 0.98 & small_samp_strata < 1) & pop_levels <= 2]
    small_samp_strata = data.table(var_code = names(small_samp_strata), samp_prop = small_samp_strata)

    small_strata = merge(merge(cal_vars, small_pop_strata, all = T), small_samp_strata, all = T)
    small_strata = small_strata[!is.na(pop_prop) | !is.na(samp_prop)]

    if(nrow(small_strata) > 0){
        print('WARNING dropping strata because <2.5% of pop or sample')
        print(small_strata)
        pop_modmat = pop_modmat[, -which(colnames(pop_modmat) %in% small_strata$var_code)]
    	samp_modmat = samp_modmat[, -which(colnames(samp_modmat) %in% small_strata$var_code)]
    }

    
    ## re-calc pop totals
    pop_totals = apply(pop_modmat, 2, function(x, pop_weight) {sum(x * pop_weight, na.rm = T)}, pop_weight = popdata$pop_weight)
    print(pop_totals)

    apply(samp_modmat, 2, function(x) length(unique(x)))

    ## make survey data
    samp_modmat_design = svydesign(id = ~1, data = data.frame(as.matrix(samp_modmat)))


    cat(paste0(Sys.time(), "\t\t Calibrating....\n"))
    
    # split into subgroups for calibration so it doesn't die
    n_vars = length(pop_totals)
    n_loop = ceiling(n_vars/20)

    for(i in 1:n_loop){
        cat(paste0(Sys.time(), "\t\t\t Iteration ",i ,"\n"))
        group = order(pop_totals)[ceiling((1:n_vars)/(n_vars/n_loop)) == i]

        ## make formula
        formula_cal = as.formula(paste0('~ -1 +', paste(names(pop_totals)[group], collapse = '+')))

        print(formula_cal)
        cat('\n')

        ## DO calibration
        weighted = calibrate(design = samp_modmat_design
                , formula = formula_cal
                , population = pop_totals[group]
                , calfun = calfun #'raking' #'logit', 'linear'
                , maxit = 5000
                , epsilon = epsilon #THIS IS KEY
                #, verbose = T
                )
        samp_modmat_design = svydesign(id = ~1, data = data.frame(as.matrix(samp_modmat)), probs = weighted$prob)
        
    }


    weighted = data.table(cbind(svydata, weight = (1/(weighted$prob + 0.00000001))/mean(1/(weighted$prob + 0.00000001), na.rm = T)))

    return(weighted)
}

# Function for weighting with logit weights
# would be easy to compare to linear here
doLogitWeight = function(data, vars, selected_ind, n_interactions, pop_weight_col = NULL, modmat = NULL){

    data_logit = copy(data)

    # set pop weight col if it's null
    if(is.null(pop_weight_col)){
        data_logit[, pop_weight := 1]
    }else{
        data_logit[, pop_weight := get(pop_weight_col)]
    }
    
    if(is.null(modmat)){
        cat(paste0(Sys.time(), "\t\t Creating mod matricies....\n"))
        # create modmat for modeling
        formula_logit = as.formula(paste0('~ -1 + (', paste(vars, collapse = ' + '), ')^', n_interactions))
        logit_modmat = modmat_all_levs(formula = formula_logit, data = data_logit, sparse = T)

        print(formula_logit)
    }else{
        logit_modmat = modmat
    }

    #############

    # get var totals
    pop = apply(logit_modmat, 2, function(x, pop_weight) sum(x*pop_weight), pop_weight = data_logit$pop_weight)
    samp = apply(logit_modmat[data_logit[, get(selected_ind)] == 1,], 2, sum)

    # get number of levels
    pop_levels = apply(logit_modmat, 2, function(x) length(unique(x)))


     # create var data table with variable codes
    lasso_vars = data.table(var_name = names(pop), var_code = paste0('v', str_pad(1:length(pop), width = 4, side = 'left', pad = '0')), n_pop = pop)
    lasso_vars[, discrete := ifelse(pop_levels <= 2, 1, 0)]

    samp = data.table(var_name = names(samp), n_samp = samp)
    lasso_vars = merge(lasso_vars, samp, by = 'var_name', all = T)
    

    # drop strata that are null in the pop or the sample
    strata_missing = lasso_vars[(is.na(n_pop) | is.na(n_samp) | n_pop == 0 | n_samp == 0) & grepl(':',var_name) & discrete == 1,]
    if(nrow(strata_missing) > 0){
        print("WARNING dropping strata because missing in sample or pop")
        print(strata_missing)
        lasso_vars = lasso_vars[!((is.na(n_pop) | is.na(n_samp) | n_pop == 0 | n_samp == 0) & grepl(':',var_name) & discrete == 1),]
    }
    lasso_vars = lasso_vars[order(var_code)]

    cat(paste0(Sys.time(), "\t\t Creating modmat....\n"))
    logit_modmat = logit_modmat[, which(colnames(logit_modmat) %in% lasso_vars$var_name)]



    ## rename columns to variable codes
    # mean(colnames(logit_modmat) == lasso_vars[order(var_code), var_name])
    # mean(colnames(outlogit_modmat) == lasso_vars[order(var_code), var_name])
    # colnames(logit_modmat) = lasso_vars[order(var_code), var_code]

    # calc distributions
    lasso_vars[, dist_pop := n_pop/nrow(logit_modmat)]
    lasso_vars[, dist_samp := n_samp/sum(data_logit[, get(selected_ind)])]

    # figure out which lasso_vars to drop
    drop_samp = which((lasso_vars$dist_samp < 0.025 | lasso_vars$dist_samp > 0.975) & lasso_vars$discrete == 1)
    drop_pop = which((lasso_vars$dist_pop < 0.025 | lasso_vars$dist_pop > 0.975) & lasso_vars$discrete == 1)

    # drop variables from lasso_vars and data
    lasso_vars = lasso_vars[-unique(drop_pop, drop_samp),]
    logit_modmat = logit_modmat[, colnames(logit_modmat) %in% lasso_vars$var_name]
    

    #############

    
    # fiit logit model

    cat(paste0(Sys.time(), "\t\t Fit model....\n")) 
    #weight = 1/as.numeric(data[, sum(get(selected_ind))/.N])


    # define lambda values based on largest coef
    #https://stats.stackexchange.com/questions/174897/choosing-the-range-and-grid-density-for-regularization-parameter-in-lasso
    # y_mat = matrix(as.numeric(data_logit[, get(selected_ind)]), ncol = 1)
    # lambda_max = max(t(y_mat) %*% logit_modmat)/nrow(logit_modmat)
    # lambda <- exp(seq(log(lambda_max * 0.001), log(lambda_max), length.out=20)) #https://github.com/lmweber/glmnet-error-example/blob/master/glmnet_error_example.R

    # for testing
    #samp = 1:3000
    samp = 1:nrow(data_logit)
    y = as.numeric(data_logit[samp, get(selected_ind)])
    X = as.big.matrix(logit_modmat[samp,])

    timing = list()

    timing$biglasso_start <- Sys.time()
    fit_biglasso = cv.biglasso(
        y = y
        , X = X
        , nfolds = 5
        , family = 'binomial'
        , nlambda = 20
        )
    timing$biglasso_end <- Sys.time()
    timing$biglasso_tot = timing$biglasso_end - timing$biglasso_start

    print(summary(fit_biglasso))

    coef_biglasso = data.table(rownames(coef(fit_biglasso, s = 'lambda.min')), coef = as.numeric(coef(fit_biglasso, s = 'lambda.min')))
    coef_biglasso = coef_biglasso[coef != 0,]

    # # RUN LASSO
    # timing$glmnet_start <- Sys.time()
    # fit_logit = cv.glmnet(y = y
    #         , x = logit_modmat[samp,]
    #         #, weights = as.numeric(data_logit[, ifelse(pop_weight == 0, 1, pop_weight)])  #because the population data is weighted, include this
    #         , family = 'binomial'
    #         , nfolds = 5
    #         #, lambda=lambda
    #         , nlambda = 20
    #         )
    # timing$glmnet_end <- Sys.time()

    # print(summary(fit_logit))

    
    # timing$glmnet_tot = timing$glmnet_end - timing$glmnet_start

    # timing

    # coef_logit = data.table(rownames(coef(fit_logit, s = 'lambda.min')), coef = as.numeric(coef(fit_logit, s = 'lambda.min')))
    # coef_logit = coef_logit[coef != 0,]

    coef_final = coef_biglasso

    print(coef_final)

    cat(paste0(Sys.time(), "\t\t Calculate weights....\n"))

    # if the lasso didn't select any vars then just fit a logit with all vars
    if(nrow(coef_final) == 1){
        logit_vars = 'all'

        fit_logit = glm(as.formula(paste0(selected_ind, "~", paste(vars, collapse = '+')))
            , data = data_logit
            , weights = as.numeric(data_logit[, ifelse(pop_weight == 0, 1, pop_weight)])  #because the population data is weighted, include this
            , family = 'binomial'
            )
        probs = fit_logit$fitted.values[data_logit[,get(selected_ind)] == 1]

    }else{
        logit_vars = coef_final$V1[-1]

        re_fit_logit = glm.fit(x = logit_modmat[, which(colnames(logit_modmat) %in% logit_vars)]
            , y = as.numeric(data_logit[, get(selected_ind)])
            , weights = as.numeric(data_logit[, ifelse(pop_weight == 0, 1, pop_weight)])  #because the population data is weighted, include this
            , family = binomial()
            )
        probs = re_fit_logit$fitted.values

        # re_fit_logit = glmnet(y = as.numeric(data_logit[, get(selected_ind)])
        #     , x = logit_modmat[, which(colnames(logit_modmat) %in% logit_vars)]
        #     , weights = as.numeric(data_logit[, ifelse(pop_weight == 0, 1, pop_weight)])  #because the population data is weighted, include this
        #     , family = 'binomial'
        #     , lambda=0 # re-fit with no penalty, like normal logit
        #     )

        # calculate weights
        # lp = predict(re_fit_logit, newx = logit_modmat[data_logit$selected == 1, which(colnames(logit_modmat) %in% logit_vars)])
        # probs = exp(lp)/(1+exp(lp))        
    }

    weighted = copy(data)
    weighted[, prob := probs]
    weighted = weighted[selected == 1,]
    weighted[, weight := (1/(weighted$prob + 0.00000001))/mean(1/(weighted$prob + 0.00000001), na.rm = T)]

    return(list(weighted[, -'prob', with = F], logit_vars))
}



doBARTweight = function(data, vars, popdata = NULL, selected_ind, pop_weight_col = NULL, ntree = 20, verbose = FALSE){

    cat(paste0(Sys.time(), "\t\t Creating model matricies....\n"))
    formula_bart = as.formula(paste0('~ -1 + (', paste(vars, collapse = ' + '), ')'))
    bart_modmat = modmat_all_levs(formula = formula_bart, data = data, sparse= T)

    cat(paste0(Sys.time(), "\t\t Fitting model....\n"))
    gc()

    # bartfit = bartMachine(X = bart_modmat
    #     #, y = factor(data[, get(selected_ind)], levels = c('1', '0'), labels = c('1', '2'))
    #     , y = data[, get(selected_ind)]
    #     , num_trees = ntree
    #     , verbose = verbose
    #     , run_in_sample = TRUE
    #     )

    bartFit = bart(x.train = as.matrix(bart_modmat)
        , y.train = as.vector(data[, get(selected_ind)])
        , ntree = ntree
        , verbose = verbose)

    cat(paste0(Sys.time(), "\t\t Predicting model....\n"))

    z = apply(bartFit$yhat.train, 2, mean)
    prob = pnorm(z) #according to the documentatiion, we need to apply normal cdf to get actual prob values
    

    # # only predict on selected data
    # bart_modmat = bart_modmat[data[, get(selected_ind)] == 1, ]

    
    #prob = predict(bartfit, new_data = bart_modmat, type = 'prob')

    # bump up predicted yhats so they're greater than 0
    #cat(paste0(Sys.time(), "\t\t Getting probs....\n"))
    #prob = bartfit$y_hat_train + max(1, abs(min(bartfit$y_hat_train)) + 0.1)

    gc()

    weighted = cbind(data, bart_weight = 1/prob)
    weighted = weighted[data[, get(selected_ind)] == 1, ] #limit to selected units
    weighted = weighted[, bart_weight := (1/(bart_weight + 0.00000001))/mean(1/(bart_weight + 0.00000001), na.rm = T)]

    rm(bart_modmat)
    gc()

    # get important vars for raking
    cat(paste0(Sys.time(), "\t\t Getting var importance....\n"))
    
    # new modmat with categorical vars
    var_levels = apply(data[, vars, with = F], 2, function(x) length(unique(x)))
    vars = names(var_levels[var_levels < 15])
    imp_modmat = data[, vars, with = F]
    imp_modmat[,(vars):=lapply(.SD, as.factor), .SDcols=vars]

    imp_fit = randomForest(y = as.factor(data[, get(selected_ind)]), x = imp_modmat, importance = T, ntree = 50)
    imp_vars = names(sort(imp_fit$importance[, 1], decreasing = T)[1:5])


    rm(imp_fit)
    rm(bartFit)
    gc()

    if(!is.null(imp_vars)){

        if(is.null(pop_weight_col)){
            popdata = copy(data)
            popdata[, pop_weight := 1]
        }else{
            popdata = copy(data[(get(pop_weight_col) > 0) & (!is.na(get(pop_weight_col))),])
            popdata[, pop_weight := get(pop_weight_col)]
        }


        cat(paste0(Sys.time(), "\t\t Raking....\n"))
        temp = tryCatch({doRaking(svydata = weighted
                        , popdata = popdata
                        , vars = imp_vars
                        , prior_weight_col = 'bart_weight'
                        , pop_weight_col = pop_weight_col
                        )}, error = function(e) print(e))

        # if raking fails, just use BART weights
        if(!'data.frame' %in% class(temp)){
            weighted[, weight := bart_weight]
            weighted = weighted[, -'bart_weight', with = F]
            imp_vars = 'none'
        }else{
            weighted[temp, on = 'eid', weight := i.weight]
            # if there are null weights because raking dropped some, set them to 1
            weighted[is.na(weight), weight := 1]
        }

    }else{
        weighted[, weight := bart_weight]
        weighted = weighted[, -'bart_weight', with = F]
        imp_vars = 'none'
    }

    return(list(weighted, imp_vars))
}



###### RUN ONE ITERATION of the simulation
runSim = function(data
    , sample
    , vars
    , vars_add = NULL
    , vars_rake = NULL
    , epsilon = 1
    , outcome
    , pop_weight_col = NULL
    , n_interactions = 2
    , verbose = FALSE
    , ntree = 100
    , modmat = modmat){

    selected_ind = 'selected'
    #data[, selected := NULL]
    data = cbind(data, selected = sample)

    sample = copy(data[selected == 1, ])

    # cat(names(data))
    # cat('\n')
    # print(head(data))
    # cat('\n')
    # cat(names(sample))

    timing = list()

    ####### CALIBRATE
    timing$calib_start <- Sys.time()
    cat(paste0(timing$calib_start, '\t', "Running calibration..."))
    calibrated_data = tryCatch({
        doCalibration(svydata = sample
            , popdata = data
            , vars = c(vars_rake, vars_add)
            , epsilon = epsilon
            , calfun = calfun)
        }, error = function(e) print(e))

    if('data.table' %in% class(calibrated_data)){
        print(summary(calibrated_data$weight))
        cat('\n\n') 
    }else{
        cat('Calibration did not converge \n\n')

        calibrated_data = copy(sample)
        calibrated_data[, weight := 1]
    }
    timing$calib_end <- Sys.time()
    timing$calib_time <- timing$calib_end - timing$calib_start

    print(gc())

    ###### LOGIT
    timing$logit_start <- Sys.time()
    cat(paste0(timing$logit_start, '\t', "Running logit weighting..."))
    logit_weighted = tryCatch({
        doLogitWeight(data = data
        , vars = c(vars, vars_add)
        , n_interactions = n_interactions
        , selected_ind = selected_ind
        , modmat = modmat)
        }, error = function(e) print(e))

    if('list' %in% class(logit_weighted)){
        print(summary(logit_weighted[[1]]$weight))  
        cat('\n\n') 
    }else{
        cat('Logit did not converge \n\n')
        logit_weighted = copy(sample)
        logit_weighted = list(logit_weighted[, weight := 1], vars = 'none')
    }

    print(gc())
    timing$logit_end <- Sys.time()
    timing$logit_time <-  timing$logit_end - timing$logit_start



    ####### POST STRAT WITH variable selection
    timing$poststrat_start <- Sys.time()
    cat(paste0(timing$poststrat_start, '\t', "Running post strat..."))
    strat_data = tryCatch({
        doPostStratVarSelect(data = data
        , vars = vars
        , selected_ind = selected_ind)
        }, error = function(e) print(e))


    if('list' %in% class(strat_data)){
        print(summary(strat_data[[1]]$weight))
        cat('\n\n') 
    }else{
        cat('PostStrat did not converge \n\n')
        strat_data = copy(sample)
        strat_data = list(strat_data[, weight := 1], vars = 'none')
    }
    timing$poststrat_end <- Sys.time()
    timing$poststrat_time <- timing$poststrat_end - timing$poststrat_start
    


    print(gc())


    ###### LASSO RAKE
    head(data)
    head(sample)
    timing$lassorake_start <- Sys.time()
    cat(paste0(timing$lassorake_start, '\t', "Running lasso rake...\n\n\n"))
    lassorake_data = tryCatch({
        doLassoRake(data = data
        , vars = vars
        , selected_ind = selected_ind
        , outcome = outcome
        , pop_weight_col = pop_weight_col
        , n_interactions = n_interactions)
        }, error = function(e) print(e))


    if('list' %in% class(lassorake_data)){
        print(summary(lassorake_data[[1]]$weight))
        cat('\n\n') 
    }else{
        cat('LassoRake did not converge \n\n')
        lassorake_data = copy(sample)
        lassorake_data = list(lassorake_data[, weight := 1], vars = 'none')
    }

    timing$lassorake_end <- Sys.time()
    timing$lassorake_time <- timing$lassorake_end - timing$lassorake_start
        
    print(gc())


    ####### BART + rake
    timing$bart_start <- Sys.time()
    cat(paste0(timing$bart_start, '\t', "Running BART...\n\n\n"))
    bart_weighted = tryCatch({
        doBARTweight(data = data
        , vars = c(vars, vars_add)
        , selected_ind = selected_ind
        , verbose = TRUE 
        , ntree = ntree)
        }, error = function(e) print(e))

    if('list' %in% class(bart_weighted)){
        print(summary(bart_weighted[[1]]$weight))
        cat('\n\n') 
    }else{
        cat('BARTWeight did not converge \n\n')
        bart_weighted = copy(sample)
        bart_weighted = list(bart_weighted[, weight := 1], vars = 'none')
    }
    timing$bart_end <- Sys.time()
    timing$bart_time <- timing$bart_end - timing$bart_start
    

    print(gc())

     ##### RAKING
    timing$rake_end <- Sys.time()
    cat(paste0(timing$rake_end, '\t', "Running raking..."))
    # calibration without continuous vars is raking
    # raked_data = tryCatch({
    #     doCalibration(svydata = sample
    #         , popdata = data
    #         , vars = vars
    #         , epsilon = epsilon
    #         , calfun = calfun)
    #     }, error = function(e) print(e))
     raked_data = tryCatch({
        doRaking(svydata = sample
            , popdata = data
            , vars = vars_rake
            )
        }, error = function(e) print(e))
  
    
    if('data.table' %in% class(raked_data)){
        print(summary(raked_data$weight))
        cat('\n\n') 
    }else{
        cat('Raking did not converge \n\n')
        raked_data = copy(sample)
        raked_data[, weight := 1]
    }

    timing$rake_end <- Sys.time()
    timing$rake_time <- timing$rake_end - timing$rake_start 
    

    weighted_list = list(
        raked_data[, .(eid, rake_weight = weight)]
        , strat_data[[1]][, .(eid, strat_weight = weight)]
        , calibrated_data[, .(eid, calib_weight = weight)]
        , lassorake_data[[1]][, .(eid, lasso_weight = weight)]
        , logit_weighted[[1]][, .(eid, logit_weight = weight)]
        , bart_weighted[[1]][, .(eid, bart_weight = weight)]
        )

    all_weights = Reduce(function(x,y) merge(x,y, by = 'eid', all = T) , weighted_list)

    #calc total time
    timing$total_time = timing$rake_end - timing$calib_start

    return(list(all_weights = all_weights
        , strat_vars = strat_data[[2]]
        , lassorake_vars = lassorake_data[[2]]
        , logit_vars = logit_weighted[[2]]
        , bart_vars = bart_weighted[[2]]
        , timing = do.call(cbind, timing)
        ))
}




# #### For testing

# sample = read.csv(sprintf("sample_%05d.csv", 2))[,1]

# # load data
# load(file = paste0('../../data.rda'))
# data = ukbdata[1:length(sample),]#limit for now


# # run simulation draft
# print(paste0(Sys.time(), '\t Weighting starting...'))

# #source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_functions.R')  #also loads lots of packages



# ####### DO WEIGHTING  #######
# vars = c('demo_sex'
#         , 'demo_age_bucket'
#         , 'demo_ethnicity_4way'
#         , 'demo_empl_employed'
#         , 'demo_empl_retired'
#         , 'demo_occupation'
#         , 'demo_educ_highest'
#         , 'demo_income_bucket'
#         #, 'demo_year_immigrated'
#         , 'demo_hh_size'
#         , 'demo_hh_ownrent'
#         , 'demo_hh_accom_type'
#         )
# vars_add = c('age', 'age_sq')
# vars_rake = c('demo_sex', 'demo_ethnicity_4way', 'demo_age_bucket')
# pop_weight_col = NULL
# epsilon = nrow(data) * 0.0001
# calfun = 'raking'
# outcome = 'MRI_brain_vol'



# all_weights = tryCatch(runSim(data = data
#         , sample = sample
#         , vars = vars
#         , vars_add = vars_add
#         , outcome = 'MRI_brain_vol'
#         , pop_weight_col = pop_weight_col
#         , verbose = FALSE
#         , ntree = 1
#         , epsilon = epsilon
#         )
# , error = function(e) print(e))
 
# #all_weights
# apply(all_weights[[1]], 2, summary)



#     bartFit = bart(x.train = as.matrix(ukbdata_modmat)
#         , y.train = as.vector(selected$V1)
#         , verbose = TRUE
#         , ntree = 50)

#     cat(paste0(Sys.time(), "\t\t Predicting model....\n"))

#     z = apply(bartFit$yhat.train, 2, mean)
#     prob = pnorm(z) #according to the documentatiion, we need to apply normal cdf to get actual prob values
   



#     bartfit = bartMachine(X = data.frame(ukbdata_modmat)
#         , y = selected$V1
#         , num_trees = 50
#         , verbose = TRUE
#         , run_in_sample = TRUE
#         )

#     summary(bartfit$y_hat_train)
#     probs = bartfit$y_hat_train + max(1, abs(min(bartfit$y_hat_train)) + 0.1)
#     summary(probs)
#     summary(1/probs)
#     probs = predict(bartfit, new_data = data.frame(ukbdata_modmat), type = 'prob') #need this and not y_hat_train to get probs

# # var_importance = investigate_var_importance(bartfit, type = "splits")

# summary(probs)
# mean(probs[selected$V1 == 1])
# mean(probs[selected$V1 == 0])
# summary(selected$V2)
# length(levels(selected$V2))
# class(selected$V2)