

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

