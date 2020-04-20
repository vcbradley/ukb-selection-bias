

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
