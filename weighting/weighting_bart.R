

doBARTweight = function(data, vars_bart, vars_rake = NULL, popdata = NULL, selected_ind, pop_weight_col = NULL, ntree = 20, verbose = FALSE){

    if(is.null(vars_rake)){
        vars_rake = vars_bart
    }

    cat(paste0(Sys.time(), "\t\t Creating model matricies....\n"))
    formula_bart = as.formula(paste0('~ -1 + (', paste(vars_bart, collapse = ' + '), ')'))
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
    var_levels = apply(data[, vars_rake, with = F], 2, function(x) length(unique(x)))
    vars_rake = names(var_levels[var_levels < 15])
    imp_modmat = data[, vars_rake, with = F]
    imp_modmat[,(vars_rake):=lapply(.SD, as.factor), .SDcols=vars_rake]

    imp_fit = randomForest(y = as.factor(data[, get(selected_ind)]), x = imp_modmat, importance = T, ntree = 50)
    imp_vars = names(sort(imp_fit$importance[, 1], decreasing = T))
    imp_vars = var_levels[match(imp_vars, names(var_levels))]
    
    # limit vars to a total of 32 levels
    n_vars = max(which(cumsum(imp_vars) <= 32))
    cat(names(imp_vars[1:n_vars]), sep = '\n')

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
                        , vars = names(imp_vars[1:7])
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

