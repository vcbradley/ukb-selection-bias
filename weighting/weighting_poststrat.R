

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