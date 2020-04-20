

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
        print("WARNING dropping population levels because not in population\n")
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
    small_pop_strata = small_pop_strata[(small_pop_strata < 0.02 | small_pop_strata > 0.98 & small_pop_strata < 1) & pop_levels <= 2]
    small_pop_strata = data.table(var_code = names(small_pop_strata), pop_prop = small_pop_strata)

    small_samp_strata = samp_totals/nrow(samp_modmat)
    small_samp_strata = small_samp_strata[(small_samp_strata < 0.02 | small_samp_strata > 0.98 & small_samp_strata < 1) & pop_levels <= 2]
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
