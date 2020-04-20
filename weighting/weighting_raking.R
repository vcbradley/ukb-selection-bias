

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

    # make model matricies
    formula = as.formula(paste0("~-1+", paste(vars, collapse = '+')))
    pop_modmat_rake = modmat_all_levs(formula = formula, popdata_rake)
    samp_modmat_rake = modmat_all_levs(formula = formula, svydata_rake)

    # get counts and dists

    dists = rbindlist(
        lapply(vars, function(v) {

            pop = popdata_rake[, .(pop_count = sum(pop_weight)), .(var = get(v))]
            samp = svydata_rake[, .(samp_count = .N), .(var = get(v))]
            cbind(var_name = v, merge(pop, samp, by = 'var', all = T))
            }))
    dists = dists[!var == '0']

    dists[, pop_dist := pop_count / nrow(popdata_rake)]
    dists[, samp_dist := samp_count / nrow(svydata_rake)]

    #pop_count = apply(pop_modmat_rake, 2, function(x) sum(x * popdata_rake$pop_weight))
    #samp_count = apply(samp_modmat_rake, 2, sum)


    #dists = data.table(var = names(pop_count), pop_count = pop_count, pop_dist = pop_count / nrow(pop_modmat_rake))
    #dists = merge(dists, data.table(var = names(samp_count), samp_count), all = T)
    #dists[, samp_dist := samp_count / nrow(samp_modmat_rake)]
    dists[is.na(dists)] <- 0
    dists[, ps_weight := pop_dist / samp_dist]

    #dists$parent_var = rbindlist(lapply(vars, function(v) data.table(var = v, ind = grep(v, dists$var))))[order(ind)]$var
    dists$var_code <- paste0('v', 1:nrow(dists))

    # rename cols in modmats with var code
    #var_order_pop = match(colnames(pop_modmat_rake), paste(dists$var))
    colnames(pop_modmat_rake) = dists[pop_count > 0, var_code]#dists$var_code[var_order_pop]
    #var_order_samp = match(colnames(samp_modmat_rake), dists$var)
    colnames(samp_modmat_rake) = dists[samp_count > 0, var_code] #dists$var_code[var_order_samp]

    pop_modmat_rake = data.table(cbind(pop_modmat_rake, pop_weight = popdata_rake$pop_weight, eid = popdata_rake$eid))
    samp_modmat_rake = data.table(cbind(samp_modmat_rake, prior_weight = svydata_rake$prior_weight, eid = svydata_rake$eid))


    # drop levels that are too small
    dists[, drop_samp := as.numeric(samp_dist < 0.01 | samp_dist > 0.99 | samp_count < 5 | (samp_count > nrow(samp_modmat_rake) - 5))]
    dists[, drop_pop := as.numeric(pop_dist < 0.01 | pop_dist > 0.99| pop_count < 5 | (pop_count > nrow(pop_modmat_rake) - 5) & pop_count < nrow(pop_modmat_rake))]

    if(sum(dists$drop_samp) > 0){
        print("WARNING DROPPING strata because null in sample")
        print(dists[drop_samp == 1])
    }

    if(sum(dists$drop_pop) > 0){
        print("WARNING DROPPING strata because null in population")
        print(dists[drop_pop == 1])
    }

    # recalc totals to account for dropped levels
    var_totals = dists[, .(total = nrow(popdata_rake), dropped = sum(pop_count * drop_samp)), var_name]
    var_totals[, total_adj := total - dropped]


    ### get list of raking variables
    # order by group size
    vars_to_use = dists[drop_samp == 0 & drop_pop == 0 & !grepl('demo_age', var_name), ][order(-abs(pop_dist - 0.5))]$var_code
    vars_to_use = c(vars_to_use, dists[grepl('demo_age', var_name) & drop_samp == 0 & drop_pop == 0, var_code])  #always have age in last group

    popmargins = lapply(vars_to_use, function(v){
        parent_var = dists[var_code == v, var_name]
        drop = dists[var_name == parent_var & drop_samp == 1]$var

        #if(length(drop) > 0){
        if(FALSE){
            t = pop_modmat_rake[!(popdata_rake[, get(parent_var)] %in% drop)
            , .(Freq = sum(get('pop_weight')))
            , .(v = get(v))]
        } else {
            t = pop_modmat_rake[
            , .(Freq = sum(get('pop_weight')))
            , .(v = get(v))]
        }

        setnames(t, old = 'v', new  = v)
        t$Freq = t$Freq / sum(t$Freq)
        
        return(t)
        })

    #popmargins = lapply(vars_to_use, getPopframe, data = pop_modmat_rake, weight_col = 'pop_weight')
    strata = lapply(vars_to_use, function(x) as.formula(paste("~", x)))

    prior_weight = as.formula(ifelse(!is.null(prior_weight_col), paste0('~prior_weight'), '~1'))
    weighted = svydesign(id = ~1, weights = prior_weight, data = samp_modmat_rake)


    # iterate through variables
    it_size = ifelse(length(vars_to_use) <= 31, length(vars_to_use), 25)
    nit = ceiling(length(vars_to_use) / it_size)

    for(i in 1:nit){
        cat(i)
        v_start = (it_size * (i - 1) + 1)
        v_stop = min((i * it_size), length(vars_to_use))
        weighted  = rake(weighted
            , sample.margins = strata[v_start:v_stop]
            , population.margins = popmargins[v_start:v_stop]
            , control = control
            )
    }
    #weighted = svydesign(id = ~1, probs = temp$prob, data = samp_modmat_rake)

    summary(weighted$prob)
    summary((1/weighted$prob)/mean(1/weighted$prob))



    #v1 = temp$prob #order(-abs(pop_dist - 0.5)) largest groups last
    #v2 = temp$prob #order(abs(pop_dist - 0.5)) smallest groups last
    #v3 = temp$prob #order(abs(ps_weight - 1)) most imbalanced last
    #v4 = temp$prob #order(-abs(ps_weight - 1)) least imbalanced last

    # svydata_v2 = cbind(svydata, v1 = (1/v1)/mean(1/v1), v2 = (1/v2)/mean(1/v2), v3 = (1/v3)/mean(1/v3), v4 = (1/v4)/mean(1/v4))

    # n_samp = nrow(svydata_v2)

    # lapply(vars_rake, function(v){
    #     svy = svydata_v2[,.(
    #         count = .N
    #     , v1 = sum(v1)/n_samp
    #     , v2 = sum(v2)/n_samp
    #     , v3 = sum(v3)/n_samp
    #     , v4 = sum(v4)/n_samp
    #     , v1_brainvol = sum(v1 * MRI_brain_vol_scaled)/sum(v1)
    #     , v2_brainvol = sum(v2 * MRI_brain_vol_scaled)/sum(v2)
    #     , v3_brainvol = sum(v3 * MRI_brain_vol_scaled)/sum(v3)
    #     , v4_brainvol = sum(v4 * MRI_brain_vol_scaled)/sum(v4)
    #     ), by = get(v)]
    #     pop = popdata[,.(pop = .N/nrow(popdata)
    #         , pop_brainvol = mean(MRI_brain_vol_scaled)
    #         ), get(v)]
    #     t = cbind(v, merge(pop, svy, by = 'get') )
    #     t[, .(v, get, v1_error = (v1_brainvol - pop_brainvol) * .N/21000
    #         , v2_error = v2_brainvol - pop_brainvol* .N/21000
    #         , v3_error = v3_brainvol - pop_brainvol* .N/21000
    #         , v4_error = v4_brainvol - pop_brainvol* .N/21000
    #         )]
    #     })
    

    




    # drop_pop = 1
    # drop_samp = 1
    # while(length(drop_samp)>0 | length(drop_pop)>0){
    #     drop_samp = unlist(lapply(vars, function(v){
    #         popmargin = popdata_rake[, .(pop_prop = sum(pop_weight)/sum(popdata_rake$pop_weight)), by = v]
    #         sampmargin = svydata_rake[, .(samp_prop = sum(prior_weight)/sum(svydata_rake$prior_weight)), by = v]
    #         margins = merge(popmargin, sampmargin, all = T)
    #         margins = cbind(var = v, margins)

    #         drop_samp = margins[is.na(pop_prop)]

    #         if(nrow(drop_samp) > 0){
    #             print("WARNING DROPPING strata because null in population")
    #             print(drop_samp)
    #         }

    #         drop = which(svydata_rake[,get(as.character(v))] %in% drop_samp[, 2])

    #         return(drop)
    #         }))

    #     if(length(drop_samp)>0){
    #         svydata_rake = svydata_rake[-unique(drop_samp), ]
    #     }

    #     drop_pop = unlist(lapply(vars, function(v){
    #         popmargin = popdata_rake[, .(pop_prop = sum(pop_weight)/sum(popdata_rake$pop_weight)), by = v]
    #         sampmargin = svydata_rake[, .(samp_prop = sum(prior_weight)/sum(svydata_rake$prior_weight)), by = v]
    #         margins = merge(popmargin, sampmargin, all = T)
    #         margins = cbind(var = v, margins)

    #         drop_pop = margins[is.na(samp_prop)]

    #         if(nrow(drop_pop) > 0){
    #             print("WARNING DROPPING strata because null in sample")
    #             print(drop_pop)
    #         }

    #         drop = which(popdata_rake[,get(as.character(v))] %in% unlist(drop_pop[, 2]))

    #         return(drop)
    #         }))

    #     if(length(drop_pop) > 0){
    #         popdata_rake = popdata_rake[-unique(drop_pop), ]
    #     }
        
    # }

    # cat(vars)

    # # prep for weighting
    # popmargins = lapply(vars, getPopframe, data = popdata_rake, weight_col = 'pop_weight')
    # strata = lapply(vars, function(x) as.formula(paste("~", x)))

    # prior_weight = as.formula(ifelse(!is.null(prior_weight_col), paste0('~', prior_weight_col), '~1'))
    # svydata_rake_design = svydesign(id = ~1, weights = prior_weight, data = svydata_rake)


    # #### do weighting

    # # try to weight with all vars at once
    # weighted = tryCatch({
    #     rake(svydata_rake_design, sample.margins = strata, population.margins = popmargins, control = control)
    #     }, error = function(e) print(e))
    # # check if that worked

    # if('error' %in% class(weighted)){

        
    #     n_vars = length(strata) 
    #     strata[1:10]

    #     temp  = rake(svydata_rake, sample.margins = strata[1:10], population.margins = popmargins[1:10], control = control)


    # }

    weighted = cbind(weighted$variables, weight = (1/(weighted$prob + 0.00000001))/mean(1/(weighted$prob + 0.00000001), na.rm = T))

    
    # dists[4,]
    # weighted[,.(sum(weight)/214), v4]


    svydata[weighted, on = 'eid', weight := i.weight]
    svydata[is.na(weight), weight := 1]
    summary(svydata$weight)

    # check age dist
    # svydata[,.(sum(weight)/214), demo_age_bucket][order(demo_age_bucket)]
    # dists[var_name == 'demo_age_bucket', .(var, pop_dist, samp_dist)]

    return(svydata)
}