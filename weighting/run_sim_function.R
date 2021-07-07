


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
        , vars_bart = c(vars, vars_add)
        , vars_rake = vars_rake
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
        doCalibration(svydata = sample
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