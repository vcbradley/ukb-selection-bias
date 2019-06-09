########################
# Weighting neuro data #
########################

setwd('/well/nichols/users/bwj567/weighting')

# source weighting functions
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_functions.R')

# load data
list.files('../data')

data_full = fread('../data/ukb25120_weighting.csv')
data_img = fread('../data/ukb25120_weighting_img.csv')

setnames(data_img
    , old = names(data_img)[grepl('^img', names(data_img))]
    , new = gsub('^img_','',names(data_img)[grepl('^img', names(data_img))])
    )

# read in census and HSE data
data_hse = fread('../data/hse16_recoded.csv')
data_census = fread('../data/census11_recoded.csv')

# drop HSE observations outside of age range
data_hse = data_hse[demo_age_bucket %in% data_img[, unique(demo_age_bucket)], ]
data_hse[, .N, demo_age_bucket][order(demo_age_bucket)]
data_img[, .N, demo_age_bucket][order(demo_age_bucket)]

# fix coding
data_hse[demo_income_bucket == '06-DNK/Refused', demo_income_bucket := '99-DNK/Refused']


intersect(names(data_hse)
,names(data_img))
#names(data_census)


#######################

selected_ind = 'selected'
epsilon = 0.00001
calfun = 'raking'
vars_rake = c('demo_sex'
	, 'demo_age_bucket'
	, 'demo_white'
	, 'demo_empl_retired'
	, 'demo_empl_employed'
	, 'demo_empl_homemaker'
	, 'demo_educ_highest'
	, 'demo_occupation'
	, 'demo_income_bucket'
	, 'demo_hh_ownrent'
	)
n_interactions = 2

####### CALIBRATE

    calibrated_data = tryCatch({
        doCalibration(svydata = data_img
            , popdata = data_hse
            , vars = vars_rake
            , epsilon = epsilon
            , calfun = calfun
            , pop_weight_col = 'wt_blood'
            )
        }, error = function(e) print(e))

    if('data.table' %in% class(calibrated_data)){
        print(summary(calibrated_data$weight))
        cat('\n\n') 
    }else{
        cat('Calibration did not converge \n\n')

        calibrated_data = copy(sample)
        calibrated_data[, weight := 1]
    }


    ###### LOGIT

    # create combined data table
    data_comb = rbind(
    	cbind(data_img[, c('eid',vars), with = F], pop_weight = 0, selected = 1)
	    , cbind(data_hse[, c('SerialA', vars, 'wt_blood'), with = F], selected = 0)
	    , use.names = F)

    # drop rows with null pop weights
	data_comb = data_comb[!is.na(pop_weight),]

	# get logit weights
    logit_weighted = tryCatch({
        doLogitWeight(data = data_comb
        , vars = vars
        , n_interactions = n_interactions
        , selected_ind = selected_ind
        , pop_weight_col = 'pop_weight' 
        )
        }, error = function(e) print(e))

    if('list' %in% class(logit_weighted)){
        print(summary(logit_weighted[[1]]$weight))  
        cat('\n\n') 
    }else{
        cat('Logit did not converge \n\n')
        logit_weighted = copy(sample)
        logit_weighted = list(logit_weighted[, weight := 1], vars = 'none')
    }
    


    ####### POST STRAT WITH variable selection

    strat_data = tryCatch({
        doPostStratVarSelect(data = data_comb
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


    ####### BART + rake
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

     ##### RAKING
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


    

    weighted_list = list(
        raked_data[, .(eid, rake_weight = weight)]
        , strat_data[[1]][, .(eid, strat_weight = weight)]
        , calibrated_data[, .(eid, calib_weight = weight)]
        , lassorake_data[[1]][, .(eid, lasso_weight = weight)]
        , logit_weighted[[1]][, .(eid, logit_weight = weight)]
        , bart_weighted[[1]][, .(eid, bart_weight = weight)]
        )
