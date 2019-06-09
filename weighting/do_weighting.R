########################
# Weighting neuro data #
########################

setwd('/well/nichols/users/bwj567/mini-project-1/weighting')

# source weighting functions
source('weighting_functions.R')


### SET OUTCOME
outcome = 'MRI_brain_vol'
pop_weight_col = 'wt_blood'


# load data
list.files('../data')

data_full = fread('/well/nichols/users/bwj567/data/ukb25120_weighting.csv')
data_img = fread('/well/nichols/users/bwj567/data/ukb25120_weighting_img.csv')

setnames(data_img
    , old = names(data_img)[grepl('^img', names(data_img))]
    , new = gsub('^img_','',names(data_img)[grepl('^img', names(data_img))])
    )

# read in census and HSE data
data_hse = fread('/well/nichols/users/bwj567/data/hse16_recoded.csv')
data_census = fread('/well/nichols/users/bwj567/data/census11_recoded.csv')

# drop HSE observations outside of age range
data_hse = data_hse[demo_age_bucket %in% data_img[, unique(demo_age_bucket)], ]
data_hse[, .N, demo_age_bucket][order(demo_age_bucket)]
data_img[, .N, demo_age_bucket][order(demo_age_bucket)]

# fix coding
data_hse[demo_income_bucket == '06-DNK/Refused', demo_income_bucket := '99-DNK/Refused']


vars = intersect(names(data_hse), names(data_img))

# create combined data table
data_comb = rbind(
	cbind(data_img[, c('eid',vars, outcome), with = F], pop_weight = 0, selected = 1)
    , cbind(data_hse[, c('SerialA', vars), with = F], MRI_brain_vol = NA, data_hse[, c(pop_weight_col), with = F], selected = 0)
    , use.names = F)
# drop rows with null pop weights
data_comb = data_comb[!is.na(pop_weight),]


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
vars_demo = vars[!grepl('health', vars)]

n_interactions = 2
ntree = 25




####### CALIBRATE
cat(paste0(Sys.time(), '\t', "Running calibration...\n\n\n"))
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
cat(paste0(Sys.time(), '\t', "Running logit...\n\n\n"))
# get logit weights
logit_weighted = tryCatch({
    doLogitWeight(data = data_comb
    , vars = vars_demo
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
cat(paste0(Sys.time(), '\t', "Running post-strat...\n\n\n"))
strat_data = tryCatch({
    doPostStratVarSelect(data = data_comb
    , vars = vars_rake
    , selected_ind = selected_ind
    , pop_weight_col = 'pop_weight'
    )
    }, error = function(e) print(e))

if('list' %in% class(strat_data)){
    print(summary(strat_data[[1]]$weight))
    cat('\n\n') 
}else{
    cat('PostStrat did not converge \n\n')
    strat_data = copy(sample)
    strat_data = list(strat_data[, weight := 1], vars = 'none')
}  


# data_hse[, .(weighted = sum(wt_blood, na.rm = T)/sum(data_hse$wt_blood, na.rm = T)
# 	, unweighted = .N/nrow(data_hse)
# 	), demo_age_bucket][order(demo_age_bucket)]

# strat_data[[1]][, .(
# 	 .N/nrow(strat_data[[1]])
# 	, sum(weight)/sum(strat_data[[1]]$weight)
# 	, mean(weight)
# 	), by =demo_age_bucket][order(demo_age_bucket)]


###### LASSO RAKE
cat(paste0(Sys.time(), '\t', "Running lasso rake...\n\n\n"))
lassorake_data = tryCatch({
    doLassoRake(data = data_comb
    , vars = vars_demo
    , selected_ind = selected_ind
    , outcome = outcome
    , pop_weight_col = 'pop_weight'
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
cat(paste0(Sys.time(), '\t', "Running BART...\n\n\n"))
bart_weighted = tryCatch({
    doBARTweight(data = data_comb
    , vars = vars_demo
    , selected_ind = selected_ind
    , verbose = TRUE 
    , pop_weight_col = 'pop_weight'
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
cat(paste0(Sys.time(), '\t', "Running raking..."))
 raked_data = tryCatch({
    doRaking(svydata = data_img
        , popdata = data_hse
        , vars = vars_rake
        , pop_weight_col = 'wt_blood'
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
all_weights = Reduce(function(x,y) merge(x,y, by = 'eid', all = T) , weighted_list)


apply(all_weights, 2, summary)

#check NAs
apply(all_weights, 2, function(x) sum(is.na(x)))


data_img_hse_weighted = merge(data_img, all_weights, by = 'eid', all = T)

data_img_hse_weighted[, .(mean(MRI_brain_vol)
	, weighted.mean(MRI_brain_vol, w = strat_weight)
	, mean(MRI_brain_vol) - weighted.mean(MRI_brain_vol, w = strat_weight)
	), by = demo_age_bucket][order(demo_age_bucket)]



data_hse[, has_t1_MRI := 1]
weight_summary = rbindlist(lapply(c('has_t1_MRI', vars), function(v){
	

	pop = data_hse[pop_weight > 0 & !is.na(pop_weight), .(
		pop_count = .N
		, pop_count_weighted = sum(pop_weight)
		, pop_prop = .N/data_hse[,sum(pop_weight > 0 & !is.na(pop_weight))]
        , pop_prop_weighted = sum(pop_weight)/sum(data_hse$pop_weight, na.rm = T)
        #, pop_brainvol = sum(as.numeric(MRI_brain_vol), na.rm = T)/.N
        ), by = v]

    samp = data_img_hse_weighted[, .(
        samp_count = .N
        , samp_prop = .N/nrow(data_img_hse_weighted)
        , samp_brainvol = sum(as.numeric(MRI_brain_vol), na.rm = T)/.N

        , rake_prop = sum(rake_weight, na.rm = T)/nrow(data_img_hse_weighted)
        , rake_brainvol = sum(as.numeric(MRI_brain_vol) * rake_weight, na.rm = T)/sum(rake_weight, na.rm = T)

        , strat_prop = sum(strat_weight, na.rm = T)/nrow(data_img_hse_weighted)
        , strat_brainvol = sum(as.numeric(MRI_brain_vol) * strat_weight, na.rm = T)/sum(strat_weight, na.rm = T)

        , calib_prop = sum(calib_weight, na.rm = T)/nrow(data_img_hse_weighted)
        , calib_brainvol = sum(as.numeric(MRI_brain_vol) * calib_weight, na.rm = T)/sum(calib_weight, na.rm = T)

        , lasso_prop = sum(lasso_weight, na.rm = T)/nrow(data_img_hse_weighted)
        , lasso_brainvol = sum(as.numeric(MRI_brain_vol) * lasso_weight, na.rm = T)/sum(lasso_weight, na.rm = T)

        , logit_prop = sum(logit_weight, na.rm = T)/nrow(data_img_hse_weighted)
        , logit_brainvol = sum(as.numeric(MRI_brain_vol) * logit_weight, na.rm = T)/sum(logit_weight, na.rm = T)

        , bart_prop = sum(bart_weight, na.rm = T)/nrow(data_img_hse_weighted)
        , bart_brainvol = sum(as.numeric(MRI_brain_vol) * bart_weight, na.rm = T)/sum(bart_weight, na.rm = T)

        # all variances
        , var_rake = var(rake_weight, na.rm = T)
		, var_strat = var(strat_weight, na.rm = T)
		, var_calib = var(calib_weight, na.rm = T)
		, var_lasso = var(lasso_weight, na.rm = T)
		, var_logit = var(logit_weight, na.rm = T)
		, var_bart = var(bart_weight, na.rm = T)

        ), by = v]

    cbind(var = v, merge(pop, samp, by = v, all = T))
    }))

setnames(weight_summary, old = 'has_t1_MRI', new = 'level')
weight_summary


methods = c('rake','strat','calib','lasso','logit','bart')
apply(weight_summary[, lapply(.SD, function(x) x - pop_prop),.SDcols = paste0(methods,'_prop')]
	, 2, function(x) sum(x^2, na.rm = T))




