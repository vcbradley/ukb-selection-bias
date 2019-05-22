library(data.table)
library(stringr)
library(knitr)

which_sim = 'samples_1_5000_0.2_2019-05-21 22:10:38'
setwd(paste0('/well/nichols/users/bwj567/simulation/', which_sim))


#system('cat results/weights*.csv >> results/all_weights.csv')

#### cat results together
system("rm results/all_weights.csv")
system('head -1 results/weights_00001.csv > ./results/all_weights.csv')
system('for filename in $(ls results/weights_*.csv); do sed 1d $filename >> results/all_weights.csv; done')


#### Load in summary data
all_weights = fread(file = 'results/all_weights.csv')

# calc number of sims
oneset = fread('results/weights_00001.csv')
n_samp = nrow(oneset)
n_sim = nrow(all_weights)/n_samp

# add sim ID to results
all_weights[, sim_id := sort(rep(1:n_sim, n_samp))]

##### Load ukb data
load('data.rda')

#### Add in demos
all_weights_demos = merge(all_weights, ukbdata, by = 'eid', all.x = T)


######## ANALYSIS
# check variance of weights
variance = all_weights[, .(
	var_rake = var(rake_weight)
	, var_strat = var(strat_weight)
	, var_calib = var(calib_weight)
	, var_lasso = var(lasso_weight)
	, var_logit = var(logit_weight)
	, var_bart = var(bart_weight)
	), by = sim_id]
variance



vars = c('demo_sex'
        , 'demo_age_bucket'
        , 'demo_ethnicity_4way'
        , 'demo_empl_employed'
        , 'demo_empl_retired'
        , 'demo_occupation'
        , 'demo_educ_highest'
        , 'demo_income_bucket'
        #, 'demo_year_immigrated'
        , 'demo_hh_size'
        , 'demo_hh_ownrent'
        , 'demo_hh_accom_type'
        )

# check accuracy
accuracy = rbindlist(lapply(c('has_t1_MRI', vars), function(v){

	pop = ukbdata[, .(
        pop_count = .N
        , pop_prop = .N/nrow(ukbdata)
        , pop_brainvol = sum(as.numeric(MRI_brain_vol), na.rm = T)/.N
        ), by = v]

    samp = all_weights_demos[, .(
        samp_count = .N
        , samp_brainvol = sum(as.numeric(MRI_brain_vol), na.rm = T)/.N

        , rake_prop = sum(rake_weight)/sum(n_samp)
        , rake_brainvol = sum(as.numeric(MRI_brain_vol) * rake_weight, na.rm = T)/sum(rake_weight)

        , strat_prop = sum(strat_weight)/sum(n_samp)
        , strat_brainvol = sum(as.numeric(MRI_brain_vol) * strat_weight, na.rm = T)/sum(strat_weight)

        , calib_prop = sum(calib_weight)/sum(n_samp)
        , calib_brainvol = sum(as.numeric(MRI_brain_vol) * calib_weight, na.rm = T)/sum(calib_weight)

        , lasso_prop = sum(lasso_weight)/sum(n_samp)
        , lasso_brainvol = sum(as.numeric(MRI_brain_vol) * lasso_weight, na.rm = T)/sum(lasso_weight)

        , logit_prop = sum(logit_weight)/sum(n_samp)
        , logit_brainvol = sum(as.numeric(MRI_brain_vol) * logit_weight, na.rm = T)/sum(logit_weight)

        , bart_prop = sum(bart_weight)/sum(n_samp)
        , bart_brainvol = sum(as.numeric(MRI_brain_vol) * bart_weight, na.rm = T)/sum(bart_weight)

        ), by = c('sim_id', v)]

    cbind(var = v, merge(pop, samp, by = v, all = T))
    }))

setnames(accuracy, old = 'has_t1_MRI', new = 'level')


mse = accuracy[, .(
		samp_brainvol_error = min(samp_brainvol - pop_brainvol)
		, rake_brainvol_mse = log(sum((rake_brainvol - pop_brainvol)^2))
		, strat_brainvol_mse = log(sum((strat_brainvol - pop_brainvol)^2))
		, calib_brainvol_mse = log(sum((calib_brainvol - pop_brainvol)^2))
		, lasso_brainvol_mse = log(sum((lasso_brainvol - pop_brainvol)^2))
		, logit_brainvol_mse = log(sum((logit_brainvol - pop_brainvol)^2))
		, bart_brainvol_mse = log(sum((bart_brainvol - pop_brainvol)^2))
	), by = .(var, level)]
mse


#save to file
save(variance, accuracy, mse, file = 'results_summary.rda')







