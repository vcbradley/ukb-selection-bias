library(data.table)
library(stringr)
library(knitr)

wd = getwd()
sim_name = gsub('/gpfs2/well/nichols/users/bwj567/simulation/', '', wd)

#system('cat results/weights*.csv >> results/all_weights.csv')

result_list = list.files('results/', pattern = 'prop')

# FIXING ISSUES
# r = 'prop_0.02'
# for(w in list.files('results/prop_0.02', pattern = 'weights_', full.names = T)){
# 	system(paste0("cat ",w," | awk -F',' '{print $1,$2,$3,$4,$5,$6,$7}' > ", gsub('.csv','_rev.csv',w)))
# }
# system('head results/prop_0.02/weights_00001_rev.csv')

# for(w in list.files('results/prop_0.02', pattern = 'rev.csv', full.names = T)){
# 	system(paste0('mv ', w, ' ', gsub('_rev.csv','.csv', w)))
# }

# for(r in list.files('results/prop_0.02', pattern = 'weights_', full.names = T)[2:1000]){
# 	data = fread(r, sep = ',')
# 	names = names(data)
# 	names = unlist(str_split(gsub('"',"",names[1]), " "))

# 	data_rev = t(apply(data[, 1], 1, function(x) {
# 		str_split(x, " ")[[1]]
# 		}))

# 	data_rev = apply(data_rev, 2, as.numeric)
# 	data_rev = data.table(data_rev)
# 	setnames(data_rev, old = names(data_rev), new = names)

# 	write.csv(data_rev, file = r, row.names = F)
# }

######## CAT TOGETHER RESULTS ########
##### create all results files
for(r_ind in 1:length(result_list)){
	r = result_list[r_ind]

	print(paste0('Running ', r))

	# remove all results file if it exists
	if(file.exists(paste0('results/',r,'/all_weights_*'))){
		system(paste0('rm results/', r, '/all_weights_', r, '.csv'))
	}

	# get header for new all results file
	system(paste0("head -1 results/", r, "/weights_00001.csv | sed 's/$/,\"prop_sampled\"/' > results/", r, "/all_weights_", r, ".csv"))

	# add in all weighting results
	system(paste0("for filename in $(ls results/",r,"/weights_*.csv); do sed 's/$/,",gsub('prop_','',r),"/' $filename | sed 1d >> results/",r,"/all_weights_",r,".csv; done"))

	# do checks
	print(system(paste0('wc results/', r, '/all_weights_', r, '.csv -l')))
	print(system(paste0('head results/', r, '/all_weights_', r, '.csv')))

	###now create full results file
	# if first file, create header
	if(r_ind == 1){

		# if the file exists, delete it
		if(file.exists('results/all_weights.csv')){
			system("rm results/all_weights.csv")
		}
		
		# add header
		system(paste0("head -1 results/", r, "/all_weights_", r, ".csv > results/all_weights.csv"))
	}

	# for all files, add results
	system(paste0("sed 1d results/",r,"/all_weights_",r,".csv >> results/all_weights.csv"))
	print(system(paste0('wc results/all_weights.csv -l')))
}



######### LOAD WEIGHTS #########
all_weights = fread(file = 'results/all_weights.csv')


# only do this for this time
# all_weights[, .(n_samp = .N/1000), by = prop_sampled]
# all_weights = cbind(all_weights, sim_num = c(sort(rep(1:1000, 428)), sort(rep(1:1000, 2140)), sort(rep(1:1000, 10703))))
# all_weights
# all_weights[, .N, .(prop_sampled, sim_num)]


##### Load ukb data
load('data.rda')

#### Add in demos
all_weights_demos = merge(all_weights, ukbdata, by = 'eid', all.x = T)


methods = gsub('_weight','',names(all_weights)[grepl('weight', names(all_weights))])

######## ANALYSIS

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
        , 'health_smoking_status'
        , 'health_apoe_phenotype'
        , 'health_BMI_bucket'
        , 'health_bp_high_ever'
        )

all_weights_demos[all_weights_demos[, .(n_samp = .N/max(sim_num)), by = prop_sampled], on = 'prop_sampled', n_samp := i.n_samp]

# check accuracy
weight_summary = rbindlist(lapply(c('has_t1_MRI', vars), function(v){

	pop = ukbdata[, .(
        pop_count = .N
        , pop_prop = .N/nrow(ukbdata)
        , pop_brainvol = sum(as.numeric(MRI_brain_vol), na.rm = T)/.N
        ), by = v]

    samp = all_weights_demos[, .(
        samp_count = .N
        , samp_prop = .N/max(n_samp)
        , samp_brainvol = sum(as.numeric(MRI_brain_vol), na.rm = T)/.N

        , rake_prop = sum(rake_weight, na.rm = T)/max(n_samp)
        , rake_brainvol = sum(as.numeric(MRI_brain_vol) * rake_weight, na.rm = T)/sum(rake_weight, na.rm = T)

        , strat_prop = sum(strat_weight, na.rm = T)/max(n_samp)
        , strat_brainvol = sum(as.numeric(MRI_brain_vol) * strat_weight, na.rm = T)/sum(strat_weight, na.rm = T)

        , calib_prop = sum(calib_weight, na.rm = T)/max(n_samp)
        , calib_brainvol = sum(as.numeric(MRI_brain_vol) * calib_weight, na.rm = T)/sum(calib_weight, na.rm = T)

        , lasso_prop = sum(lasso_weight, na.rm = T)/max(n_samp)
        , lasso_brainvol = sum(as.numeric(MRI_brain_vol) * lasso_weight, na.rm = T)/sum(lasso_weight, na.rm = T)

        , logit_prop = sum(logit_weight, na.rm = T)/max(n_samp)
        , logit_brainvol = sum(as.numeric(MRI_brain_vol) * logit_weight, na.rm = T)/sum(logit_weight, na.rm = T)

        , bart_prop = sum(bart_weight, na.rm = T)/max(n_samp)
        , bart_brainvol = sum(as.numeric(MRI_brain_vol) * bart_weight, na.rm = T)/sum(bart_weight, na.rm = T)

        # all variances
        , var_rake = var(rake_weight, na.rm = T)
		, var_strat = var(strat_weight, na.rm = T)
		, var_calib = var(calib_weight, na.rm = T)
		, var_lasso = var(lasso_weight, na.rm = T)
		, var_logit = var(logit_weight, na.rm = T)
		, var_bart = var(bart_weight, na.rm = T)

        ), by = c('prop_sampled','sim_num', v)]

    cbind(var = v, merge(pop, samp, by = v, all = T))

}))

setnames(weight_summary, old = 'has_t1_MRI', new = 'level')
weight_summary

# check NAs
weight_summary[var == 'has_t1_MRI', lapply(.SD, function(x) sum(x == 0 | is.na(x))), .SDcols = names(weight_summary)[grepl('var', names(weight_summary))]]

# check where algs didn't converge
weight_summary[var == 'has_t1_MRI', lapply(.SD, function(x) mean(x == 0)), .SDcols = names(weight_summary)[grepl('var_', names(weight_summary))], by = prop_sampled][order(prop_sampled)]

# check weights count by prop_sampled
weight_summary[var == 'has_t1_MRI', .N, prop_sampled]

# check toplines
weight_summary[var == 'has_t1_MRI' & prop_sampled == 0.01,][order(sim_num)]

# calc brainvol error
weight_summary[, samp_error := samp_brainvol - pop_brainvol]
weight_summary[, paste0(methods, '_error') := lapply(.SD, function(col) col - pop_brainvol), .SDcols = paste0(methods, '_brainvol')]


mse = weight_summary[, .(
		pop_prop = min(pop_prop)
		, samp_prop = min(samp_prop)
		, samp_brainvol_error = min(samp_error)
		, rake_brainvol_mse = log(mean((rake_error)^2))
		, strat_brainvol_mse = log(mean((strat_error)^2))
		, calib_brainvol_mse = log(mean((calib_error)^2))
		, lasso_brainvol_mse = log(mean((lasso_error)^2))
		, logit_brainvol_mse = log(mean((logit_error)^2))
		, bart_brainvol_mse = log(mean((bart_error)^2))
	), by = .(prop_sampled, var, level)]
mse



#save to file
git_path = '/well/nichols/users/bwj567/mini-project-1/simulation/results'
results_path = paste0(git_path, '/', sim_name) 
if(!dir.exists(git_path)){
	dir.create(git_path)
}
if(!dir.exists(results_path)){
		dir.create(results_path)
	}
list.files(git_path)
list.files(results_path)

save(weight_summary, mse, file = paste0(results_path, '/results_summary.rda'))


# library(data.table)
# sim_name = 'sim_1_5000_v3'
# setwd(paste0('/well/nichols/users/bwj567/simulation/', sim_name))

# covars = fread('missingness_covars.csv')
# coefs = fread('coefs.csv')

# sim_coefs = cbind(covars, coefs)[X1 != 0]


# #### missingness mech
# coefs = fread('coefs.csv')
# covars = fread('missingness_covars.csv')
# coef_table = cbind(covars, coefs)[X1 != 0]

# kable(coef_table[, .(Z = var_name, B_z = X1)], format = 'latex', booktabs = T)





