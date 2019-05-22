library(data.table)
library(stringr)
library(knitr)

setwd('/well/nichols/users/bwj567/simulation/samples_1_5000_0.2_2019-05-21 22:10:38')


#system('cat results/weights*.csv >> results/all_weights.csv')

# cat results together
system("rm results/all_weights.csv")
system('head -1 results/weights_00001.csv > ./results/all_weights.csv')
system('for filename in $(ls results/weights_*.csv); do sed 1d $filename >> results/all_weights.csv; done')

all_weights = fread(file = 'results/all_weights.csv')
oneset = fread('results/weights_00001.csv')

n_samp = nrow(oneset)

n_sim = nrow(all_weights)/n_samp

all_weights[, sim_id := sort(rep(1:n_sim, n_samp))]


# check variance of weights
all_weights[, .(
	var_rake = var(rake_weight)
	, var_strat = var(strat_weight)
	, var_calib = var(calib_weight)
	, var_lasso = var(lasso_weight)
	, var_logit = var(logit_weight)
	, var_bart = var(bart_weight)
	), by = sim_id]


# check accuracy