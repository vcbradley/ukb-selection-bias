#!/apps/well/R/3.4.3/bin/Rscript       
#$ -N sim_apoe_reg
#$ -t 1:7                                                                                                                       
#$ -cwd
#$ -q short.qc
#$ -o ./logs                                                                                                                                                                               
#$ -e ./logs   
#$ -l s_rt=24:00:00
#$ -l h_rt=24:00:00

######################
# Brain Vol and ApoE #
######################

JobId = as.numeric(Sys.getenv("SGE_TASK_ID"))
prop_sampled_options = c(0.01, 0.02, 0.04, 0.05, 0.075, 0.1, 0.25, 0.5)
prop = prop_sampled_options[JobId]

library(data.table)

sim_name = 'sim_1_5000_old_v3'
setwd(paste0('/well/nichols/users/bwj567/simulation/', sim_name))

# set results directory
results_dir = paste0('/well/nichols/users/bwj567/mini-project-1/simulation/results/', sim_name)

### DATA
# load pop data
load('data.rda')

# fix factor levels
ukbdata[, demo_sex := relevel(factor(demo_sex), 'Male')]
ukbdata[, health_apoe_phenotype := relevel(factor(health_apoe_phenotype), '03-other')]


# load weight data
all_weights = fread(file = 'results/all_weights.csv')

#limit to only the prop we want for this run
all_weights = all_weights[prop_sampled == prop, ]

#### Add in demos
all_weights_demos = merge(all_weights, ukbdata, by = 'eid', all.x = T)

# set factor levels
all_weights_demos[, health_apoe_phenotype := relevel(factor(health_apoe_phenotype), '03-other')]
all_weights_demos[, demo_sex := relevel(factor(demo_sex), 'Male')]


### RUN REGRESSIONS
methods = names(all_weights_demos)[grepl('_weight', names(all_weights_demos))]
methods = gsub('_weight','', methods)

## Samples
coefs_full = NULL
p_vals_full = NULL

for(p in sort(unique(all_weights_demos[, prop_sampled]))){
	cat(paste('Running prop ', p, '\n'))

	for(i in 1:max(all_weights_demos[prop_sampled == p, sim_num])){
		if(i/100 == 0){
			cat(paste('\tRunning iter ', i, '\n'))
		}
		for(m in c('none', methods)){
			if(m == 'none'){
				weights = rep(1, nrow(all_weights_demos[prop_sampled == p & sim_num == i,]))
			}else{
				weights = all_weights_demos[prop_sampled == p & sim_num == i,get(paste0(m,'_weight'))]
			}
			
			fit = glm(MRI_brain_vol ~ -1 +
				demo_sex * health_apoe_phenotype +
				age
				, data = all_weights_demos[prop_sampled == p & sim_num == i,]
				, weights = weights
			)
			coefs = data.table(prop_sampled = p, sim_num = i, method = m, t(coef(fit)))
			p_vals = data.table(prop_sampled = p, sim_num = i, method = m, t(summary(fit)$coef[,4]))

			if(is.null(coefs_full)){
				coefs_full = coefs
			}else{
				coefs_full = rbind(coefs_full, coefs, fill = T)
			}

			if(is.null(p_vals_full)){
				p_vals_full = p_vals
			}else{
				p_vals_full = rbind(p_vals_full, p_vals, fill = T)
			}

		}
	}
}
coefs_full
p_vals_full


## pop
pop_fit = glm(MRI_brain_vol ~ -1 +
		demo_sex * health_apoe_phenotype +
		age
		, data = ukbdata
	)
pop_coefs = data.table(prop_sampled = p, sim_num = i, method = m, t(coef(pop_fit)))
pop_p_vals = data.table(prop_sampled = p, sim_num = i, method = m, t(summary(pop_fit)$coef[,4]))


# RENAME
coef_names = c('Male','Female', 'ApoE e4/e4', 'ApoE e3/e4', 'Age', 'Female:ApoE e4/e4', 'Female:ApoE e3/e4')
setnames(coefs_full, c('prop_sampled', 'sim_num','method',coef_names))
setnames(p_vals_full, c('prop_sampled', 'sim_num','method',coef_names))
setnames(pop_coefs, c('prop_sampled', 'sim_num','method',coef_names))
setnames(pop_p_vals, c('prop_sampled', 'sim_num','method',coef_names))


#### calculate error

coef_error = lapply(coef_names, function(c){
	coefs_full[, c, with = F] - as.numeric(pop_coefs[,c, with = F])
	})
coef_error = do.call(cbind, coef_error)
setnames(coef_error, paste0('Error ', coef_names))
coefs_full = data.table(cbind(coefs_full, coef_error))



#### save results
write.csv(coefs_full, file = paste0(results_dir, '/apoe_reg_coef_prop_',prop,'.csv'))
write.csv(p_vals_full, file = paste0(results_dir, '/apoe_reg_pval_prop_',prop,'.csv'))

if(JobId == 1){
	save(pop_coefs, pop_p_vals, file = paste0(results_dir, '/apoe_reg_popvals.rda'))
}







