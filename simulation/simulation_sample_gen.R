#!/apps/well/R/3.4.3/bin/Rscript    
#$ -N sample_gen    
#$ -t 3:9
#$ -cwd
#$ -o ./logs                                                                                                                                                                               
#$ -e ./logs   
### request maximum of 24 hours of compute time
#$ -l h_rt=24:00:00
#$ -l s_rt=24:00:00

library(stringr)
library(knitr)
library(randomForest)
library(BayesTree)
library(data.table)


#source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_functions.R')  #also loads lots of packages

## GET PARAMS FROM DIRECTORY NAME
sim_name = getwd()
sim_name = str_split(sim_name, '/')[[1]]
sim_name = sim_name[length(sim_name)]

n_samples = as.numeric(str_split(sim_name, '_')[[1]][3])
n_equations = as.numeric(str_split(sim_name, '_')[[1]][2])



####### SET simulation parameters
#prop_sampled_options = c(0.01, 0.02, 0.05, 0.1, 0.25, 0.5, 0.75)
#prop_sampled_options = c(0.04, 0.075)
prop_sampled_options = c(0.01, 0.02, 0.04, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75)

# Set specific param for this task
TaskId = as.numeric(Sys.getenv("SGE_TASK_ID"))
#TaskId = 1
prop_sampled = prop_sampled_options[TaskId]

load('data_modmat.rda')
coeff_samples = fread('coefs.csv')

### hard set age
#coeff_samples[which(colnames(ukbdata_modmat) == 'age'), X1 := -5]
#ukbdata_modmat[, which(colnames(ukbdata_modmat) == 'age')]

####### USE COEFS TO GENERATE SAMPLES
lps = ukbdata_modmat %*% as.matrix(coeff_samples)
probs = exp(lps)/(1+exp(lps))

#check 
cat('Dist of probs\n')
summary(probs)


prob_mat = matrix(apply(probs, 2, rep, n_samples), ncol = ncol(probs)*n_samples)

samples = apply(prob_mat, 2, function(p, prop_sampled){
	sampled_int = sample(x = 1:length(p), size = (length(p) * prop_sampled), prob = p)
    sampled = rep(0, length(p))
    sampled[sampled_int] = 1
    sampled
	}, prop_sampled)

length(samples)
dim(samples)
head(samples)

#check that the missingness worked
mean(probs[samples[,1] == 1])
mean(probs[samples[,1] == 0])

#check that we sampled the right number
apply(samples, 2, sum)

#how'd we do on sampling different peeps?
summary(apply(samples, 1, sum))



######### Write out samples to directory to run simulation
sample_dir = paste0('samples/prop_', prop_sampled)
if(!dir.exists(sample_dir)){
	dir.create(sample_dir)
}


files = lapply(1:ncol(samples), function(s){
	samp = samples[, s]
	filename = paste0(sample_dir, '/sample_', str_pad(s, width = 5, side = 'left', pad = '0'), '.csv')
	
	write.csv(samp, file = filename, row.names = F)

	filename
	})




########### QC ING ###########
# covars = fread('missingness_covars.csv')
# load('data.rda')



# #read in first 1000 samples
# samps = NULL
# for(i in 1:1000){
# 	temp = fread(paste0('sample_',str_pad(i, width = 5, side = 'left', pad = '0'),'.csv'))

# 	if(is.null(samps)){
# 		samps = temp
# 	}else{
# 		samps = cbind(samps, temp)
# 	}
# }




# # any bias in age?
# bias = unlist(lapply(1:1000, function(s){
# 	samp = unlist(samples[, s])
# 	#samp = unlist(samps[, s, with = F])
# 	ukbdata[samp == 1, mean(MRI_brain_vol)] - ukbdata[samp == 0, mean(MRI_brain_vol)]
# 	}))

#  summary(bias)

# # ukbdata[samp == 1, mean(age)] - ukbdata[samp == 0, mean(age)]

# age = lapply(1:1000, function(s){
# 	samp = unlist(samples[, s])
# 	#samp = unlist(samps[, s, with = F])
# 	ukbdata[samp == 1, .(samp = .N), demo_age_bucket][order(demo_age_bucket)]
# 	})

# age

# # bias in brain vol by prob?
# ukbdata[, .(mean(MRI_brain_vol/1000), .N), cut(probs, breaks = quantile(probs, probs = seq(0,1,0.2)))][order(cut)]

# cbind(ukbdata, probs)[, .(mean(X1), .N), cut(age, breaks = quantile(age, probs = seq(0,1,0.1)))][order(cut)]


# mean(sample(probs, size = 1000, prob = probs))
# mean(probs)


# cbind(covars, coeff_samples)[as.vector(coeff_samples != 0),]
# cbind(ukbdata, probs)[, .(mean(X1)), demo_age_bucket][order(demo_age_bucket)]


