#!/apps/well/R/3.4.3/bin/Rscript    
#$ -N sample_gen    
#$ -t 1:9
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


source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_functions.R')  #also loads lots of packages

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
prop_sampled = prop_sampled_options[TaskId]

load('data_modmat.rda')
coeff_samples = fread('coefs.csv')

####### USE COEFS TO GENERATE SAMPLES
lps = ukbdata_modmat %*% as.matrix(coeff_samples)
probs = exp(lps)/(1+exp(lps))


samples = lapply(1:ncol(probs), function(i, probs, prop_sampled, n_samples){
    p = probs[,i]
    temp = matrix(1, nrow = length(p), ncol = n_samples)

    temp = apply(temp, 2, function(p, prop_sampled){
        sampled_int = sample.int(n = length(p), size = length(p) * prop_sampled, prob = p)
        sampled = seq(from = 1, to = length(p), by = 1)
        sampled = ifelse(sampled %in% sampled_int, 1, 0)
        }, prop_sampled)

    return(data.table(temp))
    
    }, probs, prop_sampled, n_samples)
samples = do.call(cbind, samples)

length(samples)
dim(samples)
head(samples)

#check that we sampled the right number
apply(samples, 2, sum)

#how'd we do on sampling different peeps?
summary(apply(samples, 1, sum))



######### Write out samples to directory to run simulation
sample_dir = paste0('samples/prop_', prop_sampled)
if(!dir.exists(sample_dir)){
	dir.create(sample_dir)
}


lapply(1:ncol(samples), function(s){
	samp = samples[, s, with = F]
	filename = paste0(sample_dir, '/sample_', str_pad(s, width = 5, side = 'left', pad = '0'), '.csv')
	
	write.csv(samp, file = filename, row.names = F)

	filename
	})




############ QC ING ###########
# samp = fread('samples/prop_0.02/sample_00001.csv')
# samp = fread('samples/prop_0.02/sample_00002.csv')


# samps = NULL
# for(i in 1:100){
# 	temp = fread(paste0('samples/prop_0.02/sample_',str_pad(i, width = 5, side = 'left', pad = '0'),'.csv'))

# 	if(is.null(samps)){
# 		samps = temp
# 	}else{
# 		samps = cbind(samps, temp)
# 	}
# }

# apply(samps * probs, 2, sum)/sum(samps$V1)
# apply((1-samps) * probs, 2, sum)/sum(1-samps$V1)


# summary(probs)


# sampled_int = sample.int(nrow(probs),size = 200, prob = probs[,1])

# cbind(sampled = summary(probs[sampled_int,])
# ,not_sampled = summary(probs[-sampled_int,]))



# head(ukbdata_modmat[, coeff_samples != 0])

# covars = fread('missingness_covars.csv')
# load('data.rda')

# cbind(covars, coeff_samples)[as.vector(coeff_samples != 0),]


# cbind(ukbdata, probs)[, .(mean(X1)), demo_age_bucket][order(demo_age_bucket)]


