
#!/apps/well/R/3.4.3/bin/Rscript                                                                                                                                                               
#$ -t 1                                                                                                                                                                       
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


####### SET simulation parameters
TaskId = as.numeric(Sys.getenv("SGE_TASK_ID"))

prop_sampled_options = c(0.01, 0.02, 0.05, 0.1, 0.25, 0.5, 0.75)
prop_sampled = prop_sampled_options[TaskId]


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

# number of vars used on avg
apply(coeff_samples, 2, function(x) sum(x != 0))

# which coefs did we select
cbind(missingness_covars, coeff_samples[,1])[as.vector(coeff_samples[, 1] != 0),]


# summary = data.frame(pop_mean = apply(ukbdata_modmat, 2, mean)
#     , sample_mean = apply(ukbdata_modmat[samples[,1] == 1, ], 2, mean)
#     )
# summary$diff = summary$sample_mean - summary$pop_mean
# summary


######### Write out samples to directory to run simulation
dir.create(paste0(sim_id, '/samples'))



lapply(1:ncol(samples), function(s){
	samp = samples[, s, with = F]
	filename = paste0(sim_id, '/samples/sample_', str_pad(s, width = 5, side = 'left', pad = '0'), '.csv')
	
	write.csv(samp, file = filename, row.names = F)

	filename
	})

list.files(paste0(sim_id, '/samples'))
list.files(paste0(sim_id, '/samples'))


