#!/apps/well/R/3.4.3/bin/Rscript                                                                                                                                                               
#$ -t 1:8                                                                                                                                                                       
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

JobId = as.numeric(Sys.getenv("SGE_TASK_ID"))

prop_sampled_options = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75)

## different probs for different types of vars - want to make sure we have a nonlinear var in there
# might want to think about controlling the level of noise
n_equations = 1
n_samples = 5000
prop_sampled = prop_sampled_options[JobId]



###### PREP DATA

# Read in UKB data
ukbdata = fread('/well/nichols/users/bwj567/data/ukb25120_weighting_img.csv', stringsAsFactors = F)

setnames(ukbdata
    , old = names(ukbdata)[grepl('^img', names(ukbdata))]
    , new = gsub('^img_','',names(ukbdata)[grepl('^img', names(ukbdata))])
    )

#mean impute missing BMIs
ukbdata[is.na(bmi), bmi := mean(ukbdata$bmi, na.rm = T)]

# make missing vals DNK
ukbdata[is.na(demo_hh_size), demo_hh_size := '99-DNK/Refused']

ukbdata[, age_sq := age^2]
ukbdata[, bmi_sq := bmi^2]
ukbdata[, health_alc_weekly_total_sq := health_alc_weekly_total^2]

ukbdata$noise <- rnorm(nrow(ukbdata),0,1)

# check NAs
nas = apply(ukbdata, 2, function(x) sum(is.na(x)))
nas[nas > 0]




#### GENERATE Probability of missingness

### TO DO: ADD IN INTERACTIONS
vars_to_consider = names(ukbdata)[-grep('^MRI|eid|has|assessment|demo_ethnicity_4way|demo_white|demo_educ_highest$', names(ukbdata))]
formula = paste("~-1+(", paste0(vars_to_consider, collapse = " + "), ")")
ukbdata_modmat = modmat_all_levs(as.formula(formula), data = ukbdata)


missingness_covars = data.table(var_name = colnames(ukbdata_modmat))
missingness_covars[, data_type := ifelse(grepl('bmi|^age|health_alc_weekly_total', var_name), 'int', 'char')]
missingness_covars[var_name == 'noise', data_type := 'noise']
missingness_covars[, type := ifelse(grepl('health|bmi', var_name), 'health', 'demo')]


#normalize continuous variables
ukbdata_modmat[, which(missingness_covars$data_type == 'int')] <- scale(ukbdata_modmat[, which(missingness_covars$data_type == 'int')])

#check that it looks ok
apply(ukbdata_modmat[, which(missingness_covars$data_type == 'int')], 2, mean)
apply(ukbdata_modmat[, which(missingness_covars$data_type == 'int')], 2, sd)


##### GENERATE MISSINGNESS MODEL COEFS

coeff_samples = rbindlist(lapply(missingness_covars$data_type, function(t, n_equations){
    
    # always include noise
    if(t == 'noise'){
        spike = rep(1, n_equations)
        slab_sd = 0.25
    }else{
        spike_prob = ifelse(t == 'int', 0.5, 0.1)
        spike = rbinom(n = n_equations, size = 1, prob = spike_prob)
        slab_sd = ifelse(t == 'int', 1.5, 2)
    }  
    
    slab = rnorm(n = n_equations, 0, slab_sd)

    data.frame(t(spike * slab))
    }, n_equations))

#rename coeff_samples columns
setnames(coeff_samples, old = names(coeff_samples), new = paste0('X', 1:ncol(coeff_samples)))

# check how many are non-zero
apply(coeff_samples, 2, function(x) sum(x > 0))



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
timestamp = Sys.time()
sample_dir = paste0('/well/nichols/users/bwj567/simulation/samples_',n_equations, '_', n_samples,'_', prop_sampled, '_', timestamp)
dir.create(sample_dir)
dir.create(paste0(sample_dir, '/samples'))

#save other files
write.csv(coeff_samples, file = paste0(sample_dir, "/coefs.csv"), row.names = F)
write.csv(missingness_covars, file = paste0(sample_dir, "/missingness_covars.csv"), row.names = F)



lapply(1:ncol(samples), function(s){
	samp = samples[, s, with = F]
	filename = paste0(sample_dir, '/samples/sample_', str_pad(s, width = 5, side = 'left', pad = '0'), '.csv')
	
	write.csv(samp, file = filename, row.names = F)

	filename
	})

list.files(paste0(sample_dir, '/samples'))
list.files(paste0(sample_dir, '/samples'))


save(ukbdata, file = paste0(sample_dir, '/data.rda'))


