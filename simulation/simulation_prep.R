###### Code to generate samples from UKB data

library(stringr)
library(knitr)
library(randomForest)
library(BayesTree)


setwd('/well/nichols/users/bwj567')
source('mini-project-1/weighting/weighting_functions.R')  #also loads lots of packages


####### SET simulation parameters

## different probs for different types of vars - want to make sure we have a nonlinear var in there
# might want to think about controlling the level of noise
n_equations = 1
n_samples = 15
prop_sampled = 0.2



###### PREP DATA

# Read in UKB data
ukbdata = fread('data/ukb25120_weighting_img.csv', stringsAsFactors = F)

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
ukbdata_modmat = modmat_all_levs(as.formula(formula), data = ukbdata[1:5000,])


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

length(samples)
dim(samples[[1]])
head(samples[[1]])

#check that we sampled the right number
apply(samples[[1]], 2, sum)

#how'd we do on sampling different peeps?
summary(apply(samples[[1]], 1, sum))

# number of vars used on avg
apply(coeff_samples, 2, function(x) sum(x != 0))

# which coefs did we select
cbind(missingness_covars, coeff_samples[,1])[as.vector(coeff_samples[, 1] != 0),]


sample = ukbdata[1:5000,][samples[,1] == 1,]

# summary = data.frame(pop_mean = apply(ukbdata_modmat, 2, mean)
#     , sample_mean = apply(ukbdata_modmat[samples[,1] == 1, ], 2, mean)
#     )
# summary$diff = summary$sample_mean - summary$pop_mean
# summary




