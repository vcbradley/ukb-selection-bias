#### SIMULATION

library(stringr)
library(knitr)
library(randomForest)
library(BayesTree)


setwd('/well/nichols/users/bwj567')
source('mini-project-1/weighting/weighting_functions.R')  #also loads lots of packages

# Read in UKB data
ukbdata = fread('data/ukb25120_weighting_img.csv', stringsAsFactors = F)

setnames(ukbdata
    , old = names(ukbdata)[grepl('^img', names(ukbdata))]
    , new = gsub('^img_','',names(ukbdata)[grepl('^img', names(ukbdata))])
    )

names(ukbdata)

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





#### Probability of missingness

### TO DO: ADD IN INTERACTIONS
vars_to_consider = names(ukbdata)[-grep('^MRI|eid|has|assessment|demo_ethnicity_4way|demo_white|demo_educ_highest$', names(ukbdata))]
formula = paste("~-1+(", paste0(vars_to_consider, collapse = " + "), ")")
ukbdata_modmat = modmat_all_levs(as.formula(formula), data = ukbdata[1:5000,])

colnames(ukbdata_modmat)


#summarize vars
varsum = apply(ukbdata[, vars_to_consider, with = F], 2, function(x) {
    if(length(unique(x)) < 20) {
        paste0(sort(unique(x)), collapse = ', ')
    } else {
        paste0(x[1:5], collapse = ', ')
    }
    })
head(varsum)
varsum = data.frame(variable = names(varsum), levels = varsum)
rownames(varsum) = NULL
kable(varsum, format = 'latex')



missingness_covars = data.table(var_name = colnames(ukbdata_modmat))
missingness_covars[, data_type := ifelse(grepl('bmi|^age|health_alc_weekly_total', var_name), 'int', 'char')]
missingness_covars[var_name == 'noise', data_type := 'noise']
missingness_covars[, type := ifelse(grepl('health|bmi', var_name), 'health', 'demo')]


#normalize continuous variables
ukbdata_modmat[, which(missingness_covars$data_type == 'int')] <- scale(ukbdata_modmat[, which(missingness_covars$data_type == 'int')])

#check that it looks ok
apply(ukbdata_modmat[, which(missingness_covars$data_type == 'int')], 2, mean)
apply(ukbdata_modmat[, which(missingness_covars$data_type == 'int')], 2, sd)




## SPIKE AND SLAB PRIOR
## different probs for different types of vars - want to make sure we have a nonlinear var in there
# might want to think about controlling the level of noise
n_equations = 1
n_samples = 15
prop_sampled = 0.2

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

apply(coeff_samples, 2, function(x) sum(x > 0))

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

cbind(missingness_covars, coeff_samples[,1])[as.vector(coeff_samples[, 1] != 0),]


sample = ukbdata[1:5000,][samples[,1] == 1,]

summary = data.frame(pop_mean = apply(ukbdata_modmat, 2, mean)
    , sample_mean = apply(ukbdata_modmat[samples[,1] == 1, ], 2, mean)
    )
summary$diff = summary$sample_mean - summary$pop_mean
summary



####### DO WEIGHTING  #######
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
vars_add = c('age', 'age_sq')
vars_rake = c('demo_sex', 'demo_ethnicity_4way', 'demo_age_bucket')
pop_weight_col = NULL
epsilon = 1
calfun = 'raking'
outcome = 'MRI_brain_vol'

data = cbind(ukbdata[1:5000,], selected = samples[,1])


###### RUN ONE ITERATION
all_weights = runSim(data = data
        , vars = vars
        , vars_rake = vars_rake
        , vars_add = vars_add
        , selected_ind = 'selected'
        , outcome = 'MRI_brain_vol'
        , pop_weight_col = pop_weight_col)

apply(all_weights, 2, summary)


#### EVALUATION
rbindlist(lapply(c('has_t1_MRI', vars), function(v){
    pop = ukbdata[1:5000, .(
        pop_count = .N
        , pop_prop = .N/nrow(ukbdata[1:5000])
        , pop_brainvol = sum(as.numeric(MRI_brain_vol), na.rm = T)/.N
        ), by = v]
    samp = raked_data[, .(
        samp_count = .N
        , weghted_count = sum(weight)
        , weighted_prop = sum(weight)/sum(raked_data$weight)
        , samp_brainvol = sum(as.numeric(MRI_brain_vol), na.rm = T)/.N
        , weighted_brainvol = sum(as.numeric(MRI_brain_vol) * weight, na.rm = T)/sum(weight)
        ), by = v]
    cbind(v, merge(pop, samp, all = T, by = v))
    }))









