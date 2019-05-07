#### SIMULATION

library(stringr)


setwd('/well/nichols/users/bwj567')
source('mini-project-1/weighting/weighting_functions.R')  #also loads lots of packages

# Read in UKB data
ukbdata = fread('data/ukb25120_weighting_img.csv')
ukbdata <- as_tibble(ukbdata)

# drop img prefix
ukbdata <- ukbdata %>% rename_at(vars(starts_with("img_")), funs(str_replace(., "img_", "")))

ukbdata = data.table(ukbdata, stringsAsFactors = F)

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
vars_to_consider = names(ukbdata)[-grep('^MRI|eid|has|assessment|demo_ethnicity_4way|demo_white', names(ukbdata))]
formula = paste("~-1+(", paste0(vars_to_consider, collapse = " + "), ")")
ukbdata_modmat = modmat_all_levs(as.formula(formula), data = ukbdata[1:5000,])

colnames(ukbdata_modmat)


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
n_samples = 15
prop_sampled = 0.2

coeff_samples = rbindlist(lapply(missingness_covars$data_type, function(t, n_samples){
    
    # always include noise
    if(t == 'noise'){
        spike = rep(1, n_samples)
    }else{
        spike_prob = ifelse(t == 'int', 0.5, 0.1)
        spike = rbinom(n = n_samples, size = 1, prob = spike_prob)
    }  

    slab_sd = ifelse(t == 'int', 1.5, 2)
    slab = rnorm(n = n_samples, 0, slab_sd)

    data.frame(t(spike * slab))
    }, n_samples))

apply(coeff_samples, 2, function(x) sum(x > 0))

lps = ukbdata_modmat %*% as.matrix(coeff_samples)
probs = exp(lps)/(1+exp(lps))


samples = apply(probs, 2, function(p, prop_sampled){
    sampled_int = sample.int(n = length(p), size = length(p) * prop_sampled, prob = p)
    sampled = seq(from = 1, to = length(p), by = 1)
    sampled = ifelse(sampled %in% sampled_int, 1, 0)
    sampled
    }, prop_sampled)

dim(samples)
head(samples)

#check that we sampled the right number
apply(samples, 2, sum)

#how'd we do on sampling different peeps?
apply(samples, 1, sum)


sample = ukbdata[1:5000,][samples[,1] == 1,]

summary = data.frame(pop_mean = apply(ukbdata_modmat, 2, mean)
    , sample_mean = apply(ukbdata_modmat[samples[,1] == 1, ], 2, mean)
    )
summary$diff = summary$sample_mean - summary$pop_mean
summary



##### DO WEIGHTING
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

raked_data = doRaking(svydata = sample
    , popdata = ukbdata
    , vars = vars
    )


# select vars with a regression?

cv.glmnet(ukbdata_modmat[, -which(colnames(ukbdata_modmat) %in% c('noise'))])


strat_data = doPostStrat(svydata
	, popdata
	, vars)





