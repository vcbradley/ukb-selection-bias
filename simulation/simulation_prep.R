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

# hard set working directory
setwd('/well/nichols/users/bwj567/simulation')

source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_functions.R')  #also loads lots of packages


## different probs for different types of vars - want to make sure we have a nonlinear var in there
# might want to think about controlling the level of noise

####### SET simulation parameters
n_equations = 1
n_samples = 5000


## Get JobID and create new simulation directory
#JobId = as.numeric(Sys.getenv("JOB_ID"))
sim_id = paste0('sim_', n_equations, '_', n_samples, '_', JobId)
dir.create(sim_id)



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
        spike_prob = ifelse(t == 'int', 0.75, 0.25)
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



#save other files
write.csv(coeff_samples, file = paste0(sim_id, "/coefs.csv"), row.names = F)
write.csv(missingness_covars, file = paste0(sim_id, "/missingness_covars.csv"), row.names = F)


save(ukbdata, file = paste0(sim_id, '/data.rda'))
save(ukbdata_modmat, file = paste0(sim_id, '/data_modmat.rda'))

### make other directories
dir.create(paste0(sim_id, '/results'))
dir.create(paste0(sim_id, '/logs'))
dir.create(paste0(sim_id, '/samples'))


