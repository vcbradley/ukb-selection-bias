#!/apps/well/R/3.4.3/bin/Rscript                                                                                                                                                               
#$ -N sim_prep
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
library(glmnet)

# hard set working directory
setwd('/well/nichols/users/bwj567/simulation')

source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_functions.R')  #also loads lots of packages

plot_dir = '/well/nichols/users/bwj567/mini-project-1/simulation/results/'

## different probs for different types of vars - want to make sure we have a nonlinear var in there
# might want to think about controlling the level of noise

####### SET simulation parameters
n_samples = 5000
n_equations = 1

## Get JobID and create new simulation directory
#JobId = as.numeric(Sys.getenv("JOB_ID"))
sim_id = paste0('sim_', n_equations, '_', n_samples, '_new_v1')
if(!dir.exists(sim_id)){
	dir.create(sim_id)
}
if(!dir.exists(paste0(plot_dir, sim_id))){
    dir.create(paste0(plot_dir, sim_id))
}



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

# set missing APOE levels to 0
ukbdata[is.na(health_apoe_level), health_apoe_level := 0]

ukbdata[, age_sq := age^2]
ukbdata[, bmi_sq := bmi^2]
ukbdata[, health_alc_weekly_total_sq := health_alc_weekly_total^2]

# check NAs
nas = apply(ukbdata, 2, function(x) sum(is.na(x)))
nas[nas > 0]


#### GENERATE Probability of missingness

vars_to_consider = names(ukbdata)[-grep('^MRI|eid|has|assessment|demo_ethnicity_4way|demo_white|demo_educ_highest$', names(ukbdata))]
formula = paste("~-1+(", paste0(vars_to_consider, collapse = " + "), ") ^ 2")
ukbdata_modmat = modmat_all_levs(as.formula(formula), data = ukbdata)


missingness_covars = data.table(var_name = colnames(ukbdata_modmat))
missingness_covars[, data_type := ifelse(grepl('bmi|^age|apoe_level|health_alc_weekly_total', var_name), 'int', 'char')]
missingness_covars[, type := ifelse(grepl('health|bmi', var_name), 'health', 'demo')]

#hard code exceptions
missingness_covars[grepl(':', var_name), type := 'interaction']
missingness_covars[type == 'interaction' & grepl('health|bmi', var_name), type := 'health_interaction']
missingness_covars[var_name == 'age', type := 'age']

#normalize continuous variables
ukbdata_modmat[, which(missingness_covars$data_type == 'int')] <- scale(ukbdata_modmat[, which(missingness_covars$data_type == 'int')])

#check that it looks ok
apply(ukbdata_modmat[, which(missingness_covars$data_type == 'int')], 2, mean)
apply(ukbdata_modmat[, which(missingness_covars$data_type == 'int')], 2, sd)

# drop interactions with NaNs
nans = apply(ukbdata_modmat, 2, function(x) sum(is.nan(x)))
nans[nans > 0]

if(sum(nans > 0) > 0){
    ukbdata_modmat = ukbdata_modmat[, -which(nans > 0)]
    missingness_covars = missingness_covars[-which(nans > 0),]
}

nas = apply(ukbdata_modmat, 2, function(x) sum(is.na(x)))
nas[nas > 0]

png(paste0(plot_dir, sim_id, '/hist_brainvol.png'))
hist(ukbdata$MRI_brain_vol)
dev.off()

ukbdata$MRI_brain_vol_scaled = scale(ukbdata$MRI_brain_vol)



####### make SES index
ukbdata[demo_income_bucket == '01-Under 18k', income_int := 10]
ukbdata[demo_income_bucket == '02-18k to 31k', income_int := 24.5]
ukbdata[demo_income_bucket == '03-31k to 52k', income_int := 41.5]
ukbdata[demo_income_bucket == '04-52k to 100k', income_int := 76]
ukbdata[demo_income_bucket == '05-Over 100k', income_int := 150]
ukbdata[is.na(income_int), income_int := mean(ukbdata$income_int, na.rm = T)]
ukbdata[, income_pctile := min_rank(income_int)/nrow(ukbdata)]


ukbdata[demo_hh_ownrent == '01-Own outright', hh_own_int := 3]
ukbdata[demo_hh_ownrent == '02-Own with mortgage', hh_own_int := 2]
ukbdata[is.na(hh_own_int), hh_own_int := 1]
ukbdata[, hh_own_pctile := min_rank(hh_own_int)/nrow(ukbdata)]

ukbdata[, educ_int := 8-as.numeric(factor(ukbdata$demo_educ_highest_full))]
ukbdata[, educ_pctile := min_rank(educ_int)/nrow(ukbdata)]

ukbdata[, SES_index := (income_pctile + hh_own_pctile + educ_pctile)]
ukbdata[, SES_index := SES_index/max(ukbdata$SES_index)]


ukbdata[,summary(SES_index)]

# use indicators for education instead of highest achieved
mod_brain_vol_SES = summary(lm(MRI_brain_vol_scaled ~ 
    demo_income_bucket + 
    demo_educ_collegeplus + 
    demo_educ_alevels + 
    demo_educ_olevels + 
    demo_educ_cses + 
    demo_educ_vocational + 
    demo_educ_profesh
    , data = ukbdata))


##### GET BRAIN VOL MODEL

# sample data to speed up model fitting
samp = sample.int(n = nrow(ukbdata), size = 5000)

# fit model
mod_brain_vol = cv.glmnet(x = ukbdata_modmat[samp,], y = ukbdata$MRI_brain_vol_scaled[samp])

# pull and scale coefs
mod_brain_vol_coef = data.table(names = rownames(coef(mod_brain_vol, s = 'lambda.1se'))
    , coef = as.numeric(coef(mod_brain_vol, s = 'lambda.1se')))
mod_brain_vol_coef = mod_brain_vol_coef[coef != 0 & !grepl('Intercept', names),]
mod_brain_vol_coef[, coef_scaled := scale(mod_brain_vol_coef$coef)]

# subset matrix to significant coefs
ukbdata_modmat_subset = ukbdata_modmat[, which(colnames(ukbdata_modmat) %in% mod_brain_vol_coef$names)]
dim(ukbdata_modmat_subset)

# check colnames in the right order
colnames(ukbdata_modmat_subset) == mod_brain_vol_coef$names

# calculate linear predictor
coef_mat = matrix(mod_brain_vol_coef$coef)
lp = ukbdata_modmat_subset %*% coef_mat
summary(lp)
lp = scale(lp, center = FALSE, scale = sd(lp)/3)

png(paste0(plot_dir, sim_id, '/hist_lp.png'))
hist(lp)
dev.off()


## calculatte probability of selection as combination of brain volume and SES index
prob = exp(lp)/(1+exp(lp)) * ukbdata$SES_index 
summary(prob)

png(paste0(plot_dir, sim_id, '/prob_hist.png'))
hist(prob)
dev.off()


sim_summary = data.table(eid = ukbdata$eid, ses_index = ukbdata$SES_index, lp = lp[,1], prob = prob[,1])

### CHECK FOR SES
# check that we're substantively modifying SES and volume relationship
beta = coef(lm(MRI_brain_vol_scaled~ukbdata$SES_index, data = ukbdata))[2]

beta_hat = rbindlist(lapply(1:1000, function(x){
    sample = sample.int(nrow(ukbdata), size = 5000, prob = prob)
    mod = summary(lm(MRI_brain_vol_scaled~SES_index, data = ukbdata[sample]))
    b = mod$coef[2, 1]
    se = mod$coef[2, 2]
    data.table(b,se)
    }))
beta_hat$lb = beta_hat$b - 2*beta_hat$se
beta_hat$ub = beta_hat$b + 2*beta_hat$se

beta_hat$diff = (beta < beta_hat$lb | beta > beta_hat$ub)

# prop sig different from actual relationship
mean(beta_hat$diff)


### AND FOR AGE
# check that we're substantively modifying SES and volume relationship
beta = coef(lm(MRI_brain_vol_scaled~ukbdata$age, data = ukbdata))[2]

beta_hat = rbindlist(lapply(1:1000, function(x){
    sample = sample.int(nrow(ukbdata), size = 5000, prob = prob)
    mod = summary(lm(MRI_brain_vol_scaled~age, data = ukbdata[sample]))
    b = mod$coef[2, 1]
    se = mod$coef[2, 2]
    data.table(b,se)
    }))
beta_hat$lb = beta_hat$b - 2*beta_hat$se
beta_hat$ub = beta_hat$b + 2*beta_hat$se

beta_hat$diff = (beta < beta_hat$lb | beta > beta_hat$ub)

# prop sig different from actual relationship
mean(beta_hat$diff)





# ##### GENERATE MISSINGNESS MODEL COEFS
# coeff_samples = rbindlist(lapply(1:nrow(missingness_covars), function(t, n_equations){
# 	data_type = missingness_covars[t,2]
# 	type = missingness_covars[t,3]
    
#     # always include age
#     if(type == 'age'){
#         spike = rep(1, n_equations)
#         slab_sd = 2
#     }else{

#     	# set prob of coef being non-zero based on type and data type
#     	if(type == 'interaction'){
#     		spike_prob = 0.003
#     	} else if (type == 'health_interaction'){
# 			spike_prob = 0
#     	} else if(data_type == 'int'){
#     		spike_prob = 0.75
#     	} else{
#     		spike_prob = 0.25
#     	}

#     	if(!use_health_vars & type == 'health'){
#     		spike_prob = 0
#     	}


#         spike = rbinom(n = n_equations, size = 1, prob = spike_prob)
#         slab_sd = 1.5
#     }  
    
#     slab = rnorm(n = n_equations, 0, slab_sd)

#     data.frame(coef = t(spike * slab))
#     }, n_equations))

# #rename coeff_samples columns
# setnames(coeff_samples, old = names(coeff_samples), new = paste0('X', 1:ncol(coeff_samples)))


# # check that things are behaving well
# cbind(missingness_covars, coeff_samples)[X1 != 0]
# cbind(missingness_covars, coeff_samples)[type == 'interaction' & as.vector(coeff_samples$X1) != 0,]
# cbind(missingness_covars, coeff_samples)[type == 'age' & as.vector(coeff_samples$X1) != 0,]
# cbind(missingness_covars, coeff_samples)[data_type == 'int' & as.vector(coeff_samples$X1) != 0,]

# # check how many are non-zero
# apply(coeff_samples, 2, function(x) sum(x > 0))


#coeff_samples

#save other files
#write.csv(coeff_samples, file = paste0(sim_id, "/coefs.csv"), row.names = F)
#write.csv(missingness_covars, file = paste0(sim_id, "/missingness_covars.csv"), row.names = F)

write.csv(sim_summary, file = paste0(sim_id, "/sim_summary.csv", row.names = F))
write.csv(mod_brain_vol_coef, file = paste0(sim_id, "/mod_brain_vol_coef.csv", row.names = F))

save(ukbdata, file = paste0(sim_id, '/data.rda'))
save(ukbdata_modmat, file = paste0(sim_id, '/data_modmat.rda'))

### make other directories
dir.create(paste0(sim_id, '/results'))
dir.create(paste0(sim_id, '/logs'))
dir.create(paste0(sim_id, '/samples'))


