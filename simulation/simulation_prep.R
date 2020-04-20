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
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_poststrat.R')
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_calibration.R')
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_raking.R')
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_lassorake.R')
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_logit.R')
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_bart.R')
source('/well/nichols/users/bwj567/mini-project-1/weighting/run_sim_function.R')

plot_dir = '/well/nichols/users/bwj567/mini-project-1/simulation/results/'

## different probs for different types of vars - want to make sure we have a nonlinear var in there
# might want to think about controlling the level of noise


## Get JobID and create new simulation directory
#JobId = as.numeric(Sys.getenv("JOB_ID"))
sim_id = paste0('sim_new_v2')
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

#mean impute missing BMIs and alc consumption by age and gender
ukbdata[, bmi_orig := bmi]
bmi_imp = ukbdata[, .( 
    bmi_imp = mean(bmi, na.rm = T)
    ), .(demo_sex, demo_age_bucket)][order(demo_sex, demo_age_bucket)]

ukbdata[bmi_imp, on = c('demo_sex', 'demo_age_bucket'), bmi := ifelse(is.na(bmi_orig), i.bmi_imp, bmi_orig)]

summary(ukbdata[, .(bmi_orig, bmi)])


ukbdata[, health_alc_weekly_total_orig := health_alc_weekly_total]
alc_imp = ukbdata[health_alc_weekly_total > 0 & !is.na(health_alc_weekly_total), .( 
    health_alc_weekly_total_imp = mean(health_alc_weekly_total, na.rm = T)
    ), .(demo_sex, demo_age_bucket)][order(demo_sex, demo_age_bucket)]

ukbdata[alc_imp, on = c('demo_sex', 'demo_age_bucket'), health_alc_weekly_total := ifelse(is.na(health_alc_weekly_total_orig) | health_alc_weekly_total_orig < 0, i.health_alc_weekly_total_imp, health_alc_weekly_total_orig)]

summary(ukbdata[, .(health_alc_weekly_total_orig, health_alc_weekly_total)])


# make missing vals DNK
ukbdata[is.na(demo_hh_size), demo_hh_size := '99-DNK/Refused']

# set missing APOE levels to 0
ukbdata[is.na(health_apoe_level), health_apoe_level := 0]

ukbdata[, age_sq := age^2]
ukbdata[, age_cb := age^3]
ukbdata[, bmi_sq := bmi^2]
ukbdata[, health_alc_weekly_total_sq := health_alc_weekly_total^2]

# check NAs
nas = apply(ukbdata, 2, function(x) sum(is.na(x)))
nas[nas > 0]

#cat(paste0("'",names(ukbdata)[-grep('^MRI|eid|has|assessment|demo_ethnicity_4way|demo_white|demo_educ_highest$|age|health_cogfn_bucket|apoe_level|orig$', names(ukbdata))],"'"), sep = ',\n')
vars_cat = c(
    'demo_sex',
    'demo_ethnicity_full',
    'demo_empl_employed',
    'demo_empl_retired',
    'demo_empl_homemaker',
    'demo_empl_disabled',
    'demo_empl_unemployed',
    'demo_empl_volunteer',
    'demo_empl_student',
    'demo_occupation',
    'demo_educ_collegeplus',
    'demo_educ_alevels',
    'demo_educ_olevels',
    'demo_educ_cses',
    'demo_educ_vocational',
    'demo_educ_profesh',
    'demo_educ_highest_full',
    'demo_income_bucket',
    'demo_year_immigrated',
    'demo_hh_size',
    'demo_hh_ownrent',
    'demo_hh_accom_type',
    'health_smoking_status',
    'health_smoking_current',
    'health_BMI_bucket',
    'health_alc_freq',
    'health_bp_cat',
    'health_bp_high_ever',
    'health_bp_meds_current',
    'health_diabetes',
    'health_apoe_phenotype'
    )

vars_cont = c(
    'age',
    'age_sq',
    'bmi',
    'bmi_sq',
    'health_alc_weekly_total',
    'health_alc_weekly_total_sq'
    )

#### scale brain vol to make easier to work with, cv.glmnet will scale for you
ukbdata$MRI_brain_vol_scaled = scale(ukbdata$MRI_brain_vol)

for(v in vars_cont){
    ukbdata[, paste0(v,'_scaled')] <- scale(ukbdata[, get(v)])
}


###### MAKE MODEL MATRIX
formula = paste("~-1+(", paste0(c(vars_cat, vars_cont), collapse = " + "), ") ^ 2")
ukbdata_modmat = modmat_all_levs(as.formula(formula), data = ukbdata)

#scale modmat
#ukbdata_modmat_scaled = apply(ukbdata_modmat, 2, scale)

# drop interactions with NaNs
nans = apply(ukbdata_modmat, 2, function(x) sum(is.nan(x)))
nans[nans > 0]

if(sum(nans > 0) > 0){
    ukbdata_modmat = ukbdata_modmat[, -which(nans > 0)]
    #missingness_covars = missingness_covars[-which(nans > 0),]
}

nas = apply(ukbdata_modmat, 2, function(x) sum(is.na(x)))
nas[nas > 0]





#### GENERATE Probability of missingness
##### GET BRAIN VOL MODEL
set.seed(1234)

# sample data to speed up model fitting
samp = sample.int(n = nrow(ukbdata), size = 10000)
#samp = 1:nrow(ukbdata_modmat)

# fit model
drop_age_col = grep('^age|:age$|:age_sq$', colnames(ukbdata_modmat))
mod_brain_vol = cv.glmnet(x = ukbdata_modmat[samp,-drop_age_col], y = ukbdata$MRI_brain_vol_scaled[samp])

# pull coefs
mod_brain_vol_coef = data.table(names = rownames(coef(mod_brain_vol, s = 'lambda.min'))
    , coef = as.numeric(coef(mod_brain_vol, s = 'lambda.min')))
mod_brain_vol_coef = mod_brain_vol_coef[coef != 0 & !grepl('Intercept', names),]
mod_brain_vol_coef


# subset matrix to significant coefs
ukbdata_modmat_subset = ukbdata_modmat[, which(colnames(ukbdata_modmat) %in% mod_brain_vol_coef$names)]
dim(ukbdata_modmat_subset)


temp = data.table(cbind(ukbdata_modmat_subset, MRI_brain_vol_scaled = ukbdata$MRI_brain_vol_scaled))
summary(lm(MRI_brain_vol_scaled~., data = temp))


# calculate linear predictor
coef_mat = matrix(mod_brain_vol_coef$coef)
lp = ukbdata_modmat_subset %*% coef_mat
summary(lp)

summary(lm(ukbdata$MRI_brain_vol_scaled ~ lp))

## Combine age and lp and multiply by a factor to make selection probs more extreme
lp_comb = 2.5 * (scale(lp) - scale(ukbdata$age_sq))
summary(lp_comb)

prob = exp(lp_comb)/(1+exp(lp_comb))
summary(prob)

# check that prob is correlated with brain vol
summary(lm(ukbdata$MRI_brain_vol_scaled ~ prob))


####### CHECK THAT WE"VE IMPACTED AGE AND BRAIN VOL ASSOC
# check that we're substantively modifying SES and volume relationship
mod_pop = summary(lm(MRI_brain_vol_scaled~age_sq
    , data = ukbdata))
beta = mod_pop$coef[2,1]
beta2 = mod_pop$coef[3,1]

beta_hat = rbindlist(lapply(1:1000, function(x){
    sample = sample.int(nrow(ukbdata), size = 500, prob = prob)
    mod = summary(lm(MRI_brain_vol_scaled~age_sq
        , data = ukbdata[sample]))
    b = mod$coef[2, 1]
    se = mod$coef[2, 2]
    if(nrow(mod$coef)> 2){
        b2 = mod$coef[3, 1]
        se2 = mod$coef[3, 2]
        r = data.table(b,se, b2, se2)
    }else{
        data.table(b,se)
    }

    
    }))

mean(dnorm(abs(beta_hat$b - beta)/beta_hat$se) < 0.05)
mean(dnorm(abs(beta_hat$b2 - beta2)/beta_hat$se2) < 0.05)

min(1 - mean(beta < beta_hat$b), mean(beta < beta_hat$b))
min(1 - mean(beta2 < beta_hat$b2), mean(beta2 < beta_hat$b2))





####### SAVE RESULTS
sim_summary = data.table(eid = ukbdata$eid
    , lp_ses = lp[,1]
    , age_sq = ukbdata$age_sq
    , lp_full = lp_comb[,1]
    , prob = prob[,1]
    )


write.csv(sim_summary, file = paste0(sim_id, "/sim_summary.csv"), row.names = F)
write.csv(mod_brain_vol_coef, file = paste0(sim_id, "/mod_brain_vol_coef.csv"), row.names = F)

save(ukbdata, file = paste0(sim_id, '/data.rda'))
save(ukbdata_modmat, file = paste0(sim_id, '/data_modmat.rda'))

### make other directories
dir.create(paste0(sim_id, '/results'))
dir.create(paste0(sim_id, '/logs'))
dir.create(paste0(sim_id, '/samples'))


