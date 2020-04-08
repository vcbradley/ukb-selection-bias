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
ukbdata[, age_sq_scaled := age_sq / max(ukbdata$age_sq)]
ukbdata[, age_cb := age^3]
ukbdata[, bmi_sq := bmi^2]
ukbdata[, health_alc_weekly_total_sq := health_alc_weekly_total^2]

# check NAs
nas = apply(ukbdata, 2, function(x) sum(is.na(x)))
nas[nas > 0]


#### GENERATE Probability of missingness

vars_to_consider = names(ukbdata)[-grep('^MRI|eid|has|assessment|demo_ethnicity_4way|demo_white|demo_educ_highest$|age|health_cogfn_bucket|apoe_level|orig$', names(ukbdata))]
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



# ####### make SES index
# ukbdata[demo_income_bucket == '01-Under 18k', income_int := 10]
# ukbdata[demo_income_bucket == '02-18k to 31k', income_int := 24.5]
# ukbdata[demo_income_bucket == '03-31k to 52k', income_int := 41.5]
# ukbdata[demo_income_bucket == '04-52k to 100k', income_int := 76]
# ukbdata[demo_income_bucket == '05-Over 100k', income_int := 150]
# ukbdata[is.na(income_int), income_int := mean(ukbdata$income_int, na.rm = T)]
# ukbdata[, income_pctile := min_rank(income_int)/nrow(ukbdata)]


# ukbdata[demo_hh_ownrent == '01-Own outright', hh_own_int := 3]
# ukbdata[demo_hh_ownrent == '02-Own with mortgage', hh_own_int := 2]
# ukbdata[is.na(hh_own_int), hh_own_int := 1]
# ukbdata[, hh_own_pctile := min_rank(hh_own_int)/nrow(ukbdata)]

# ukbdata[, educ_int := 8-as.numeric(factor(ukbdata$demo_educ_highest_full))]
# ukbdata[, educ_pctile := min_rank(educ_int)/nrow(ukbdata)]

# ukbdata[, SES_index := (income_pctile + hh_own_pctile + educ_pctile)]
# ukbdata[, SES_index := SES_index/max(ukbdata$SES_index)]


# ukbdata[,summary(SES_index)]

# # # use indicators for education instead of highest achieved
# mod_brain_vol_SES = lm(MRI_brain_vol_scaled ~ demo_sex +
#     demo_income_bucket + 
#     demo_educ_collegeplus + 
#     demo_educ_alevels + 
#     demo_educ_olevels + 
#     demo_educ_cses + 
#     demo_educ_vocational + 
#     demo_educ_profesh +
#     demo_hh_size +
#     as.numeric(demo_hh_ownrent == '01-Own outright')+ 
#     as.numeric(demo_hh_ownrent == '02-Own with mortgage')+ 
#     as.numeric(demo_year_immigrated == '01-Before 1941') +
#     as.numeric(demo_year_immigrated == '02-1941 to 1950')
#     , data = ukbdata)
# summary(mod_brain_vol_SES)

# lp = predict(mod_brain_vol_SES)


##### GET BRAIN VOL MODEL
set.seed(1234)

# sample data to speed up model fitting
samp = sample.int(n = nrow(ukbdata), size = 10000)
#samp = 1:nrow(ukbdata_modmat)

# fit model
mod_brain_vol = cv.glmnet(x = ukbdata_modmat[samp,], y = ukbdata$MRI_brain_vol_scaled[samp])


# pull and scale coefs
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

png(paste0(plot_dir, sim_id, '/hist_lp.png'))
hist(lp)
dev.off()


summary(lm(lp ~ ukbdata$age_sq))

summary(lm(lp ~ ukbdata$MRI_brain_vol_scaled))


## calculate probability of selection as combination of brain volume and SES index
lp_comb = (scale(lp) - 2*(ukbdata$age_sq_scaled))
prob = exp(lp_comb)/(1+exp(lp_comb))
summary(prob)

# summary(lm(ukbdata$MRI_brain_vol_scaled ~ lp))

# summary(lm(ukbdata$MRI_brain_vol_scaled ~ ukbdata$age_sq))

# summary(lm(ukbdata$MRI_brain_vol_scaled ~ prob))


png(paste0(plot_dir, sim_id, '/prob_hist.png'))
hist(prob)
dev.off()


age_beta = summary(lm(ukbdata$MRI_brain_vol_scaled ~ ukbdata$age_sq_scaled))$coef[2,1]

props = seq(0,1,0.01)
age_hat = rbindlist(lapply(props, function(p){
    lp_comb = 10 * ((1 - p) * scale(lp) - p *(ukbdata$age_sq_scaled))
    prob = exp(lp_comb)/(1+exp(lp_comb))

    samp = sample.int(nrow(ukbdata), size = 1000, prob = prob)
    
    mod = summary(lm(ukbdata$MRI_brain_vol_scaled[samp] ~ ukbdata$age_sq_scaled[samp] + prob[samp]))
    age_beta_hat = mod$coef[2,1]
    age_se_hat = mod$coef[2,2]

    data.frame(cbind(age_beta_hat, age_se_hat))
    }))
(age_hat$age_beta_hat - age_beta)/ age_hat$age_se_hat

p_final = props[which.max((age_hat$age_beta_hat - age_beta)/ age_hat$age_se_hat)]
p_final = 0.6


lp_comb = 10 *(((1 - p_final) * scale(lp) - p_final *(ukbdata$age_sq_scaled)))
summary(lp_comb)
prob = exp(lp_comb)/(1+exp(lp_comb))
summary(prob)

sim_summary = data.table(eid = ukbdata$eid
    , lp_ses = lp[,1]
    , age_sq_scaled = (ukbdata$age_sq / max(ukbdata$age_sq))
    , lp_full = lp_comb[,1]
    , prob = prob[,1]
    )

### CHECK FOR SES
# # check that we're substantively modifying SES and volume relationship
# beta = coef(lm(MRI_brain_vol_scaled~ukbdata$SES_index, data = ukbdata))[2]

# beta_hat = rbindlist(lapply(1:1000, function(x){
#     sample = sample.int(nrow(ukbdata), size = 300, prob = prob)
#     mod = summary(lm(MRI_brain_vol_scaled~SES_index, data = ukbdata[sample]))
#     b = mod$coef[2, 1]
#     se = mod$coef[2, 2]
#     data.table(b,se)
#     }))
# beta_hat$lb = beta_hat$b - 2*beta_hat$se
# beta_hat$ub = beta_hat$b + 2*beta_hat$se

# beta_hat$diff = (beta < beta_hat$lb | beta > beta_hat$ub)

# # prop sig different from actual relationship
# mean(beta_hat$diff)


### AND FOR AGE
# check that we're substantively modifying SES and volume relationship
mod_pop = summary(lm(MRI_brain_vol_scaled~age_sq_scaled
    , data = ukbdata))
beta = mod_pop$coef[2,1]
beta2 = mod_pop$coef[3,1]

beta_hat = rbindlist(lapply(1:1000, function(x){
    sample = sample.int(nrow(ukbdata), size = 500, prob = prob)
    mod = summary(lm(MRI_brain_vol_scaled~age_sq_scaled
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



write.csv(sim_summary, file = paste0(sim_id, "/sim_summary.csv"), row.names = F)
write.csv(mod_brain_vol_coef, file = paste0(sim_id, "/mod_brain_vol_coef.csv"), row.names = F)

save(ukbdata, file = paste0(sim_id, '/data.rda'))
save(ukbdata_modmat, file = paste0(sim_id, '/data_modmat.rda'))

### make other directories
dir.create(paste0(sim_id, '/results'))
dir.create(paste0(sim_id, '/logs'))
dir.create(paste0(sim_id, '/samples'))


