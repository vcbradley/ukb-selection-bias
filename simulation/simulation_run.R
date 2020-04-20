#!/apps/well/R/3.4.3/bin/Rscript       
#$ -N sim_run
#$ -t 1:500
#$ -tc 50                                                                                                                         
#$ -cwd
#$ -q short.qc
#$ -o ../../logs                                                                                                                                                                               
#$ -e ../../logs   
#$ -l s_rt=24:00:00
#$ -l h_rt=24:00:00

gc()

library(data.table)
library(stringr)
library(knitr)
# options(java.parameters = "-Xmx10g" ) 
# library(bartMachine)


JobId = as.numeric(Sys.getenv("SGE_TASK_ID"))
#JobId = 10

dir = getwd()
cat(dir)

prop = str_split(dir, '/')[[1]]
prop = prop[grepl('prop', prop)]
prop = gsub('prop_', '', prop)

sim_dir = str_split(dir, '/samples/')[[1]][1]

# load sample
sample = read.csv(sprintf("sample_%05d.csv",JobId))[,1]

# load data
load(file = paste0(sim_dir,'/data.rda'))
data = ukbdata#limit for now
rm(ukbdata)

load(file = paste0(sim_dir,'/data_modmat.rda'))
modmat = ukbdata_modmat
rm(ukbdata_modmat_scaled)


# run simulation draft
print(paste0(Sys.time(), '\t Weighting starting...'))

source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_functions.R')  #also loads lots of packages
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_poststrat.R')
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_calibration.R')
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_raking.R')
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_lassorake.R')
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_logit.R')
source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_bart.R')
source('/well/nichols/users/bwj567/mini-project-1/weighting/run_sim_function.R')




####### DO WEIGHTING  #######
vars = c('demo_sex'
, 'demo_age_bucket'
, 'demo_ethnicity_full'
, 'demo_empl_employed'
, 'demo_empl_retired'
, 'demo_empl_homemaker'
, 'demo_empl_disabled'
, 'demo_empl_unemployed'
, 'demo_empl_volunteer'
, 'demo_empl_student'
, 'demo_occupation'
, 'demo_educ_highest'
, 'demo_income_bucket'
, 'demo_year_immigrated'
, 'demo_hh_size'
, 'demo_hh_ownrent'
, 'demo_hh_accom_type'
, 'health_smoking_status'
, 'health_alc_freq'
, 'health_bp_cat'
, 'health_bp_high_ever'
, 'health_bp_meds_current'
, 'health_diabetes'
, 'health_apoe_phenotype')

# apply(data[,which(names(data) %in% vars_to_consider[grepl('health', vars_to_consider)]), with = F], 2, )

# lapply(vars[grepl('health', vars_to_consider)], function(x){
#         print(x)
#         cbind(x, data[, .(prop = .N/nrow(data)), by = get(x)])
#         })

lapply(c(vars, 'demo_white'), function(v){
        cbind(v, data[,.N/nrow(data), get(v)][order(get)])
        })
# get rid of really small levels
data[, demo_white_rollup := ifelse(demo_white == '99-DNK/Refused', '02-Non-white', demo_white)]
data[, demo_hh_accom_type_rollup := ifelse(demo_hh_accom_type == '99-DNK/Refused', '02-Flat, apartment or temp', demo_hh_accom_type)]
data[, demo_hh_size_rollup := ifelse(demo_hh_size == '99-DNK/Refused', '2', demo_hh_size)]
data[, demo_occupation_rollup := ifelse(unlist(lapply(data$demo_occupation, function(x) as.numeric(unlist(strsplit(x, '-'))[1]))) %in% c(1,2,3,4,10), demo_occupation, '11-Other/Refused')]
data[, demo_hh_ownrent_rollup := ifelse(unlist(lapply(data$demo_hh_ownrent, function(x) as.numeric(unlist(strsplit(x, '-'))[1]))) %in% c(1,2), demo_hh_ownrent, '11-Other/Refused')]

data[, health_smoking_status_rollup := ifelse(health_smoking_status == '99-DNK/Refused', '03-Never', health_smoking_status)]
data[, health_alc_freq_rollup := ifelse(health_alc_freq == '99-DNK/Refused', '06-Never', health_alc_freq)]
data[, health_bp_high_ever_rollup := ifelse(health_bp_high_ever == '99-DNK/Refused', '02-No', health_bp_high_ever)]
data[, health_bp_meds_current_rollup := ifelse(health_bp_meds_current == '99-DNK/Refused', '02-No', health_bp_meds_current)]

#exclude vars from raking and calibration (pop % < 1)
vars_rake = vars[-which(vars %in% c('demo_ethnicity_full','demo_year_immigrated', 'demo_empl_unemployed', 'demo_empl_student', 'demo_empl_disabled'))]
vars_rollup = names(data)[grep('rollup', names(data))]
vars_rake = vars_rake[-which(vars_rake %in% gsub('_rollup','',vars_rollup))]
vars_rake =c(vars_rake, vars_rollup)

# and add in rolled-up ethnicity
#vars_rake = c(vars_rake, 'demo_white')

vars_add = c('age', 'age_sq', 'bmi', 'bmi_sq', 'health_alc_weekly_total', 'health_alc_weekly_total_sq')
pop_weight_col = NULL

epsilon = nrow(data) * 0.0003
calfun = 'raking'
outcome = 'MRI_brain_vol'

#lapply(vars_rake, getPopframe, data = data)


###### RUN ONE ITERATION
all_weights = tryCatch(runSim(data = data
        , sample = sample
        , vars = vars
        , vars_add = vars_add
        , vars_rake = vars_rake
        , outcome = outcome
        , pop_weight_col = pop_weight_col
        , verbose = FALSE
        , ntree = 50
        , epsilon = epsilon
        , modmat = modmat
        )
, error = function(e) print(e))

print(paste0(Sys.time(), '\t Weighting complete...'))
apply(all_weights[[1]], 2, summary)


#write out results
results_dir = paste0(sim_dir,'/results/prop_', prop)
if(!dir.exists(results_dir)){
	dir.create(results_dir)
}

# add sim num
all_weights[[1]][, sim_num := JobId]
write.csv(all_weights[[1]], file = paste0(results_dir, sprintf("/weights_%05d.csv", JobId)), row.names = F)


if(!dir.exists(paste0(results_dir, '/vars'))){
	dir.create(paste0(results_dir, '/vars'))
}
if(!dir.exists(paste0(results_dir, '/timing'))){
	dir.create(paste0(results_dir, '/timing'))
}

write.csv(all_weights[['strat_vars']], file = paste0(results_dir, sprintf("/vars/strat_vars_%05d.csv", JobId)), row.names = F)
write.csv(all_weights[['lassorake_vars']], file = paste0(results_dir, sprintf("/vars/lassorake_vars_%05d.csv", JobId)), row.names = F)
write.csv(all_weights[['logit_vars']], file = paste0(results_dir, sprintf("/vars/logit_vars_%05d.csv", JobId)), row.names = F)
write.csv(all_weights[['bart_vars']], file = paste0(results_dir, sprintf("/vars/bart_vars_%05d.csv", JobId)), row.names = F)
write.csv(all_weights[['timing']], file = paste0(results_dir, sprintf("/timing/timing_%05d.csv", JobId)), row.names = F)

# resulting object is all_weights

