#!/apps/well/R/3.4.3/bin/Rscript       
#$ -N sim_run
#$ -t 1:10
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
#JobId = 1

dir = getwd()

prop = str_split(dir, '/')[[1]]
prop = prop[grepl('prop', prop)]
prop = gsub('prop_', '', prop)

sim_dir = str_split(dir, '/samples/')[[1]][1]

# load sample
sample = read.csv(sprintf("sample_%05d.csv",JobId))[,1]

# load data
load(file = paste0(sim_dir,'/data.rda'))
data = ukbdata[1:length(sample),]#limit for now
rm(ukbdata)


# run simulation draft
print(paste0(Sys.time(), '\t Weighting starting...'))

source('/well/nichols/users/bwj567/mini-project-1/weighting/weighting_functions.R')  #also loads lots of packages



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
epsilon = nrow(data) * 0.0001
calfun = 'raking'
outcome = 'MRI_brain_vol'



###### RUN ONE ITERATION
all_weights = tryCatch(runSim(data = data
        , sample = sample
        , vars = vars
        , vars_add = vars_add
        , outcome = 'MRI_brain_vol'
        , pop_weight_col = pop_weight_col
        , verbose = FALSE
        , ntree = 25
        , epsilon = epsilon
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

