#!/apps/well/R/3.4.3/bin/Rscript       
#$ -N sim_run
#$ -t 1:100    
#$ -tc 50                                                                                                                         
#$ -cwd
#$ -o ./logs                                                                                                                                                                               
#$ -e ./logs   
### request maximum of 72 hours of compute time
#$ -l s_rt=24:00:00
#$ -l h_rt=24:00:00
####$ -l h_vmem=size



library(data.table)
library(stringr)
library(knitr)
library(randomForest)
library(BayesTree)


JobId = as.numeric(Sys.getenv("SGE_TASK_ID"))
#JobId = 3

# load sample
sample = read.csv(sprintf("samples/sample_%05d.csv",JobId))[,1]

# load data
load(file = 'data.rda')
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
epsilon = nrow(data) * 0.0002
calfun = 'raking'
outcome = 'MRI_brain_vol'



###### RUN ONE ITERATION
all_weights = tryCatch(runSim(data = data
        , sample = sample
        , vars = vars
        , vars_rake = vars_rake
        , vars_add = vars_add
        , outcome = 'MRI_brain_vol'
        , pop_weight_col = pop_weight_col
        , verbose = FALSE
        , ntree = 50
        , epsilon = epsilon
        )
, error = function(e) print(e))

print(paste0(Sys.time(), '\t Weighting complete...'))
apply(all_weights, 2, summary)


#write out results
write.csv(all_weights, file = sprintf("results/weights_%05d.csv", JobId), row.names = F)



# resulting object is all_weights

