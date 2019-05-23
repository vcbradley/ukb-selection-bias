#### Code to RUN simulations

library(stringr)
library(knitr)
library(randomForest)
library(BayesTree)

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
epsilon = 1
calfun = 'raking'
outcome = 'MRI_brain_vol'




###### RUN ONE ITERATION
all_weights = runSim(data = data
        , sample = sample
        , vars = vars
        , vars_rake = vars_rake
        , vars_add = vars_add
        , outcome = 'MRI_brain_vol'
        , pop_weight_col = pop_weight_col)

apply(all_weights, 2, summary)


# #### EVALUATION
# rbindlist(lapply(c('has_t1_MRI', vars), function(v){
#     pop = ukbdata[1:5000, .(
#         pop_count = .N
#         , pop_prop = .N/nrow(ukbdata[1:5000])
#         , pop_brainvol = sum(as.numeric(MRI_brain_vol), na.rm = T)/.N
#         ), by = v]
#     samp = raked_data[, .(
#         samp_count = .N
#         , weghted_count = sum(weight)
#         , weighted_prop = sum(weight)/sum(raked_data$weight)
#         , samp_brainvol = sum(as.numeric(MRI_brain_vol), na.rm = T)/.N
#         , weighted_brainvol = sum(as.numeric(MRI_brain_vol) * weight, na.rm = T)/sum(weight)
#         ), by = v]
#     cbind(v, merge(pop, samp, all = T, by = v))
#     }))









