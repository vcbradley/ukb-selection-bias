#########################
# Weighted UKB Analysis #
#########################
rm(list = ls())

library(ggplot2)
library(data.table)
library(knitr)
library(kableExtra)

setwd('~/github/mini-project-1/weighting')
list.files()
load('ukb_weighted_hse16_summary.rda')



#brainvol_age_melted = melt(brainvol_age, id.vars = c('age', 'health_apoe_phenotype','demo_sex','demo_income_bucket', 'brain_vol'), measure.vars = names(brainvol_age)[grepl('_brain_vol', names(brainvol_age))])
brainvol_age_melted = melt(brainvol_age, id.vars = c('age', 'brain_vol'), measure.vars = names(brainvol_age)[grepl('_brain_vol', names(brainvol_age))])
brainvol_age_melted[, variable := gsub('_brain_vol', '', variable)]


ggplot(brainvol_age_melted, aes(x = age, y = brain_vol, color = variable), alpha = 0.2) + 
  geom_point()


ggplot(weight_summary[var == 'demo_age_bucket'], aes(x = level)) + 
  geom_line(aes(y = rake_brainvol)) +
  geom_line(aes(y = bart_brainvol)) +
  geom_line(aes(y = strat_brainvol))

weight_summary[var == '03-Age Bucket', grepl('_prop', names(weight_summary)), with = F, by = level]
weight_summary[var == '02-Sex', grepl('_prop', names(weight_summary)), with = F, by = level]
# weight_summary[var == 'demo_income_bucket', grepl('_prop', names(weight_summary)), with = F, by = level]
# weight_summary[var == 'demo_hh_ownrent', grepl('_prop', names(weight_summary)), with = F, by = level]
# 
# weight_summary[var == 'health_smoking_status', grepl('_prop', names(weight_summary)), with = F, by = level]
# weight_summary[var == 'health_bp_meds_current', grepl('_prop', names(weight_summary)), with = F, by = level]
# weight_summary[var == 'health_diabetes', grepl('_prop', names(weight_summary)), with = F, by = level]
# weight_summary[var == 'health_BMI_bucket', grepl('_prop', names(weight_summary)), with = F]
# weight_summary[var == 'health_apoe_phenotype', grepl('_prop', names(weight_summary)), with = F]


outcome_melted = melt(weight_summary
                      , id.vars = c('var', 'level')
                      , measure.vars = names(weight_summary)[grepl('_brainvol', names(weight_summary))]
                      , variable.name = 'method'
                      , value.name = 'brainvol')
outcome_melted[, method := gsub('_brainvol', '', method)]

prop_melted = melt(weight_summary
                      , id.vars = c('var', 'level')
                      , measure.vars = names(weight_summary)[grepl('_prop', names(weight_summary))]
                      , variable.name = 'method'
                      , value.name = 'prop')
prop_melted[, method := gsub('_prop', '', method)]

ukb_weighted_melted = merge(outcome_melted, prop_melted, by = c('var','level', 'method'), all = T)

max(outcome_melted[, .N, .(var, level, method)]$N)

health_outcomes = c('16-ApoE Phenotype'
                    ,'20-Smoking Status'
                    , '22-High BP Ever'
                    , '21-BMI Bucket'
                    , '23-Diabetes Ever'
                    )

weight_summary[, unique(var)]


results_tab = weight_summary[var %in% health_outcomes
               , c('var', 'level', names(weight_summary)[grepl('_prop', names(weight_summary))]), with = F]
results_tab[, pop_prop := NULL]

results_tab = cbind(results_tab[,1:2]
  ,apply(results_tab[, 3:ncol(results_tab)], 2, function(x) round(as.numeric(x) * 100, 1)))
results_tab[is.na(results_tab)] <- '-'
setnames(results_tab, c("Variable", "Level", "HSE", "UKB", "UKB IMG", "Rake", "Strat", "Calib", "LASSO", "Logit", "BART"))
results_tab[, Variable := gsub('\\d\\d-', '', Variable)]
results_tab = results_tab[order(Variable, as.character(Level))]


results_tab_kable = kable(results_tab[, -c('Variable'), with = F], booktabs = T, format = 'latex')
tablevars = unique(results_tab$Variable)

for(i in 1:length(tablevars)){
  varname = tablevars[i]
  
  rows = which(results_tab$Variable == tablevars[i])
  
  results_tab_kable = results_tab_kable %>% 
    group_rows(varname, min(rows), max(rows))
}

results_tab_kable %>% add_header_above(c(" " = 3, "UK Biobank adjusted" = 6)) %>% kable_styling(font_size = 10)


#########################################
# Make table with weighted brain volume #
#########################################

brainvol_tab = weight_summary[var == '01-Topline'
                             , c(names(weight_summary)[grepl('_brainvol', names(weight_summary))]), with = F]

setnames(brainvol_tab, c("Unweighted", "Rake", "Strat", "Calib", "LASSO", "Logit", "BART"))
kable(cbind(t(brainvol_tab),  t(brainvol_tab) - brainvol_tab$Unweighted), format = 'latex', booktabs = T)


###############################
# Make table with deff and DB #
###############################

var_melted = melt(weight_summary[var == '01-Topline']
                   , id.vars = c('var', 'level')
                   , measure.vars = names(weight_summary)[grepl('var_', names(weight_summary))]
                   , variable.name = 'method'
                   , value.name = 'variance')
var_melted[, method := gsub('var_', '', method)]
var_melted[, level := NULL]
var_melted[, var := NULL]
var_melted[, deff := round(1 + variance, 2)]
var_melted[, variance := NULL]
var_melted = rbind(var_melted, cbind('samp', 1), use.names = F)

dist_bias_table = merge(prop_melted[!method %in% c('pop', 'pop_weighted'), .(var, level, method, samp_prop = prop)], prop_melted[method == 'pop_weighted', .(var, level, pop_prop = prop)], by = c('var', 'level'))
dist_bias_table = dist_bias_table[!grepl('Diabetes|Smoking|BP|ApoE|BMI|Topline', var), .(dist_bias = round(sum((as.numeric(samp_prop) - as.numeric(pop_prop))^2, na.rm = T), 2)), by = .(method)]

all_coefs[method == 'none', method:= 'samp']


summary_tab = merge(dist_bias_table, var_melted, by = 'method')
summary_tab = merge(summary_tab, all_coefs[, .(method, age)])
summary_tab

setnames(summary_tab, c("Method", "Distribution Bias", "deff", "Beta_age"))

kable(summary_tab, format = 'latex', booktabs = T)



