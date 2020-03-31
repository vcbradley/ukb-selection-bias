rm(list = ls())

library(data.table)
library(ggplot2)
library(gridExtra)
library(knitr)


wd = getwd()
which_sim = gsub('/gpfs2/well/nichols/users/bwj567/simulation/', '', wd)


### BRAIN VOL DATA
# load brain vol data
load(paste0('results/', which_sim, '/results_summary.rda'))

### APOE DATA
# pop_coefs, pop_p_vals
load(paste0('results/', which_sim, '/apoe_reg_popvals.rda'))

# load apoe coef data
apoe_coef = fread(paste0('results/', which_sim, '/apoe_reg_coef_all.csv'))[, -1, with = F]
#apoe_pval = fread(paste0('results/', which_sim, '/apoe_reg_pval_all.csv'))[, -1, with = F]

#set directory for plots
plot_dir = paste0('results/', which_sim, '/plots')
if(!dir.exists(plot_dir)){
  dir.create(plot_dir)
}


### OTHER PREP
#get list of methods
methods = gsub('_brainvol','',names(weight_summary)[grepl('brainvol', names(weight_summary))])
methods = methods[-which(methods %in% c('pop', 'samp'))]

all_props = unique(weight_summary[, prop_sampled])

weight_summary[, .N, prop_sampled]



#### create melted df
error_melted = melt(weight_summary, id.vars = c('prop_sampled', 'var', 'level', 'sim_num'),value.name = 'error', measure.vars = names(weight_summary)[grepl('error', names(weight_summary))])
error_melted[, variable := gsub('_error', '', variable)]

var_melted = melt(weight_summary, id.vars = c('prop_sampled', 'var', 'level', 'sim_num'), value.name = 'variance', measure.vars = names(weight_summary)[grepl('var_', names(weight_summary))])
var_melted[, variable := gsub('var_', '', variable)]

outcome_melted = melt(weight_summary, id.vars = c('prop_sampled', 'var', 'level', 'sim_num'),value.name = 'MRI_brain_vol', measure.vars = names(weight_summary)[grepl('brainvol', names(weight_summary))])
outcome_melted[, variable := gsub('_brainvol', '', variable)]

prop_melted = melt(weight_summary, id.vars = c('prop_sampled', 'var', 'level', 'sim_num'),value.name = 'weighted_prop', measure.vars = names(weight_summary)[grepl('_prop', names(weight_summary))])
prop_melted[, variable := gsub('_prop', '', variable)]


weight_summary_melted = merge(error_melted, var_melted, by = c('prop_sampled', 'var', 'level', 'sim_num', 'variable'), all = T)
weight_summary_melted = merge(weight_summary_melted, outcome_melted, by = c('prop_sampled', 'var', 'level', 'sim_num', 'variable'), all = T)
weight_summary_melted = merge(weight_summary_melted, prop_melted, by = c('prop_sampled', 'var', 'level', 'sim_num', 'variable'), all = T)

# drop pop
weight_summary_melted = weight_summary_melted[!variable %in% c('pop', 'samp'),]

# add in samp error
weight_summary_melted = merge(weight_summary_melted, weight_summary[, .(pop_prop = min(pop_prop), samp_prop = min(samp_prop), samp_error = min(samp_error)), by = .(var, level, prop_sampled, sim_num)], by = c('prop_sampled', 'sim_num', 'var','level'))

#check dupes
weight_summary_melted[,.N, .(prop_sampled, sim_num, var, level, variable)][, max(N)]


#######################
# PLOT Selection Bias #
#######################
plot_selection_bias = ggplot(weight_summary_melted[variable == 'bart' & var == 'has_t1_MRI',], aes(x = samp_error)) + 
  geom_histogram() + 
  facet_grid(.~prop_sampled) +
  geom_vline(xintercept = 0, color = 'blue', lty = 2) + 
  ggtitle('Selection bias in total brain volume') +
  xlab('Total brain volume selection bias') +
  theme_light() +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = paste0(plot_dir, '/selection_bias.png'), plot = plot_selection_bias, device = 'png', width = 8, height = 2)


#############################
# PLOT Error by sample size #
#############################

plot_error_by_sampsize = ggplot(weight_summary_melted[var == 'has_t1_MRI' & variable != 'samp']
                                , aes(x = factor(round(prop_sampled * max(weight_summary$pop_count)))
                                      , y = error, color = factor(round(prop_sampled * max(weight_summary$pop_count))))) + 
  geom_boxplot() +
  facet_grid(. ~ variable) +
  xlab('Sample size') + ylab('Post-adjustment bias') + 
  ggtitle('Post-adjustment bias in total brain volume ') + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color = "N") 
ggsave(filename = paste0(plot_dir, '/error_by_sample_size_method.png'), plot = plot_error_by_sampsize, device = 'png', width = 10, height = 4)


################################
# PLOT Variance by sample size #
################################

plot_variance_by_sampsize = ggplot(weight_summary_melted[var == 'has_t1_MRI' & variable != 'samp']
                                , aes(x = factor(round(prop_sampled * max(weight_summary$pop_count)))
                                      , y = log(1+variance), color = factor(round(prop_sampled * max(weight_summary$pop_count))))) + 
  geom_boxplot() +
  facet_grid(. ~ gsub("_error","",variable)) +
  xlab('Sample size') + ylab('Log deff of weights') + ggtitle('Design effect of weights by method and sample size') + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color = "N") 
ggsave(filename = paste0(plot_dir, '/variance_by_sample_size_method.png'), plot = plot_variance_by_sampsize, device = 'png', width = 10, height = 4)



##############################
# PLOT Weighted v. raw error #
##############################


plot_error_by_samp_error = ggplot(weight_summary_melted[var == 'has_t1_MRI']) + 
  geom_point(aes(x = samp_error, y = error)) + 
  facet_grid(prop_sampled ~ variable, labeller = labeller(vs = label_both, am = label_value)) +
  geom_hline(yintercept = 0, color = 'blue', lty = 2) +
  ggtitle('Weigted bias by sample bias (total brain volume)') +
  xlab('Sample bias') + ylab('Weighted bias') +
  theme_light() +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

ggsave(filename = paste0(plot_dir, '/error_by_samp_error.png'), plot = plot_error_by_samp_error, device = 'png', width = 8, height = 10)


############
# PLOT MSE #
############
plot_MSE_brainvol = ggplot(weight_summary_melted[var == 'has_t1_MRI', .(MSE = mean(error ^2)), by = .(prop_sampled, variable)], 
       aes(x = prop_sampled, y = log(MSE), color = variable)) + geom_line() +
  theme_light() +
  ggtitle("MSE of total brain volume by proportion sampled") +
  xlab('Proportion sampled') + ylab("Log MSE")

ggsave(filename = paste0(plot_dir, '/mse_by_sample_size.png'), plot = plot_MSE_brainvol, device = 'png', width = 6, height = 3)


###################
# BIAS & DEFF #
###################
plot_bias_deff = ggplot(weight_summary_melted[var == 'has_t1_MRI', .(avg_bias = mean(error), med_deff = median(1+variance)),by=.(prop_sampled, variable)]
       , aes(x = avg_bias, y = log(med_deff), color = variable, size = prop_sampled)) + 
  geom_point(alpha = 0.5) +
  ggtitle("Design effect and bias in total brain volume") +
  geom_vline(xintercept = 0, color = 'blue', lty = 2) +
  theme_light() +
  ggtitle("Design effect and total brain volume bias") +
  xlab("Average bias") +
  ylab("Log median design effect")

ggsave(filename = paste0(plot_dir, '/bias_deff.png'), plot = plot_bias_deff, device = 'png', width = 6, height = 3)



#######################
# PLOT Subgroup error #
#######################

vars = c('demo_sex','demo_age_bucket')
ggplot(weight_summary_melted[var %in% vars & prop_sampled == 0.01, ], aes(x = variable, y = error)) + geom_boxplot() +
  facet_grid(.~level)


plot_subgroup_mse = ggplot(weight_summary_melted[, .(
  log_mse = log(mean(error^2))
  , log_deff = mean(1+ifelse(is.na(variance), 0, variance))
  , pop_prop = min(pop_prop)
), by = .(prop_sampled,variable, var, level)]
, aes(y = log_mse, x = pop_prop, color = variable)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE) +
  ggtitle("Subgroup MSE by % of the population") +
  facet_wrap(~prop_sampled, labeller = label_both) +
  xlab("% of population") +
  ylab("Log MSE") +
  theme_light()

ggsave(filename = paste0(plot_dir, '/subgroup_mse.png'), plot = plot_subgroup_mse, device = 'png', width = 10, height = 6)


plot_subgroup_err_age = ggplot(weight_summary_melted[prop_sampled == 0.01 & var == 'demo_age_bucket']) +
  geom_boxplot(aes(x = variable, y = error, color = variable))+ #coord_flip() + 
  facet_grid(.~level) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total brain volume bias by age group - 1% sampled") +
  xlab("Method") +
  ylab("Mean bias") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = paste0(plot_dir, '/subgroup_bias_age.png'), plot = plot_subgroup_err_age, device = 'png', width = 10, height = 3)

plot_subgroup_err_income = ggplot(weight_summary_melted[prop_sampled == 0.01 & var == 'demo_income_bucket']) +
  geom_boxplot(aes(x = variable, y = error, color = variable))+ #coord_flip() + 
  facet_grid(.~level) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total brain volume bias by income bracket - 1% sampled") +
  xlab("Method") +
  ylab("Mean bias") + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = paste0(plot_dir, '/subgroup_bias_income.png'), plot = plot_subgroup_err_income, device = 'png', width = 10, height = 3)





###########################
# PLOT Distribution Error #
###########################

plot_dist_error = ggplot(weight_summary_melted[, .(dist_error = sum((weighted_prop - pop_prop)^2))
                             , by = .(prop_sampled, sim_num, variable)]) +
  geom_boxplot(aes(x = factor(round(prop_sampled * max(weight_summary$pop_count)))
                   , y = log(dist_error)
                   , color = factor(round(prop_sampled * max(weight_summary$pop_count))))) + 
  facet_grid(.~variable) +
  labs(color = "N")  +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sample size") +
  ylab("Log distribution error") +
  ggtitle("Distribution error by method and sample size")

ggsave(filename = paste0(plot_dir, '/distribution_error.png'), plot = plot_dist_error, device = 'png', width = 10, height = 4)



######################
# ApoE and Age Coefs #
######################

# apoe_coef[apoe_coef[method == 'none',], samp_Male := i.Male,on = c('prop_sampled', 'sim_num')]
# apoe_coef[apoe_coef[method == 'none',], samp_Female := i.Female,on = c('prop_sampled', 'sim_num')]
# apoe_coef[apoe_coef[method == 'none',], `samp_ApoE e4/e4` := `i.ApoE e4/e4`, on = c('prop_sampled', 'sim_num')]
# apoe_coef[apoe_coef[method == 'none',], `samp_Female:ApoE e4/e4` := `i.Female:ApoE e4/e4`, on = c('prop_sampled', 'sim_num')]
apoe_coef[apoe_coef[method == 'none',], samp_age := i.age, on = c('prop_sampled', 'sim_num')]
apoe_coef[, pop_age := pop_coefs$age]

###### Bias exists in association
plot_bias_age = ggplot(data = apoe_coef[method == 'none'], aes(x = (age - pop_age))) + 
  geom_histogram() + 
  facet_grid(.~prop_sampled) +
  geom_vline(xintercept = 0, color = 'blue', lty = 2) + 
  ggtitle('Selection bias in age coefficient') +
  xlab('Bias in age coefficient') +
  theme_light() +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = paste0(plot_dir, '/selection_bias_assoc.png'), plot = plot_bias_age, device = 'png', width = 8, height = 2)

####### WE generally correct it
# plot_bias_age_brain_vol = ggplot(apoe_coef[method != 'none'], aes(x = (samp_Age - pop_Age), y = `Error Age`)) + 
#   geom_point() + facet_grid(prop_sampled ~ method) +
#   xlab('Bias in unweighted age coefficient') +
#   ylab('Bias in weighted age coefficient') +
#   ggtitle("Bias in age and total brain volume association")

# plot_bias_age_brain_vol = ggplot(apoe_coef[method != 'none', .(mean_samp_error = mean(samp_age - pop_age)
#                                      , mean_weighted_error = mean(`Error age`, na.rm = T)
#                                      #, var_samp_error = var(samp_Age - pop_Age, na.rm = T)
#                                      ), by = c('prop_sampled', 'method')], aes(x = mean_samp_error
#                                         , y = mean_weighted_error
#                                         , size = prop_sampled
#                                         , color = method)) +
#   geom_point() +
#   xlab("Mean unweighted sample bias") +
#   ylab("Mean adjusted sample bias") +
#   ggtitle("Sample bias in brain vol and age association")

#ggsave(filename = paste0(plot_dir, '/bias_age_brainvol.png'), plot = plot_bias_age_brain_vol, device = 'png', width = 6, height = 3)

###### Which method does best
plot_bias_age_brainvol = ggplot(apoe_coef[method != 'none'], aes(x = factor(round(prop_sampled * 20827)), y = `Error age`
                                                                 , color = factor(round(prop_sampled * 20827)))) +
  geom_boxplot() +
  facet_grid(.~method) +
  #geom_hline(yintercept = -300) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Adjusted bias in total brain vol and age association") +
  xlab("Sample size") +
  ylab("Bias in age coefficient") +
  labs(color = "N") +
  ylim(c(-15000, 15000))

ggsave(filename = paste0(plot_dir, '/bias_age_brainvol.png'), plot = plot_bias_age_brainvol, device = 'png', width = 10, height = 4)


plot_MSE_assoc = ggplot(apoe_coef[method != 'none', .(MSE = mean(`Error age` ^2)), by = .(prop_sampled, method)], 
                           aes(x = prop_sampled, y = log(MSE), color = method)) + geom_line() +
  theme_light() +
  ggtitle("MSE of age coefficient by proportion sampled") +
  xlab('Proportion sampled') + ylab("Log MSE")

ggsave(filename = paste0(plot_dir, '/mse_age_brainvol.png'), plot = plot_MSE_assoc, device = 'png', width = 6, height = 3)


#####################
# PLOT: Median Deff #
#####################

pres_plot_data = weight_summary_melted[var == 'has_t1_MRI', .(avg_bias = mean(abs(error)), med_deff = log(median( 1+ variance))),by=.(prop_sampled, variable)]
setnames(pres_plot_data, old = 'variable', new = 'method')
pres_plot_data_melted = melt(pres_plot_data, id.vars = c('prop_sampled','method'))

pres_plot_data_melted[variable == 'avg_bias', variable := 'Average absolute bias']
pres_plot_data_melted[variable == 'med_deff', variable := 'Log median design effect']

plot_pres_results = ggplot(pres_plot_data_melted, aes(x = prop_sampled, y = value, color = method)) + 
  #geom_hline(data = data.frame(variable = 'Average absolute bias', Z = 0), aes(yintercept = Z), color = 'black', lty = 2) +
  facet_grid(variable~., scales="free") +
  geom_line() +
  ylab("") +
  xlab("proportion sampled") +
  ggtitle("Bias and deff by weighting method") +
  theme_minimal()
  

ggsave(filename = paste0(plot_dir, '/sim_results_presentation.png'), plot = plot_pres_results, device = 'png', width = 6, height = 5)



  