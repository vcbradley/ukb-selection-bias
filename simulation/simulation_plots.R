rm(list = ls())

library(data.table)
library(ggplot2)
library(gridExtra)

setwd('~/github/mini-project-1/simulation/')
list.files('results')

which_sim = 'sim_1_5000_old_v3'

load(paste0('results/', which_sim, '/results_summary.rda'))

#set directory for plots
plot_dir = paste0('results/', which_sim, '/plots')
if(!dir.exists(plot_dir)){
  dir.create(plot_dir)
}

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

weight_summary_melted = merge(error_melted, var_melted, by = c('prop_sampled', 'var', 'level', 'sim_num', 'variable'), all = T)
weight_summary_melted = merge(weight_summary_melted, outcome_melted, by = c('prop_sampled', 'var', 'level', 'sim_num', 'variable'), all = T)

# drop pop
weight_summary_melted = weight_summary_melted[!variable %in% c('pop', 'samp'),]

# add in samp error
weight_summary_melted = merge(weight_summary_melted, weight_summary[var == 'has_t1_MRI', .(var, level, prop_sampled, sim_num, samp_error)], by = c('prop_sampled', 'var', 'level', 'sim_num'))



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

ggsave(filename = paste0(plot_dir, '/selection_bias.png'), plot = plot_selection_bias, device = 'png', width = 6, height = 2)


#############################
# PLOT Error by sample size #
#############################

plot_error_by_sampsize = ggplot(weight_summary_melted[var == 'has_t1_MRI' & variable != 'samp']
                                , aes(x = factor(round(prop_sampled * max(weight_summary$pop_count)))
                                      , y = error, color = factor(round(prop_sampled * max(weight_summary$pop_count))))) + 
  geom_boxplot() +
  facet_grid(. ~ gsub("_error","",variable)) +
  xlab('Sample size') + ylab('Post-adjustment bias') + ggtitle('Post-adjustment bias in total brain volume ') + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color = "N") 
ggsave(filename = paste0(plot_dir, '/error_by_sample_size_method.png'), plot = plot_error_by_sampsize, device = 'png', width = 10, height = 5)


################################
# PLOT Variance by sample size #
################################

plot_variance_by_sampsize = ggplot(weight_summary_melted[var == 'has_t1_MRI' & variable != 'samp']
                                , aes(x = factor(round(prop_sampled * max(weight_summary$pop_count)))
                                      , y = log(variance), color = factor(round(prop_sampled * max(weight_summary$pop_count))))) + 
  geom_boxplot() +
  facet_grid(. ~ gsub("_error","",variable)) +
  xlab('Sample size') + ylab('Log variance of weights') + ggtitle('Variance of weights by method and sample size') + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color = "N") 
ggsave(filename = paste0(plot_dir, '/variance_by_sample_size_method.png'), plot = plot_variance_by_sampsize, device = 'png', width = 10, height = 5)



##############################
# PLOT Weighted v. raw error #
##############################


plot_error_by_samp_error = ggplot(weight_summary_melted) + 
  geom_point(aes(x = samp_error, y = error)) + 
  facet_grid(prop_sampled ~ variable, labeller = labeller(vs = label_both, am = label_value)) +
  geom_hline(yintercept = 0, color = 'blue', lty = 2)
  ggtitle('Weigted bias by sample bias (total brain volume)') +
  xlab('Sample bias') + ylab('Weighted bias') +
  theme_light() +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

ggsave(filename = paste0(plot_dir, '/error_by_samp_error.png'), plot = plot_error_by_samp_error, device = 'png', width = 8, height = 10)


######################
# PLOT MSE 
#####################

ggplot(weight_summary_melted, )

plotMSE = function(data, methods, x_axis = 'pop_prop'){
  plot = ggplot(data, aes(x = get(x_axis)))
  
  plot = plot + unlist(lapply(methods, function(m){
    geom_smooth(aes(y = get(paste0(m, '_brainvol_mse')), color = m))
  }))
  
  plot = plot + ylab('Weighted error') + ggtitle('Weighted error')
  plot = plot + xlab(if(x_axis == 'samp_prop') "Samp %" else if(x_axis == 'pop_prop') "Pop %" else x_axis )
  
  return(plot)
}


print(plotMSE(mse[prop_sampled == 0.02,], methods = methods, x_axis = 'samp_prop'))



weight_summary[var == 'has_t1_MRI', ]



#######################
# PLOT Subgroup error #
#######################

plotSubgroupError = function(data, methods, p, extra_title){
  subplots = lapply(methods, function(m){
    plot = ggplot(data, aes(x = samp_brainvol_error, y = get(paste0(m, '_brainvol_mse')), size = pop_prop)) + 
      #stat_density_2d(aes(fill = ..level..), geom = "polygon") +
      geom_point() +
      xlab('log(Sample error)') + ylab('Weighted log(MSE)') +
      ggtitle(m)
  })
  
  ncol = ceiling(sqrt(length(methods)))
  nrow = ceiling(length(methods)/ncol)
  plot = marrangeGrob(subplots, ncol = ncol, nrow = nrow, top=paste0('Subgroup brain volume error', ifelse(!is.null(extra_title), extra_title, '')))
  
  return(plot)
}



for(p in all_props){
  plot_subgroup_err = plotSubgroupError(data = mse[prop_sampled == p,], methods = methods, extra_title = paste0('\nprop sampled = ', p))
  ggsave(filename = paste0(plot_dir, '/plot_subgroup_err_', p, '.pdf'), plot = plot_subgroup_err, device = 'pdf', width = 10, height = 6, units = 'in')
}


ggplot(weight_summary[prop_sampled == 0.01, .(pop_prop = min(pop_prop)
                                        , rake_error = log(sum(rake_error ^ 2))
                      , strat_error = log(sum(strat_error ^ 2))
                      , calib_error = log(sum(calib_error ^ 2))
                      , lasso_error = log(sum(lasso_error ^ 2))
                      , logit_error = log(sum(logit_error ^ 2))
                      , bart_error = log(sum(bart_error ^ 2))
                      )
                , by = .(var, level, prop_sampled)]) + 
  geom_point(aes(x = pop_prop, y = rake_error)) +
  geom_point(aes(x = pop_prop, y = strat_error)) +
  geom_point(aes(x = pop_prop, y = calib_error)) +
  geom_point(aes(x = pop_prop, y = lasso_error)) +
  geom_point(aes(x = pop_prop, y = logit_error)) +
  geom_point(aes(x = pop_prop, y = bart_error))










plot_total_error = lapply(methods, function(m){
  ymin = min(weight_summary[var == 'has_t1_MRI', lapply(.SD, min), .SDcols = paste0('var_', methods)])
  #ymax = max(total_error[, lapply(.SD, max), .SDcols = paste0('var_', methods)])
  ymax = 0.5
  
  plot = ggplot(weight_summary[var == 'has_t1_MRI',], aes(x = get(paste0(m, '_error')), y = get(paste0('var_', m)))) + 
    stat_density_2d(aes(fill = ..level..), geom = "polygon")
  
  plot = plot + ggtitle(m) + xlab(paste0(m, '_error')) + ylab(paste0('var_', m)) + theme(legend.title = element_blank())
  
  plot = plot + ylim(ymin, ymax)
  
  return(plot)
})
marrangeGrob(plot_total_error, nrow = 2, ncol = 3)



# var distributions

plot = ggplot(variance[prop_sampled == 0.5])
plot = plot + unlist(lapply(methods[-which(methods == 'logit')], function(m){
  geom_density(aes(get(paste0('var_', m)), color = m))
}))

print(plot)




mse[var == 'has_t1_MRI', .(prop_sampled
                           , rake_brainvol_mse
                           , strat_brainvol_mse
                           , calib_brainvol_mse
                           , lasso_brainvol_mse
                           , logit_brainvol_mse
                           , bart_brainvol_mse
)]

ggplot(mse[var == 'has_t1_MRI']) + 
  geom_line(aes(x = prop_sampled, y = rake_brainvol_mse, color = 'rake')) +
  geom_line(aes(x = prop_sampled, y = strat_brainvol_mse, color = 'strat')) +
  geom_line(aes(x = prop_sampled, y = calib_brainvol_mse, color = 'calib')) +
  geom_line(aes(x = prop_sampled, y = lasso_brainvol_mse, color = 'lassp')) +
  geom_line(aes(x = prop_sampled, y = logit_brainvol_mse, color = 'logit')) +
  geom_line(aes(x = prop_sampled, y = bart_brainvol_mse, color = 'bart'))



###########################
# PLOT var x prop sampled #
###########################

plot_var_x_prop_sampled = ggplot(weight_summary[var == 'has_t1_MRI' & prop_sampled < 0.7, lapply(.SD, mean), .SDcols = grepl('var_', names(weight_summary)), by = prop_sampled]) + 
  xlab('Proportion sampled') +
  ylab('Weight variance') +
  ggtitle("Variance by proportion sampled")+
  theme_classic() +
  geom_line(aes(x = prop_sampled, y = var_rake, color = 'rake')) +
  geom_line(aes(x = prop_sampled, y = var_strat, color = 'strat')) +
  geom_line(aes(x = prop_sampled, y = var_calib, color = 'calib')) +
  geom_line(aes(x = prop_sampled, y = var_lasso, color = 'lasso')) +
  geom_line(aes(x = prop_sampled, y = var_logit, color = 'logit')) +
  geom_line(aes(x = prop_sampled, y = var_bart, color = 'bart')) 

ggsave(filename = paste0(plot_dir, '/plot_var_x_prop_sampled.pdf'), plot = plot_var_x_prop_sampled, device = 'pdf', width = 10, height = 6, units = 'in')



###################
# PLOT var by MSE #
###################

variance_melted = melt(weight_summary[var == 'has_t1_MRI'], id.vars = c('prop_sampled', 'sim_num'), measure.vars = names(weight_summary)[grepl('var_', names(weight_summary))])
variance_melted = variance_melted[value > 0, .(Var = mean(value)), by = .(prop_sampled, variable)]
variance_melted[, variable := gsub('var_', '', variable)]

bias_melted = melt(weight_summary[var == 'has_t1_MRI'], id.vars = c('prop_sampled', 'sim_num'), measure.vars = names(weight_summary)[grepl('_error', names(weight_summary))])
bias_melted = bias_melted[, .(Bias = mean(value)), by = .(prop_sampled, variable)]
bias_melted[, variable := gsub('_error', '', variable)]

# mse_melted = melt(mse[var == 'has_t1_MRI', grepl('prop_sampled|mse', names(mse)), with = F], id.vars = c('prop_sampled'), value.name = 'MSE')
# mse_melted[, variable := gsub('_brainvol_mse', '', variable)]
# 
# var_and_mse = merge(mse_melted, variance_melted, by = c('prop_sampled', 'variable'))

var_and_error = merge(bias_melted, variance_melted, by = c('prop_sampled', 'variable'))


plot_var_by_bias = ggplot(var_and_error[prop_sampled < 0.7][order(variable, prop_sampled)]
                          , aes(x = Bias, y = log(Var), color = variable, size = prop_sampled)) + 
  geom_point(alpha = 0.5) + #geom_line(alpha = 0.2, size = 3) + 
  theme_classic() +
  xlab('Avg bias') + ylab('Log avg weight variance') +
  ggtitle('Variance by Bias')
plot_var_by_bias

ggsave(filename = paste0(plot_dir, '/plot_var_by_bias.pdf'), plot = plot_var_by_bias, device = 'pdf', width = 10, height = 6, units = 'in')


var_and_mse[order(variable)]

############################
# Var x MSE x Prop sampled #
############################

plot_list = lapply(sort(all_props[all_props < 0.75]), function(p){
  mean_error = weight_summary[var == 'has_t1_MRI'& prop_sampled == p, lapply(.SD, mean), .SDcols = paste0(methods, '_error')]
  mean_var = weight_summary[var == 'has_t1_MRI'& prop_sampled == p, lapply(.SD, mean), .SDcols = paste0('var_',methods)]
  summary = data.frame(methods, error = as.numeric(mean_error), var = as.numeric(mean_var))
  
  actual_bias = weight_summary[var == 'has_t1_MRI' & prop_sampled == p, .(actual_bias = mean(samp_brainvol - pop_brainvol))]
  
  ggplot(weight_summary[var == 'has_t1_MRI'& prop_sampled == p]) + 
    geom_hline(yintercept = actual_bias$actual_bias, color = 'black', lty = 2) +
    geom_hline(yintercept = 0, color = 'black') +
    
    geom_point(aes(y = rake_error, x = var_rake, color = 'rake'), alpha = 0.1) +
    geom_point(aes(y = strat_error, x = var_strat, color = 'strat'), alpha = 0.1) +
    geom_point(aes(y = calib_error, x = var_calib, color = 'lasso'), alpha = 0.1) +
    geom_point(aes(y = calib_error, x = var_calib, color = 'calib'), alpha = 0.1) +
    geom_point(aes(y = logit_error, x = var_logit, color = 'logit'), alpha = 0.1) +
    geom_point(aes(y = bart_error, x = var_bart, color = 'bart'), alpha = 0.1) +
    
    geom_point(data = summary, aes(y = error, x = var, fill = methods), color = 'black', shape = 24, size = 2, alpha = 1) +
    ggtitle(paste0("Prop=",p)) + theme_light() + #theme(legend.position = "none") +
    ylim(-50000,25000) + xlim(0,10) + guides(colour=FALSE)
    
  
})

marrangeGrob(plot_list, ncol = 3, nrow = 3, top=paste0('Error x Variance'))


###############
# BY DEMOS #

ggplot(weight_summary[var == 'demo_age_bucket' & prop_sampled == 0.02,]) +
  geom_boxplot(aes(x = level, y = samp_error, color = 'prop_sampled'))


weight_summary_melted = melt(weight_summary[var == 'has_t1_MRI' & prop_sampled == 0.02,], id.vars = c('level', 'prop_sampled', 'sim_num')
                       , measure.vars = c('pop_brainvol', 'samp_brainvol'))

ggplot(weight_summary[var == 'has_t1_MRI']) +
  geom_boxplot(aes(x = level, y = samp_error))


weight_summary[var == 'has_t1_MRI', summary(samp_error)]
  

